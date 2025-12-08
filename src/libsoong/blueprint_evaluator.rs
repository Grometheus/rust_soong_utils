use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::metadata;
use std::fs::read_dir;
use std::fs::DirEntry;
use std::fs::File;
use std::io;
use std::io::Read;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use std::rc::Weak;

use crate::libsoong::blueprint_parser::*;
use crate::libsoong::errors::*;
use anyhow::{Context, Result};

#[derive(Debug)]
struct DirState {
    parent: Option<Weak<RefCell<DirState>>>,

    variables: HashMap<String, BlueprintValue>,
}
impl DirState {
    fn new(parent: Option<Weak<RefCell<DirState>>>) -> DirState {
        DirState {
            parent,
            variables: HashMap::new(),
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<BlueprintValue> {
        if let Some(val) = self.variables.get(name) {
            return Some((*val).clone());
        }

        match &self.parent {
            None => None,
            Some(parent) => {
                let parent = parent
                    .upgrade()
                    .expect("Something went wrong with the evaluation state");
                parent.borrow().lookup_variable(name)
            }
        }
    }

    #[inline(always)]
    fn insert_variable(&mut self, name: String, value: BlueprintValue) -> Option<BlueprintValue> {
        return self.variables.insert(name, value);
    }
}

#[derive(Debug)]
pub struct EvaluationState {
    dir_states: HashMap<PathBuf, Rc<RefCell<DirState>>>,
}

#[derive(Debug)]
pub struct FileState {
    parent_dir_state: Rc<RefCell<DirState>>,
    rules: HashMap<String, HashMap<String, BlueprintValue>>,
}

#[inline]
fn neg_value_eval(
    state: Rc<RefCell<DirState>>,
    inner_value: BlueprintValue,
) -> Result<BlueprintValue> {
    use BlueprintValue::*;
    match value_evaluate(state, inner_value)? {
        Integer(num) => {
            return Ok(Integer(-num));
        }
        UnknownIdentifer(ident) => Ok(UnknownIdentifer(ident)),

        val @ _ => Err(ParseError::from(
            ParseErrorType::UnexpectedValue,
            format!("Cannot negate value of type {:?}", val),
        )
        .into()),
    }
}

#[inline]
fn unknown_identifier_eval(state: Rc<RefCell<DirState>>, name: String) -> Result<BlueprintValue> {
    use BlueprintValue::*;
    let value = match state.borrow().lookup_variable(&name) {
        None => {
            return Ok(UnknownIdentifer(name));
        }
        Some(val) => val,
    };
    value_evaluate(state, value)
}
#[inline]
fn map_eval(
    state: Rc<RefCell<DirState>>,
    mut mapping: HashMap<String, BlueprintValue>,
) -> Result<BlueprintValue> {
    use BlueprintValue::*;

    for (_, value) in mapping.iter_mut() {
        *value = value_evaluate(state.clone(), value.clone())?;

    }

    Ok(Map(mapping))
}

#[inline]
fn list_eval(state: Rc<RefCell<DirState>>, list: Vec<BlueprintValue>) -> Result<BlueprintValue> {
    use BlueprintValue::*;
    let mut new_vec = Vec::with_capacity(list.capacity());
    for value in list {
        new_vec.push(value_evaluate(state.clone(), value)?);
    }

    Ok(List(new_vec))
}
#[inline]
fn add_eval(
    state: Rc<RefCell<DirState>>,
    a: BlueprintValue,
    b: BlueprintValue,
) -> Result<BlueprintValue> {
    use BlueprintValue::*;
    let a = value_evaluate(state.clone(), a)?;
    let b = value_evaluate(state.clone(), b)?;

    Ok(match (a, b) {
        (Integer(a), Integer(b)) => Integer(a.saturating_add(b)),
        (String(a), String(b)) => String(a + &b),
        (List(mut a), List(mut b)) => {
            a.append(&mut b);
            List(a)
        }
        (Map(mut a), Map(mut b)) => {
            a.extend(b);
            Map(a)
        }

        // Allow for unresolved variables
        (
            a @ ((Integer(_) | String(_) | List(_) | Map(_)) | UnknownIdentifer(_)),
            b @ ((Integer(_) | String(_) | List(_) | Map(_)) | UnknownIdentifer(_)),
        ) => Add(Box::from(a), Box::from(b)),

        (a, b) => {
            return Err(ParseError::from(
                ParseErrorType::UnexpectedValue,
                format!("It is invalid to add {:?} and {:?}", a, b),
            )
            .into())
        }
    })
}

fn value_evaluate(state: Rc<RefCell<DirState>>, value: BlueprintValue) -> Result<BlueprintValue> {
    use BlueprintValue::*;
    match value {
        Integer(_) | String(_) => Ok(value),
        UnknownIdentifer(name) => unknown_identifier_eval(state, name),

        // used for unknown identifiers
        Negative(val) => neg_value_eval(state, val.as_ref().clone()),

        Map(mapping) => map_eval(state, mapping),
        List(list) => list_eval(state, list),

        Add(a, b) => add_eval(state, a.as_ref().clone(), b.as_ref().clone()),
    }
}

fn file_evaluate(state: Rc<RefCell<DirState>>, file: &str) -> Result<FileState> {
    let mut out = FileState {
        parent_dir_state: state.clone(),
        rules: HashMap::with_capacity(64),
    };
    for line in ASTGenerator::from(file)? {
        let line = line?;

        match line {
            ASTLine::VarSet(ident, value) => {
                let value = value_evaluate(state.clone(), value)?;

                state.borrow_mut().insert_variable(ident, value);
            }
            ASTLine::VarAddSet(ident, value) => {
                let original = state.borrow().lookup_variable(&ident).with_context(|| {
                    format!("Add set failed for undefined variable: '{}'", ident)
                })?;

                // TODO: This can be refactored to remove the heap
                let value = add_eval(state.clone(), original, value)?;

                state.borrow_mut().insert_variable(ident, value);
            }
            ASTLine::Rule(ident, value) => {
                let value = match map_eval(state.clone(), value)? {
                    BlueprintValue::Map(map) => map,
                    _ => unreachable!(),
                };
                out.rules.insert(ident, value);
            }
        };
    }
    Ok(dbg!(out))
}

impl EvaluationState {
    pub fn new() -> EvaluationState {
        EvaluationState {
            dir_states: HashMap::new(),
        }
    }

    fn get_or_create_dir_state(&mut self, dir: &Path) -> Rc<RefCell<DirState>> {
        if let Some(r) = self.dir_states.get(dir) {
            return r.clone();
        }

        let parent = dir
            .parent()
            .map(|parent| Rc::downgrade(&self.get_or_create_dir_state(parent)));

        let r = Rc::new(RefCell::new(DirState::new(parent)));

        self.dir_states.insert(dir.to_path_buf(), r.clone());
        r
    }

    pub fn injest_file(&mut self, path: &Path) -> Result<(FileState)> {
        self.injest_file_to_path(path, path)
    }
    pub fn injest_file_to_path(
        &mut self,
        path: &Path,
        internel_path: &Path,
    ) -> Result<(FileState)> {
        let mut f =
            File::open(dbg!(path)).with_context(|| format!("Cannot open file during injestion"))?;
        let mut file_contents = String::new();

        f.read_to_string(&mut file_contents).with_context(|| {
            format!(
                "Cannot read file contents during injestion: {}",
                path.to_str().unwrap_or("")
            )
        })?;

        let state = self.get_or_create_dir_state(path.parent().expect("Parent dir must exist"));

        file_evaluate(state, &file_contents).with_context(|| {
            format!(
                "Parsing error during injestion: {}",
                path.to_str().unwrap_or("")
            )
        })
    }

    pub fn injest_directory(&mut self, dir: &Path) -> Result<()> {
        self.injest_directory_to_path(dir, &dir)
    }
    pub fn injest_directory_to_path(&mut self, dir: &Path, internel_dir: &Path) -> Result<()> {
        // List the directory, moving errors outwards
        let dir_listing: Result<Vec<DirEntry>, io::Error> = read_dir(dir)
            .with_context(|| format!("Cannot read dir {}", dir.to_str().unwrap_or("")))?
            .collect();
        // Bubble it up
        let mut dir_listing = dir_listing?;
        // We must first sort, this give us a canonical way of evaluating variables
        dir_listing.sort_by_cached_key(|a| a.file_name());

        for entry in dir_listing {
            let meta = entry.metadata()?;
            let file_path = entry.path();

            // Safty: The path should never be empty
            let file_name = file_path.file_name().unwrap();

            if meta.is_dir() {
                self.injest_directory_to_path(&file_path, &internel_dir.join(file_name))?
            } else if (meta.is_file() && file_name.as_bytes().ends_with(b".bp")) {
                self.injest_file_to_path(&dir.join(file_name), &internel_dir.join(file_name))?;
            }
        }

        Ok(())
    }
}
