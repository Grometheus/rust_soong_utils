/*
This file is part of the Grometheus project
Copyright (C) PsychedelicPalimpsest - 2025

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

//=============================
//           Notes
//=============================
//
// This file is responsible for the progressive
// evaluation system.

use std::{cell::RefCell, collections::HashMap, mem, path::PathBuf, rc::Rc};

use crate::libsoong::blueprint_parser::{BlueprintArena, BlueprintValue, BlueprintValueRef};

/// Recursivly move `from` into the arena `to`. Skipping any that are already in `to`
///
/// Do not use needlessly. This is O(N) and very recursive.
fn flatten_blueprint_arenas(from: BlueprintValueRef, to: BlueprintArena) -> BlueprintValueRef {
    use BlueprintValue::*;

    macro_rules! in_arena {
        ($v:expr) => {
            to.alloc(RefCell::from($v))
        };
    }

    if from.get_arena().unwrap() == to {
        return from;
    }

    match &mut *from.borrow_mut() {
        val @ (Integer(_) | UnknownIdentifer(_) | String(_)) => in_arena!(val.clone()),
        Negative(child) => in_arena!(Negative(flatten_blueprint_arenas(
            child.clone(),
            to.clone()
        ))),
        Map(mapping) => in_arena!(Map(mem::take(mapping)
            .into_iter()
            .map(|(k, v)| (k, flatten_blueprint_arenas(v, to.clone())))
            .collect())),
        List(listing) => in_arena!(List(
            mem::take(listing)
                .into_iter()
                .map(|v| flatten_blueprint_arenas(v, to.clone()))
                .collect()
        )),
        Add(x, y) => in_arena!(Add(
            flatten_blueprint_arenas(x.clone(), to.clone()),
            flatten_blueprint_arenas(y.clone(), to.clone())
        )),
    }
}

pub fn evaluate_value(
    value: BlueprintValueRef,
    arena: &BlueprintArena,
    state: &RefCell<DirState>,
) -> BlueprintValueRef {
    use BlueprintValue::*;
    match &mut *value.clone().borrow_mut() {
        Integer(_) | String(_) => value,
        UnknownIdentifer(id) => {
            match state.borrow().get_variable(&id) {
                Some(val) => val,
                None => value
            }

        }
        Negative(child) => {
            let new_child = evaluate_value(child.clone(), arena, state);

            if !new_child.ptr_eq(child) {
                // Put the new value in where the old one was!
                // This way we dont leak the value itself, but any changed values in the tree are
                // still leaked to the Arena.
                *child = new_child;
            }
            value
        }
        List(listing) => {
            let new_listing = mem::take(listing)
                .iter()
                .map(|value| evaluate_value(value.clone(), arena, state))
                .collect();

            *listing = new_listing;
            value
        }
        Map(mapping) => {
            let new_mapping = mem::take(mapping)
                .into_iter()
                .map(|(k, v)| (k, evaluate_value(v.clone(), arena, state)))
                .collect();

            *mapping = new_mapping;
            value
        }

        Add(a, b) => {
            let new_a = evaluate_value(a.clone(), arena, state);
            let new_b = evaluate_value(b.clone(), arena, state);

            if matches!(*new_a.borrow(), UnknownIdentifer(_) | Add(_, _))
                || matches!(*new_b.borrow(), UnknownIdentifer(_) | Add(_, _))
            {
                mem::replace(a, new_a);
                mem::replace(b, new_b);
                return value;
            }
            let mut new_a_ref = new_a.borrow_mut();
            match (&mut *new_a_ref, &mut *new_b.borrow_mut()) {
                (Integer(x), Integer(y)) => {
                    let result = Integer(x.wrapping_add(y.clone()));

                    // Lets us move result into a
                    drop(new_a_ref);
                    *new_a.borrow_mut() = result;

                    new_a
                }
                (String(x), String(y)) => {
                    x.push_str(&y);

                    drop(new_a_ref);
                    new_a
                }
                (List(x), List(y)) => {
                    x.append(y);

                    drop(new_a_ref);
                    new_a
                }
                (Map(x), Map(y)) => {
                    x.reserve(y.len());
                    for (k, v) in mem::take(y) {
                        x.insert(k, v.clone());
                    }

                    drop(new_a_ref);
                    new_a
                }
                _ => todo!(),
            }
        }
    }
}

struct FileState {}

pub struct DirState {
    parent: Option<Rc<RefCell<DirState>>>,

    variables: HashMap<String, BlueprintValueRef>,

    // NOTE ON ARENA USAGE: This does cause a small memory leaks, which can only be repaired by the
    // `move_blueprint_values` method!
    arena: BlueprintArena,

    files: HashMap<String, RefCell<FileState>>,
}
impl DirState {
    pub fn new(parent: Option<Rc<RefCell<DirState>>>) -> DirState {
        DirState {
            parent,
            variables: HashMap::new(),

            arena: BlueprintArena::new(),
            files: HashMap::new(),
        }
    }

    pub fn set_variable(&mut self, name: String, val: BlueprintValueRef) {
        self.variables.insert(name, val);
    }
    pub fn get_variable(&self, name : &str) -> Option<BlueprintValueRef> {
        let value = self.variables.get(name).map(|x| x.clone());
        
        if matches!(value, Some(_)) {
            return value;
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().get_variable(name);
        }

        None

    }
}

pub struct EvaluationTree {
    dirs: HashMap<PathBuf, Rc<RefCell<DirState>>>,
}
