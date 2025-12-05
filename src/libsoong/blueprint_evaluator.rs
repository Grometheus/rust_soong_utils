use std::collections::HashMap;
use std::fs::metadata;
use std::fs::read_dir;
use std::fs::DirEntry;
use std::io;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;

use crate::libsoong::blueprint_parser::*;
use crate::libsoong::errors::*;
use anyhow::{Context, Result};


struct DirState {

}
impl DirState {
    fn new() -> DirState{
        DirState {  }
    }

}

pub struct EvaluationState {
    dir_states : HashMap<PathBuf, Rc<DirState>>

}

impl EvaluationState {
    pub fn new() -> EvaluationState {
        EvaluationState {
            dir_states :HashMap::new()
        }
    }

    fn get_or_create_dir_state(&mut self, dir: &Path) -> Rc<DirState> {
        match self.dir_states.get(dir) {
            Some(r) => r.clone(),
            None => {
                let r = Rc::new(DirState::new());
            
                self.dir_states.insert(dir.to_path_buf(), r.clone());
                r
            }
        }
}

    pub fn injest_file(&mut self, path: &Path) -> Result<()> {
        self.injest_file_to_path(path, path)
    }
    pub fn injest_file_to_path(&mut self, path: &Path, internel_path: &Path) -> Result<()> {
        path.to_path_buf();

        let state = self.get_or_create_dir_state(path.parent().unwrap());


        todo!()
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
