use std::path::Path;

mod libsoong;

fn main() {
    let mut state = libsoong::blueprint_evaluator::EvaluationState::new();

    state.injest_directory(Path::new("tests")).unwrap();
}
