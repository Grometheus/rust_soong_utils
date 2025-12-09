use std::path::Path;

mod libsoong;

fn main() {
    let mut state = libsoong::blueprint_evaluator::EvaluationState::new();
    let mut x = vec![1, 2, 3];
    x.leak();

    state.injest_directory(Path::new("tests")).unwrap();
}
