use std::{cell::RefCell, rc::Rc};

use crate::libsoong::{blueprint_evaluator::{evaluate_value, DirState}, blueprint_parser::ASTGenerator};

mod libsoong;

fn main() {
    let genn = ASTGenerator::from("x ={a:a} + {} + {b:4}").unwrap();
    let arena = genn.get_arena();

    let ds = RefCell::new(DirState::new(None));

    for line in genn {
        match line.unwrap() {
            libsoong::blueprint_parser::ASTLine::VarSet(_, v) => {
                dbg!(evaluate_value(v, &arena, &ds));


            }
            _ => unreachable!(),
        }
    }
}
