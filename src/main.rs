use std::{fs::File, io::Read};

use crate::libsoong::blueprint_parser::ASTGenerator;

mod libsoong;



fn main(){
    let mut f = File::open("tests/Android.bp").unwrap();
    let mut str = String::new();
    f.read_to_string(&mut str).unwrap();

    let mut ast_gen = ASTGenerator::from(&*str).unwrap();
    for n in ast_gen{
        dbg!(n.unwrap());
    }



}







