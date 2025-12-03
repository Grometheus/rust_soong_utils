use crate::libsoong::blueprint_parser::ASTGenerator;

mod libsoong;



fn main(){
    let mut ast_gen = ASTGenerator::from("").unwrap();
    for n in ast_gen{
        dbg!(n.unwrap());
    }



}







