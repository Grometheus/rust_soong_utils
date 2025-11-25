mod libsoong;



fn main(){

        dbg!(libsoong::blueprint_parser::tokenize("

1    Foo : \"Foo foo bar\"



    ")).unwrap();

    let x : usize = 1;
}







