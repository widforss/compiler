mod parser;

use parser::{Ast, Error};
use std::env;
use std::fs;

fn main() {
    let input = match file_content() {
        Ok(contents) => contents,
        Err(err) => {
            println!("{}", err);
            return;
        },
    };
    let ast = Ast::parse(&input);
    print_ast(ast);
}

fn file_content() -> Result<String, String> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(String::from("Exactly one argument expected (name of file to parse)!"));
    }
    let filename = args.get(1).unwrap();
    match fs::read_to_string(filename) {
        Ok(string) => Ok(string),
        Err(_) => Err(String::from("Could not read input file!")),
    }
}

fn print_ast(ast: Result<Ast, Error>) {
    match ast {
        Ok(ast) => {
            print!("{}", ast);
        }
        Err(Error {
            span: Some(span),
            error,
            ..
        }) => println!(
            "{} near line {}, column {}:\n    {}",
            error.description(),
            span.line,
            span.get_utf8_column(),
            span.fragment
                .split("\n")
                .collect::<Vec<&str>>()
                .get(0)
                .unwrap(),
        ),
        _ => panic!("Unhandled error!"),
    };
}
