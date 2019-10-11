mod ast;

use ast::{interpreter, parser, Ast, Expr, Span, Value};
use std::env;
use std::fs;

fn main() {
    let input = match file_content() {
        Ok(contents) => contents,
        Err(err) => {
            println!("{}", err);
            return;
        }
    };
    let ast = Ast::parse(&input);
    print_ast(&ast);

    let ast = match ast {
        Ok(ast) => ast,
        Err(_) => return,
    };

    let main_call = Expr {
        value: Value::Call(Span::new("main"), vec![]),
        span: Span::new("main()"),
    };
    let result = ast.run(&main_call);
    print_intprt(&result);
}

fn file_content() -> Result<String, String> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(String::from(
            "Exactly one argument expected (name of file to parse)!",
        ));
    }
    let filename = args.get(1).unwrap();
    match fs::read_to_string(filename) {
        Ok(string) => Ok(string),
        Err(_) => Err(String::from("Could not read input file!")),
    }
}

fn print_ast(ast: &Result<Ast, parser::Error>) {
    match ast {
        Ok(ast) => {
            print!("{}", ast);
        }
        Err(parser::Error {
            span: Some(span),
            error,
            ..
        }) => print_err(error.description(), *span),
        _ => panic!("Unhandled error!"),
    };
}

fn print_intprt(result: &Result<(), interpreter::Error>) {
    match result {
        Ok(_) => (),
        Err(interpreter::Error {
            span: Some(span),
            error,
        }) => print_err(error.description(), *span),
        _ => panic!("Unhandled error!"),
    }
}

fn print_err(desc: &str, span: Span) {
    println!(
        "{} near line {}, column {}:\n    {}",
        desc,
        span.line,
        span.get_utf8_column(),
        span.fragment
            .split("\n")
            .collect::<Vec<&str>>()
            .get(0)
            .unwrap(),
    );
}
