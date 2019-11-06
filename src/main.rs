mod ast;

use ast::{Ast, Error, Expr, Span, Value};
use nom::character::complete as character;
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
    let ast = match ast {
        Ok(ast) => {
            print!("{}", ast);
            ast
        }
        Err(error) => {
            print_err(&input, error);
            return;
        }
    };

    match ast.typecheck() {
        Ok(_) => (),
        Err(error) => {
            print_err(&input, error);
            return;
        }
    };

    match ast.borrowcheck() {
        Ok(_) => (),
        Err(error) => {
            print_err(&input, error);
            return;
        }
    }

    let main_call = Expr {
        value: Value::Call("main", vec![]),
        span: Span::new("main()"),
    };
    match ast.run(&main_call) {
        Ok(_) => (),
        Err(error) => {
            print_err(&input, error);
            return;
        }
    };
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

fn print_err<E: Error>(input: &String, error: E) {
    if let Some(span) = error.span() {
        eprintln!(
            "ERROR: {} near line {}, column {}:\n{}",
            error.description(),
            span.line,
            span.get_utf8_column(),
            fmt_context(input, span),
        );
    } else {
        eprintln!("ERROR: {}", error.description());
    }
}

fn fmt_context(input: &String, span: Span) -> String {
    let mut offset = span.offset;
    if offset != 0 {
        while let Some(char) = input.as_bytes().get(offset - 1..offset) {
            if char[0] == b'\n' || offset == 1 {
                break;
            }
            offset -= 1;
        }
    }

    let mut end_offset = span.offset;
    while let Some(char) = input.as_bytes().get(end_offset..end_offset + 1) {
        if char[0] == b'\n' {
            break;
        }
        end_offset += 1;
    }

    let context = input.get(offset..end_offset).unwrap();
    let (context, spaces) =
        character::multispace0::<&str, (&str, nom::error::ErrorKind)>(context).unwrap();
    let mut marker_length = span.fragment.len();
    if marker_length > end_offset - span.offset {
        marker_length = end_offset - span.offset;
    }
    let marker = format!(
        "{}{}",
        " ".repeat(span.offset - (offset + spaces.len())),
        "^".repeat(marker_length),
    );
    format!("    {}\n    {}", context, marker)
}
