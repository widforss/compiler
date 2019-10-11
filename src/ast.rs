pub mod interpreter;
pub mod parser;

use nom_locate::LocatedSpan;
use std::collections::HashMap;
use std::fmt;

pub type Span<'a> = LocatedSpan<&'a str>;

pub struct Ast<'a>(HashMap<&'a str, Func<'a>>);

pub struct Func<'a> {
    typ: Type,
    params: Vec<(Type, Mutability, Span<'a>)>,
    body: Stmt<'a>,
    span: Span<'a>,
}

pub type Mutability = bool;

pub struct Stmt<'a> {
    pub stmt: Statement<'a>,
    pub span: Span<'a>,
}

pub enum Statement<'a> {
    Let(Mutability, Span<'a>, Type, Expr<'a>),
    Assign(Span<'a>, Expr<'a>),
    While(Expr<'a>, Box<Stmt<'a>>),
    IfElse(Expr<'a>, Box<Stmt<'a>>, Box<Stmt<'a>>),
    Block(Vec<Stmt<'a>>),
    Return(Expr<'a>),
    Print(Expr<'a>),
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
}

pub struct Expr<'a> {
    pub value: Value<'a>,
    pub span: Span<'a>,
}

pub enum Value<'a> {
    Literal(Literal),
    Ident(Span<'a>),
    BinOp(BinOp, Box<Expr<'a>>, Box<Expr<'a>>),
    UnOp(UnOp, Box<Expr<'a>>),
    Call(Span<'a>, Vec<Expr<'a>>),
}

#[derive(Clone, Copy, Debug)]
pub enum Literal {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Pow,
    Mult,
    Rem,
    IntDiv,
    Div,
    Add,
    Sub,
    Lt,
    Gt,
    Leq,
    Geq,
    Eq,
    Neq,
    And,
    Or,
}

#[derive(Clone, Copy, Debug)]
pub enum UnOp {
    Sub,
    Inv,
}

impl<'a> fmt::Display for Ast<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Ast(functions) = self;
        for (name, function) in functions.iter() {
            write!(f, "\"{}\": {}\n", name, function)?;
        }
        Ok(())
    }
}

impl<'a> fmt::Display for Func<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut paramstr = String::from("[\n");
        for (typ, mutability, ident) in self.params.iter() {
            paramstr = format!(
                "{}        {{\n            \
                 Type: {},\n            \
                 Mutable: {},\n            \
                 Identifier: Ident({}),\n        \
                 }},\n",
                paramstr, typ, mutability, ident.fragment,
            );
        }
        paramstr = format!("{}    ]", paramstr);
        write!(
            f,
            "{{\n    \
             Type: {},\n    \
             Parameters: {},\n    \
             Function: {},\n\
             }},",
            self.typ,
            paramstr,
            self.body.stmt.block(1)
        )
    }
}

impl<'a> fmt::Display for Stmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.stmt)
    }
}

impl<'a> fmt::Display for Statement<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.print(2))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.print(0))
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<'a> Statement<'a> {
    pub fn block(&self, indent: usize) -> String {
        let instr = "    ".repeat(indent);
        if let Statement::Block(stmts) = self {
            let mut string = String::from("Block [\n");
            for stmt in stmts.iter() {
                let statement = &stmt.stmt;
                string = format!("{}{}", string, statement.print(indent + 1));
            }
            format!("{}{}]", string, instr)
        } else {
            panic!();
        }
    }

    fn print(&self, indent: usize) -> String {
        use Statement::*;

        let instr = "    ".repeat(indent);
        match self {
            Block(statements) => {
                let mut string = format!("{}Block [\n", instr);
                for stmt in statements.iter() {
                    let statement = &stmt.stmt;
                    string = format!("{}{}", string, statement.print(indent + 1));
                }
                format!("{}{}],\n", string, instr)
            }
            Let(mutable, ident, typ, expr) => format!(
                "{}Let: {{\n\
                 {}    Type: {},\n\
                 {}    Mutable: {},\n\
                 {}    Identifier: \"{}\",\n\
                 {}    Expression: {},\n\
                 {}}},\n",
                instr,
                instr,
                typ,
                instr,
                mutable,
                instr,
                ident.fragment,
                instr,
                expr.value.print(indent + 1),
                instr,
            ),
            While(expr, stmt) => {
                let statement = &stmt.stmt;
                format!(
                    "{}While: {{\n\
                     {}    Condition: {},\n\
                     {}    Do: {},\n\
                     {}}},\n",
                    instr,
                    instr,
                    expr.value.print(indent + 1),
                    instr,
                    statement.block(indent + 1),
                    instr,
                )
            }
            IfElse(expr, if_stmt, else_stmt) => {
                let if_stmt = &if_stmt.stmt;
                let else_stmt = &else_stmt.stmt;

                let pre_else = match else_stmt {
                    Statement::Block(_) => format!("{}", else_stmt.block(indent + 1)),
                    Statement::IfElse(_, _, _) => {
                        format!("{}", &else_stmt.print(indent + 1)[(4 * indent + 4)..])
                    }
                    _ => panic!(),
                };

                format!(
                    "{}IfElse: {{\n\
                     {}    Condition: {}\n\
                     {}    If: {},\n\
                     {}    Else: {},\n\
                     {}}},\n",
                    instr,
                    instr,
                    expr.value.print(indent + 1),
                    instr,
                    if_stmt.block(indent + 1),
                    instr,
                    pre_else,
                    instr,
                )
            }
            Return(expr) => format!("{}Return: {},\n", instr, expr.value.print(indent + 1)),
            Assign(ident, expr) => format!(
                "{}Assign: {{\n\
                 {}    Identifier: \"{}\",\n\
                 {}    Expression: {},\n\
                 {}}},\n",
                instr,
                instr,
                ident.fragment,
                instr,
                expr.value.print(indent + 1),
                instr
            ),
            Print(expr) => format!("{}Print: {},\n", instr, expr.value.print(indent + 1)),
        }
    }
}

impl<'a> Value<'a> {
    pub fn print(&self, indent: usize) -> String {
        use Value::*;

        let instr = "    ".repeat(indent);
        match self {
            Literal(val) => format!("{}", val),
            BinOp(op, left, right) => format!(
                "BinOp({}) [\n\
                 {}    {},\n\
                 {}    {},\n\
                 {}]",
                op, instr, left.value.print(indent + 1), instr, right.value.print(indent + 1), instr
            ),
            UnOp(op, arg) => format!(
                "UnOp({}) [\n\
                 {}    {},\n\
                 {}]",
                op, instr, arg.value.print(indent + 1), instr
            ),
            Call(func, exprs) => {
                let mut string = format!("Call({}) [\n", func.fragment);
                for expr in exprs.iter() {
                    string = format!("{}{}    {},\n", string, instr, expr.value.print(indent + 1));
                }
                format!("{}{}]", string, instr)
            }
            Ident(span) => format!("Ident(\"{}\")", span.fragment),
        }
    }
}
