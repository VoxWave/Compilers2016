use std::ops::Deref;

use num_bigint::BigInt;

use util::{Direction, Source, Sink};

use scanner::{Token, KeyWord};

//All of these enums make up our AST.

//  <stmt> ::= 
//    "var" <var_ident> ":" <type> [ ":=" <expr> ]
//  | <var_ident> ":=" <expr>
//  | "for" <var_ident> "in" <expr> ".." <expr> "do" <stmts> "end" "for"
//  | "read" <var_ident>
//  | "print" <expr>
//  | "assert" "(" <expr> ")"
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Declaration(String, Type, Option<Expression>),
    Assignment(String, Expression),
    For(String, Expression, Expression, Vec<Statement>),
    Read(String),
    Print(Expression),
    Assert(Expression),
}

//grammar has been changed.
//Original:
// <expr> ::= <opnd> <op> <opnd>
//         | [ <unary_op> ] <opnd>
//New:
// <expr> ::= <opnd> <op> <opnd>
//         |  <unary_op> <opnd>
//         |  <opnd>

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Binary(Operand, BinaryOperator, Operand),
    Unary(UnaryOperator, Operand),
    Sigleton(Operand),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Int(BigInt),
    StringLiteral(String),
    Bool,
    Expr(Box<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    LessThan,
    Equals,
    And,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Str,
    Bool,
}

pub struct Parser<'a, O>
where
    O: Sink<Statement> + 'a,
{
    buffer: Vec<Token>,
    statements: &'a mut O,
}

pub fn parse<I, O>(tokens: &mut I, statements: &mut O)
where
    I: Source<Token>,
    O: Sink<Statement>,
{
    let mut parser = Parser::new(statements);
    let mut state = State(Parser::normal_parse);
    while let Some(t) = tokens.take() {
        state = state(&mut parser, t);
    }
}

//  <prog> ::= <stmts>
//  <stmts> ::= <stmt> ";" ( <stmt> ";" )*
//  <stmt> ::= "var" <var_ident> ":" <type> [ ":=" <expr> ]
//  | <var_ident> ":=" <expr>
//  | "for" <var_ident> "in" <expr> ".." <expr> "do"
//  <stmts> "end" "for"
//  | "read" <var_ident>
//  | "print" <expr>
//  | "assert" "(" <expr> ")"
//  <expr> ::= <opnd> <op> <opnd>
//  | [ <unary_op> ] <opnd>
//  <opnd> ::= <int>
//  | <string>
//  | <var_ident>
//  | "(" expr ")"
//  <type> ::= "int" | "string" | "bool"
//  <var_ident> ::= <ident>
//  <reserved keyword> ::=
//  "var" | "for" | "end" | "in" | "do" | "read" |
//  "print" | "int" | "string" | "bool" | "assert"

impl<'a, O> Parser<'a, O>
where
    O: Sink<Statement>,
{
    fn new(statements: &'a mut O) -> Self {
        Parser {
            buffer: Vec::new(),
            statements,
        }
    }

    fn normal_parse(&mut self, t: Token) -> State<'a, O> {
        match t {
            Token::Identifier(identifier) => State(Self::assignment_parse),
            Token::KeyWord(keyword) => {
                match keyword {
                    KeyWord::Var => State(Self::v)
                }
            },
            Token::Semicolon => State(Self::normal_parse),
            _ => panic!("unexpected token: {:?}", t),
        }
    }

    fn assignment_parse(&mut self, t: Token) -> State<'a, O> {
        State(Self::assignment_parse)
    }
}

struct State<'a, O>(fn(&mut Parser<'a, O>, Token) -> State<'a, O>)
where
    O: Sink<Statement> + 'a;
impl<'a, O> Deref for State<'a, O>
where
    O: Sink<Statement>,
{
    type Target = fn(&mut Parser<'a, O>, Token) -> State<'a, O>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
