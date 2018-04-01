//     mini-pl compiler.
//     Copyright (C) 2018  Victor Bankowski

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

use std::ops::Deref;

use num_bigint::BigInt;

use util::{Direction, Sink, Source};

use scanner::{KeyWord, Operator, Token};

#[cfg(test)]
mod test;

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

// grammar has been changed.
// Original:
// <expr> ::= <opnd> <op> <opnd>
//         | [ <unary_op> ] <opnd>
// New:
// <expr> ::= <opnd> <op> <opnd>
//         |  <unary_op> <opnd>
//         |  <opnd>

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Binary(Operand, BinaryOperator, Operand),
    Unary(UnaryOperator, Operand),
    Singleton(Operand),
}

// <opnd> ::=
//   <int>
// | <string>
// | <var_ident>
// | "(" expr ")"
#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Int(BigInt),
    StringLiteral(String),
    Identifier(String),
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

#[derive(Clone, Debug, PartialEq, Copy)]
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
    for_buffer: Vec<(String, Expression, Expression, Vec<Statement>)>,
    for_range_pointer: usize,
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
// I tried the design pattern described here
// https://dev.to/mindflavor/lets-build-zork-using-rust-1opm
impl<'a, O> Parser<'a, O>
where
    O: Sink<Statement>,
{
    fn new(statements: &'a mut O) -> Self {
        Parser {
            buffer: Vec::new(),
            for_buffer: Vec::new(),
            for_range_pointer: 0,
            statements,
        }
    }

    fn normal_parse(&mut self, t: Token) -> State<'a, O> {
        match t {
            Token::Identifier(_) => {
                self.buffer.push(t);
                State(Self::assignment_parse)
            }
            Token::KeyWord(keyword) => match keyword {
                KeyWord::Var => State(Self::variable_definition_parse),
                KeyWord::For => State(Self::for_loop_parse),
                KeyWord::Read => State(Self::read_parse),
                KeyWord::Print => State(Self::print_parse),
                KeyWord::Assert => State(Self::assert_parse),
                KeyWord::End => State(Self::expect_end_for),
                _ => panic!("a statement cannot start with the keyword {:#?}", keyword),
            },
            //empty statements are allowed. They are skiped.
            Token::Semicolon => State(Self::normal_parse),

            _ => panic!("unexpected token: {:#?}", t),
        }
    }

    // "var" <var_ident> ":" <type> [ ":=" <expr> ]
    fn variable_definition_parse(&mut self, t: Token) -> State<'a, O> {
        match self.buffer.len() {
            0 => match t {
                Token::Identifier(_) => self.buffer.push(t),
                _ => panic!("Expected an identifier but found {:#?} instead", t),
            },
            1 => match t {
                Token::Colon => self.buffer.push(t),
                _ => panic!("Expected a colon but found {:#?} instead", t),
            },
            2 => match t {
                Token::KeyWord(KeyWord::String)
                | Token::KeyWord(KeyWord::Int)
                | Token::KeyWord(KeyWord::Bool) => self.buffer.push(t),
                _ => panic!("Expected a type signature but found {:#?} instead", t),
            },
            n => match t {
                Token::Semicolon => {
                    let identifier = match self.buffer[0] {
                        Token::Identifier(ref i) => i.clone(),
                        _ => unreachable!(),
                    };
                    let typ = match self.buffer[2] {
                        Token::KeyWord(KeyWord::String) => Type::Str,
                        Token::KeyWord(KeyWord::Int) => Type::Int,
                        Token::KeyWord(KeyWord::Bool) => Type::Bool,
                        _ => unreachable!(),
                    };
                    let expr = match n {
                        3 => None,
                        4 => panic!("Var statement ended prematurely."),
                        n if n > 4 => Some(parse_expression(&self.buffer[4..])),
                        _ => unreachable!(),
                    };
                    self.handle_statement(Statement::Declaration(identifier, typ, expr));
                    return State(Self::normal_parse);
                }
                Token::Assignment => match n {
                    3 => self.buffer.push(t),
                    _ => panic!("an assignment is not valid in an expression"),
                },
                _ => self.buffer.push(t),
            },
        }
        State(Self::variable_definition_parse)
    }

    fn assignment_parse(&mut self, t: Token) -> State<'a, O> {
        //let len = self.buffer.len();
        if self.buffer.len() == 1 {
            match t {
                Token::Assignment => self.buffer.push(t),
                _ => panic!("expected a := but found {:#?} instead", t),
            }
            State(Self::assignment_parse)
        } else {
            match t {
                Token::Semicolon => {
                    let statement = match &self.buffer[0] {
                        &Token::Identifier(ref identifier) => {
                            Statement::Assignment(
                                    identifier.clone(),
                                    parse_expression(&self.buffer[2..])
                            )
                        },
                        _ => unreachable!(
                            "the first token of the buffer during assignment parsing was something other than an identifier"
                        ),
                    };
                    self.handle_statement(statement);
                    State(Self::normal_parse)
                }
                _ => {
                    self.buffer.push(t);
                    State(Self::assignment_parse)
                }
                _ => panic!("unexpected Token {:#?} read during", t),
            }
        }
    }

    // "for" <var_ident> "in" <expr> ".." <expr> "do" <stmts> "end" "for"
    fn for_loop_parse(&mut self, t: Token) -> State<'a, O> {
        match self.buffer.len() {
            0 => match t {
                Token::Identifier(_) => self.buffer.push(t),
                _ => panic!("Expected an identifier, found {:#?}", t),
            },
            1 => match t {
                Token::KeyWord(KeyWord::In) => self.buffer.push(t),
                _ => panic!("Expected keyword 'in', found {:#?}"),
            },
            _ => match t {
                Token::KeyWord(KeyWord::Do) => {
                    if self.for_range_pointer < 3 {
                        panic!("incorrect for loop range");
                    }
                    let identifier = match self.buffer[0] {
                        Token::Identifier(ref i) => i.clone(),
                        _ => unreachable!("the buffer did not have an identifier as the first element when parsing a for loop"),
                    };
                    self.for_buffer.push((
                        identifier,
                        parse_expression(&self.buffer[2..self.for_range_pointer]),
                        parse_expression(
                            &self.buffer[(self.for_range_pointer + 1)..self.buffer.len()],
                        ),
                        Vec::new(),
                    ));
                    self.for_range_pointer = 0;
                    self.buffer.clear();
                    return State(Self::normal_parse);
                }
                Token::Range => {
                    if self.for_range_pointer == 0 {
                        self.for_range_pointer = self.buffer.len();
                        self.buffer.push(t);
                    } else {
                        panic!("found more than one range during for loop parsing");
                    }
                }
                _ => {
                    self.buffer.push(t);
                }
            },
        }
        State(Self::for_loop_parse)
    }

    fn expect_end_for(&mut self, t: Token) -> State<'a, O> {
        match t {
            Token::KeyWord(KeyWord::For) => {
                let (identifier, from, to, statements) = self.for_buffer
                    .pop()
                    .expect("encountered an end for but no for loops were initialized.");

                let for_statement = Statement::For(identifier, from, to, statements);

                self.handle_statement(for_statement);
            }
            _ => panic!("Expected end after for, found {:#?} instead", t),
        };
        State(Self::expect_semicolon)
    }

    // "read" <var_ident>
    fn read_parse(&mut self, t: Token) -> State<'a, O> {
        match t {
            Token::Identifier(i) => self.handle_statement(Statement::Read(i)),
            _ => panic!("expected an identifier after a read"),
        };
        State(Self::normal_parse)
    }

    // "print" <expr>
    fn print_parse(&mut self, t: Token) -> State<'a, O> {
        match t {
            Token::Semicolon => {
                if 0 < self.buffer.len() {
                    let expression = parse_expression(&self.buffer);
                    self.handle_statement(Statement::Print(expression));
                    return State(Self::normal_parse);
                } else {
                    panic!("expected an expression after print.")
                }
            }
            _ => self.buffer.push(t),
        }
        State(Self::print_parse)
    }

    // "assert" "(" <expr> ")"
    fn assert_parse(&mut self, t: Token) -> State<'a, O> {
        match self.buffer.len() {
            0 => match t {
                Token::Bracket(Direction::Left) => self.buffer.push(t),
                _ => panic!(
                    "expected a left bracket after assert but found {:#?} instead",
                    t
                ),
            },
            _ => match t {
                Token::Semicolon => match self.buffer.pop().unwrap() {
                    Token::Bracket(Direction::Right) => {
                        if self.buffer.len() > 1 {
                            let expression = parse_expression(&self.buffer[1..]);
                            self.handle_statement(Statement::Assert(expression));
                            return State(Self::normal_parse);
                        } else {
                            panic!("invalid expression.");
                        }
                    }
                    _ => panic!(
                        "expected a right parenthesis to end the expression in an assert statement"
                    ),
                },
                _ => self.buffer.push(t),
            },
        }
        State(Self::assert_parse)
    }

    // <expr> ::= <opnd> <op> <opnd>
    //         |  <unary_op> <opnd>
    //         |  <opnd>
    //  <opnd> ::= <int>
    //  | <string>
    //  | <var_ident>
    //  | "(" expr ")"

    fn handle_statement(&mut self, statement: Statement) {
        if self.for_buffer.is_empty() {
            self.statements.put(statement);
        } else {
            let len = self.for_buffer.len() - 1;
            self.for_buffer[len].3.push(statement);
        }
        self.buffer.clear();
    }

    fn expect_semicolon(&mut self, t: Token) -> State<'a, O> {
        match t {
            Token::Semicolon => State(Self::normal_parse),
            _ => panic!("expected a semicolon, found {:#?} instead", t),
        }
    }
}

fn parse_expression(tokens: &[Token]) -> Expression {
    match tokens.len() {
        0 => panic!("tried to parse an expression but there was nothing to parse."),
        // There's only one token to handle so it must be a singleton expression.
        1 => {
            let operand = match_operand(tokens[0].clone());
            Expression::Singleton(operand)
        }

        // a Two token expression must be an unary operator and an operand that isn't an expression.
        2 => {
            let operator = match_unary_operator(tokens[0].clone());
            let operand = match_operand(tokens[1].clone());
            Expression::Unary(operator, operand)
        }
        // a Three token expression must be a binary expression with both of the Operands
        // being non-expression.
        3 => {
            let operand1 = match_operand(tokens[0].clone());
            let operator = match_binary_operator(tokens[1].clone());
            let operand2 = match_operand(tokens[2].clone());
            Expression::Binary(operand1, operator, operand2)
        }
        _ => {
            match tokens[0] {
                Token::Bracket(Direction::Left) => {
                    let closing_index = find_closing_bracket_index(tokens);
                    let operand1 =
                        Operand::Expr(Box::new(parse_expression(&tokens[1..closing_index])));
                    // the operand is the whole expression therefore we return an singleton expression.
                    match tokens.len() - (closing_index + 1) {
                        0 => Expression::Singleton(operand1),
                        1 => panic!("expected an operator and operand after a expression in parenthesis. found only {:#?}", tokens[closing_index+1]),
                        2 => {
                            let operator = match_binary_operator(tokens[closing_index + 1].clone());
                            let operand2 = match_operand(tokens[closing_index + 2].clone());
                            Expression::Binary(operand1, operator, operand2)
                        },
                        _ => {
                            let operator = match_binary_operator(tokens[closing_index + 1].clone());
                            let opening_index = closing_index + 2;
                            let closing_index_2 = opening_index + find_closing_bracket_index(&tokens[opening_index..]);
                            let operand2 = Operand::Expr(Box::new(parse_expression(&tokens[opening_index..(closing_index_2+1)])));
                            Expression::Binary(operand1, operator, operand2)
                        }
                    }
                }
                Token::Operator(Operator::Not) => {
                    let closing_index = 1 + find_closing_bracket_index(&tokens[1..]);
                    if closing_index + 1 == tokens.len() {
                        let operand =
                            Operand::Expr(Box::new(parse_expression(&tokens[2..tokens.len() - 1])));
                        Expression::Unary(UnaryOperator::Not, operand)
                    } else {
                        panic!("invalid operand");
                    }
                }
                Token::Identifier(_) | Token::Number(_) | Token::StringLiteral(_) => {
                    let operand1 = match_operand(tokens[0].clone());
                    let operator = match_binary_operator(tokens[1].clone());

                    let closing_index = 3 + find_closing_bracket_index(&tokens[3..]);
                    if closing_index + 1 == tokens.len() {
                        let operand2 =
                            Operand::Expr(Box::new(parse_expression(&tokens[2..tokens.len() - 1])));
                        Expression::Binary(operand1, operator, operand2)
                    } else {
                        panic!("invalid operand");
                    }
                }
                _ => panic!("expression started with and invalid token {:#?}"),
            }
        }
    }
}

/// a matches for the single token operands and then
fn match_operand(token: Token) -> Operand {
    match token {
        Token::Identifier(i) => Operand::Identifier(i),
        Token::Number(n) => Operand::Int(n),
        Token::StringLiteral(s) => Operand::StringLiteral(s),
        _ => panic!("expected an operand but found a {:#?} instead"),
    }
}

fn match_unary_operator(token: Token) -> UnaryOperator {
    match token {
        Token::Operator(Operator::Not) => UnaryOperator::Not,
        t => panic!("expected an unary operator, found {:#?} instead", t),
    }
}

fn match_binary_operator(token: Token) -> BinaryOperator {
    match token {
        Token::Operator(o) => match o {
            Operator::And => BinaryOperator::And,
            Operator::Divide => BinaryOperator::Divide,
            Operator::Equals => BinaryOperator::Equals,
            Operator::LessThan => BinaryOperator::LessThan,
            Operator::Minus => BinaryOperator::Minus,
            Operator::Multiply => BinaryOperator::Multiply,
            Operator::Plus => BinaryOperator::Plus,
            _ => panic!("expected a binary operator but found a {:#?} instead", o),
        },
        _ => panic!("expected an operator but found {:#?} instead"),
    }
}

fn find_closing_bracket_index(tokens: &[Token]) -> usize {
    let mut opened_brackets = 0;
    for (i, t) in tokens.iter().enumerate() {
        match i {
            0 => match t {
                &Token::Bracket(Direction::Left) => opened_brackets += 1,
                _ => panic!("the first token was a {:#?} instead of an opening bracket"),
            },
            _ => match t {
                &Token::Bracket(Direction::Left) => opened_brackets += 1,
                &Token::Bracket(Direction::Right) => {
                    opened_brackets -= 1;
                    if opened_brackets == 0 {
                        return i;
                    }
                }
                _ => {}
            },
        }
    }
    panic!("could not find a closing bracket for expression.");
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
