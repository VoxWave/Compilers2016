use super::{KeyWord, Operator, Scanner, Token};
use util::Direction;

#[test]
fn example_program_1() {
    let source = r#" var X : int := 4 + (6 * 2);
 print X;"#;

    let mut scanner = Scanner::new();
    let mut tokens = Vec::new();
    scanner.scan(source, &mut tokens);
    assert_eq!(
        tokens,
        vec![
            Token::KeyWord(KeyWord::Var),
            Token::Identifier(String::from("X")),
            Token::Colon,
            Token::KeyWord(KeyWord::Int),
            Token::Assignment,
            Token::Number(4.into()),
            Token::Operator(Operator::Plus),
            Token::Bracket(Direction::Left),
            Token::Number(6.into()),
            Token::Operator(Operator::Multiply),
            Token::Number(2.into()),
            Token::Bracket(Direction::Right),
            Token::Semicolon,
            Token::KeyWord(KeyWord::Print),
            Token::Identifier(String::from("X")),
            Token::Semicolon,
        ]
    );
}

#[test]
fn example_program_2() {
    let source = r#" var nTimes : int := 0;
 print "How many times?";
 read nTimes;
 var x : int;
 for x in 0..nTimes-1 do
  print x;
  print " : Hello, World!\n";
 end for;
 assert (x = nTimes);"#;
    let mut scanner = Scanner::new();
    let mut tokens = Vec::new();
    scanner.scan(source, &mut tokens);
    assert_eq!(
        tokens,
        vec![
            Token::KeyWord(KeyWord::Var),
            Token::Identifier(String::from("nTimes")),
            Token::Colon,
            Token::KeyWord(KeyWord::Int),
            Token::Assignment,
            Token::Number(0.into()),
            Token::Semicolon,
            Token::KeyWord(KeyWord::Print),
            Token::StringLiteral(String::from("How many times?")),
            Token::Semicolon,
            Token::KeyWord(KeyWord::Read),
            Token::Identifier(String::from("nTimes")),
            Token::Semicolon,
            Token::KeyWord(KeyWord::Var),
            Token::Identifier(String::from("x")),
            Token::Colon,
            Token::KeyWord(KeyWord::Int),
            Token::Semicolon,
            Token::KeyWord(KeyWord::For),
            Token::Identifier(String::from("x")),
            Token::KeyWord(KeyWord::In),
            Token::Number(0.into()),
            Token::Range,
            Token::Identifier(String::from("nTimes")),
            Token::Operator(Operator::Minus),
            Token::Number(1.into()),
            Token::KeyWord(KeyWord::Do),
            Token::KeyWord(KeyWord::Print),
            Token::Identifier(String::from("x")),
            Token::Semicolon,
            Token::KeyWord(KeyWord::Print),
            Token::StringLiteral(String::from(" : Hello, World!\n")),
            Token::Semicolon,
            Token::KeyWord(KeyWord::End),
            Token::KeyWord(KeyWord::For),
            Token::Semicolon,
            Token::KeyWord(KeyWord::Assert),
            Token::Bracket(Direction::Left),
            Token::Identifier(String::from("x")),
            Token::Operator(Operator::Equals),
            Token::Identifier(String::from("nTimes")),
            Token::Bracket(Direction::Right),
            Token::Semicolon,
        ]
    );
}

#[test]
fn example_program_3() {
    let source = r#" print "Give a number";
 var n : int;
 read n;
 var v : int := 1;
 var i : int;
 for i in 1..n do
    v := v * i;
 end for;
 print "The result is: ";
 print v; "#;
    let mut scanner = Scanner::new();
    let mut tokens = Vec::new();
    scanner.scan(source, &mut tokens);
    assert_eq!(
        tokens,
        vec![
            Token::KeyWord(KeyWord::Print),
            Token::StringLiteral(String::from("Give a number")),
            Token::Semicolon,
            Token::KeyWord(KeyWord::Var),
            Token::Identifier(String::from("n")),
            Token::Colon,
            Token::KeyWord(KeyWord::Int),
            Token::Semicolon,
            Token::KeyWord(KeyWord::Read),
            Token::Identifier(String::from("n")),
            Token::Semicolon,
            Token::KeyWord(KeyWord::Var),
            Token::Identifier(String::from("v")),
            Token::Colon,
            Token::KeyWord(KeyWord::Int),
            Token::Assignment,
            Token::Number(1.into()),
            Token::Semicolon,
            Token::KeyWord(KeyWord::Var),
            Token::Identifier(String::from("i")),
            Token::Colon,
            Token::KeyWord(KeyWord::Int),
            Token::Semicolon,
            Token::KeyWord(KeyWord::For),
            Token::Identifier(String::from("i")),
            Token::KeyWord(KeyWord::In),
            Token::Number(1.into()),
            Token::Range,
            Token::Identifier(String::from("n")),
            Token::KeyWord(KeyWord::Do),
            Token::Identifier(String::from("v")),
            Token::Assignment,
            Token::Identifier(String::from("v")),
            Token::Operator(Operator::Multiply),
            Token::Identifier(String::from("i")),
            Token::Semicolon,
            Token::KeyWord(KeyWord::End),
            Token::KeyWord(KeyWord::For),
            Token::Semicolon,
            Token::KeyWord(KeyWord::Print),
            Token::StringLiteral(String::from("The result is: ")),
            Token::Semicolon,
            Token::KeyWord(KeyWord::Print),
            Token::Identifier(String::from("v")),
            Token::Semicolon,
        ]
    );
}
