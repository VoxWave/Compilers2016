use std::collections::VecDeque;
use super::{parse, BinaryOperator, Expression, Operand, Statement, Type};
use scanner::Scanner;

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
    let mut tokens = VecDeque::new();
    scanner.scan(source, &mut tokens);
    let mut statements = VecDeque::new();
    let mut expected: VecDeque<_> = vec![
        Statement::Declaration(
            "nTimes".to_string(),
            Type::Int,
            Some(Expression::Singleton(Operand::Int(0.into()))),
        ),
        Statement::Print(Expression::Singleton(Operand::StringLiteral(
            "How many times?".to_string(),
        ))),
        Statement::Read("nTimes".to_string()),
        Statement::Declaration("x".to_string(), Type::Int, None),
        Statement::For(
            "x".to_string(),
            Expression::Singleton(Operand::Int(0.into())),
            Expression::Binary(
                Operand::Identifier("nTimes".to_string()),
                BinaryOperator::Minus,
                Operand::Int(1.into()),
            ),
            vec![
                Statement::Print(Expression::Singleton(Operand::Identifier("x".to_string()))),
                Statement::Print(Expression::Singleton(Operand::StringLiteral(
                    " : Hello, World!\n".to_string(),
                ))),
            ],
        ),
        Statement::Assert(Expression::Binary(
            Operand::Identifier("x".to_string()),
            BinaryOperator::Equals,
            Operand::Identifier("nTimes".to_string()),
        )),
    ].into_iter()
        .collect();
    parse(&mut tokens, &mut statements);
    assert_eq!(statements, expected);
}
