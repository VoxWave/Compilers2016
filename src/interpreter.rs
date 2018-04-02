use num_bigint::BigInt;
use parser::{BinaryOperator, Expression, Operand, Statement, Type, UnaryOperator};
use util::Source;
use std::collections::HashMap;
use self::Variable::*;
use char_stream::CharStream;

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Int(BigInt),
    String(String),
    Bool(bool),
}

impl Value {
    fn default_from_type(typ: Type) -> Self {
        match typ {
            Type::Bool => Value::Bool(false),
            Type::Int => Value::Int(0.into()),
            Type::Str => Value::String("".into()),
        }
    }
}

enum Variable {
    Mutable(Value),
    Immutable(Value),
}

impl Variable {
    fn freeze(&self) -> Variable {
        match *self {
            Mutable(ref v) => Immutable(v.clone()),
            Immutable(ref v) => Immutable(v.clone()),
        }
    }
    fn thaw(&self) -> Variable {
        match *self {
            Mutable(ref v) => Mutable(v.clone()),
            Immutable(ref v) => Mutable(v.clone()),
        }
    }
}

pub struct Interpreter {
    context: HashMap<String, Variable>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            context: HashMap::new(),
        }
    }

    fn eval_expr(&mut self, expr: &Expression) -> Value {
        match *expr {
            Expression::Binary(ref lhs, ref op, ref rhs) => {
                let lhs = self.eval_oprnd(lhs);
                let rhs = self.eval_oprnd(rhs);
                match *op {
                    BinaryOperator::And => match (lhs, rhs) {
                        (Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs && rhs),
                        _ => panic!("non boolean operands during and."),
                    },
                    BinaryOperator::Divide => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs / rhs),
                        _ => panic!("non integer operands during division."),
                    },
                    BinaryOperator::Equals => match (lhs, rhs) {
                        (Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs == rhs),
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Bool(lhs == rhs),
                        (Value::String(lhs), Value::String(rhs)) => Value::Bool(lhs == rhs),
                        _ => panic!("cannot test equality of different types"),
                    },
                    BinaryOperator::LessThan => match (lhs, rhs) {
                        (Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs < rhs),
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Bool(lhs < rhs),
                        (Value::String(lhs), Value::String(rhs)) => Value::Bool(lhs < rhs),
                        _ => panic!("cannot test ordering of different types"),
                    },
                    BinaryOperator::Minus => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs - rhs),
                        _ => panic!("non integer operands during substraction."),
                    },
                    BinaryOperator::Multiply => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs * rhs),
                        _ => panic!("non integer operands during multiplication."),
                    },
                    BinaryOperator::Plus => match (lhs, rhs) {
                        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs + rhs),
                        (Value::String(lhs), Value::String(rhs)) => Value::String(lhs + &rhs),
                        _ => panic!("invalid operand during addition/concatenation"),
                    },
                }
            }
            Expression::Unary(ref op, ref rhs) => match *op {
                UnaryOperator::Not => match self.eval_oprnd(rhs) {
                    Value::Bool(b) => Value::Bool(!b),
                    _ => panic!("Cannot apply Not to non-boolean values"),
                },
            },
            Expression::Singleton(ref oprnd) => self.eval_oprnd(oprnd),
        }
    }

    fn eval_oprnd(&mut self, oprnd: &Operand) -> Value {
        match *oprnd {
            Operand::Expr(ref expr) => self.eval_expr(expr),
            Operand::Identifier(ref iden) => match *self.context
                .get(iden)
                .expect(&format!("undefined variable {}", iden))
            {
                Mutable(ref n) | Immutable(ref n) => n.clone(),
            },
            Operand::Int(ref n) => Value::Int(n.clone()),
            Operand::StringLiteral(ref s) => Value::String(s.clone()),
        }
    }

    pub fn interpret<S>(&mut self, statements: &mut S)
    where
        S: Source<Statement>,
    {
        while let Some(stmt) = statements.take() {
            match stmt {
                Statement::Assert(expr) => match self.eval_expr(&expr) {
                    Value::Bool(b) => if !b {
                        panic!("assert failed!");
                    },
                    _ => panic!("{:#?} did not evaluate to a boolean"),
                },
                Statement::Assignment(var, expr) => {
                    let new_val = self.eval_expr(&expr);
                    match self.context.get_mut(&var) {
                        Some(v) => match *v {
                            Mutable(ref mut val) => match (val, &new_val) {
                                (val @ &mut Value::Bool(_), &Value::Bool(_))
                                | (val @ &mut Value::Int(_), &Value::Int(_))
                                | (val @ &mut Value::String(_), &Value::String(_)) => {
                                    *val = new_val;
                                }
                                _ => panic!(
                                    "expression did not evaluate to the same type as the variable"
                                ),
                            },
                            Immutable(_) => panic!("cannot modify {}", var),
                        },
                        None => panic!("{} not initialised", var),
                    };
                }
                Statement::Declaration(var, typ, o_expr) => {
                    let value = match o_expr {
                        Some(expr) => {
                            let val = self.eval_expr(&expr);
                            match (&val, typ) {
                                (&Value::Bool(_), Type::Bool)
                                | (&Value::Int(_), Type::Int)
                                | (&Value::String(_), Type::Str) => val,
                                _ => panic!("expression did not evaluate to {:#?}", typ),
                            }
                        }
                        None => Value::default_from_type(typ),
                    };
                    if self.context.contains_key(&var) {
                        panic!("Tried to initialize a variable that was already initialized");
                    } else {
                        self.context.insert(var, Mutable(value));
                    }
                }
                Statement::For(var, from, to, mut stmts) => {
                    match self.context.get_mut(&var) {
                        Some(variable) => {
                            match *variable {
                                Mutable(ref val) => match *val {
                                    Value::Int(_) => {}
                                    _ => panic!("loop control variable was not an integer"),
                                },
                                _ => panic!("loop variable cannot be reused"),
                            }
                            *variable = variable.freeze();
                        }
                        None => panic!("for loop variable uninitialized"),
                    }
                    if let Value::Int(from) = self.eval_expr(&from) {
                        if let Value::Int(to) = self.eval_expr(&to) {
                            for i in ::num::range_inclusive(from, to + &BigInt::from(1)) {
                                if let Immutable(Value::Int(ref mut n)) =
                                    *self.context.get_mut(&var).unwrap()
                                {
                                    *n = i;
                                } else {
                                    unreachable!();
                                }
                                self.interpret(&mut stmts.clone())
                            }
                        } else {
                            panic!("range expression did not evaluate to an integer");
                        }
                    } else {
                        panic!("range expression did not evaluate to an integer");
                    }
                    let mut control_variable = self.context.get_mut(&var).unwrap();
                    *control_variable = control_variable.thaw();
                }
                Statement::Print(expr) => match self.eval_expr(&expr) {
                    Value::Bool(b) => panic!("boolean printing is not supported"),
                    Value::Int(i) => println!("{}", i),
                    Value::String(s) => println!("{}", s),
                },
                Statement::Read(var) => {
                    let input: String = CharStream::from_stdin()
                        .take_while(|c| !c.is_whitespace())
                        .collect();
                    match *self.context.get_mut(&var).expect("variable was undefined") {
                        Mutable(ref mut val) => match *val {
                            Value::Int(ref mut i) => {
                                *i = input.parse().expect("invalid integer input");
                            }
                            Value::String(ref mut s) => {
                                *s = input;
                            }
                            Value::Bool(_) => panic!("Tried to read into boolean variable"),
                        },
                        Immutable(_) => panic!("tried to read to a loop control variable"),
                    }
                }
            }
        }
    }
}
