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

use util::Direction;
use util::Direction::*;

/// All the different tokens mini-pl has.
#[derive(Clone)]
pub enum Token {
	Bracket(Direction),
	Identifier(String),
	StringLiteral(String),
	Number(String),
	Semicolon,
	Colon,
	Operator(Operator),
	KeyWord(KeyWord),
}

/// All the different operators mini-pl has.
#[derive(Clone)]
enum Operator {
	Plus, Minus, Multiply, Divide, LessThan, Equals, And, Not,
}

///All the keywords mini-pl has.
#[derive(Clone)]
enum KeyWord {
	Var, For, End, In, Do, Read, Print, Int, String, Bool, Assert,
}
/// ScanModes can be thought as parts of an finite automaton that handle recognizing different token types.
enum ScanMode {
	Normal, String, Number, PossibleComment, LineComment, BlockComment, Other, Escape
}

/// Scanner is essentially a finite state automaton that takes in a source code as a string and 
pub struct Scanner {
	/// Tokens that have been parsed.
	tokens: Vec<Token>,
	/// Current state of scanning. It used to choose the approriate function to scan for a token.
	scan_mode: ScanMode,
	/// a String used to store previously scanned characters that are needed in the next token.
	buffer_string: String,
	/// a String used to store characters related to escape sequences (in strings).
	escape_buffer: String,
}

impl Scanner {
	/// Creates a new Scanner.
	pub fn new() -> Self {
		Scanner {
			tokens: Vec::new(),
			scan_mode: ScanMode::Normal,
			buffer_string: String::new(),
			escape_buffer: String::new(),
		}
	}
	/// Goes trough the whole source string character by character and produces a vector of tokens.
	pub fn scan(&mut self, source: &str) -> Vec<Token> {
		// Foreach through the source string and choose the approriate handling function for the current character 
		// according to what state(´ScanMode´) the scanner is currently in.
	    for c in source.chars() {
			match self.scan_mode {
				ScanMode::Normal => self.normal_scan(c),
				ScanMode::String => self.string_scan(c),
				ScanMode::Number => self.number_scan(c),
				ScanMode::PossibleComment => self.check_for_comment(c),
				ScanMode::LineComment => self.line_comment_handling(c),
				ScanMode::BlockComment => self.block_comment_handling(c),
				ScanMode::Other => self.identifier_and_keyword_scan(c),
				ScanMode::Escape => self.escape_scan(c),
			}
	    }
		self.tokens.clone()
	}

	fn normal_scan(&mut self, c: char) {
		match c {
			' ' | '\n' | '\t' | '\r' => {}
			'0'...'9' => {
				self.buffer_string.push(c);
				self.scan_mode = ScanMode::Number;
			},
			'"' => {
				self.scan_mode = ScanMode::String;
			},
			'(' => self.tokens.push(Token::Bracket(Left)),
			')' => self.tokens.push(Token::Bracket(Right)),
			';' => self.tokens.push(Token::Semicolon),
			':' => self.tokens.push(Token::Colon),
			'+' => self.tokens.push(Token::Operator(Operator::Plus)),
			'-' => self.tokens.push(Token::Operator(Operator::Minus)),
			'*' => self.tokens.push(Token::Operator(Operator::Multiply)),
			'/' => {
				self.buffer_string.push(c);
				self.scan_mode = ScanMode::PossibleComment;
			},
			'<' => self.tokens.push(Token::Operator(Operator::LessThan)),
			'=' => self.tokens.push(Token::Operator(Operator::Equals)),
			'&' => self.tokens.push(Token::Operator(Operator::And)),
			'!' => self.tokens.push(Token::Operator(Operator::Not)),
			_ => {
				self.buffer_string.push(c);
				self.scan_mode = ScanMode::Other;
			},
		}
	}

	fn string_scan(&mut self, c: char) {
		match c {
			'\\' => {
				self.scan_mode = ScanMode::Escape;
			}
			'"' => {
				self.tokens.push(Token::StringLiteral(self.buffer_string.clone()));
				self.buffer_string.clear();
				self.scan_mode = ScanMode::Normal;
			}
			//It is possible that I need to add problematic characters here.
			
			_ => self.buffer_string.push(c),
		}
	}

	fn escape_scan(&mut self, c: char) {
		if self.escape_buffer.is_empty() {
			//match the escape to an actual escape character and store it in a variable.
			let escaped_char = match c {
				'a' => '\x07',
				'b' => '\x08',
				'f' => '\x0C',
				'n' => '\n',
				'r' => '\r',
				't' => '\t',
				'v' => '\x0B',
				'\\' | '\'' | '"' | '?' => c,
				'0' ... '8' | 'x' | 'U' | 'u' => {
					self.escape_buffer.push(c);
					//return because in the case these characters we want to gather more characters in order to parse the escape correctly.
					return;
				},
				_ => panic!("Escape \\{} not supported", c),
			};
			//the escape has been handled. push the character into the string we're forming and return back to normal string scanning.
			self.buffer_string.push(escaped_char);
			self.escape_buffer.clear();
			self.scan_mode = ScanMode::String;
		} else {
			//we have found an escape sequence that's larger than one character long.
			match self.escape_buffer.chars().next().unwrap() {
				'x' => {
					match c {
						'0' ... '9' | 'a' | 'A' | 'b' | 'B' | 'c' | 'C' | 'd' | 'D' | 'e' | 'E' | 'f' | 'F' => {
							if self.escape_buffer.len() > 2 {
								panic!("hex escape sequences longer than a byte(two digits) are not supported.");
							} 
							self.escape_buffer.push(c);
						},
						_ => {
							if self.escape_buffer.len() < 2 {
								panic!("Atleast one hexadecimal digit is needed after \\x");
							}
							let chara = u8::from_str_radix(&self.escape_buffer[1..], 16).expect("error occured when parsing the hex escape into a char");
							self.buffer_string.push(chara as char);
							self.escape_buffer.clear();
							if c == '"' {
								self.tokens.push(Token::StringLiteral(self.buffer_string.clone()));
								self.buffer_string.clear();
								self.scan_mode = ScanMode::Normal;
							} else {
								self.scan_mode = ScanMode::String;
							}
						}
					}
				},
				'U' => {

				},
				'u' => {

				},
				_ => panic!(),
			}
		}
	}

	fn number_scan(&mut self, c: char) {

	}
	fn eval_buffer(&mut self) {
		let token = match &*self.buffer_string {
			"var" => Token::KeyWord(KeyWord::Var),
			"end" => Token::KeyWord(KeyWord::End),
			"for" => Token::KeyWord(KeyWord::For),
			"in" => Token::KeyWord(KeyWord::In),
			"do" => Token::KeyWord(KeyWord::Do),
			"read" => Token::KeyWord(KeyWord::Read),
			"print" => Token::KeyWord(KeyWord::Print),
			"int" => Token::KeyWord(KeyWord::Int),
			"string" => Token::KeyWord(KeyWord::String),
			"bool" => Token::KeyWord(KeyWord::Bool),
			"assert" => Token::KeyWord(KeyWord::Assert),
			_ => Token::Identifier(self.buffer_string.clone()),
		};
		self.tokens.push(token);
	}

	fn identifier_and_keyword_scan(&mut self, c: char) {
		match c {
			w if w.is_whitespace() => {
				self.eval_buffer();
				self.scan_mode = ScanMode::Normal;
			}
			'0'...'9' => {
				self.eval_buffer();
				self.buffer_string.push(c);
				self.scan_mode = ScanMode::Number;
			},
			'"' => {
				self.scan_mode = ScanMode::String;
			},
			'(' => {
				self.eval_buffer();
				self.tokens.push(Token::Bracket(Left));
				self.scan_mode = ScanMode::Normal;
			},
			'(' => {
				self.tokens.push(Token::Bracket(Right));
				self.scan_mode = ScanMode::Normal;
			},
			';' => {
				self.tokens.push(Token::Semicolon);
				self.scan_mode = ScanMode::Normal;
			},
			':' => {
				self.tokens.push(Token::Colon);
				self.scan_mode = ScanMode::Normal;
			},
			'+' => {
				self.tokens.push(Token::Operator(Operator::Plus));
				self.scan_mode = ScanMode::Normal;
			},
			'-' => {
				self.tokens.push(Token::Operator(Operator::Plus));
				self.scan_mode = ScanMode::Normal;
			},
			'*' => {
				self.tokens.push(Token::Operator(Operator::Plus));
				self.scan_mode = ScanMode::Normal;
			},
			'/' => {
				self.buffer_string.push(c);
				self.scan_mode = ScanMode::PossibleComment;
			},
			_ => unreachable!(),
		}
	}

	fn check_for_comment(&mut self, c: char) {

	}

	fn block_comment_handling(&mut self, c: char) {

	}

	fn line_comment_handling(&mut self, c: char) {

	}
}
