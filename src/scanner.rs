use util::Direction;
use util::Direction::*;

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

enum Operator {
	Plus, Minus, Multiply, Divide, LessThan, Equals, And, Not,
}

enum KeyWord {
	Var, For, End, In, Do, Read, Print, Int, String, Bool, Assert,
}
/// ScanModes can be thought as parts of an finite automaton that handle recognizing different token types.
enum ScanMode {
	Normal, String, Number, PossibleComment, LineComment, BlockComment, Other,
}

pub struct Scanner {
	source: String,
	tokens: Vec<Token>,
	scan_mode: ScanMode,
	buffer_string: String
}

impl Scanner {
	pub fn new(source: String) -> Self {
		Scanner {
			source,
			tokens: Vec::new(),
			scan_mode: ScanMode::Normal,
			buffer_string: String::new(),
		}
	}

	pub fn scan(&mut self) -> Vec<Token> {
	    for c in self.source.chars() {
			match self.scan_mode {
				ScanMode::Normal => self.normal_scan(c),
				ScanMode::String => self.string_scan(c),
				ScanMode::Number => self.number_scan(c),
				ScanMode::PossibleComment => self.check_for_comment(c),
				ScanMode::LineComment => self.line_comment_handling(c),
				ScanMode::BlockComment => self.block_comment_handling(c),
				ScanMode::Other => self.identifier_and_keyword_scan(c),
			}
	    }
		self.tokens
	}

	pub fn set_new_source(&mut self, source: String) {
		self.source = source;
	}

	fn eval_buffer(&mut self) {
		match &*self.buffer_string {
			"var" => self.tokens.push(Token::KeyWord(KeyWord::Var)),
			"for" => self.tokens.push(Token::KeyWord(KeyWord::For)),
			"end" => self.tokens.push(Token::KeyWord(KeyWord::End)),
			"in" => self.tokens.push(Token::KeyWord(KeyWord::In)),
			"do" => self.tokens.push(Token::KeyWord(KeyWord::Do)),
			"read" => self.tokens.push(Token::KeyWord(KeyWord::Read)),
			"print" => self.tokens.push(Token::KeyWord(KeyWord::Print)),
			"int" => self.tokens.push(Token::KeyWord(KeyWord::Int)),
			"string" => self.tokens.push(Token::KeyWord(KeyWord::String)),
			"bool" => self.tokens.push(Token::KeyWord(KeyWord::Bool)),
			"assert" => self.tokens.push(Token::KeyWord(KeyWord::Assert)),
			_ => {},
		}
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
			'(' => self.tokens.push(Token::Bracket(Right)),
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

	}

	fn number_scan(&mut self, c: char) {

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
		}
	}

	fn check_for_comment(&mut self, c: char) {

	}

	fn block_comment_handling(&mut self, c: char) {

	}

	fn line_comment_handling(&mut self, c: char) {

	}
}
