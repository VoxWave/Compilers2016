use util::Direction::*;

enum Token {
	Bracket(Direction),
	CurlyBracket(Direction),
	Identifier(String),
	StringLiteral(String),
	Number(String),
	Semicolon,
	Colon,
	Operator(Operator),
	KeyWord(KeyWord),
}

enum Operator {
	Plus, Minus, Multiply, Divide,
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
	pub fn scan(&mut self) -> Vec<Token> {
	    for c in source.chars() {
			match scan_mode {
				ScanMode::Normal => self.normal_scan(c),
				ScanMode::String => self.string_scan(c),
				ScanMode::Number => self.number_scan(c),
				ScanMode::PossibleComment => self.check_for_comment(c),
				ScanMode::LineComment => self.line_comment_handling(c),
				ScanMode::BlockComment => self.block_comment_handling(c),
				ScanModes::Other => self.identifier_and_keyword_scan(c),
			}
	    }
	}

	pub fn set_new_source(&mut self, source: String) {
		self.source = source;
	}

	fn eval_buffer(&mut self) {

	}

	fn normal_scan(&mut self, c: char) {
		match c {
			' ', '\n', '\t' => {}
			'0'...'9' => {
				buffer_string.push(c);
				self.scan_mode = ScanMode::Number;
			},
			'"' => {
				self.scan_mode = ScanMode::String;
			}
			'{' => tokens.push(Token::CurlyBracket(Left)),
			'}' => tokens.push(Token::CurlyBracket(Right)),
			'(' => tokens.push(Token::Bracket(Left)),
			'(' => tokens.push(Token::Bracket(Right)),
			';' => tokens.push(Token::Semicolon),
			':' => tokens.push(Token::Colon),
			'+' => tokens.push(Token::Operator(Operator::Plus)),
			'-' => tokens.push(Token::Operator(Operator::Plus)),
			'*' => tokens.push(Token::Operator(Operator::Plus)),
			'/' => {
				buffer_string.push(c);
				self.scan_mode = ScanMode::PossibleComment;
			},
			_ => {
				buffer_string.push(c);
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
			' ', '\n', '\t' => {
				self.eval_buffer();
				self.scan_mode = ScanModes::Normal;
			}
			'0'...'9' => {
				self.eval_buffer();
				buffer_string.push(c);
				self.scan_mode = ScanMode::Number;
			},
			'"' => {
				self.scan_mode = ScanMode::String;
			},
			'{' => {
				self.eval_buffer();
				tokens.push(Token::CurlyBracket(Left));
				self.scan_mode = ScanModes::Normal;
			},
			'}' => {
				self.eval_buffer();
				tokens.push(Token::CurlyBracket(Right));
				self.scan_mode = ScanModes::Normal;
			},
			'(' => {
				self.eval_buffer();
				tokens.push(Token::Bracket(Left));
				self.scan_mode = ScanModes::Normal;
			},
			'(' => {
				tokens.push(Token::Bracket(Right));
				self.scan_mode = ScanModes::Normal;
			},
			';' => {
				tokens.push(Token::Semicolon);
				self.scan_mode = ScanModes::Normal;
			},
			':' => {
				tokens.push(Token::Colon);
				self.scan_mode = ScanModes::Normal;
			},
			'+' => {
				tokens.push(Token::Operator(Operator::Plus));
				self.scan_mode = ScanModes::Normal;
			},
			'-' => {
				tokens.push(Token::Operator(Operator::Plus));
				self.scan_mode = ScanModes::Normal;
			},
			'*' => {
				tokens.push(Token::Operator(Operator::Plus));
				self.scan_mode = ScanModes::Normal;
			},
			'/' => {
				buffer_string.push(c);
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
