use util::Direction;

enum Token {
	Bracket(Direction),
	CurlyBracket(Direction),
	Identifier(String),
	StringLiteral(String),
	Semicolon,
	Colon
	Var,
	For,
	End,
	In,
	Do,
	Read,
	Print,
	Int,
	String,
	Bool,
	Assert,
}

enum ScanMode {
	Normal, String, Number,
}

fn scan(source: String) -> Vec<Token> {
	let mut scan_mode = ScanMode::Normal;
    let mut processed_strings: Vec<Token> = Vec::new();
    let mut buffer_string = String::new();

    for c in source.chars() {
		match scan_mode {
			ScanMode::Normal => {
				match c {
			        ';' => {
						if !buffer_string.is_empty() {
							eval_buffer(&mut buffer_string);
						}
					},
					'0'...'9' => {

					},
			    }
			}
			ScanMode::String => {

			},
		}
    }
}

fn eval_buffer(buffer_string: &mut String) -> Token {

}
