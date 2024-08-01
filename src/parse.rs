use winnow::prelude::*;

enum ParseTree {
	Number(u64),
	Ident(String),
	Assign(Box<ParseTree>, Box<ParseTree>),
	Type(Box<ParseTree>, Box<ParseTree>),
}


#[test]
fn parse_number() {
	let to_pase = "thing := 5";
}