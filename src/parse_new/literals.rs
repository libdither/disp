use super::SyntaxNode;


pub struct StringLiteral {
	string: &str,
}
pub enum StringError<'a> {
	NoClosingQuote,
}
fn parse_string<'a>(stream: impl Iterator<Item=SyntaxNode<'a>>) -> Result<impl Iterator<Item=SyntaxNode<'a>>, ParseError<'a>> {
	Ok(stream.try_map(|s| match s {
		SyntaxNode::Stream(raw_string) => {
			let mut string_start = None;
			raw_string.char_indices().filter_map(|(pos, ch)| match ch {
				'"' => if let Some(start) = string_start {
					Some(SyntaxNode::String(&raw_string[start+1..pos]))
				} else {
					string_start = Some(pos);
					None
				}
				_ => None,
			})
		},
		s => std::iter::once(s),
	})?.flatten())
}
fn print_string<'a>(string: &str, out: &mut String) -> Result<impl Iterator<Item=SyntaxNode<'a>>, ParseError<'a>> {
	out += r#"""#;
	out += string;
	out += r#"""#;
}