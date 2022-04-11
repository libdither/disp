
pub struct Location<'a> {
	context: &'a str,
	location: usize,
	stride: usize,
	string: &'a str,
}
impl<'a> Location<'a> {
	fn display_span(&self, span: &Span) -> String {
		self.display(span.len())
	}
	fn display_str(&self, string: &'a str) -> String {
		self.display(string.len())
	}
	fn display(&self, len: usize) -> String {
		let whitespace_amount = self.char_position.saturating_sub(len);
		let span_marking = std::iter::repeat(" ").take(whitespace_amount)
			.chain(std::iter::repeat("^").take(len));
		format!("{}\n{}", self.string, span_marking.collect::<String>())
	}
}