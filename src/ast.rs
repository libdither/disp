

pub enum SyntaxTree<'a> {
	Namespace(&'a Namespace<'a>),
	Expr(&'a Expr<'a>),
}

pub struct Namespace<'a> {
	items: Vec<&'a SyntaxTree<'a>>
}