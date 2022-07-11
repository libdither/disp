
/// Named 
struct Named<'e> {
	/// Name for this expr
	name: Option<&'e String>,
	// This expr
	expr: &'e Expr<'e>,
	/// Names for any sub expressions
	named_expr: NamedSubExpr<'e>,
}
enum NamedSubExpr<'e> {
	Variable(&'e Named<'e>),
	Lambda(&'e Named<'e>),
	Application(&'e Named<'e>, &'e Named<'e>),
}