//! Everything type-checking related
use std::fmt;

use hashdb::LinkArena;
use thiserror::Error;

use super::{BindError, BindTypeTree, Expr};

#[derive(Debug, Error)]
pub enum TypeError<'a> {
	#[error("not a type {0}")]
	NotAType(&'a Expr<'a>),
	#[error("attempted to apply rule {0} to term {1}")]
	InvalidRuleForTerm(Rule<'a>, &'a Expr<'a>),

	#[error("mismatching lambda judgement, found {found}, expected {expected_term} : {expected_type}")]
	MismatchingLambdaJudgement { found: &'a Judgement<'a>, expected_term: &'a Expr<'a>, expected_type: &'a Expr<'a> },
	#[error("invalid type for lambda, expected dependent type found type: {0}")]
	InvalidTypeForLambda(&'a Expr<'a>),

	#[error("mismatching application judgement")]
	MismatchingApplicationJudgement, /* { found: &'a Judgement<'a>, expected_term: &'a Expr<'a>, expected_type: &'a Expr<'a> }, */
	#[error("expected function term (lambda or pi) for application, found {0}")]
	ExpectedFunctionType(&'a Expr<'a>),
	#[error("argument type {0} does not match what function expects for {1}")]
	UnexpectedArgumentType(&'a Expr<'a>, &'a Expr<'a>),

	#[error("universe order {0} is too small to contain the term: {1}")]
	SmallUniverseOrder(usize, &'a Expr<'a>),
	#[error("universe order {0} is unnecessarily large to contain the term {1}")]
	OverlyLargeUniverseOrder(usize, &'a Expr<'a>),
	#[error("invalid type for Universe, found type: {0}")]
	InvalidTypeForUniverse(&'a Expr<'a>),
	#[error("mismatching lambda judgement term, found {found}, expected {expected}")]
	MismatchingPiJudgement { found: &'a Expr<'a>, expected: &'a Expr<'a> },

	#[error("binding expected variable to bind but none found")]
	NoVariableToBind,
	#[error("mismatched variable binding")]
	MismatchingVariableBinding,
	#[error("bind error")]
	BindError(#[from] BindError),
}

/// Assertion that a term is typed in a certain context containing dependent Judgements.
#[derive(Debug)]
pub struct Judgement<'a> {
	term: &'a Expr<'a>,
	of_type: &'a Expr<'a>,
	rule: Rule<'a>,
}
impl<'a> fmt::Display for Judgement<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.term, self.of_type)
    }
}

/// Rule that defines what Judgements are needed to check a term.
#[derive(Debug, Clone)]
pub enum Rule<'a> {
	// The type of Expr::Variable must be Expr::Variable
	Var,
	Lambda {
		judge_expr: &'a Judgement<'a>, // Judgement for the body of the lambda
	},
	App {
		judge_func: &'a Judgement<'a>,
		judge_args: &'a Judgement<'a>,
	}
}
impl<'a> fmt::Display for Rule<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
			Rule::Var => "Var",
			Rule::Lambda { .. } => "Lambda",
			Rule::App { .. } => "App",
		})
    }
}

impl<'a> Judgement<'a> {
	pub fn partial_check<'b>(self: &'a Judgement<'a>, mut tree: &'a BindTypeTree<'b, 'a>, reps: &'b LinkArena<'b>) -> Result<(), TypeError> {
		use TypeError as TE;
		
		let bind_type = if let BindTypeTree::End(bind_type) = tree { Some(*bind_type) } else { None };
		match (&self.rule, self.term) {
			(Rule::Var, Expr::Variable) => {
				// Rule: Type of Variable must be Variable, otherwise its type must match its binding
				if let Some(bind_type) = bind_type {
					if self.of_type == bind_type { Ok(()) }
					else { Err(TE::MismatchingVariableBinding) }
				} else {
					Ok(())
				}
			}
			_ if bind_type.is_some() => { Err(TE::NoVariableToBind) }
			(Rule::Lambda { judge_expr }, &Expr::Lambda { bind, expr }) => {
				// Rule: Type of Lambda must be Lambda with the expression matching
				if let &Expr::Lambda { bind: bind_type, expr: expr_type } = self.of_type {
					// Make sure judgement of Lambda expression contains the correct term and type
					if judge_expr.term == expr && judge_expr.of_type == expr_type {
						// Make sure that the subexpression checks correctly with added binding
						tree.push_binding(bind, bind_type, reps)?;
						judge_expr.partial_check(tree, reps)
					} else { Err(TE::MismatchingLambdaJudgement { found: judge_expr, expected_term: expr, expected_type: pi_expr }) }
				} else { Err(TE::InvalidTypeForLambda(self.of_type)) }
			}
			(Rule::App { judge_func, judge_args }, &Expr::Application { func, args }) => {
				// Rule: Application func must be of type lambda, Type of application is output type of func so long as type of args is the same as the input type of func.
				if judge_func.term == func && judge_args.term == args {
					let (func_tree, args_tree) = tree.split()?;
					// Make sure subexpressions are valid first
					judge_func.partial_check(func_tree, reps)?;
					judge_args.partial_check(args_tree, reps)?;
					// Make sure func is a term that can be applied
					if let (&Expr::Lambda { expr: _, .. }, &Expr::Pi { bind_type, expr: _, .. }) = (judge_func.term, judge_func.of_type) {
						if bind_type == judge_args.of_type {
							Ok(())
						} else { Err(TE::UnexpectedArgumentType(bind_type, judge_args.of_type)) }
					} else { Err(TE::ExpectedFunctionType(judge_func.term)) }
				} else { Err(TE::MismatchingApplicationJudgement) }
			}
			_ => Err(TE::InvalidRuleForTerm(self.rule.clone(), self.term))
		}
	}
}