use std::{fmt, marker::PhantomData};

use bytes::{Buf, BufMut};
use lazy_static::lazy_static;
use serde::{Serialize, Deserialize};

use hashdb::{Link, Datastore, Hash, Hashtype, HashtypeResolveError, TypedHash};

use crate::{lambda_calculus::DatastoreDisplay, symbol::Symbol};

use super::{DisplayWithDatastore, LambdaError};

pub const VARIABLE:    Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
pub const LAMBDA:      Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]);
pub const APPLICATION: Hash = Hash::from_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2]);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LambdaPointer {
	End,
	Left(Link<LambdaPointer>),
	Right(Link<LambdaPointer>),
	Both(Link<LambdaPointer>, Link<LambdaPointer>),
}
impl Hashtype for LambdaPointer {
	fn hash(&self) -> TypedHash<Self> {
		let mut buf = Vec::new();
		use LambdaPointer::*;
		buf.put_u8(match self { End => 0, Left(_) => 1, Right(_) => 2, Both(_, _) => 3 });
		match self {
			End => {},
			Left(v) | Right(v) => buf.put(v.hash().as_bytes()),
			Both(l, r) => {
				buf.put(l.hash().as_bytes());
				buf.put(r.hash().as_bytes());
			}
		}
		Hash::hash(&buf).into()
	}
	/* fn resolve(hash: &TypedHash<Self>, db: &Datastore) -> Result<Self, HashtypeResolveError> {
		use bytes::Buf;
		let mut data = db.get(hash)?;
		match data.get_u8() {
			0 => Self::End,
			1 => Self::Left(Link::resolve(&mut data, db)?),
			2 => Self::Right(Link::resolve(&mut data, db)?),
			3 => Self::Both(Link::resolve(&mut data, db)?, Link::resolve(&mut data, db)?),
			4 => HashtypeResolveError::FormatError("lambda_pointer: invalid u8 value"),
		}
	} */
}
impl DisplayWithDatastore for Link<LambdaPointer> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> fmt::Result {
		match self.as_ref() { 
			LambdaPointer::Left(left) => {
				write!(f, "<{}", left.display(db))
			},
			LambdaPointer::Right(right) => {
				write!(f, ">{}", right.display(db))
			},
			LambdaPointer::Both(left, right) => {
				write!(f, "({},{})", left.display(db), right.display(db))
			},
			LambdaPointer::End => write!(f, "."),
		}
	}
}
impl DisplayWithDatastore for Option<Link<LambdaPointer>>{
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> fmt::Result {
		if let Some(pointer) = self {
			write!(f, "{}", DatastoreDisplay(pointer, db))
		} else { write!(f, "") }
	}
}

pub mod pointer_helpers {
    use hashdb::{Datastore, Link, hashtype::{Hashtype, TypedHash}};

    use super::LambdaPointer;

	pub fn end(db: &mut Datastore) -> Link<LambdaPointer> { LambdaPointer::End.store(db) }
	pub fn left(p: Link<LambdaPointer>, db: &mut Datastore) -> Link<LambdaPointer> { LambdaPointer::Left(p).store(db) }
	pub fn right(p: Link<LambdaPointer>, db: &mut Datastore) -> Link<LambdaPointer> { LambdaPointer::Right(p).store(db) }
	pub fn both(l: Link<LambdaPointer>, r: Link<LambdaPointer>, db: &mut Datastore) -> Link<LambdaPointer> { LambdaPointer::Both(l, r).store(db) }
}


#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr {
	Variable,
	Lambda { pointers: Option<Link<LambdaPointer>>, expr: Link<Expr> },
	Application { function: Link<Expr>, substitution: Link<Expr> },
}
impl Hashtype for Expr {
	fn hash(&self) -> TypedHash<Self> {
		match self {
			Expr::Variable => VARIABLE.into(),
			Expr::Lambda { pointers, expr } => {
				let mut buf = Vec::new();
				buf.put(LAMBDA.as_bytes());
				buf.put_u8(pointers.is_some() as u8);
				if let Some(pointers) = pointers {
					buf.put(pointers.hash().as_bytes());
				}
				buf.put(expr.hash().as_bytes());
				Hash::hash(&buf).into()
			}
			Expr::Application { function, substitution } => {
				let mut buf = Vec::new();
				buf.put(APPLICATION.as_bytes());
				buf.put(function.hash().as_bytes());
				buf.put(substitution.hash().as_bytes());
				Hash::hash(&buf).into()
			}
		}
	}
	/* fn resolve(hash: &TypedHash<Self>, db: &Datastore) -> Result<Self, HashtypeResolveError> {
		// Variable doesn't have any data attached to it
		if hash.into() == VARIABLE { return Ok(Expr::Variable); }

		let data = db.get(hash)?.as_bytes();
		let layout = Hash::from_reader(&mut data)?;
		Ok( match layout {
			_ if layout == LAMBDA => Expr::Lambda {
				pointers: if data.get_u8() == 1 {
					Some(Link::resolve(data, db)?)
				} else { None },
				expr: Link::resolve(data, db)?,
			},
			_ if layout == APPLICATION => Expr::Application {
				function: Link::resolve(&mut data, db)?,
				substitution: Link::resolve(&mut data, db)?,
			},
			_ => Err(LambdaError::InvalidLayout(layout))?
		})
	} */
}
impl DisplayWithDatastore for Expr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &Datastore) -> fmt::Result {
		match db.lookup::<Expr, Symbol>(&self.hash()) {
			Ok(symbol) => return write!(f, "{}", symbol.name()),
			Err(_) => {},
		}
		match self {
			Expr::Variable => write!(f, "x"),
			Expr::Lambda { pointers, expr } => {
				write!(f, "(λ[{}] {})", pointers.display(db), expr.display(db))
			},
			Expr::Application { function, substitution } => {
				write!(f, "({} {})", function.display(db), substitution.display(db))
			},
		}
	}
}
impl Expr {
	pub fn var(db: &mut Datastore) -> Link<Expr> {
		Expr::Variable.store(db)
	}
	pub fn lambda(pointers: Option<Link<LambdaPointer>>, expr: &Link<Expr>, db: &mut Datastore) -> Link<Expr> {
		Expr::Lambda { pointers, expr: expr.clone() }.store(db)
	}
	pub fn app(function: &Link<Expr>, substitution: &Link<Expr>, db: &mut Datastore) -> Link<Expr> {
		Expr::Application { function: function.clone(), substitution: substitution.clone() }.store(db)
	}
	pub fn store(&self, db: &mut Datastore) -> TypedHash<Expr> { self.store(db) }
}