use std::{fmt, marker::PhantomData};

use bytes::{Buf, BufMut};
use lazy_static::lazy_static;
use serde::{Serialize, Deserialize};

use hashdb::{Data, Datastore, Hash};

use super::{LambdaError, typed::{Datatype, Hashtype, TypedHash}};

lazy_static! {
	pub static ref VARIABLE:    Hash = Hash::from_sha256_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
	pub static ref LAMBDA:      Hash = Hash::from_sha256_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]);
	pub static ref APPLICATION: Hash = Hash::from_sha256_digest([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2]);
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum LambdaPointer {
	End,
	Left(TypedHash<LambdaPointer>),
	Right(TypedHash<LambdaPointer>),
	Both(TypedHash<LambdaPointer>, TypedHash<LambdaPointer>),
}
impl Datatype for LambdaPointer {
	type Error = LambdaError;
	fn to_data_untyped(&self) -> Data {
		let mut buf = Vec::new();
		bincode::serialize_into(&mut buf, self).expect("bincode should not fail");
		Data::from_vec(buf)
	}
	fn from_data_untyped(data: &Data) -> Result<Self, Self::Error> {
		Ok(bincode::deserialize_from(data.as_bytes())?)
	}
	fn db_error(hash: Hash) -> Self::Error { LambdaError::NotInDatastore(hash) }
}
pub mod pointer_helpers {
    use hashdb::Datastore;

    use crate::lambda_calculus::{TypedHash, typed::Hashtype};

    use super::LambdaPointer;

	pub fn end(db: &mut Datastore) -> TypedHash<LambdaPointer> { LambdaPointer::End.to_hash(db) }
	pub fn left(p: TypedHash<LambdaPointer>, db: &mut Datastore) -> TypedHash<LambdaPointer> { LambdaPointer::Left(p).to_hash(db) }
	pub fn right(p: TypedHash<LambdaPointer>, db: &mut Datastore) -> TypedHash<LambdaPointer> { LambdaPointer::Right(p).to_hash(db) }
	pub fn both(l: TypedHash<LambdaPointer>, r: TypedHash<LambdaPointer>, db: &mut Datastore) -> TypedHash<LambdaPointer> { LambdaPointer::Both(l, r).to_hash(db) }
}

	
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable;
impl Hashtype for Variable {
	type Error = LambdaError;
	fn from_hash(_: &TypedHash<Self>, _: &Datastore) -> Result<Self, Self::Error> {
		Ok(Variable)
	}
	fn to_hash(&self, _: &mut Datastore) -> TypedHash<Self> {
		// Safety, hash is static, type is known
		unsafe { TypedHash::from_hash_unchecked(VARIABLE.clone(), PhantomData::<Self>::default()) }
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Lambda { pub pointers: Option<TypedHash<LambdaPointer>>, pub expr: TypedHash<Expr> }

impl Datatype for Lambda {
	type Error = LambdaError;
	fn to_data_untyped(&self) -> Data {
		let mut buf = Vec::new();
		buf.put(LAMBDA.as_bytes());
		let pointers_serialized = bincode::serialize(&self.pointers).expect("this should not fail to serialize");
		buf.put(&pointers_serialized[..]);
		buf.put(self.expr.as_bytes());
		
		Data::from_vec(buf)
	}
	fn from_data_untyped(data: &Data) -> Result<Self, Self::Error> {
		let mut reader = data.as_bytes();
		let layout = &reader.copy_to_bytes(LAMBDA.len())[..];
		if layout != LAMBDA.as_bytes() { return Err(LambdaError::InvalidLayout(Hash::from_raw_bytes(layout))) }
		let pointers = bincode::deserialize_from(&mut reader)?;
		//let pointers = unsafe { TypedHash::from_bytes(&reader.copy_to_bytes(LAMBDA.len())[..]) };
		let expr = unsafe { TypedHash::from_bytes(&reader.copy_to_bytes(LAMBDA.len())[..]) };
				
		Ok(Lambda { pointers, expr })
	}
	fn db_error(hash: Hash) -> Self::Error { LambdaError::NotInDatastore(hash) }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Application { pub function: TypedHash<Expr>, pub substitution: TypedHash<Expr> }
impl Datatype for Application {
	type Error = LambdaError;
	fn to_data_untyped(&self) -> Data {
		let mut buf = Vec::new();
		buf.put(APPLICATION.as_bytes());
		buf.put(self.function.as_bytes());
		buf.put(self.substitution.as_bytes());
		Data::from_vec(buf)
	}
	fn from_data_untyped(data: &Data) -> Result<Self, Self::Error> {
		let mut reader = data.as_bytes();
		let layout = &reader.copy_to_bytes(LAMBDA.len())[..];
		if layout != APPLICATION.as_bytes() { return Err(LambdaError::InvalidLayout(Hash::from_raw_bytes(layout))) }
		// Safety: this function is used in leau of Hash::from_raw_parts
		unsafe { Ok(Application {
			function: TypedHash::from_bytes(&reader.copy_to_bytes(LAMBDA.len())[..]),
			substitution: TypedHash::from_bytes(&reader.copy_to_bytes(LAMBDA.len())[..])
		}) }
	}
	fn db_error(hash: Hash) -> Self::Error { LambdaError::NotInDatastore(hash) }
}


#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr {
	Var(Variable),
	Lam(Lambda),
	App(Application)
}
impl Hashtype for Expr {
	type Error = LambdaError;
	fn to_hash(&self, db: &mut Datastore) -> TypedHash<Self> {
		unsafe { match self {
			Expr::Var(variable) => variable.to_hash(db).cast_type(),
			Expr::Lam(lambda) => lambda.to_hash(db).cast_type(),
			Expr::App(application) => application.to_hash(db).cast_type(),
		} }
	}
	fn from_hash(hash: &TypedHash<Self>, db: &Datastore) -> Result<Self, Self::Error> {
		// Variable doesn't have any data attached to it
		if hash.as_bytes() == VARIABLE.as_bytes() { return Ok(Expr::Var(Variable)); }

		let data = db.get(hash.untyped()).ok_or(LambdaError::NotInDatastore(hash.untyped().clone()))?;
		let layout = &data.as_bytes().copy_to_bytes(LAMBDA.len())[..];
		Ok( match layout {
			_ if layout == LAMBDA.as_bytes() => Expr::Lam(Lambda::from_data_untyped(data)?),
			_ if layout == APPLICATION.as_bytes() => Expr::App(Application::from_data_untyped(data)?),
			_ => Err(LambdaError::InvalidLayout(Hash::from_raw_bytes(layout)))?
		})
	}
}
impl Expr {
	pub fn var() -> TypedHash<Expr> {
		unsafe { TypedHash::from_hash_unchecked(VARIABLE.clone(), PhantomData::default()) }
	}
	pub fn lambda(pointers: Option<TypedHash<LambdaPointer>>, expr: &TypedHash<Expr>, db: &mut Datastore) -> TypedHash<Expr> {
		Expr::Lam(Lambda { pointers, expr: expr.clone() }).to_hash(db)
	}
	pub fn app(function: &TypedHash<Expr>, substitution: &TypedHash<Expr>, db: &mut Datastore) -> TypedHash<Expr> {
		Expr::App(Application { function: function.clone(), substitution: substitution.clone() }).to_hash(db)
	}
	pub fn store(&self, db: &mut Datastore) -> TypedHash<Expr> { self.to_hash(db) }
}

// TODO: Make this better
impl fmt::Display for Expr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}", self)
	}
}
pub fn print_expr(hash: &TypedHash<Expr>, db: &mut Datastore) -> Result<(), LambdaError> {
	Ok(println!("{}", format_expr(&Expr::from_hash(hash, db)?, db)?))
}
pub fn write_lambda_pointers(string: &mut String, pointers: &TypedHash<LambdaPointer>, db: &Datastore) -> Result<(), LambdaError> {
	use fmt::Write;
	match &pointers.resolve(db)? { 
		LambdaPointer::Left(hash) => {
			write!(string, "<")?;
			write_lambda_pointers(string, hash, db)?;
		},
		LambdaPointer::Right(hash) => {
			write!(string, ">")?;
			write_lambda_pointers(string, hash, db)?;
		}
		LambdaPointer::Both(left, right) => {
			write!(string, "(")?;
			write_lambda_pointers(string, left, db)?;
			write!(string, ",")?;
			write_lambda_pointers(string, right, db)?;
			write!(string, ")")?;
		}
		LambdaPointer::End => write!(string, ".")?,
	}
	Ok(())
}
pub fn format_expr(expr: &Expr, db: &mut Datastore) -> Result<String, LambdaError> {
	Ok(match expr {
		Expr::Var(_) => "x".to_owned(),
		Expr::Lam(Lambda { pointers, expr }) => {
			let mut pointer_tree = String::new();
			if let Some(pointers) = pointers { write_lambda_pointers(&mut pointer_tree, pointers, db)? };
			format!("(λ[{}] {})", pointer_tree, format_expr(&expr.resolve(db).unwrap(), db)?)
		},
		Expr::App(Application { function, substitution: _ }) => {
			let lambda_string = format_expr(&function.resolve(db)?, db)?;
			let expr_string = format_expr(&Expr::from_hash(function, db)?, db)?;
			format!("({} {})", lambda_string, expr_string)
		},
	})
}

pub fn parse_hash(hash: &TypedHash<Expr>, db: &mut Datastore) -> Result<String, LambdaError> {
	format_expr(&Expr::from_hash(hash, db)?, db)
}