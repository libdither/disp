
use hashdb::{Data, Datastore, Hash, Hashtype, HashtypeResolveError, Link, TypedHash};

use crate::lambda_calculus::Expr;
pub struct Symbol {
	name: Link<String>,
	expr: Link<Expr>,
}

impl Symbol {
	pub fn new(name: impl Into<String>, expr: &Link<Expr>, db: &mut Datastore) -> Link<Self> {
        Self { name: name.into().store(db), expr: expr.clone() }.store(db)
    }
    pub fn expr(&self) -> Link<Expr> {
        self.expr.clone()
    }
}
impl Hashtype for Symbol {
    fn hash(&self) -> TypedHash<Self> {
        use bytes::BufMut;
        let mut data = Vec::new();
        data.put(self.name.hash().as_bytes());
        data.put(self.expr.hash().as_bytes());
        Hash::hash(&data).into()
    }
    /* fn resolve(hash: &TypedHash<Self>, db: &Datastore) -> Result<Self, HashtypeResolveError> {
        let mut data = db.get(hash)?.as_bytes();
        Ok(Self {
            name: Link::resolve(&mut data, db),
            expr: Link::resolve(&mut data, db),
        })
    } */

    fn reverse_links(&self) -> Vec<&Hash> {
        vec![self.name.hash().into(), self.expr.hash().into()]
    }
}