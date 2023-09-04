
use std::{cell::{RefCell}, collections::HashMap, marker::PhantomData, rc::Rc, io, iter, fmt};


use crate::{HashType, LinkArena, ArchiveStore, ArchiveDeserializer, ArchiveFetchable, ArchiveStorable, Datastore, DatastoreError};

use super::{RevHashType, RevTypeStore, TypeStore, ArchiveToType};

pub struct RevLinkStore<'a, A: TypeStore<'a>> {
	// Reverse lookup map, first u64 is hash of linked object, u64 is unique_id of link. Maps to list of links.
	reverse_links_map: RefCell<HashMap< (u64, u64), Rc<RefCell<Vec<*const ()>>> >>, 
	store: &'a A,
	db: RefCell<Datastore>,
	_phantom: PhantomData<&'a ()>
}

impl<'a, A: TypeStore<'a>> fmt::Debug for RevLinkStore<'a, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RevLinkStore").field("reverse_links_map", &self.reverse_links_map).finish()
    }
}

impl<'a, A: TypeStore<'a>> TypeStore<'a> for RevLinkStore<'a, A> {
    fn add<T: HashType<'a>>(&'a self, val: T) -> &'a T {
        self.store.add(val)
    }

    fn add_str(&'a self, string: &str) -> &'a str {
        self.store.add_str(string)
    }
	
}

impl<'a, A: TypeStore<'a>> RevTypeStore<'a> for RevLinkStore<'a, A> {
    fn rev_add<T: RevHashType<'a>>(&'a self, val: T) -> &'a T 
	where
		T: for<'s> ArchiveFetchable<'a, ArchiveToType<'s, 'a, Datastore, Self>> + ArchiveStorable<Datastore>,
	{
		// attempt to populate unarchived map if empty from archived map
		let unique_link_id = T::unique_id();
		let hashes = T::reverse_links(&val);

		self.try_populate_links::<T>(unique_link_id, hashes).unwrap();

		let multihash = ArchiveStore::store(&mut *self.db.borrow_mut(), &val).map_err(|_|"failed to store RevHashType").unwrap();

        let ret = self.store.add(val);
		let ptr = (ret as *const T) as *const ();

		// Store Reverse Links
		let mut reverse_links_map = self.reverse_links_map.borrow_mut();
		let archived_map = &mut self.db.borrow_mut().links_map;
		for hash in T::reverse_links(ret) {

			let entry = reverse_links_map.entry((hash, unique_link_id)).or_default();

			// Note: See safety note in find_reverse_links, ptr points to a T
			entry.borrow_mut().push(ptr);

			// Push hash
			let hash_entry = archived_map.entry((hash, T::unique_id())).or_default();
			hash_entry.push(multihash.clone().into());
		}
		// println!("Added reverse links {:?}", ret);

		ret
    }

	// TODO: Only the deprecated version of this where clause works for some reason.
	#[allow(deprecated_where_clause_location)]
	type Iter<'i: 'a, L: 'i> where Self: 'i = ReverseLinkIter<'i, L>;

	fn links<T: HashType<'a>, L: RevHashType<'a>>(&'a self, val: &T) -> Self::Iter<'a, L> 
	where
		L: for<'s> ArchiveFetchable<'a, ArchiveToType<'s, 'a, Datastore, Self>>,
	{

		let hash = super::arena::get_hash(val);

		self.try_populate_links::<L>(L::unique_id(), iter::once(hash)).unwrap();

		let key = &(hash, L::unique_id());
		let links = self.reverse_links_map.borrow().get(key).cloned();
		// println!("Looking up reverse links for {:?}", val);
		ReverseLinkIter::<'a, L> { links, index: 0, _phantom: Default::default(), _type: Default::default() }
	}
}
impl<'a, A: TypeStore<'a>> RevLinkStore<'a, A> {
	pub fn new(store: &'a A) -> Self {
		RevLinkStore {
			reverse_links_map: RefCell::new(HashMap::new()),
			store,
			db: RefCell::new(Datastore::new()),
			_phantom: Default::default(),
		}
	}

	fn try_populate_links<T: RevHashType<'a>>(&'a self, unique_link_id: u64, hashes: impl Iterator<Item = u64>) -> Result<(), DatastoreError> 
	where
		T: for<'s> ArchiveFetchable<'a, ArchiveToType<'s, 'a, Datastore, Self>>,
	{
		let db = &*self.db.borrow();
		let mut de = ArchiveToType { store: db, type_store: self };

		let mut reverse_links_map = self.reverse_links_map.borrow_mut();
		let archived_map = &db.links_map;
		for hash in hashes {
			// If links map is empty 
			if !reverse_links_map.contains_key(&(hash, unique_link_id)) {
				let mut entry = reverse_links_map.entry((hash, unique_link_id)).or_default().borrow_mut();
				// and archived map contains items
				if let Some(multihashes) = archived_map.get(&(hash, unique_link_id)) {
					for multihash in multihashes {
						let deserialized = de.fetch_ref(multihash)?;
						let ptr = (deserialized as *const T) as *const ();
						entry.push(ptr);
					}
				}
			}
		}

		Ok(())
	}
	pub fn save(&self, writer: &mut impl io::Write) -> Result<(), DatastoreError> {
		self.db.borrow().save(writer)?;
		Ok(())
	}
	pub fn load(&self, reader: &mut impl io::Read) -> Result<(), DatastoreError> {
		self.db.borrow_mut().load(reader)?;

		Ok(())
	}
}

// Iterator that lives for lifetime 'i and returns references to objects L that also live for lifetime 'i
pub struct ReverseLinkIter<'i, L: 'i> {
	links: Option<Rc<RefCell<Vec<*const ()>>>>,
	index: usize,
	_type: PhantomData<L>,
	_phantom: PhantomData<&'i ()>,
}
impl<'i, L: 'i> Iterator for ReverseLinkIter<'i, L> {
    type Item = &'i L;

    fn next(&mut self) -> Option<Self::Item> {
		if let Some(links) = &mut self.links {
			links.borrow().get(self.index).map(|ptr| {
				// Safety: All ptrs put in Vec should be of TypeId L
				self.index += 1;
				unsafe { &*ptr.cast::<L>() }
			})
		} else { None }
		
    }
}

pub type RevLinkArena<'a> = RevLinkStore<'a, LinkArena<'a>>;



/* pub struct DatastoreRef<'a>(Ref<'a, Datastore>);

impl<'a> ArchiveStoreRead for DatastoreRef<'a> {
    fn fetch_archive<T>(&self, hash: &crate::TypedHash<T>) -> Result<&rkyv::Archived<T>, Self::Error>
	where
		T: rkyv::Archive<Archived: for<'v> bytecheck::CheckBytes<rkyv::validation::validators::DefaultValidator<'v>>> + super::ArchiveInterpretable {
        self.0.fetch_archive(hash)
    }
}
impl<'a> DataStoreRead for DatastoreRef<'a> {
    type DataError = <Datastore as DataStoreRead>::DataError;

    fn get(&self, hash: &Hash) -> Result<&[u8], Self::DataError> {
        self.0.get(hash)
    }
}

impl<'a> Fallible for DatastoreRef<'a> {
    type Error = <Datastore as Fallible>::Error;
} */