use std::collections::HashMap;

use rkyv::{
	ser::{
		serializers::{AlignedSerializer, AllocScratch, AllocScratchError, FallbackScratch, HeapScratch, SharedSerializeMapError},
		ScratchSpace, Serializer,
	},
	AlignedVec, Fallible, Serialize,
};

use crate::{Data, Datastore, DatastoreError, Hash, NativeHashtype, TypedHash};

/// Represents a Serializer that serializes to an archive and stores in a Datastore
pub trait DatastoreSerializer: Fallible + Serializer + Sized + ScratchSpace {
	fn store<T: NativeHashtype>(&mut self, hashtype: &T) -> Result<Hash, <Self as Fallible>::Error>
	where
		T: Serialize<Self>;
}

#[derive(Debug, Error)]
pub enum LinkSerializeError {
	#[error("scratch error: {0}")]
	AllocScratchError(#[from] AllocScratchError),
	#[error("data store error: {0}")]
	DatastoreError(#[from] DatastoreError),
	#[error("shared map error: {0}")]
	SharedSerializeMapError(#[from] SharedSerializeMapError),
}

/// Serialize object trees into linked hashes and store into a Datastore
pub struct LinkSerializer {
	scratch: FallbackScratch<HeapScratch<1024>, AllocScratch>, // Scratch space
	serializer: AlignedSerializer<AlignedVec>,                 // Serializer
	pointer_map: HashMap<*const (), (Hash, usize)>,            // Check for shared pointers to avoid serializing the same object twice
}

impl LinkSerializer {
	pub fn new() -> Self {
		Self {
			scratch: Default::default(),
			serializer: Default::default(),
			pointer_map: Default::default(),
		}
	}
	pub fn get_vec(&mut self) -> AlignedVec {
		std::mem::take(&mut self.serializer).into_inner()
	}
	pub fn hash<T: NativeHashtype + Serialize<Self>>(&mut self, val: &T) -> TypedHash<T> {
		self.store(val).unwrap().into()
	}
	pub fn join<'a>(&'a mut self, db: &'a mut Datastore) -> DatastoreLinkSerializer<'a> {
		DatastoreLinkSerializer(db, self)
	}
}
impl Fallible for LinkSerializer {
	type Error = LinkSerializeError;
}

impl ScratchSpace for LinkSerializer {
	unsafe fn push_scratch(&mut self, layout: std::alloc::Layout) -> Result<std::ptr::NonNull<[u8]>, Self::Error> {
		Ok(self.scratch.push_scratch(layout)?)
	}

	unsafe fn pop_scratch(&mut self, ptr: std::ptr::NonNull<u8>, layout: std::alloc::Layout) -> Result<(), Self::Error> {
		Ok(self.scratch.pop_scratch(ptr, layout)?)
	}
}

impl Serializer for LinkSerializer {
	fn pos(&self) -> usize {
		self.serializer.pos()
	}

	fn write(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
		Ok(self.serializer.write(bytes).unwrap())
	}
}
impl DatastoreSerializer for LinkSerializer {
	fn store<T: NativeHashtype>(&mut self, hashtype: &T) -> Result<Hash, Self::Error>
	where
		T: Serialize<Self>,
	{
		let _pos = self.serialize_value(hashtype)?;
		let data = Data::new(self.get_vec().into());
		Ok(data.hash())
	}
}

pub struct DatastoreLinkSerializer<'a>(&'a mut Datastore, &'a mut LinkSerializer);

impl<'a> DatastoreSerializer for DatastoreLinkSerializer<'a> {
	fn store<T: NativeHashtype + Serialize<Self>>(&mut self, hashtype: &T) -> Result<Hash, Self::Error> {
		let ptr = (hashtype as *const T).cast::<()>();
		Ok(if let Some((hash, _)) = self.1.pointer_map.get(&ptr) {
			hash.clone().into()
		} else {
			let _pos = self.serialize_value(hashtype)?;
			let vec = self.1.get_vec();
			// Safety: We just serialized this data, therefore it is a valid archive
			let data = Data::new(vec.into());
			let hash = self.0.store(data).into();
			let reverse_links = hashtype.reverse_links(self.1);
			self.0.register::<LinkSerializer, T>(reverse_links, &hash);
			hash
		})
	}
}

impl<'a> Fallible for DatastoreLinkSerializer<'a> {
	type Error = LinkSerializeError;
}

impl<'a> ScratchSpace for DatastoreLinkSerializer<'a> {
	unsafe fn push_scratch(&mut self, layout: std::alloc::Layout) -> Result<std::ptr::NonNull<[u8]>, Self::Error> {
		Ok(self.1.scratch.push_scratch(layout)?)
	}

	unsafe fn pop_scratch(&mut self, ptr: std::ptr::NonNull<u8>, layout: std::alloc::Layout) -> Result<(), Self::Error> {
		Ok(self.1.scratch.pop_scratch(ptr, layout)?)
	}
}

impl<'a> Serializer for DatastoreLinkSerializer<'a> {
	fn pos(&self) -> usize {
		self.1.serializer.pos()
	}

	fn write(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
		Ok(self.1.serializer.write(bytes).unwrap())
	}
}
