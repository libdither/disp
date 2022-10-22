#![feature(proc_macro_quote)]

use std::{collections::hash_map::DefaultHasher, hash::{Hash, Hasher}};

use syn::{Attribute, Field, Fields, Generics, Item, ItemEnum, ItemImpl, ItemStruct, ItemType, ItemUnion, Lit, Meta, MetaList, NestedMeta, Token, WhereClause, parse_macro_input, parse_quote, punctuated::Punctuated};
use quote::quote;
use proc_macro2::TokenStream;
use proc_macro::{TokenStream as ProcTokenStream};

/// # Hashtype
/// Implements HashType for struct, enum, or type if type is not generic over type or constants.
/// Also derives Hash, PartialEq, Eq, rkyv::Archive, rkyv::Serialize, and rkyv::Deserialize if applied to struct or enum.
/// 
/// # Usage
/// ```
/// // Derives traits, doesn't implement HashType because of generic T.
/// #[hashtype]
/// struct Vec2<T> { first: T, second: T }
/// 
/// // Derives traits and HashType, Wraps &'a String with WithHashType wrapper for rkyv.
/// #[hashtype]
/// struct Name<'a> { #[hashtype] name: &'a String }
/// 
/// // Derives traits. Derives HashType with reverse linked "name" field.
/// #[hashtype(reverse_links("name"))]
/// struct NameList<'a> { name: Name<'a>, #[hashtype] rest: &'a NameList<'a> }
/// 
/// // Doesn't derive traits because not struct or enum, derives HashType with reverse linked "first" and "second" fields.
/// #[hashtype(reverse_links("first", "second"))]
/// type Vec2f32 = Vec2<f32>;
/// ```
#[proc_macro_attribute]
pub fn hashtype(attr: ProcTokenStream, input: ProcTokenStream) -> ProcTokenStream {
	// let args = parse_macro_input!(attr as Meta);

	/* let matches: Vec<String> = match args {
		Meta::List(MetaList { nested, .. }) => {
			nested.into_iter().flat_map(|meta| match meta {
				NestedMeta::Lit(Lit::Str(string)) => Some(string.value()),
				_ => None,
			}).collect()
		}
		Meta::Path(_) => vec![],
		Meta::NameValue(_) => unimplemented!(),	
	}; */

	let mut item = parse_macro_input!(input as Item);

	fn replace_field_attrs(field: &mut Field) {
		let attrs = &mut field.attrs;
		if let Some((idx, _)) = attrs.into_iter().enumerate().flat_map(|(idx, attr)| attr.path.get_ident().map(|id|(idx, id))).find(|(_, ident)| &format!("{}", ident) == "subtype") {
			attrs.remove(idx);
			attrs.push(parse_quote! {
				#[with(WithHashType)]
			});
			attrs.push(parse_quote! {
				#[omit_bounds]
			});
		}
	}
	match &mut item {
		Item::Union(ItemUnion { fields, .. }) | Item::Struct(ItemStruct { fields: Fields::Named(fields), .. }) => {
			for field in fields.named.iter_mut() { replace_field_attrs(field) }
		}
		Item::Struct(ItemStruct { fields: Fields::Unnamed(fields), .. }) => {
			for field in fields.unnamed.iter_mut() { replace_field_attrs(field) }
		}
		Item::Enum(ItemEnum { variants, .. }) => {
			for variant in variants {
				for field in &mut variant.fields {
					replace_field_attrs(field)
				}
			}
		}
		_ => unimplemented!()
	}

	let (derives, ident, hash) = match &item {
		Item::Type(ItemType { ident, generics, .. }) => {
			(
				quote! { #item },
				ident,
				if generics.type_params().next().is_none() && generics.const_params().next().is_none() {
					let hash = get_hash(&item);
					quote! { Some(#hash) }
				} else { quote! { None } }
			)
		}
		Item::Struct(ItemStruct { ident, generics, .. })
		 | Item::Enum(ItemEnum { ident, generics, .. })
		 | Item::Union(ItemUnion { ident, generics, .. }) => {
			let lifetime = generics.lifetimes().next().expect("#[hashtype] requires object to have at least one lifetime").clone().lifetime;
			let deserialize_literal = format!("__D: ArchiveDeserializer<{lifetime}>");
			(
				quote! {
					#[derive(Hash, PartialEq, rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)]
					#[archive_attr(derive(bytecheck::CheckBytes))]
					#[archive(bound(serialize = "__S: ArchiveStore", deserialize = #deserialize_literal))]
					#item
				},
				ident,
				if generics.type_params().next().is_none() && generics.const_params().next().is_none() {
					let hash = get_hash(&item);
					quote! { Some(#hash) }
				} else { quote! { None } }
			)
		}
		_ => unimplemented!(),
	};
	
    let output = quote! {
		#derives

		impl HashType for #ident {
			fn reverse_links(&self) -> impl Iterator<Item = u64> {
				std::iter::empty::<u64>()
			}
			fn unique_id() -> Option<UniqueHashTypeId> {
				#hash
			}
		}
    };
	output.into()
}

fn get_hash(hash_object: &impl Hash) -> u64 {
	let mut hasher = DefaultHasher::new();
	hash_object.hash(&mut hasher);
	hasher.finish()
}


// return HashType impl for some Data (struct, enum or union)
fn impl_hashtype(item: &syn::ItemImpl) -> TokenStream {
	let ItemImpl { generics, self_ty, .. } = item;
	quote! {
		impl #generics HashType for #self_ty {
			fn reverse_links(&self) -> impl Iterator<Item = u64> {
				std::iter::empty::<u64>()
			}
			fn unique_id() -> Option<UniqueHashTypeId> {
				None
			}
		}
	}
}

#[proc_macro]
pub fn impl_hashtype_for(input: ProcTokenStream) -> ProcTokenStream {
	let path = parse_macro_input!(input as syn::ItemImpl);
	let output = impl_hashtype(&path);

	output.into()
}

#[proc_macro]
pub fn impl_hashtype_for_many(input: ProcTokenStream) -> ProcTokenStream {
	let list = parse_macro_input!(input with Punctuated<ItemImpl, Token![,]>::parse_terminated).into_iter();
	let mut output = TokenStream::new();
	output.extend(list.map(|path|impl_hashtype(&path)));

	output.into()
}