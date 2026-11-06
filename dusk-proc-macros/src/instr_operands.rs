//! `#[derive(InstrOperands)]`: generates an inherent impl for walking and rewriting the `ValueId`s and `BlockId`s
//! that a MIR instruction (or a piece of one) refers to.
//!
//! Field handling, applied recursively through `Vec<T>`, `SmallVec<[T; N]>` and `Option<T>`:
//! - `ValueId` fields are value operands.
//! - `BlockId` fields are block operands.
//! - Fields whose type is listed in `DERIVING_TYPES` delegate to that type's generated methods.
//! - Everything else is ignored.
//!
//! Operands are visited in field declaration order.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, Fields, GenericArgument, Member, PathArguments, Type};

/// Every type that derives `InstrOperands`. Fields of these types delegate to the type's generated methods. Deriving
/// `InstrOperands` on a type not in this list is a compile error, so that a new type can't be silently skipped when it
/// appears as a field of another.
const DERIVING_TYPES: &[&str] = &["InstrKind", "JumpTarget", "SwitchCase"];

#[derive(Clone, Copy)]
enum Visit {
    Values,
    ValuesMut,
    Blocks,
    BlocksMut,
}

impl Visit {
    fn method(self) -> TokenStream {
        match self {
            Visit::Values => quote!(visit_values),
            Visit::ValuesMut => quote!(visit_values_mut),
            Visit::Blocks => quote!(visit_blocks),
            Visit::BlocksMut => quote!(visit_blocks_mut),
        }
    }

    fn is_mut(self) -> bool {
        matches!(self, Visit::ValuesMut | Visit::BlocksMut)
    }

    /// The leaf type this visit cares about.
    fn leaf(self) -> &'static str {
        match self {
            Visit::Values | Visit::ValuesMut => "ValueId",
            Visit::Blocks | Visit::BlocksMut => "BlockId",
        }
    }
}

/// If `ty` is `Vec<T>`, `SmallVec<[T; N]>` or `Option<T>`, returns `T` and whether it's an `Option`.
fn container_element(ty: &Type) -> Option<(&Type, bool)> {
    let Type::Path(path) = ty else { return None };
    let seg = path.path.segments.last()?;
    let PathArguments::AngleBracketed(args) = &seg.arguments else { return None };
    let Some(GenericArgument::Type(arg)) = args.args.first() else { return None };
    match seg.ident.to_string().as_str() {
        "Vec" => Some((arg, false)),
        "Option" => Some((arg, true)),
        "SmallVec" => match arg {
            Type::Array(array) => Some((&array.elem, false)),
            _ => None,
        },
        _ => None,
    }
}

fn is_named(ty: &Type, name: &str) -> bool {
    match ty {
        Type::Path(path) => path.qself.is_none() && path.path.segments.last().is_some_and(|seg| seg.ident == name && seg.arguments.is_empty()),
        _ => false,
    }
}

/// Generates code visiting `binding` (an expression of type `&T` or `&mut T`), or `None` if `ty` contains nothing
/// relevant to this visit.
fn visit_field(ty: &Type, binding: &TokenStream, visit: Visit, depth: usize) -> Option<TokenStream> {
    if is_named(ty, visit.leaf()) {
        return Some(if visit.is_mut() {
            quote!(f(#binding);)
        } else {
            quote!(f(*#binding);)
        });
    }
    if is_named(ty, "ValueId") || is_named(ty, "BlockId") {
        return None;
    }
    if let Some((elem, is_option)) = container_element(ty) {
        let item = format_ident!("__item{}", depth);
        let item_tokens = quote!(#item);
        let body = visit_field(elem, &item_tokens, visit, depth + 1)?;
        return Some(if is_option {
            quote!(if let Some(#item) = #binding { #body })
        } else if visit.is_mut() {
            quote!(for #item in #binding.iter_mut() { #body })
        } else {
            quote!(for #item in #binding.iter() { #body })
        });
    }
    if DERIVING_TYPES.iter().any(|name| is_named(ty, name)) {
        let method = visit.method();
        return Some(quote!(#binding.#method(&mut *f);));
    }
    None
}

/// Generates `{ Path { field: __f0, other: __f1, .. } => body }`-style pattern + body for one set of fields.
fn visit_fields(fields: &Fields, visit: Visit) -> (TokenStream, TokenStream) {
    let mut pattern = Vec::new();
    let mut body = TokenStream::new();
    for (i, field) in fields.iter().enumerate() {
        let binding = format_ident!("__f{}", i);
        let binding_tokens = quote!(#binding);
        if let Some(code) = visit_field(&field.ty, &binding_tokens, visit, 0) {
            let member = match &field.ident {
                Some(ident) => Member::Named(ident.clone()),
                None => Member::Unnamed(i.into()),
            };
            pattern.push(quote!(#member: #binding));
            body.extend(code);
        }
    }
    (quote!({ #(#pattern,)* .. }), body)
}

fn visit_method(input: &DeriveInput, visit: Visit) -> TokenStream {
    let method = visit.method();
    let (receiver, callback) = match visit {
        Visit::Values => (quote!(&self), quote!(&mut impl FnMut(ValueId))),
        Visit::ValuesMut => (quote!(&mut self), quote!(&mut impl FnMut(&mut ValueId))),
        Visit::Blocks => (quote!(&self), quote!(&mut impl FnMut(BlockId))),
        Visit::BlocksMut => (quote!(&mut self), quote!(&mut impl FnMut(&mut BlockId))),
    };
    let body = match &input.data {
        Data::Struct(data) => {
            let (pattern, body) = visit_fields(&data.fields, visit);
            quote! {
                let Self #pattern = self;
                #body
            }
        },
        Data::Enum(data) => {
            let arms = data.variants.iter().map(|variant| {
                let ident = &variant.ident;
                let (pattern, body) = visit_fields(&variant.fields, visit);
                quote!(Self::#ident #pattern => { #body })
            });
            quote! {
                match self {
                    #(#arms)*
                }
            }
        },
        Data::Union(_) => unreachable!(),
    };
    quote! {
        #[allow(unused_variables, clippy::needless_borrow)]
        pub fn #method(#receiver, f: #callback) {
            #body
        }
    }
}

pub fn derive(input: DeriveInput) -> TokenStream {
    if let Data::Union(_) = input.data {
        return syn::Error::new_spanned(&input.ident, "InstrOperands cannot be derived for unions").to_compile_error();
    }
    let name = &input.ident;
    if !DERIVING_TYPES.iter().any(|known| name == known) {
        let message = format!(
            "`{name}` is not in `DERIVING_TYPES` in dusk-proc-macros/src/instr_operands.rs; add it there so that fields of \
             type `{name}` are visited",
        );
        return syn::Error::new_spanned(name, message).to_compile_error();
    }
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let methods = [Visit::Values, Visit::ValuesMut, Visit::Blocks, Visit::BlocksMut].map(|visit| visit_method(&input, visit));
    quote! {
        impl #impl_generics #name #ty_generics #where_clause {
            #(#methods)*

            /// All values referenced by `self`, in field order.
            pub fn referenced_values(&self) -> Vec<ValueId> {
                let mut values = Vec::new();
                self.visit_values(&mut |value| values.push(value));
                values
            }

            pub fn references_value(&self, val: ValueId) -> bool {
                let mut found = false;
                self.visit_values(&mut |value| found |= value == val);
                found
            }

            pub fn replace_value(&mut self, old: ValueId, new: ValueId) {
                self.visit_values_mut(&mut |value| if *value == old { *value = new; });
            }

            pub fn replace_block(&mut self, old: BlockId, new: BlockId) {
                self.visit_blocks_mut(&mut |block| if *block == old { *block = new; });
            }
        }
    }
}
