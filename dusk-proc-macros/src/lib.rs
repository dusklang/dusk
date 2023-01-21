extern crate proc_macro;

use proc_macro::{TokenStream, TokenTree, Ident, Span, Punct, Spacing, Group, Delimiter};
use proc_macro::token_stream::IntoIter as TokenIter;

use quote::quote;
use syn::parse::{ParseStream, Parse};
use syn::{Item, Meta, Lit, LitStr, Type, ImplItem, FnArg, ReturnType, Attribute};
use syn::spanned::Spanned;

use std::iter::Peekable;

struct Parser {
    iter: Peekable<TokenIter>,
}

enum Expression {
    Simple(TokenStream),
    Dot {
        base: TokenStream,
        dot: TokenTree,
        field: TokenStream,
    }
}

impl Expression {
    fn simplify(self) -> TokenStream {
        match self {
            Expression::Simple(simple) => simple,
            Expression::Dot { mut base, dot, field } => {
                base.extend([dot]);
                base.extend(field);
                base
            }
        }
    }

    fn adding(self, dot: TokenTree, field: TokenStream) -> Expression {
        let stream = self.simplify();
        Expression::Dot {
            base: stream,
            dot,
            field
        }
    }
}

impl Parser {
    fn parse_expression(&mut self) -> Expression {
        let base = self.iter.next().unwrap();
        let mut expression = Expression::Simple(TokenStream::from(base));
        while let Some(tok) = self.iter.peek() {
            let dot = match tok {
                TokenTree::Punct(punct) if punct.as_char() == '.' => {
                    self.iter.next().unwrap()
                },
                TokenTree::Punct(punct) if punct.as_char() == ',' => {
                    break;
                },
                _ => panic!("unexpected token {}", tok),
            };
            let tok = self.iter.next().unwrap();
            match tok {
                TokenTree::Ident(_) => expression = expression.adding(dot, TokenStream::from(tok)),
                _ => panic!("unexpected token {}", tok),
            }
        }
        expression
    }

    fn try_parse_comma(&mut self) -> bool {
        match self.iter.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => {
                self.iter.next();
                true
            },
            _ => false,
        }
    }
}

struct Builder {
    stream: TokenStream,
}

impl Builder {
    fn new() -> Self {
        Builder { stream: TokenStream::new() }
    }

    fn ident(mut self, ident: &str) -> Self {
        self.stream.extend([
            TokenTree::Ident(Ident::new(ident, Span::call_site())),
        ]);
        self
    }

    fn field(mut self, name: &str) -> Self {
        self.stream.extend([
            TokenTree::Punct(Punct::new('.', Spacing::Alone)),
        ]);
        self.ident(name)
    }

    fn brackets(mut self, index: impl Into<TokenStream>) -> Self {
        self.stream.extend([
            TokenTree::Group(
                Group::new(
                    Delimiter::Bracket,
                    index.into(),
                )
            )
        ]);
        self
    }

    fn ast_code(mut self, driver: impl Into<TokenStream>) -> Self {
        self.stream.extend(driver.into());
        self
            .field("code")
            .field("ast")
    }

    fn expr_to_item(self, driver: impl Into<TokenStream>, expr_id: impl Into<TokenStream>) -> Self {
        self.ast_code(driver)
            .field("expr_to_items")
            .brackets(expr_id.into())
    }

    fn decl_to_item(self, driver: impl Into<TokenStream>, expr_id: impl Into<TokenStream>) -> Self {
        self.ast_code(driver)
            .field("decl_to_items")
            .brackets(expr_id.into())
    }

    fn source_range(self, driver: impl Into<TokenStream>, item_id: impl Into<TokenStream>) -> Self {
        self
            .ast_code(driver)
            .field("source_ranges")
            .brackets(item_id)
    }

    fn generic_ctx_id(self, driver: impl Into<TokenStream>, item_id: impl Into<TokenStream>) -> Self {
        self
            .ast_code(driver)
            .field("item_generic_ctxs")
            .brackets(item_id)
    }

    fn generic_ctx_id_from_expr(self, driver: impl Into<TokenStream> + Clone, expr_id: impl Into<TokenStream>) -> Self {
        self.generic_ctx_id(driver.clone(), Builder::new().expr_to_item(driver, expr_id))
    }
}

impl From<Builder> for TokenStream {
    fn from(b: Builder) -> TokenStream {
        b.stream
    }
}

fn parse_input(input: TokenStream) -> (TokenStream, Expression) {
    let mut parser = Parser { iter: input.into_iter().peekable() };
    let mut driver = Builder::new().ident("self").stream;
    let mut field = parser.parse_expression();
    if parser.try_parse_comma() {
        driver = field.simplify();
        field = parser.parse_expression();
    }
    (driver, field)
}

#[proc_macro]
pub fn ef(input: TokenStream) -> TokenStream {
    let (driver, field) = parse_input(input);
    match field {
        Expression::Dot { base, field, .. } => {
            match field.to_string().as_str() {
                "range" => {
                    Builder::new()
                        .source_range(
                            driver.clone(),
                            Builder::new().expr_to_item(driver, base)
                        )
                },
                "generic_ctx" => {
                    Builder::new()
                        .ast_code(driver.clone())
                        .field("generic_ctxs")
                        .brackets(Builder::new().generic_ctx_id_from_expr(driver, base))
                }
                "generic_ctx_id" => {
                    Builder::new()
                        .generic_ctx_id_from_expr(
                            driver,
                            base,
                        )
                },
                "ast" => {
                    Builder::new()
                        .ast_code(driver)
                        .field("exprs")
                        .brackets(base)
                },
                "item" => {
                    Builder::new()
                        .expr_to_item(driver, base)
                }
                name => panic!("Unrecognized expression field name {}", name),
            }
        },
        Expression::Simple(expr_id) => panic!("Unexpected bare identifier '{}'. Must have field", expr_id),
    }.stream
}

#[proc_macro]
pub fn df(input: TokenStream) -> TokenStream {
    let (driver, field) = parse_input(input);
    match field {
        Expression::Dot { base, field, .. } => {
            match field.to_string().as_str() {
                "range" => {
                    Builder::new()
                        .source_range(
                            driver.clone(),
                            Builder::new().decl_to_item(driver, base)
                        )
                        .stream
                },
                "generic_ctx_id" => {
                    Builder::new()
                        .generic_ctx_id(
                            driver.clone(),
                            Builder::new().decl_to_item(driver, base)
                        )
                        .stream
                },
                "ast" => {
                    Builder::new()
                        .ast_code(driver)
                        .field("decls")
                        .brackets(base)
                        .stream
                },
                "item" => {
                    Builder::new()
                        .decl_to_item(driver, base)
                        .stream
                }
                name => panic!("Unrecognized expression field name {}", name),
            }
        },
        Expression::Simple(decl_id) => panic!("Unexpected bare identifier '{}'. Must have field", decl_id),
    }
}

#[proc_macro_derive(DuskBridge, attributes(module, name, variant))]
pub fn derive_dusk_bridge(item: TokenStream) -> TokenStream {
    let item: Item = syn::parse(item).unwrap();
    fn derive(attrs: Vec<Attribute>, decl_name: syn::Ident) -> TokenStream {
        let Some(module) = attrs.iter()
            .find(|attr| attr.path.is_ident("module"))
            .map(|attr| {
                match attr.parse_meta().unwrap() {
                    Meta::Path(_) | Meta::List(_) => panic!("expected module path in 'module' attribute, like #[module = \"the.module\"]"),
                    Meta::NameValue(attr) => match attr.lit {
                        Lit::Str(lit) => lit,
                        _ => panic!("expected string literal for module path")
                    }
                }
            })
        else {
            panic!("expected 'module' attribute to specify where to put bridged type")
        };
        let variant = attrs.iter()
            .find(|attr| attr.path.is_ident("variant"))
            .map(|attr| {
                match attr.parse_meta().unwrap() {
                    Meta::Path(_) | Meta::List(_) => panic!("expected `Type` variant in 'variant' attribute, like #[variant = \"Ty\"]"),
                    Meta::NameValue(attr) => match attr.lit {
                        Lit::Str(lit) => lit,
                        _ => panic!("expected string literal for bridged `Type` name")
                    }
                }
            });
        let bridged_name = attrs.iter()
            .find(|attr| attr.path.is_ident("name"))
            .map(|attr| {
                match attr.parse_meta().unwrap() {
                    Meta::Path(_) | Meta::List(_) => panic!("expected bridged type name in 'name' attribute, like #[name = \"SomeTypeName\"]"),
                    Meta::NameValue(attr) => match attr.lit {
                        Lit::Str(lit) => lit,
                        _ => panic!("expected string literal for bridged type name")
                    }
                }
            })
            .unwrap_or_else(|| LitStr::new(&decl_name.to_string(), decl_name.span()));
        
        let get_ty_to_register = if let Some(variant) = variant {
            let name = syn::Ident::new(&variant.value(), variant.span());
            quote! {
                let ty = Type::#name;
            }
        } else {
            quote! {
                let namespace = d.code.ast.new_namespaces.push(NewNamespace::default());
                let internal_type = InternalType { name: String::from(#bridged_name), size: std::mem::size_of::<#decl_name>(), namespace };
                let type_id = d.code.ast.internal_types.push(internal_type);
                let ty = Type::Internal(type_id);
            }
        };
        quote! {
            impl crate::dire::internal_types::DuskBridge for #decl_name {
                fn register(d: &mut crate::driver::Driver) {
                    use std::any::TypeId;
                    use crate::dire::{ast::*, ty::*, mir::*};

                    #get_ty_to_register // defines `ty` variable used below

                    let konst = Const::Ty(ty.clone());
                    let expr = d.add_const_expr(konst);
                    d.add_decl_to_path(#bridged_name, #module, Decl::Const(expr), None);

                    d.code.ast.bridged_types.insert(TypeId::of::<Self>(), ty);
                }

                fn bridge_from_dusk(value: &crate::interpreter::Value, _d: &crate::driver::Driver) -> Self {
                    use std::{mem, ptr};
                    unsafe {
                        // I do this in order to make non-Copy types "work".
                        // This is extremely unsafe.
                        let addr: &Self = value.as_arbitrary_value();
                        let mut val = mem::MaybeUninit::uninit();
                        ptr::copy(addr, val.as_mut_ptr(), 1);
                        val.assume_init()
                    }
                }

                fn bridge_to_dusk(self, d: &crate::driver::Driver) -> crate::interpreter::Value {
                    unsafe { crate::interpreter::Value::from_arbitrary_value(self) }
                }
            }

            impl crate::dire::internal_types::DuskBridge for &'static mut #decl_name {
                fn register(d: &mut crate::driver::Driver) {
                    use std::any::TypeId;
                    use crate::dire::{ast::*, ty::*, mir::*};

                    let base_ty = d.code.ast.bridged_types.get(&TypeId::of::<#decl_name>()).unwrap().clone();

                    d.code.ast.bridged_types.insert(TypeId::of::<Self>(), base_ty.mut_ptr());
                }

                fn bridge_from_dusk(value: &crate::interpreter::Value, _d: &crate::driver::Driver) -> Self {
                    use std::{mem, ptr};
                    unsafe {
                        &mut *(value.as_raw_ptr() as *mut #decl_name)
                    }
                }

                fn bridge_to_dusk(self, d: &crate::driver::Driver) -> crate::interpreter::Value {
                    unsafe { crate::interpreter::Value::from_arbitrary_value(self) }
                }
            }
        }.into()
    }
    match item {
        Item::Struct(strukt) => derive(strukt.attrs, strukt.ident),
        Item::Enum(enuum) => derive(enuum.attrs, enuum.ident),
        _ => panic!("unable to bridge type: unknown item kind"),
    }
}

#[proc_macro_attribute]
pub fn dusk_bridge(attr: TokenStream, item: TokenStream) -> TokenStream {
    assert!(attr.is_empty());

    let mut item: Item = syn::parse(item).unwrap();
    let mut registrations = Vec::new();
    match &mut item {
        Item::Impl(impl_block) => {
            assert!(impl_block.defaultness.is_none());
            assert!(impl_block.generics.params.is_empty());
            assert!(impl_block.generics.where_clause.is_none());
            assert!(impl_block.trait_.is_none());
            assert!(impl_block.unsafety.is_none());
            let Type::Path(self_ty) = impl_block.self_ty.as_ref() else {
                panic!("invalid self type for bridging to Dusk; expected 'Driver'");
            };
            assert!(self_ty.qself.is_none());
            assert!(self_ty.path.is_ident("Driver"));

            for item in &mut impl_block.items {
                match item {
                    ImplItem::Method(method) => {
                        assert!(method.defaultness.is_none());
                        assert!(method.sig.constness.is_none());
                        assert!(method.sig.asyncness.is_none());
                        assert!(method.sig.unsafety.is_none());
                        assert!(method.sig.abi.is_none());
                        assert!(method.sig.generics.params.is_empty());
                        assert!(method.sig.generics.where_clause.is_none());
                        assert!(method.sig.variadic.is_none());

                        let path = method.attrs.iter()
                            .find(|attr| attr.path.is_ident("path"))
                            .map(|attr| {
                                match attr.parse_meta().unwrap() {
                                    Meta::Path(_) | Meta::List(_) => panic!("expected module or type path in 'path' attribute, like #[path = \"the.module\"] or #[path = \"the.module.TheType\"]"),
                                    Meta::NameValue(attr) => match attr.lit {
                                        Lit::Str(lit) => lit.value(),
                                        _ => panic!("expected string literal for module or type path")
                                    }
                                }
                            })
                            .unwrap_or_else(|| String::from(""));
                        method.attrs.retain(|attr| !attr.path.is_ident("path"));

                        let mut has_driver = false;
                        let mut has_self = false;
                        let mut param_index = 0usize;

                        let mut param_tys = Vec::new();
                        let mut param_val_decls = Vec::new();
                        let mut param_val_names = Vec::new();

                        for input in &mut method.sig.inputs {
                            match input {
                                FnArg::Receiver(receiver) => {
                                    assert!(!has_driver);
                                    assert!(param_index == 0);
                                    assert!(receiver.lifetime().is_none());
                                    assert!(receiver.reference.is_some());

                                    has_driver = true;
                                },
                                FnArg::Typed(ty) => {
                                    if ty.attrs.iter().find(|attr| attr.path.is_ident("self")).is_some() {
                                        assert!(!has_self);
                                        assert!(param_index == 0);

                                        ty.attrs.retain(|attr| !attr.path.is_ident("self"));

                                        has_self = true;
                                    }

                                    let ty = ty.ty.clone();
                                    let param_val_variable_name = format!("__param_{}", param_index);
                                    let param_val_variable_name = syn::Ident::new(&param_val_variable_name, input.span());
                                    param_tys.push(ty.clone());
                                    param_val_decls.push(
                                        quote! {
                                            let #param_val_variable_name = <#ty>::bridge_from_dusk(parameters[#param_index], &d.read());
                                        }
                                    );
                                    param_val_names.push(param_val_variable_name);
                                    param_index += 1;
                                },
                            }
                        }

                        let ret_ty = match &method.sig.output {
                            ReturnType::Default => syn::parse2(quote! { () }).unwrap(),
                            ReturnType::Type(_, ty) => ty.as_ref().clone(),
                        };
                        let name = method.sig.ident.to_string();

                        let mangled_name = format!("builtin_{}{}",
                            if path.is_empty() { path.clone() } else { path.replace(".", "_") },
                            method.sig.ident.to_string(),
                        );
                        let mangled_name = syn::Ident::new(&mangled_name, method.sig.ident.span());
                        struct IShouldntHaveToWriteThisStructImo {
                            attrs: Vec<Attribute>,
                        }
                        impl Parse for IShouldntHaveToWriteThisStructImo {
                            fn parse(input: ParseStream) -> syn::Result<Self> {
                                Ok(IShouldntHaveToWriteThisStructImo {
                                    attrs: input.call(Attribute::parse_outer)?,
                                })
                            }
                        }
                        let new_attrs = syn::parse2::<IShouldntHaveToWriteThisStructImo>(quote! { #[allow(non_snake_case)] }).unwrap();
                        
                        method.sig.ident = mangled_name.clone();
                        method.attrs.extend(new_attrs.attrs);

                        let implementation = if has_driver {
                            quote! { d.write().#mangled_name }
                        } else {
                            quote! { Driver::#mangled_name }
                        };
                        let decl = if has_self {
                            quote! { Decl::MethodIntrinsic(intr_id) }
                        } else {
                            quote! { Decl::Intrinsic(intr_id) }
                        };
                        registrations.push(
                            quote! {
                                fn thunk(d: &mut DriverRef, parameters: Vec<&Value>) -> Value {
                                    #(#param_val_decls)*
                                    let val = #implementation(#(#param_val_names),*);
                                    val.bridge_to_dusk(&d.read())
                                }
                                let ret_ty = <#ret_ty>::to_dusk_type(d);
                                let intr = Intrinsic {
                                    param_tys: smallvec![
                                        #(
                                            d.add_const_ty(<#param_tys>::to_dusk_type(d)),
                                        )*
                                    ],
                                    ret_ty: ret_ty.clone(),
                                    name: String::from(#name),
                                    implementation: thunk,
                                };
                                let ret_ty = d.add_const_ty(ret_ty);
                                let intr_id = d.code.ast.intrinsics.push(intr);
                                d.add_decl_to_path(#name, #path, #decl, Some(ret_ty));
                            }
                        );
                    },
                    _ => panic!("unhandled item in dusk_bridge impl block"),
                }
            }
        },
        _ => panic!("unrecognized use of `dusk_bridge` macro: only valid on impl items")
    }
    quote! {
        #item

        pub fn register_bridged_rust_methods(d: &mut crate::driver::Driver) {
            use crate::dire::internal_types::DuskBridge;
            use crate::dire::{ast::*, ty::*, mir::*};
            use crate::driver::DriverRef;
            use crate::ast::Intrinsic;
            use crate::interpreter::Value;
            use smallvec::smallvec;

            #({ #registrations })*
        }
    }.into()
}
