extern crate proc_macro;

use proc_macro::{TokenStream, TokenTree, Ident, Span, Punct, Spacing, Group, Delimiter};
use proc_macro::token_stream::IntoIter as TokenIter;

use quote::{quote, ToTokens};
use syn::parse::{ParseStream, Parse};
use syn::{parse_macro_input, parse_quote, Item, Lifetime, Meta, Lit, LitStr, Type, ImplItem, FnArg, ReturnType, Attribute, DeriveInput, Data, Fields};
use syn::spanned::Spanned;

use std::iter::Peekable;

mod instr_operands;

/// Generates inherent methods for visiting and rewriting the `ValueId`s and `BlockId`s a MIR instruction refers to.
/// See the `instr_operands` module for details.
#[proc_macro_derive(InstrOperands)]
pub fn derive_instr_operands(input: TokenStream) -> TokenStream {
    instr_operands::derive(parse_macro_input!(input as DeriveInput)).into()
}

#[proc_macro_attribute]
pub fn display_adapter(attr: TokenStream, item: TokenStream) -> TokenStream {
    let lifetime = if attr.is_empty() {
        parse_quote! { 'display_adapter_lifetime }
    } else {
        parse_macro_input!(attr as Lifetime)
    };

    let item = parse_macro_input!(item as Item);
    let func = match item {
        Item::Fn(func) => func,
        _ => panic!("expected function"),
    };
    let mut attrs = Default::default();
    for attr in func.attrs {
        attr.to_tokens(&mut attrs);
    };
    let vis = func.vis;
    let mut sig = func.sig;
    assert!(sig.generics.lt_token.is_none(), "generics not supported yet");
    sig.generics = parse_quote! { <#lifetime> };
    let mut w_ident: Option<syn::Ident> = None;
    let mut inputs = Default::default();
    for (input, p) in sig.inputs.pairs().map(|pair| pair.into_tuple()) {
        let mut ident: Option<syn::Ident> = None;
        let mut input = input.clone();
        match input {
            FnArg::Typed(ref mut arg) => {
                if let Type::Reference(ref mut reference) = *arg.ty {
                    if reference.lifetime.is_none() {
                        reference.lifetime = Some(lifetime.clone());
                    }
                    if reference.mutability.is_some()
                        && let Type::Path(ref path) = *reference.elem
                            && path.qself.is_none() {
                                let last_segment = path.path.segments.last().unwrap().ident.to_string();

                                if last_segment == "Formatter" {
                                    let pat = arg.pat.clone();
                                    ident = Some(parse_quote! { #pat });
                                }
                            }
                }
            },
            FnArg::Receiver(ref mut receiver) => {
                let (_, recv_lifetime) = receiver.reference.as_mut().unwrap();
                if recv_lifetime.is_none() {
                    *recv_lifetime = Some(lifetime.clone());
                }
            }
        }

        if let Some(ident) = ident {
            w_ident = Some(ident);
        } else {
            input.to_tokens(&mut inputs);
            p.to_tokens(&mut inputs);
        }
    };
    let w_ident = w_ident.unwrap();
    sig.inputs = parse_quote! { #inputs };
    sig.output = parse_quote! { -> impl std::fmt::Display + #lifetime };
    let block = func.block;
    quote! {
        #attrs #vis #sig {
            crate::display_adapter::display_adapter_impl(move |#w_ident| #block)
        }
    }.into()
}

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
            .field("mutable_state")
            .field("ast")
    }

    fn expr_to_item(self, driver: impl Into<TokenStream>, expr_id: impl Into<TokenStream>) -> Self {
        self.ast_code(driver)
            .field("expr_to_items")
            .brackets(expr_id.into())
    }

    fn decl_to_item(self, driver: impl Into<TokenStream>, decl_id: impl Into<TokenStream>) -> Self {
        self.ast_code(driver)
            .field("decl_to_items")
            .brackets(decl_id.into())
    }

    fn attr_to_item(self, driver: impl Into<TokenStream>, attr_id: impl Into<TokenStream>) -> Self {
        self.ast_code(driver)
            .field("attr_to_items")
            .brackets(attr_id.into())
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
                name => panic!("Unrecognized declaration field name {}", name),
            }
        },
        Expression::Simple(decl_id) => panic!("Unexpected bare identifier '{}'. Must have field", decl_id),
    }
}

#[proc_macro]
pub fn af(input: TokenStream) -> TokenStream {
    let (driver, field) = parse_input(input);
    match field {
        Expression::Dot { base, field, .. } => {
            match field.to_string().as_str() {
                "range" => {
                    Builder::new()
                        .source_range(
                            driver.clone(),
                            Builder::new().attr_to_item(driver, base)
                        )
                        .stream
                },
                "ast" => {
                    Builder::new()
                        .ast_code(driver)
                        .field("attrs")
                        .brackets(base)
                        .stream
                },
                name => panic!("Unrecognized attribute field name {}", name),
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
                let namespace = d.mutable_state.ast.new_namespaces.push(NewNamespace::default());
                let internal_type = InternalType { name: String::from(#bridged_name), size: size_of::<#decl_name>(), namespace };
                let type_id = d.mutable_state.ast.internal_types.push(internal_type);
                let ty = Type::Internal(type_id);
            }
        };
        quote! {
            impl crate::internal_types::DuskBridge for #decl_name {
                fn register(b: &mut crate::ast::Builder, d: &mut crate::driver::Driver) {
                    use std::any;
                    use crate::{ast::*, ty::*, mir::*};
                    use crate::index_vec::empty_range;

                    #get_ty_to_register // defines `ty` variable used below

                    let konst = Const::Ty(ty.clone());
                    let expr = d.add_const_expr(b, konst);
                    d.add_decl_to_path(b, #bridged_name, #module, Decl::Const { assigned_expr: expr, generic_params: empty_range() }, None);

                    d.mutable_state.ast.bridged_types.pin().insert(any::TypeId::of::<Self>(), ty);
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

            impl crate::internal_types::DuskBridge for &'static mut #decl_name {
                fn register(b: &mut crate::ast::Builder, d: &mut crate::driver::Driver) {
                    use std::any;
                    use crate::{ast::*, ty::*, mir::*};

                    let base_ty = d.mutable_state.ast.bridged_types.pin().get(&any::TypeId::of::<#decl_name>()).unwrap().clone();

                    d.mutable_state.ast.bridged_types.pin().insert(any::TypeId::of::<Self>(), base_ty.mut_ptr());
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

/// The interpreter pointer types that `#[dusk_bridge]` knows how to bridge intrinsically.
#[derive(Copy, Clone, PartialEq, Eq)]
enum PointerKind {
    /// `crate::interpreter::Pointer`; allowed for parameters and return values.
    Pointer,
    /// `crate::interpreter::UnownedPointer`; allowed for return values only.
    UnownedPointer,
}

/// Recognizes `Pointer` and `UnownedPointer` by the last segment of the type's path (so a type alias won't be
/// recognized).
fn pointer_kind(ty: &Type) -> Option<PointerKind> {
    let Type::Path(path) = ty else { return None };
    if path.qself.is_some() {
        return None;
    }
    let segment = path.path.segments.last()?;
    if !segment.arguments.is_empty() {
        return None;
    }
    match segment.ident.to_string().as_str() {
        "Pointer" => Some(PointerKind::Pointer),
        "UnownedPointer" => Some(PointerKind::UnownedPointer),
        _ => None,
    }
}

/// Removes the attribute named `name` from `attrs` and parses its contents as a Dusk pointer type, if present.
fn take_dusk_ptr_annotation(attrs: &mut Vec<Attribute>, name: &str) -> syn::Result<Option<(proc_macro2::Span, proc_macro2::TokenStream)>> {
    let mut result = None;
    for attr in attrs.iter().filter(|attr| attr.path.is_ident(name)) {
        if result.is_some() {
            return Err(syn::Error::new(attr.span(), format!("duplicate `{}` attribute", name)));
        }
        let (ty, is_ptr) = attr.parse_args_with(parse_dusk_type)?;
        if !is_ptr {
            return Err(syn::Error::new(attr.span(), format!("the Dusk type in `{}` must be a pointer type, like `void *mut`", name)));
        }
        result = Some((attr.span(), ty));
    }
    attrs.retain(|attr| !attr.path.is_ident(name));
    Ok(result)
}

/// Parses a Dusk type written in Dusk syntax (e.g. `void *mut`, `u8 * *mut`) into an expression that constructs the
/// corresponding `crate::ty::Type`, plus whether it's a pointer type. Supports a primitive base type followed by any
/// number of `*` / `*mut` suffixes.
fn parse_dusk_type(input: ParseStream) -> syn::Result<(proc_macro2::TokenStream, bool)> {
    let base: syn::Ident = input.parse()?;
    let mut ty = match base.to_string().as_str() {
        "void" => quote! { crate::ty::Type::Void },
        "never" => quote! { crate::ty::Type::Never },
        "bool" => quote! { crate::ty::Type::Bool },
        "u8" | "u16" | "u32" | "u64" | "usize" |
        "i8" | "i16" | "i32" | "i64" | "isize" |
        "f32" | "f64" => quote! { crate::ty::Type::#base() },
        other => return Err(syn::Error::new(base.span(), format!("unsupported base type `{}` in Dusk type", other))),
    };
    let mut is_ptr = false;
    while !input.is_empty() {
        input.parse::<syn::Token![*]>()?;
        is_ptr = true;
        if input.peek(syn::Token![mut]) {
            input.parse::<syn::Token![mut]>()?;
            ty = quote! { #ty.mut_ptr() };
        } else {
            ty = quote! { #ty.ptr() };
        }
    }
    Ok((ty, is_ptr))
}

/// Bridges each method in an `impl Driver` block to Dusk as an intrinsic.
///
/// Parameter and return types are normally bridged via their `DuskBridge` impls. The exception is the interpreter's
/// pointer types, which the macro bridges intrinsically and which can stand for any Dusk pointer type, so they must be
/// annotated with the Dusk type they represent (a missing annotation is a compile error):
/// - Parameters of type `Pointer`, annotated like `#[ty(void *mut)] ptr: Pointer`.
/// - Return values of type `Pointer` or `UnownedPointer`, annotated on the method like `#[ret_ty(void *mut)]`.
///
/// Per-method attributes:
/// - `#[path = "a.b.C"]`: the module or type path to declare the intrinsic under.
/// - `#[ret_ty(<dusk pointer type>)]`: the Dusk type of a `Pointer`/`UnownedPointer` return value.
///
/// Per-parameter attributes:
/// - `#[self]`: makes the intrinsic callable as a method on the first parameter.
/// - `#[ty(<dusk pointer type>)]`: the Dusk type of a `Pointer` parameter.
#[proc_macro_attribute]
pub fn dusk_bridge(attr: TokenStream, item: TokenStream) -> TokenStream {
    assert!(attr.is_empty());

    /// Converts a bridged method's dotted path and Rust name into a PascalCase variant name,
    /// e.g. ("compiler.ModuleBuilder", "add_usize_constant") -> `CompilerModuleBuilderAddUsizeConstant`.
    fn variant_name(path: &str, method: &str) -> String {
        let mut result = String::new();
        for segment in path.split('.').chain(method.split('_')) {
            let mut chars = segment.chars();
            if let Some(first) = chars.next() {
                result.extend(first.to_uppercase());
                result.push_str(chars.as_str());
            }
        }
        result
    }

    let mut item: Item = syn::parse(item).unwrap();
    let mut variants: Vec<syn::Ident> = Vec::new();
    let mut names = Vec::new();
    let mut paths = Vec::new();
    let mut is_methods = Vec::new();
    let mut call_arms = Vec::new();
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

                        let ret_ty_annotation = match take_dusk_ptr_annotation(&mut method.attrs, "ret_ty") {
                            Ok(annotation) => annotation,
                            Err(err) => return err.to_compile_error().into(),
                        };

                        struct DriverReceiver {
                            is_mut: bool,
                        }

                        let mut driver: Option<DriverReceiver> = None;
                        let mut has_self = false;
                        let mut param_index = 0usize;

                        let mut param_tys = Vec::new();
                        let mut param_val_decls = Vec::new();
                        let mut param_val_names = Vec::new();

                        for input in &mut method.sig.inputs {
                            match input {
                                FnArg::Receiver(receiver) => {
                                    assert!(driver.is_none());
                                    assert!(param_index == 0);
                                    assert!(receiver.lifetime().is_none());
                                    assert!(receiver.reference.is_some());

                                    driver = Some(DriverReceiver { is_mut: receiver.mutability.is_some() });
                                },
                                FnArg::Typed(ty) => {
                                    if ty.attrs.iter().find(|attr| attr.path.is_ident("self")).is_some() {
                                        assert!(!has_self);
                                        assert!(param_index == 0);

                                        ty.attrs.retain(|attr| !attr.path.is_ident("self"));

                                        has_self = true;
                                    }

                                    let ty_annotation = match take_dusk_ptr_annotation(&mut ty.attrs, "ty") {
                                        Ok(annotation) => annotation,
                                        Err(err) => return err.to_compile_error().into(),
                                    };
                                    let ty = ty.ty.clone();
                                    let param_val_variable_name = format!("__param_{}", param_index);
                                    let param_val_variable_name = syn::Ident::new(&param_val_variable_name, input.span());
                                    match (pointer_kind(&ty), ty_annotation) {
                                        (Some(PointerKind::Pointer), Some((_, dusk_ty))) => {
                                            param_tys.push(dusk_ty);
                                            param_val_decls.push(
                                                quote! {
                                                    let #param_val_variable_name = parameters[#param_index].as_allocation();
                                                }
                                            );
                                        },
                                        (Some(PointerKind::Pointer), None) => {
                                            return syn::Error::new(
                                                ty.span(),
                                                "`Pointer` parameters must be annotated with the Dusk type they represent, like `#[ty(void *mut)]`",
                                            ).to_compile_error().into();
                                        },
                                        (Some(PointerKind::UnownedPointer), _) => {
                                            return syn::Error::new(
                                                ty.span(),
                                                "`UnownedPointer` is not supported for parameters, because a pointer passed from Dusk may not point into an interpreter allocation; use `Pointer` instead",
                                            ).to_compile_error().into();
                                        },
                                        (None, Some((span, _))) => {
                                            return syn::Error::new(
                                                span,
                                                "`ty` attribute is only supported on parameters of type `Pointer`",
                                            ).to_compile_error().into();
                                        },
                                        (None, None) => {
                                            param_tys.push(quote! { <#ty>::to_dusk_type(d) });
                                            param_val_decls.push(
                                                quote! {
                                                    let #param_val_variable_name = <#ty>::bridge_from_dusk(parameters[#param_index], &d.read());
                                                }
                                            );
                                        },
                                    }
                                    param_val_names.push(param_val_variable_name);
                                    param_index += 1;
                                },
                            }
                        }

                        let ret_ty: Type = match &method.sig.output {
                            ReturnType::Default => syn::parse2(quote! { () }).unwrap(),
                            ReturnType::Type(_, ty) => ty.as_ref().clone(),
                        };
                        let (ret_dusk_ty, ret_val_to_dusk) = match (pointer_kind(&ret_ty), ret_ty_annotation) {
                            (Some(kind), Some((_, dusk_ty))) => {
                                let to_dusk = match kind {
                                    PointerKind::Pointer => quote! { crate::interpreter::Value::from_allocation(val) },
                                    PointerKind::UnownedPointer => quote! { crate::interpreter::Value::UnownedPointer(val) },
                                };
                                (dusk_ty, to_dusk)
                            },
                            (Some(_), None) => {
                                return syn::Error::new(
                                    ret_ty.span(),
                                    "pointer return values must be annotated with the Dusk type they represent, like `#[ret_ty(void *mut)]` on the method",
                                ).to_compile_error().into();
                            },
                            (None, Some((span, _))) => {
                                return syn::Error::new(
                                    span,
                                    "`ret_ty` attribute is only supported on methods returning `Pointer` or `UnownedPointer`",
                                ).to_compile_error().into();
                            },
                            (None, None) => (
                                quote! { <#ret_ty>::to_dusk_type(d) },
                                quote! { val.bridge_to_dusk(&d.read()) },
                            ),
                        };
                        let name = method.sig.ident.to_string();

                        let mangled_name = format!("builtin_{}{}",
                            if path.is_empty() { path.clone() } else { path.replace(".", "_") },
                            method.sig.ident,
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

                        let implementation = if let Some(driver) = driver {
                            if driver.is_mut {
                                quote! { d.write().#mangled_name }
                            } else {
                                quote! { d.read().#mangled_name }
                            }
                        } else {
                            quote! { Driver::#mangled_name }
                        };
                        let variant = syn::Ident::new(&variant_name(&path, &name), method.sig.ident.span());
                        if let Some(existing) = variants.iter().find(|v| **v == variant) {
                            return syn::Error::new(
                                existing.span(),
                                format!("bridged methods `{}` at path \"{}\" would produce a duplicate `IntrinsicKind::{}` variant", name, path, variant),
                            ).to_compile_error().into();
                        }

                        let decl = if has_self {
                            quote! { Decl::MethodIntrinsic(intr_id) }
                        } else {
                            quote! { Decl::Intrinsic(intr_id) }
                        };
                        call_arms.push(
                            quote! {
                                IntrinsicKind::#variant => {
                                    #(#param_val_decls)*
                                    let val = #implementation(#(#param_val_names),*);
                                    #ret_val_to_dusk
                                }
                            }
                        );
                        registrations.push(
                            quote! {
                                let ret_ty = #ret_dusk_ty;
                                let info = crate::ast::IntrinsicInfo {
                                    param_tys: smallvec![
                                        #(
                                            d.add_const_ty(b, #param_tys),
                                        )*
                                    ],
                                    ret_ty: ret_ty.clone(),
                                    kind: self::IntrinsicKind::#variant,
                                };
                                let ret_ty = d.add_const_ty(b, ret_ty);
                                let intr_id = d.mutable_state.ast.intrinsics.push(info);
                                d.add_decl_to_path(b, #name, #path, #decl, Some(ret_ty));
                            }
                        );
                        variants.push(variant);
                        names.push(name);
                        paths.push(path);
                        is_methods.push(has_self);
                    },
                    _ => panic!("unhandled item in dusk_bridge impl block"),
                }
            }
        },
        _ => panic!("unrecognized use of `dusk_bridge` macro: only valid on impl items")
    }
    quote! {
        #item

        /// One variant per Rust method bridged to Dusk via `#[dusk_bridge]`.
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub enum IntrinsicKind {
            #(#variants,)*
        }

        impl IntrinsicKind {
            /// Every bridged intrinsic, in declaration order.
            pub const ALL: &'static [IntrinsicKind] = &[#(IntrinsicKind::#variants,)*];

            /// The name of the intrinsic as seen from Dusk code.
            pub fn name(self) -> &'static str {
                match self {
                    #(IntrinsicKind::#variants => #names,)*
                }
            }

            /// The dotted module or type path the intrinsic is declared under in Dusk (empty for the root).
            pub fn path(self) -> &'static str {
                match self {
                    #(IntrinsicKind::#variants => #paths,)*
                }
            }

            /// Whether the intrinsic takes a `#[self]` parameter, and is therefore callable as a method.
            pub fn is_method(self) -> bool {
                match self {
                    #(IntrinsicKind::#variants => #is_methods,)*
                }
            }

            /// Bridges `parameters` to Rust, calls the underlying Rust method, and bridges the result back to Dusk.
            pub fn call(self, d: &mut crate::driver::DriverRwRef, parameters: Vec<&crate::interpreter::Value>) -> crate::interpreter::Value {
                use crate::internal_types::DuskBridge;
                match self {
                    #(#call_arms)*
                }
            }
        }

        pub fn register_bridged_rust_methods(b: &mut crate::ast::Builder, d: &mut crate::driver::Driver) {
            use crate::internal_types::DuskBridge;
            use crate::{ast::*, ty::*, mir::*};
            use smallvec::smallvec;

            #({ #registrations })*
        }
    }.into()
}

// Taken from https://github.com/memflow/memflow/tree/main/memflow-derive out of pure laziness
#[proc_macro_derive(ByteSwap)]
pub fn byteswap_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut field_names = Vec::new();
    match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(named) => {
                for field in &named.named {
                    let field_name = field.ident.as_ref().unwrap();
                    field_names.push(quote! { #field_name });
                }
            },
            Fields::Unnamed(unnamed) => {
                for i in (0..unnamed.unnamed.len()).map(syn::Index::from) {
                    field_names.push(quote! { #i });
                }
            },
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let generic = quote!(
        impl #impl_generics crate::linker::byte_swap::ByteSwap for #name #ty_generics #where_clause {
            fn write_to(&self, buf: &mut [u8], big_endian: bool) {
                #({
                    let offset = core::mem::offset_of!(#name #ty_generics, #field_names);
                    let value = self.#field_names;
                    let size = size_of_val(&value);
                    value.write_to(&mut buf[offset..][..size], big_endian);
                })*
            }

            fn read_from(buf: &[u8], big_endian: bool) -> #name #ty_generics {
                #name {
                    #(#field_names: {
                        let offset = core::mem::offset_of!(#name #ty_generics, #field_names);
                        crate::linker::byte_swap::read_bs_from_oversized_buf(&buf[offset..], big_endian)
                    }),*
                }
            }
        }
    );

    generic.into()
}

#[proc_macro_derive(ByteSwapBitflags)]
pub fn byteswap_bitflags_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut field_names = Vec::new();
    match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Unnamed(unnamed) => {
                for i in (0..unnamed.unnamed.len()).map(syn::Index::from) {
                    field_names.push(quote! { #i });
                }
            },
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let generic = quote!(
        impl #impl_generics crate::linker::byte_swap::ByteSwap for #name #ty_generics #where_clause {
            fn write_to(&self, buf: &mut [u8], big_endian: bool) {
                #(self.#field_names.bits().write_to(buf, big_endian));*
            }

            fn read_from(buf: &[u8], big_endian: bool) -> #name #ty_generics {
                #name::from_bits(crate::linker::byte_swap::ByteSwap::read_from(buf, big_endian)).unwrap()
            }
        }
    );

    generic.into()
}
