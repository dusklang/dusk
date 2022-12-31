extern crate proc_macro;

use proc_macro::{TokenStream, TokenTree, Ident, Span, Punct, Spacing, Group, Delimiter};
use proc_macro::token_stream::IntoIter as TokenIter;

use quote::quote;
use syn::{Item, Meta, Lit, LitStr};

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

    fn hir_code(mut self, driver: impl Into<TokenStream>) -> Self {
        self.stream.extend(driver.into());
        self
            .field("code")
            .field("hir")
    }

    fn expr_to_item(self, driver: impl Into<TokenStream>, expr_id: impl Into<TokenStream>) -> Self {
        self.hir_code(driver)
            .field("expr_to_items")
            .brackets(expr_id.into())
    }

    fn decl_to_item(self, driver: impl Into<TokenStream>, expr_id: impl Into<TokenStream>) -> Self {
        self.hir_code(driver)
            .field("decl_to_items")
            .brackets(expr_id.into())
    }

    fn source_range(self, driver: impl Into<TokenStream>, item_id: impl Into<TokenStream>) -> Self {
        self
            .hir_code(driver)
            .field("source_ranges")
            .brackets(item_id)
    }

    fn generic_ctx_id(self, driver: impl Into<TokenStream>, item_id: impl Into<TokenStream>) -> Self {
        self
            .hir_code(driver)
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
                        .hir_code(driver.clone())
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
                "hir" => {
                    Builder::new()
                        .hir_code(driver)
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
                "hir" => {
                    Builder::new()
                        .hir_code(driver)
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

#[proc_macro_derive(DuskBridge, attributes(module))]
pub fn derive_dusk_bridge(item: TokenStream) -> TokenStream {
    let item: Item = syn::parse(item).unwrap();
    match item {
        Item::Struct(strukt) => {
            let Some(module) = strukt.attrs.iter()
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
            let struct_name = strukt.ident;
            let struct_name_as_string = LitStr::new(&struct_name.to_string(), struct_name.span());
            quote! {
                impl crate::dire::internal_types::DuskBridge for #struct_name {
                    fn register(d: &mut crate::driver::Driver) {
                        use crate::dire::{hir::*, ty::*, mir::*};

                        let internal_type = InternalType { name: String::from(#struct_name_as_string), size: std::mem::size_of::<#struct_name>() };
                        let type_id = d.code.hir.internal_types.push(internal_type);
                        let ty = Type::Internal(type_id);
                        let konst = Const::Ty(ty);
                        let expr = d.add_const_expr(konst);
                        d.add_decl_to_module(#struct_name_as_string, #module, Decl::Const(expr), 0, None);
                    }
                }
            }.into()
        },
        _ => panic!("unable to bridge type: unknown item kind"),
    }
}