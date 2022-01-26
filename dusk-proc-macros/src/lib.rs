extern crate proc_macro;

use proc_macro::{TokenStream, TokenTree, Ident, Span, Punct, Spacing, Group, Delimiter};
use proc_macro::token_stream::IntoIter as TokenIter;
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
        loop {
            let dot = if let Some(tok) = self.iter.peek() {
                match tok {
                    TokenTree::Punct(punct) if punct.as_char() == '.' => {
                        self.iter.next().unwrap()
                    },
                    TokenTree::Punct(punct) if punct.as_char() == ',' => {
                        break;
                    },
                    _ => panic!("unexpected token {}", tok),
                }
            } else {
                break;
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
            .field("hir_code")
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
}

impl Into<TokenStream> for Builder {
    fn into(self) -> TokenStream {
        self.stream
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
                        .stream
                },
                "hir" => {
                    Builder::new()
                        .hir_code(driver)
                        .field("exprs")
                        .brackets(base)
                        .stream
                },
                "item" => {
                    Builder::new()
                        .expr_to_item(driver, base)
                        .stream
                }
                name => panic!("Unrecognized expression field name {}", name),
            }
        },
        Expression::Simple(expr_id) => panic!("Unexpected bare identifier '{}'. Must have field", expr_id),
    }
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
