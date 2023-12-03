use std::ops::Range;

use crate::ast::{ExprId, DeclId, ItemId};

use crate::driver::Driver;

macro_rules! generate_new_code {
    (pub struct $name:ident {
        $(pub $field_name:ident: Range<$id_ty:ty>),*$(,)*
    }) => {
        #[derive(Debug)]
        pub struct $name {
            $(pub $field_name: Range<$id_ty>),*,
        }

        #[derive(Debug)]
        pub struct CodeSnapshot {
            $($field_name: $id_ty),*,
        }

        impl Default for CodeSnapshot {
            fn default() -> Self {
                Self {
                    $($field_name: <$id_ty>::new(0)),*,
                }
            }
        }

        impl Driver {
            pub fn take_snapshot(&self) -> CodeSnapshot {
                CodeSnapshot {
                    exprs: self.code.ast.exprs.next_idx(),
                    decls: self.code.ast.decls.next_idx(),
                    items: self.code.ast.items.next_idx(),
                }
            }

            pub fn get_new_code_since(&self, before: CodeSnapshot) -> NewCode {
                let after = self.take_snapshot();
                NewCode {
                    $($field_name: before.$field_name..after.$field_name),*,
                }
            }
        }

        impl $name {
            pub fn placeholder() -> Self {
                Self {
                    $($field_name: <$id_ty>::new(0)..<$id_ty>::new(0)),*,
                }
            }
        }
    }
}

generate_new_code! {
    pub struct NewCode {
        pub exprs: Range<ExprId>,
        pub decls: Range<DeclId>,
        pub items: Range<ItemId>,
    }
}
