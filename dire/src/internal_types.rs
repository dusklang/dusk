// This is a higher-order macro which takes in a macro and passes it all internal types and their members
use crate::ty::Type;
#[macro_export]
macro_rules! define_internal_types {
    ($name:ident) => {
        $name!(
            struct StringLiteral {
                length: Type::usize(),
                data: Type::u8().ptr(),
            }
        );
    };
}

macro_rules! define_internal_types_impl {
    ($(struct $name:ident {
        $($field_name:ident: $ty:expr),*$(,)?
    })*) => {
        #[derive(Copy, Clone, Debug)]
        pub enum InternalNamespace {
            $($name),*
        }

        pub mod internal_field_decls {
            $(
                use crate::hir::DeclId;
                #[derive(Debug)]
                pub struct $name {
                    $(
                        pub $field_name: DeclId
                    ),*
                }
                impl Default for $name {
                    fn default() -> Self {
                        $name {
                            $(
                                $field_name: DeclId::new(0)
                            ),*
                        }
                    }
                }
            )*
        }

        #[derive(Default, Debug)]
        #[allow(non_snake_case)]
        pub struct InternalFieldDecls {
            $(
                pub $name: internal_field_decls::$name
            ),*
        }

        pub mod internal_fields {
            $(
                #[derive(Copy, Clone, Debug, PartialEq, Eq)]
                #[allow(non_camel_case_types)]
                pub enum $name {
                    $(
                        $field_name
                    ),*
                }
            )*
        }
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        pub enum InternalField {
            $(
                $name(internal_fields::$name)
            ),*
        }

        impl InternalField {
            pub fn ty(&self) -> Type {
                match self {
                    $(
                        $(InternalField::$name(internal_fields::$name::$field_name) => $ty),*
                    )*
                }
            }

            pub fn name(&self) -> &'static str {
                match self {
                    $(
                        $(InternalField::$name(internal_fields::$name::$field_name) => stringify!($field_name)),*
                    )*
                }
            }
        }
    };
}
define_internal_types!(define_internal_types_impl);