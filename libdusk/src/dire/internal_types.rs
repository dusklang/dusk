use std::any::TypeId;

use dusk_proc_macros::DuskBridge;

use crate::dire::ast::NewNamespaceId;
use crate::ty::Type;
use crate::driver::Driver;
use crate::interpreter::Value;

#[derive(DuskBridge, Copy, Clone)]
#[module = "compiler"]
pub struct ModuleBuilder {
    pub namespace: NewNamespaceId,
}

#[derive(Clone)]
pub struct ExternParam {
    pub name: String,
    pub ty: Type,
}

#[derive(DuskBridge, Clone)]
#[module = "compiler"]
pub struct ExternFunctionBuilder {
    pub name: String,
    pub ret_ty: Type,
    pub lib_name: String,
    pub params: Vec<ExternParam>,
    pub has_variadic_param: bool,
}

#[derive(Copy, Clone)]
pub struct Module(pub NewNamespaceId);

pub trait DuskBridge: 'static {
    fn to_dusk_type(d: &Driver) -> Type {
        let id = TypeId::of::<Self>();

        d.code.ast.bridged_types[&id].clone()
    }
    fn register(d: &mut Driver);
    fn bridge_from_dusk(value: &Value, d: &Driver) -> Self;
    fn bridge_to_dusk(self, d: &Driver) -> Value;
}

impl DuskBridge for () {
    fn register(d: &mut Driver) {
        d.code.ast.bridged_types.insert(TypeId::of::<Self>(), Type::Void);
    }

    fn bridge_from_dusk(_value: &Value, _d: &Driver) -> Self {
        ()
    }

    fn bridge_to_dusk(self, _d: &Driver) -> Value {
        Value::Nothing
    }
}

impl DuskBridge for &'static str {
    fn register(d: &mut Driver) {
        d.code.ast.bridged_types.insert(TypeId::of::<Self>(), Type::i8().ptr());
    }

    fn bridge_from_dusk(value: &Value, _d: &Driver) -> Self {
        unsafe { &*(value.as_str() as *const str) }
    }

    fn bridge_to_dusk(self, _d: &Driver) -> Value {
        unimplemented!("Rust strings are not null-terminated; use CStr instead")
    }
}

impl DuskBridge for Module {
    fn register(d: &mut Driver) {
        d.code.ast.bridged_types.insert(TypeId::of::<Self>(), Type::Mod);
    }

    fn bridge_from_dusk(value: &Value, _d: &Driver) -> Self {
        Self(value.as_mod())
    }

    fn bridge_to_dusk(self, _d: &Driver) -> Value {
        Value::from_mod(self.0)
    }
}

macro_rules! bridge_ints {
    ($($int_name: ident),*) => {
        $(
            impl DuskBridge for $int_name {
                fn register(d: &mut Driver) {
                    d.code.ast.bridged_types.insert(TypeId::of::<$int_name>(), Type::$int_name());
                }
            
                fn bridge_from_dusk(value: &Value, _d: &Driver) -> Self {
                    unsafe { *value.as_arbitrary_value() }
                }
            
                fn bridge_to_dusk(self, _d: &Driver) -> Value {
                    unsafe { Value::from_arbitrary_value(self) }
                }
            }
        )*
    }
}

bridge_ints!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);

macro_rules! declare_internal_types {
    ($register_name:ident : $($name:ty),*) => {
        pub fn $register_name(d: &mut Driver) {
            $(
                <$name>::register(d);
            )*
        }
    };
}

declare_internal_types!(
    register:
        ModuleBuilder, ExternFunctionBuilder, &'static mut ExternFunctionBuilder,
        Module, Type,
        u8, u16, u32, u64, usize, i8, i16, i32, i64, isize,
        &'static str,
        ()
);

// This is a higher-order macro which takes in a macro and passes it all internal types and their members
macro_rules! define_legacy_internal_types {
    ($name:ident) => {
        $name!(
            struct StringLiteral {
                length: Type::usize(),
                data: Type::u8().ptr(),
            }
        );
    };
}

macro_rules! define_legacy_internal_types_impl {
    ($(struct $name:ident {
        $($field_name:ident: $ty:expr),*$(,)?
    })*) => {
        #[derive(Copy, Clone, Debug)]
        pub enum InternalNamespace {
            $($name),*
        }

        pub mod internal_field_decls {
            use crate::dire::ast::DeclId;
            $(
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
                        $(InternalField::$name(internal_fields::$name::$field_name) => $ty),*,
                    )*
                }
            }

            pub fn name(&self) -> &'static str {
                match self {
                    $(
                        $(InternalField::$name(internal_fields::$name::$field_name) => stringify!($field_name)),*,
                    )*
                }
            }
        }
    };
}
define_legacy_internal_types!(define_legacy_internal_types_impl);
