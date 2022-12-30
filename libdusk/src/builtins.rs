use std::mem;

use smallvec::{smallvec, SmallVec};

use crate::dire::source_info::SourceRange;
use crate::dire::hir::{ModScopeNs, ModScope, Intrinsic, Decl, VOID_TYPE, ModScopedDecl, ModScopeNsId, EnumId, ExprId, VariantDecl};
use crate::dire::ty::{Type, InternalType};
use crate::dire::mir::Const;

use crate::driver::Driver;
use crate::hir::ScopeState;
use crate::autopop::AutoPopStackEntry;
use crate::parser::ParseResult;

struct EnumBuilder {
    expr: ExprId,
    id:  EnumId,
    variants: Vec<VariantDecl>,
}
impl Drop for EnumBuilder {
    fn drop(&mut self) {
        panic!("Must end enum with Driver::end_enum()");
    }
}

impl Driver {
    pub fn add_prelude(&mut self) {
        assert!(self.hir.prelude_namespace.is_none());
        let prelude_scope = self.code.hir.mod_scopes.push(ModScope::default());
        let prelude_namespace = self.code.hir.mod_ns.push(
            ModScopeNs {
                scope: prelude_scope,
                parent: None
            }
        );
        let _prelude_scope = self.push_to_scope_stack(prelude_namespace, ScopeState::Mod { id: prelude_scope, namespace: prelude_namespace, extern_mod: None });
        self.hir.prelude_namespace = Some(prelude_namespace);

        // Add intrinsics to prelude

        // Integers, floats and bool
        let types = [
            Type::u8(), Type::u16(), Type::u32(), Type::u64(), Type::usize(),
            Type::i8(), Type::i16(), Type::i32(), Type::i64(), Type::isize(),
            Type::f32(), Type::f64(), Type::Bool
        ];
        let inout_types = types.clone().into_iter().map(|ty| ty.inout());
        let types: Vec<_> = types.iter().map(|ty| self.add_const_ty(ty.clone())).collect();
        let inout_types: Vec<_> = inout_types.map(|ty| self.add_const_ty(ty)).collect();

        let numerics = &types[0..12];
        let inout_numerics = &inout_types[0..12];
        let signed_numerics = &numerics[5..];
        let integers = &numerics[0..10];
        let inout_integers = &inout_numerics[0..10];

        let boool        = types[12];
        let inout_bool   = inout_types[12];
        let uu8          = types[0];
        let never        = self.add_const_ty(Type::Never);
        let uusize       = self.add_const_ty(Type::usize());
        let u8_ptr       = self.add_const_ty(Type::u8().ptr());
        let void_mut_ptr = self.add_const_ty(Type::Void.mut_ptr());
        let type_type    = self.add_const_ty(Type::Ty);
        let mod_type     = self.add_const_ty(Type::Mod);

        use Intrinsic::*;
        for &intr in &[Mult, Div, Mod, Add, Sub] {
            for &ty in numerics {
                self.add_intrinsic(intr, smallvec![ty, ty], ty, true);
            }
        }
        for &intr in &[MultAssign, DivAssign, ModAssign, AddAssign, SubAssign] {
            for (&inout_ty, &ty) in inout_numerics.iter().zip(numerics) {
                self.add_intrinsic(intr, smallvec![inout_ty, ty], VOID_TYPE, true);
            }
        }
        for &intr in &[Less, LessOrEq, Greater, GreaterOrEq] {
            for &ty in numerics {
                self.add_intrinsic(intr, smallvec![ty, ty], boool, true);
            }
        }
        for &intr in &[Eq, NotEq] {
            for &ty in &types {
                self.add_intrinsic(intr, smallvec![ty, ty], boool, true);
            }
        }
        for &intr in &[BitwiseAnd, BitwiseOr, BitwiseXor, LeftShift, RightShift] {
            for &ty in integers {
                self.add_intrinsic(intr, smallvec![ty, ty], ty, true);
            }
        }
        for &intr in &[AndAssign, OrAssign, XorAssign, LeftShiftAssign, RightShiftAssign] {
            for (&inout_ty, &ty) in inout_integers.iter().zip(integers) {
                self.add_intrinsic(intr, smallvec![inout_ty, ty], VOID_TYPE, true);
            }
        }
        for &intr in &[AndAssign, OrAssign, XorAssign] {
            self.add_intrinsic(intr, smallvec![inout_bool, boool], VOID_TYPE, true);
        }
        for &ty in integers {
            self.add_intrinsic(BitwiseNot, smallvec![ty], ty, true);
        }
        for &intr in &[LogicalAnd, LogicalOr] {
            self.add_intrinsic(intr, smallvec![boool, boool], boool, true);
        }
        for &ty in signed_numerics {
            self.add_intrinsic(Neg, smallvec![ty], ty, true);
        }
        for &ty in numerics {
            self.add_intrinsic(Pos, smallvec![ty], ty, true);
        }
        self.add_intrinsic(LogicalNot, smallvec![boool], boool, true);

        self.add_intrinsic(Panic, SmallVec::new(), never, true);
        self.add_intrinsic(Panic, smallvec![u8_ptr], never, true);

        self.add_intrinsic(Malloc, smallvec![uusize], void_mut_ptr, true);
        self.add_intrinsic(Free, smallvec![void_mut_ptr], VOID_TYPE, true);

        self.add_intrinsic(Print, smallvec![u8_ptr], VOID_TYPE, true);
        self.add_intrinsic(Print, smallvec![uu8], VOID_TYPE, true);
        self.add_intrinsic(PrintType, smallvec![type_type], VOID_TYPE, true);

        self.add_intrinsic(AlignOf, smallvec![type_type], uusize, true);
        self.add_intrinsic(SizeOf, smallvec![type_type], uusize, true);
        self.add_intrinsic(StrideOf, smallvec![type_type], uusize, true);
        self.add_intrinsic(OffsetOf, smallvec![type_type, u8_ptr], uusize, true);

        self.add_intrinsic(Import, smallvec![u8_ptr], mod_type, true);

        macro_rules! types {
            ($($ty:ident),+) => {
                $(self.add_constant_type_decl(stringify!($ty), Type::$ty());)+
            };
        }

        types!(
            i8, i16, i32, i64, isize,
            u8, u16, u32, u64, usize,
            f32, f64
        );
        self.add_constant_type_decl("never", Type::Never);
        self.add_constant_type_decl("bool", Type::Bool);
        self.add_constant_type_decl("void", Type::Void);
        self.add_constant_type_decl("type", Type::Ty);
        self.add_constant_type_decl("module", Type::Mod);

        let compiler_module = self.add_module_decl("compiler");
        let string_lit_type = self.add_const_ty(Type::Internal(InternalType::StringLiteral));
        self.add_constant_type_decl("StringLiteral", Type::Internal(InternalType::StringLiteral));

        // Add Platform enum
        let mut platform_enum = self.start_enum("Platform");
        let platform_id = platform_enum.id;
        self.add_variant(&mut platform_enum, "windows", None);
        self.add_variant(&mut platform_enum, "linux", None);
        self.add_variant(&mut platform_enum, "macos", None);
        self.end_enum(platform_enum);

        // TODO: brittle
        let index = if cfg!(target_os = "windows") {
            0
        } else if cfg!(target_os = "linux") {
            1
        } else if cfg!(target_os = "macos") {
            2
        } else {
            todo!("unsupported OS");
        };
        self.add_constant_decl("target", Const::BasicVariant { enuum: platform_id, index });
        drop(compiler_module);

        let runtime_module = self.add_module_decl("runtime");
        self.add_intrinsic(GetNumArgs, smallvec![], uusize, true);
        self.add_intrinsic(GetArg, smallvec![uusize], string_lit_type, true);
        drop(runtime_module);

        if !self.no_core {
            self.add_virtual_file_module("core", include_str!("../core/core.dusk")).unwrap();
        }
    }

    fn add_constant_decl(&mut self, name: &str, value: Const) {
        let expr = self.add_const_expr(value);
        let name = self.interner.get_or_intern(name);
        let decl = self.add_decl(Decl::Const(expr), name, None, SourceRange::default());
        self.mod_scoped_decl(
            name,
            ModScopedDecl {
                num_params: 0,
                id: decl,
            }
        );
    }

    fn add_constant_type_decl(&mut self, name: &str, ty: Type) {
        self.add_constant_decl(name, Const::Ty(ty));
    }

    fn add_module_decl(&mut self, name: &str) -> AutoPopStackEntry<ScopeState, ModScopeNsId> {
        let prelude_scope = self.code.hir.mod_scopes.push(ModScope::default());
        let prelude_namespace = self.code.hir.mod_ns.push(
            ModScopeNs {
                scope: prelude_scope,

                // technically this field could be filled with whatever is on the top of the stack, but it doesn't
                // matter because the only thing going in this module will be builtins, which don't refer to anything
                // else by name
                parent: None,
            }
        );
        self.add_constant_decl(name, Const::Mod(prelude_scope));
        self.push_to_scope_stack(prelude_namespace, ScopeState::Mod { id: prelude_scope, namespace: prelude_namespace, extern_mod: None })
    }
    
    fn start_enum(&mut self, name: &str) -> EnumBuilder {
        let (expr, id) = self.reserve_enum();
        self.add_constant_type_decl(name, Type::Enum(id));
        EnumBuilder { expr, id, variants: Default::default() }
    }

    fn add_variant(&mut self, b: &mut EnumBuilder, name: &str, payload_ty: Option<Type>) {
        let name = self.interner.get_or_intern(name);
        let payload_ty = payload_ty.map(|ty| self.add_const_expr(Const::Ty(ty)));
        let next_variant = self.variant_decl(name, b.expr, b.id, b.variants.len(), payload_ty, SourceRange::default());
        b.variants.push(next_variant);
    }

    fn end_enum(&mut self, mut b: EnumBuilder) {
        self.finish_enum(mem::take(&mut b.variants), SourceRange::default(), b.expr, b.id);
        mem::forget(b);
    }

    fn add_virtual_file_module(&mut self, name: &str, src: &str) -> ParseResult<()>  {
        let file = self.src_map.add_virtual_file(name, src.to_string()).unwrap();
        self.parse_file(file)?;
        let scope = self.code.hir.global_scopes[&file];
        self.add_constant_decl(name, Const::Mod(scope));
        Ok(())
    }
}