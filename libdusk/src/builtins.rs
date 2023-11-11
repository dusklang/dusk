use std::ffi::CString;
use std::mem;

use internal_types::ExternParam;
use smallvec::{smallvec, SmallVec};
use num_bigint::BigInt;

use crate::index_vec::empty_range;
use crate::internal_types;
use crate::source_info::SourceRange;
use crate::ast::{ModScopeNs, LegacyIntrinsic, Expr, Decl, VOID_TYPE, ModScopeNsId, NewNamespaceId, EnumId, ExprId, VariantDecl, StaticDecl, InstanceDecl, NewNamespace, ExternFunctionRef, ExternFunction, ParamList};
use crate::ty::{Type, LegacyInternalType};
use crate::mir::Const;

use crate::driver::Driver;
use crate::ast::ScopeState;
use crate::autopop::AutoPopStackEntry;
use crate::parser::ParseResult;

use crate::internal_types::{ModuleBuilder, Module, ExternFunctionBuilder};

use dusk_proc_macros::{ef, df, dusk_bridge};

#[dusk_bridge]
impl Driver {
    #[path="compiler.ModuleBuilder"]
    fn new(&mut self) -> ModuleBuilder {
        let namespace = self.code.ast.new_namespaces.push(NewNamespace::default());
        ModuleBuilder { namespace }
    }

    #[path="compiler.ModuleBuilder"]
    fn add_usize_constant(&mut self, #[self] b: ModuleBuilder, name: &'static str, value: usize) {
        let before = self.take_snapshot();

        let name = self.interner.get_or_intern(name);
        let konst = self.add_const_expr(Const::Int { lit: BigInt::from(value), ty: Type::usize() });
        let ty = self.add_const_ty(Type::usize());
        let decl_id = self.add_decl(Decl::Const { assigned_expr: konst, generic_params: empty_range() }, name, Some(ty), SourceRange::default());
        let static_decl = StaticDecl { name, decl: decl_id };
        self.code.ast.new_namespaces[b.namespace].static_decls.push(static_decl);

        // NOTE: we currently have to do this every time this function is called, to be safe. Once we have proper collection types
        // implemented in the language, the user can just build up an array of decls and add them all at once.
        let new_code = self.get_new_code_since(before);
        self.finalize_ast();
        self.initialize_tir(&new_code);
    }

    #[path="compiler.ModuleBuilder"]
    fn add_extern_function(&mut self, #[self] b: ModuleBuilder, func_builder: ExternFunctionBuilder) {
        let before = self.take_snapshot();

        // TODO: should group functions from the same library into the same ExternMod, and then also de-duplicate functions within the same ExternMod
        // TODO: also, rename ExternMod, since it no longer corresponds to a lexical module in the source code
        let name = self.interner.get_or_intern(&func_builder.name);
        let ret_ty = self.add_const_ty(func_builder.ret_ty);
        let mut param_tys = SmallVec::new();
        for param in func_builder.params {
            let ty = self.add_const_ty(param.ty);
            param_tys.push(ty);
        }
        let library_path = self.add_const_expr(Const::StrLit(CString::new(func_builder.lib_name).unwrap()));
        let param_list = ParamList { param_tys: param_tys.clone(), has_c_variadic_param: func_builder.has_variadic_param };
        let func = ExternFunction {
            name: func_builder.name,
            param_list: param_list.clone(),
            return_ty: ret_ty,
        };
        let extern_mod = crate::ast::ExternMod { library_path, imported_functions: vec![func], objc_class_references: Default::default() };
        let extern_mod = self.code.ast.extern_mods.push(extern_mod);
        let extern_func_ref = ExternFunctionRef {
            extern_mod,
            index: 0,
        };
        let decl_id = self.add_decl(Decl::ComputedPrototype { param_list, extern_func: Some(extern_func_ref) }, name, Some(ret_ty), SourceRange::default());
        let static_decl = StaticDecl { name, decl: decl_id };
        self.code.ast.new_namespaces[b.namespace].static_decls.push(static_decl);

        // NOTE: we currently have to do this every time this function is called, to be safe. Once we have proper collection types
        // implemented in the language, the user can just build up an array and add them all at once.
        let new_code = self.get_new_code_since(before);
        self.finalize_ast();
        self.initialize_tir(&new_code);
    }

    #[path="compiler.ModuleBuilder"]
    fn add_objc_class_ref(&mut self, #[self] b: ModuleBuilder, class_name: &'static str, lib_name: &'static str) {
        let before = self.take_snapshot();
        
        let name = self.interner.get_or_intern(class_name);
        let library_path = self.add_const_expr(Const::StrLit(CString::new(lib_name).unwrap()));
        let extern_mod = crate::ast::ExternMod { library_path, imported_functions: Default::default(), objc_class_references: vec![class_name.to_string()] };
        let extern_mod = self.code.ast.extern_mods.push(extern_mod);
        let void_ptr = self.add_const_ty(Type::Void.ptr());
        let decl_id = self.add_decl(Decl::ObjcClassRef { extern_mod, index: 0 }, name, Some(void_ptr), SourceRange::default());
        let static_decl = StaticDecl { name, decl: decl_id };
        self.code.ast.new_namespaces[b.namespace].static_decls.push(static_decl);

        // NOTE: we currently have to do this every time this function is called, to be safe. Once we have proper collection types
        // implemented in the language, the user can just build up an array and add them all at once.
        let new_code = self.get_new_code_since(before);
        self.finalize_ast();
        self.initialize_tir(&new_code);
    }

    #[path="compiler.ModuleBuilder"]
    fn build(&mut self, #[self] b: ModuleBuilder) -> Module {
        Module(b.namespace)
    }

    #[path="compiler.ExternFunctionBuilder"]
    fn new(&mut self, name: &'static str, ret_ty: Type, lib_name: &'static str) -> ExternFunctionBuilder {
        ExternFunctionBuilder { name: name.to_string(), ret_ty, lib_name: lib_name.to_string(), params: Default::default(), has_variadic_param: false }
    }

    #[path="compiler.ExternFunctionBuilder"]
    fn add_param(&mut self, #[self] b: &mut ExternFunctionBuilder, name: &'static str, ty: Type) {
        assert!(!b.has_variadic_param, "no parameters can be added after a variadic parameter");
        b.params.push(ExternParam { name: name.to_string(), ty });
    }

    #[path="compiler.ExternFunctionBuilder"]
    fn add_variadic_param(&mut self, #[self] b: &mut ExternFunctionBuilder) {
        assert!(!b.has_variadic_param, "a variadic parameter can only be added once");
        b.has_variadic_param = true;
    }

    fn print_int(&mut self, val: usize) {
        println!("int: {}", val);
    }
}

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
        assert!(self.ast.prelude_namespace.is_none());
        let prelude_scope = self.code.ast.new_namespaces.push(NewNamespace::default());
        let prelude_namespace = self.code.ast.mod_ns.push(
            ModScopeNs {
                scope: prelude_scope,
                parent: None
            }
        );
        let _prelude_scope = self.push_to_scope_stack(prelude_namespace, ScopeState::Mod { id: prelude_scope, namespace: prelude_namespace, extern_mod: None });
        self.ast.prelude_namespace = Some(prelude_namespace);

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

        use LegacyIntrinsic::*;
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
        self.add_constant_type_decl("module", Type::Mod);

        let compiler_module = self.add_module_decl("compiler");
        let string_lit_type = self.add_const_ty(Type::LegacyInternal(LegacyInternalType::StringLiteral));
        self.add_constant_type_decl("StringLiteral", Type::LegacyInternal(LegacyInternalType::StringLiteral));

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

        internal_types::register(self);
        register_bridged_rust_methods(self);
    }

    fn add_constant_decl(&mut self, name: &str, value: Const) {
        let expr = self.add_const_expr(value);
        let name = self.interner.get_or_intern(name);
        let decl = self.add_decl(Decl::Const { assigned_expr: expr, generic_params: empty_range() }, name, None, SourceRange::default());
        self.mod_scoped_decl(
            StaticDecl {
                name,
                decl,
            }
        );
    }

    fn add_constant_type_decl(&mut self, name: &str, ty: Type) {
        self.add_constant_decl(name, Const::Ty(ty));
    }

    pub fn add_decl_to_path(&mut self, name: &str, path: &str, decl: Decl, explicit_ty: Option<ExprId>) {
        let scope = self.find_or_build_relative_ns_path(path);
        let name = self.interner.get_or_intern(name);
        if let Decl::MethodIntrinsic(id) = decl {
            let decl_id = self.add_decl(Decl::Intrinsic(id), name, explicit_ty, SourceRange::default());
            let static_decl = StaticDecl { name, decl: decl_id };
            self.code.ast.new_namespaces[scope].static_decls.push(static_decl);

            let decl_id = self.add_decl(decl, name, explicit_ty, SourceRange::default());
            self.code.ast.new_namespaces[scope].instance_decls.push(InstanceDecl { decl: decl_id, field_info: None });
        } else {
            let decl_id = self.add_decl(decl, name, explicit_ty, SourceRange::default());
            let static_decl = StaticDecl { name, decl: decl_id };
            self.code.ast.new_namespaces[scope].static_decls.push(static_decl);
        }
    }

    fn add_module_decl(&mut self, name: &str) -> AutoPopStackEntry<ScopeState, ModScopeNsId> {
        let scope = self.code.ast.new_namespaces.push(NewNamespace::default());
        let namespace = self.code.ast.mod_ns.push(
            ModScopeNs {
                scope,

                // technically this field could be filled with whatever is on the top of the stack, but it doesn't
                // matter because the only thing going in this module will be builtins, which don't refer to anything
                // else by name
                parent: None,
            }
        );
        self.add_constant_decl(name, Const::Mod(scope));
        self.push_to_scope_stack(namespace, ScopeState::Mod { id: scope, namespace, extern_mod: None })
    }

    pub fn find_or_build_relative_ns_path(&mut self, path: &str) -> NewNamespaceId {
        let mut ns = self.find_nearest_mod_scope().unwrap();

        if path.trim().is_empty() {
            return ns;
        }

        for name in path.split('.').map(str::trim) {
            assert!(!name.is_empty());

            let name = self.interner.get_or_intern(name);
            let matching_decls: Vec<&StaticDecl> = self.code.ast.new_namespaces[ns].static_decls.iter().filter(|decl| decl.name == name).collect();

            assert!(matching_decls.len() == 1);
            let decl = matching_decls[0];
            let Decl::Const { assigned_expr: expr, .. } = df!(decl.decl.ast) else { panic!("internal compiler error: expected const decl") };
            let Expr::Const(konst) = &ef!(expr.ast) else { panic!("internal compiler error: expected const expr") };
            ns = match *konst {
                Const::Mod(new_ns) => new_ns,
                Const::Ty(Type::Internal(id)) => self.code.ast.internal_types[id].namespace,
                _ => panic!("internal compiler error: expected const mod or type"),
            };
        }
        ns
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
        let scope = self.code.ast.global_scopes[&file];
        self.add_constant_decl(name, Const::Mod(scope));
        Ok(())
    }
}
