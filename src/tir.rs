use std::ops::{Deref, DerefMut};
use string_interner::{Sym, DefaultStringInterner};
use smallvec::{SmallVec, smallvec};

use crate::dep_vec::DepVec;
use crate::source_info::SourceRange;
use crate::index_vec::{Idx, IdxVec};
use crate::ty::{Type, QualType};
use crate::builder::{self, *};

#[derive(Debug)]
pub struct Ret { pub expr: ExprId, pub ty: Type }
#[derive(Debug)]
pub struct Cast { pub expr: ExprId, pub ty: Type }
#[derive(Debug)]
pub struct AddrOf { pub expr: ExprId, pub is_mut: bool }
#[derive(Debug)]
pub struct Dereference { pub expr: ExprId }
#[derive(Debug)]
pub struct Stmt { pub root_expr: ExprId }
#[derive(Debug)]
pub struct Do { pub terminal_expr: ExprId }
#[derive(Debug)]
pub struct AssignedDecl { pub explicit_ty: Option<Type>, pub root_expr: ExprId, pub decl_id: DeclId }
#[derive(Debug)]
pub struct Assignment { pub lhs: ExprId, pub rhs: ExprId }
#[derive(Debug)]
pub struct DeclRef { pub args: SmallVec<[ExprId; 2]>, pub decl_ref_id: DeclRefId }
#[derive(Debug)]
pub struct If { pub condition: ExprId, pub then_expr: ExprId, pub else_expr: ExprId }
#[derive(Debug)]
pub struct While { pub condition: ExprId }

#[derive(Debug)]
pub struct Expr<T> {
    pub id: ExprId,
    pub data: T,
}

impl<T> Deref for Expr<T> {
    type Target = T;
    fn deref(&self) -> &T { &self.data }
}

impl<T> DerefMut for Expr<T> {
    fn deref_mut(&mut self) -> &mut T { &mut self.data }
}

#[derive(Debug)]
pub struct Decl {
    pub name: Sym,
    pub param_tys: SmallVec<[Type; 2]>,
    pub ret_ty: QualType,
}

impl Decl {
    fn new(name: Sym, param_tys: SmallVec<[Type; 2]>, ret_ty: impl Into<QualType>) -> Self {
        Decl { name, param_tys, ret_ty: ret_ty.into() }
    }
}

#[derive(Clone)]
struct LocalDecl {
    name: Sym,
    level: u32,
    decl: DeclId,
}

struct ScopeState {
    id: ScopeId,
    previous_decls: usize,
    stmt_buffer: Option<ExprId>,
}

struct CompDeclState {
    ret_ty: Type,
    decls: Vec<LocalDecl>,
    scope_stack: Vec<ScopeState>,
}

impl CompDeclState {
    fn new(ret_ty: Type) -> Self {
        Self {
            ret_ty,
            decls: Vec::new(),
            scope_stack: Vec::new(),
        }
    }
}

struct GlobalDeclRef {
    id: DeclRefId,
    name: Sym,
    num_arguments: u32,
}

#[derive(Debug)]
pub struct Program {
    pub int_lits: Vec<Expr<()>>,
    pub dec_lits: Vec<Expr<()>>,
    pub str_lits: Vec<Expr<()>>,
    pub char_lits: Vec<Expr<()>>,
    pub stmts: Vec<Stmt>,
    pub dos: DepVec<Expr<Do>>,
    pub assigned_decls: DepVec<AssignedDecl>,
    pub assignments: DepVec<Expr<Assignment>>,
    pub decl_refs: DepVec<Expr<DeclRef>>,
    pub addr_ofs: DepVec<Expr<AddrOf>>,
    pub derefs: DepVec<Expr<Dereference>>,
    pub rets: Vec<Expr<Ret>>,
    pub implicit_rets: Vec<Ret>,
    pub casts: Vec<Expr<Cast>>,
    pub ifs: DepVec<Expr<If>>,
    pub whiles: Vec<Expr<While>>,
    /// An expression to uniquely represent the void value
    pub void_expr: ExprId,

    pub source_ranges: IdxVec<SourceRange, ExprId>,
    pub global_decls: IdxVec<Decl, GlobalDeclId>,
    pub local_decls: IdxVec<Decl, LocalDeclId>,
    /// Each declref's overload choices
    pub overloads: IdxVec<Vec<DeclId>, DeclRefId>,
    /// Number of expressions in the entire program
    pub num_exprs: usize,
}

impl Program {
    pub fn decl(&self, id: DeclId) -> &Decl { 
        match id {
            DeclId::Global(id) => &self.global_decls[id],
            DeclId::Local(id) => &self.local_decls[id],
        }
    }
}

pub struct Builder<'a> {
    int_lits: Vec<Expr<()>>,
    dec_lits: Vec<Expr<()>>,
    str_lits: Vec<Expr<()>>,
    char_lits: Vec<Expr<()>>,
    stmts: Vec<Stmt>,
    dos: DepVec<Expr<Do>>,
    assigned_decls: DepVec<AssignedDecl>,
    assignments: DepVec<Expr<Assignment>>,
    decl_refs: DepVec<Expr<DeclRef>>,
    addr_ofs: DepVec<Expr<AddrOf>>,
    derefs: DepVec<Expr<Dereference>>,
    rets: Vec<Expr<Ret>>,
    implicit_rets: Vec<Ret>,
    casts: Vec<Expr<Cast>>,
    ifs: DepVec<Expr<If>>,
    whiles: Vec<Expr<While>>,
    // An expression to uniquely represent the void value
    void_expr: ExprId,

    source_ranges: IdxVec<SourceRange, ExprId>,
    levels: IdxVec<u32, ExprId>,
    global_decls: IdxVec<Decl, GlobalDeclId>,
    local_decls: IdxVec<Decl, LocalDeclId>,
    /// Each declref's overload choices
    overloads: IdxVec<Vec<DeclId>, DeclRefId>,

    global_decl_refs: Vec<GlobalDeclRef>,
    /// State related to each nested computed decl
    comp_decl_stack: Vec<CompDeclState>,

    /// The terminal expression for each scope so far 
    /// (or the void expression if there is no terminal expression)
    terminal_exprs: IdxVec<ExprId, ScopeId>,

    interner: &'a mut DefaultStringInterner,
}

impl<'a> builder::Builder<'a> for Builder<'a> {
    type Output = Program;

    fn new(interner: &'a mut DefaultStringInterner) -> Self {
        // Create the void expression
        let mut levels = IdxVec::new();
        let mut source_ranges = IdxVec::new();
        let void_expr = levels.push(0);
        source_ranges.push(0..0);

        Self {
            int_lits: Vec::new(),
            dec_lits: Vec::new(),
            str_lits: Vec::new(),
            char_lits: Vec::new(),
            stmts: Vec::new(),
            dos: DepVec::new(),
            assigned_decls: DepVec::new(),
            assignments: DepVec::new(),
            decl_refs: DepVec::new(),
            addr_ofs: DepVec::new(),
            derefs: DepVec::new(),
            rets: Vec::new(),
            implicit_rets: Vec::new(),
            casts: Vec::new(),
            ifs: DepVec::new(),
            whiles: Vec::new(),
            void_expr,
            source_ranges,
            levels,
            global_decls: IdxVec::new(),
            local_decls: IdxVec::new(),
            overloads: IdxVec::new(),
            global_decl_refs: Vec::new(),
            comp_decl_stack: vec![CompDeclState::new(Type::Void)],
            terminal_exprs: IdxVec::new(),

            interner,
        }
    }

    fn output(mut self) -> Program {
        for decl_ref in self.global_decl_refs {
            self.overloads[decl_ref.id] = self.global_decls.indices_satisfying(|decl| {
                decl.name == decl_ref.name && decl.param_tys.len() == decl_ref.num_arguments as usize
            }).iter().map(|&i| DeclId::Global(i)).collect();
        }

        Program {
            int_lits: self.int_lits,
            dec_lits: self.dec_lits,
            str_lits: self.str_lits,
            char_lits: self.char_lits,
            stmts: self.stmts,
            dos: self.dos,
            assigned_decls: self.assigned_decls,
            assignments: self.assignments,
            decl_refs: self.decl_refs,
            addr_ofs: self.addr_ofs,
            derefs: self.derefs,
            rets: self.rets,
            implicit_rets: self.implicit_rets,
            casts: self.casts,
            ifs: self.ifs,
            whiles: self.whiles,
            void_expr: self.void_expr,
            source_ranges: self.source_ranges,
            local_decls: self.local_decls,
            global_decls: self.global_decls,
            overloads: self.overloads,
            num_exprs: self.levels.len(),
        }
    }

    fn add_intrinsic(&mut self, intrinsic: Intrinsic, param_tys: SmallVec<[Type; 2]>, ret_ty: Type) {
        let name = self.interner.get_or_intern(intrinsic.name());
        self.global_decls.push(Decl::new(name, param_tys, ret_ty));
    }

    fn interner(&self) -> &DefaultStringInterner { &self.interner }

    fn void_expr(&self) -> ExprId { self.void_expr }

    fn int_lit(&mut self, _lit: u64, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        self.int_lits.push(Expr { id, data: () });
        self.levels.push(0);
        self.source_ranges.push(range);
        id
    }

    fn dec_lit(&mut self, _lit: f64, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        self.dec_lits.push(Expr { id, data: () });
        self.levels.push(0);
        self.source_ranges.push(range);

        id
    }

    fn str_lit(&mut self, _lit: String, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        self.str_lits.push(Expr { id, data: () });
        self.levels.push(0);
        self.source_ranges.push(range);

        id
    }

    fn char_lit(&mut self, _lit: i8, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        self.char_lits.push(Expr { id, data: () });
        self.levels.push(0);
        self.source_ranges.push(range);

        id
    }

    fn bin_op(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        let level = match op {
            BinOp::Assign => {
                self.assignments.insert(
                    &[self.levels[lhs], self.levels[rhs]],
                    Expr { id, data: Assignment { lhs, rhs } },
                )
            }, 
            _ => {
                let decl_ref_id = self.overloads.push(Vec::new());
                self.global_decl_refs.push(GlobalDeclRef { id: decl_ref_id, name: self.interner.get_or_intern(op.symbol()), num_arguments: 2 });
                self.decl_refs.insert(
                    &[self.levels[lhs], self.levels[rhs]],
                    Expr { id, data: DeclRef { args: smallvec![lhs, rhs], decl_ref_id } },
                )
            }
        };
        self.levels.push(level);
        self.source_ranges.push(range);

        id
    }

    fn cast(&mut self, expr: ExprId, ty: Type, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        self.casts.push(Expr { id, data: Cast { expr, ty }});
        self.levels.push(0);
        self.source_ranges.push(range);

        id
    }

    fn un_op(&mut self, op: UnOp, expr: ExprId, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        let level = match op {
            UnOp::AddrOf => self.addr_ofs.insert(
                &[self.levels[expr]],
                Expr { id, data: AddrOf { expr, is_mut: false } },
            ),
            UnOp::AddrOfMut => self.addr_ofs.insert(
                &[self.levels[expr]],
                Expr { id, data: AddrOf { expr, is_mut: true } },
            ),
            UnOp::Deref => self.derefs.insert(
                &[self.levels[expr]],
                Expr { id, data: Dereference { expr } },
            ),
            _ => {
                let decl_ref_id = self.overloads.push(Vec::new());
                self.global_decl_refs.push(GlobalDeclRef { id: decl_ref_id, name: self.interner.get_or_intern(op.symbol()), num_arguments: 1 });
                self.decl_refs.insert(
                    &[self.levels[expr]],
                    Expr { id, data: DeclRef { args: smallvec![expr], decl_ref_id } },
                )
            }
        };
        self.levels.push(level);
        self.source_ranges.push(range);

        id
    }

    fn stored_decl(&mut self, name: Sym, explicit_ty: Option<Type>, is_mut: bool, root_expr: ExprId, range: SourceRange) {
        let decl_id = self.local_decls.push(
            Decl::new(name, SmallVec::new(), QualType { ty: Type::Error, is_mut }),
        );
        let decl_id = DeclId::Local(decl_id);
        let level = self.assigned_decls.insert(&[self.levels[root_expr]], AssignedDecl { explicit_ty, root_expr, decl_id });
        self.source_ranges.push(range);

        self.comp_decl_stack.last_mut().unwrap().decls.push(LocalDecl { name, level, decl: decl_id });
    }

    fn ret(&mut self, expr: ExprId, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        let ty = self.comp_decl_stack.last().unwrap().ret_ty.clone();
        self.rets.push(Expr { id, data: Ret { expr, ty }});
        self.levels.push(0);
        self.source_ranges.push(range);

        id
    }
    
    fn implicit_ret(&mut self, expr: ExprId) {
        let ty = self.comp_decl_stack.last().unwrap().ret_ty.clone();
        self.implicit_rets.push(Ret { expr, ty });
    }

    fn if_expr(&mut self, condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId>, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        let then_expr = self.terminal_exprs[then_scope];
        let else_expr = else_scope.map_or_else(|| self.void_expr(), |scope| self.terminal_exprs[scope]);
        let level = self.ifs.insert(
            &[self.levels[condition], self.levels[then_expr], self.levels[else_expr]],
            Expr { id, data: If { condition, then_expr, else_expr } }
        );
        self.levels.push(level);
        self.source_ranges.push(range);

        id
    }

    fn while_expr(&mut self, condition: ExprId, _scope: ScopeId, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        self.whiles.push(Expr { id, data: While { condition }});
        self.levels.push(0);
        self.source_ranges.push(range);

        id
    }

    fn stmt(&mut self, expr: ExprId) {
        let scope_state = self.comp_decl_stack.last_mut().unwrap().scope_stack.last_mut().unwrap();
        if let Some(stmt) = scope_state.stmt_buffer {
            self.stmts.push(Stmt { root_expr: stmt });
        }
        scope_state.stmt_buffer = Some(expr);
    }

    fn do_expr(&mut self, scope: ScopeId) -> ExprId {
        let id = ExprId::new(self.levels.len());
        let terminal_expr = self.terminal_exprs[scope];
        let level = self.dos.insert(
            &[self.levels[terminal_expr]],
            Expr { id, data: Do { terminal_expr } }
        );
        self.levels.push(level);
        self.source_ranges.push(0..0);

        id
    }

    fn begin_scope(&mut self) -> ScopeId {
        let scope = self.terminal_exprs.push(self.void_expr());
        let stack = self.comp_decl_stack.last_mut().unwrap();
        stack.scope_stack.push(
            ScopeState {
                id: scope,
                previous_decls: stack.decls.len(),
                stmt_buffer: None,
            }
        );
        scope
    }

    fn end_scope(&mut self, has_terminal_expr: bool) {
        let stack = self.comp_decl_stack.last_mut().unwrap();
        let scope = stack.scope_stack.pop().unwrap();
        if has_terminal_expr {
            let terminal_expr = scope.stmt_buffer.expect("must pass terminal expression via Builder::stmt()");
            self.terminal_exprs[scope.id] = terminal_expr;
        } else if let Some(stmt) = scope.stmt_buffer {
            self.stmts.push(Stmt { root_expr: stmt });
        }
        stack.decls.truncate(scope.previous_decls);
    }

    fn begin_computed_decl(&mut self, name: Sym, param_names: SmallVec<[Sym; 2]>, param_tys: SmallVec<[Type; 2]>, ret_ty: Type, _proto_range: SourceRange) {
        assert_eq!(param_names.len(), param_tys.len());
        let decl_id = self.local_decls.push(Decl::new(name, param_tys.clone(), ret_ty.clone()));
        let decl_id = DeclId::Local(decl_id);
        let local_decl = LocalDecl { name: name, level: 0, decl: decl_id };
        // Add decl to enclosing scope
        self.comp_decl_stack.last_mut().unwrap().decls.push(local_decl.clone());
        let mut decl_state = CompDeclState::new(ret_ty);
        // Add decl to its own scope to enable recursion
        decl_state.decls.push(local_decl);
        // Add parameters to scope
        for (&name, ty) in param_names.iter().zip(&param_tys) {
            let id = self.local_decls.push(Decl::new(name, SmallVec::new(), ty.clone()));
            decl_state.decls.push(LocalDecl { name, level: 0, decl: DeclId::Local(id) });
        }
        self.comp_decl_stack.push(decl_state);
    }

    fn end_computed_decl(&mut self) {
        self.comp_decl_stack.pop().unwrap();
    }

    fn decl_ref(&mut self, name: Sym, arguments: SmallVec<[ExprId; 2]>, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());

        let mut decl: Option<(u32, DeclId)> = None;
        for &LocalDecl { name: other_name, level: other_level, decl: other_decl } in self.comp_decl_stack.last().unwrap().decls.iter().rev() {
            if name == other_name {
                decl = Some((other_level, other_decl));
                break;
            }
        }

        let arguments_max_level = arguments.iter().map(|&id| self.levels[id]).max().unwrap_or(0);
        let level = if let Some((level, decl)) = decl {
            // Local decl
            let decl_ref_id = self.overloads.push(vec![decl]);
            self.decl_refs.insert(
                &[arguments_max_level, level],
                Expr { id, data: DeclRef { args: arguments, decl_ref_id } },
            )
        } else {
            // Global decl
            let decl_ref_id = self.overloads.push(Vec::new());
            self.global_decl_refs.push(GlobalDeclRef { id: decl_ref_id, name: name, num_arguments: arguments.len() as u32 });
            self.decl_refs.insert(
                &[arguments_max_level],
                Expr { id, data: DeclRef { args: arguments, decl_ref_id } },
            )
        };
        self.levels.push(level);
        self.source_ranges.push(range);

        id
    }

    fn get_range(&self, id: ExprId) -> SourceRange {
        self.source_ranges[id].clone()
    }

    fn get_terminal_expr(&self, scope: ScopeId) -> ExprId {
        self.terminal_exprs[scope]
    }
}
