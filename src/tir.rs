use std::ops::{Deref, DerefMut};
use std::marker::PhantomData;

use string_interner::DefaultStringInterner;
use smallvec::{SmallVec, smallvec};

use crate::dep_vec::DepVec;
use crate::source_info::SourceRange;
use crate::index_vec::{Idx, IdxVec};
use crate::ty::{Type, QualType};
use crate::builder::*;

newtype_index!(TreeId pub);
newtype_index!(RetGroupId);

#[derive(Debug)]
pub struct RetGroup { pub ty: Type, pub exprs: SmallVec<[ExprId; 1]> }
#[derive(Debug)]
pub struct Cast { pub expr: ExprId, pub ty: Type, pub cast_id: CastId }
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

impl RetGroup {
    pub fn new(ty: Type) -> RetGroup {
        RetGroup { ty, exprs: SmallVec::new() }
    }
}

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
    pub param_tys: SmallVec<[Type; 2]>,
    pub ret_ty: QualType,
    pub level: Level,
}

impl Decl {
    fn new(param_tys: SmallVec<[Type; 2]>, ret_ty: impl Into<QualType>, level: Level) -> Self {
        Decl { param_tys, ret_ty: ret_ty.into(), level }
    }
}

#[derive(Debug)]
pub struct Tree {
    pub dos: DepVec<Expr<Do>>,
    pub assigned_decls: DepVec<AssignedDecl>,
    pub assignments: DepVec<Expr<Assignment>>,
    pub decl_refs: DepVec<Expr<DeclRef>>,
    pub addr_ofs: DepVec<Expr<AddrOf>>,
    pub derefs: DepVec<Expr<Dereference>>,
    pub ifs: DepVec<Expr<If>>,
}

impl Tree {
    fn new() -> Self {
        Self {
            dos: DepVec::new(),
            assigned_decls: DepVec::new(),
            assignments: DepVec::new(),
            decl_refs: DepVec::new(),
            addr_ofs: DepVec::new(),
            derefs: DepVec::new(),
            ifs: DepVec::new(),
        }
    }

    fn extend(&mut self, offset: u32, other: Tree) {
        self.dos.extend(offset, other.dos);
        self.assigned_decls.extend(offset, other.assigned_decls);
        self.assignments.extend(offset, other.assignments);
        self.decl_refs.extend(offset, other.decl_refs);
        self.addr_ofs.extend(offset, other.addr_ofs);
        self.derefs.extend(offset, other.derefs);
        self.ifs.extend(offset, other.ifs);
    }
}

#[derive(Debug)]
pub struct TreeDependencies {
    pub global: u32,
    pub local: Vec<(TreeId, u32)>,
    /// Depend on the deepest overload of a decl ref
    pub decl_ref: Option<DeclRefId>,
}

#[derive(Debug, Copy, Clone)]
pub enum Level {
    Global(u32),
    Local(TreeId, u32),
}

#[derive(Debug)]
pub struct Program {
    pub int_lits: Vec<ExprId>,
    pub dec_lits: Vec<ExprId>,
    pub str_lits: Vec<ExprId>,
    pub char_lits: Vec<ExprId>,
    pub stmts: Vec<Stmt>,
    pub explicit_rets: Vec<ExprId>,
    pub ret_groups: Vec<RetGroup>,
    pub casts: Vec<Expr<Cast>>,
    pub whiles: Vec<Expr<While>>,
    /// An expression to uniquely represent the void value
    pub void_expr: ExprId,
    pub tree: Tree,
    pub source_ranges: IdxVec<SourceRange, ExprId>,
    pub decls: IdxVec<Decl, DeclId>,
    /// Each declref's overload choices
    pub overloads: IdxVec<Vec<DeclId>, DeclRefId>,
    /// Number of expressions in the entire program
    pub num_exprs: usize,
}

pub struct Builder<'src> {
    int_lits: Vec<ExprId>,
    dec_lits: Vec<ExprId>,
    str_lits: Vec<ExprId>,
    char_lits: Vec<ExprId>,
    stmts: Vec<Stmt>,
    explicit_rets: Vec<ExprId>,
    ret_groups: IdxVec<RetGroup, RetGroupId>,
    casts: Vec<Expr<Cast>>,
    whiles: Vec<Expr<While>>,
    // An expression to uniquely represent the void value
    void_expr: ExprId,

    global_tree: Tree,
    local_trees: IdxVec<Tree, TreeId>,
    tree_dependencies: IdxVec<TreeDependencies, TreeId>,

    source_ranges: IdxVec<SourceRange, ExprId>,
    levels: IdxVec<Level, ExprId>,
    decls: IdxVec<Decl, DeclId>,
    /// Each declref's overload choices
    overloads: IdxVec<Vec<DeclId>, DeclRefId>,

    /// The terminal expression for each scope so far 
    /// (or the void expression if there is no terminal expression)
    terminal_exprs: IdxVec<ExprId, ScopeId>,

    interner: DefaultStringInterner,

    _phantom_src_lifetime: PhantomData<&'src str>,
}

impl<'src> Builder<'src> {
    pub fn new() -> Self {
        // Create the void expression
        let mut levels = IdxVec::new();
        let mut source_ranges = IdxVec::new();
        let void_expr = levels.push(Level::Global(0));
        source_ranges.push(0..0);

        Self {
            int_lits: Vec::new(),
            dec_lits: Vec::new(),
            str_lits: Vec::new(),
            char_lits: Vec::new(),
            stmts: Vec::new(),
            explicit_rets: Vec::new(),
            ret_groups: IdxVec::new(),
            casts: Vec::new(),
            whiles: Vec::new(),
            void_expr,
            global_tree: Tree::new(),
            local_trees: IdxVec::new(),
            tree_dependencies: IdxVec::new(),
            source_ranges,
            levels,
            decls: IdxVec::new(),
            overloads: IdxVec::new(),
            terminal_exprs: IdxVec::new(),
            interner: DefaultStringInterner::default(),

            _phantom_src_lifetime: PhantomData,
        }
    }
}

pub trait Item: Sized {
    fn dependencies<'src>(&'src self, builder: &'src Builder<'src>) -> SmallVec<[Level; 3]>;
    fn storage(tree: &mut Tree) -> &mut DepVec<Self>;
}

impl Item for Expr<Do> {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        smallvec![b.levels[self.terminal_expr]]
    }
    fn storage(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.dos }
}

impl Item for Expr<Assignment> {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        smallvec![b.levels[self.lhs], b.levels[self.rhs]]
    }
    fn storage(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.assignments }
}

impl Item for Expr<DeclRef> {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        self.args.iter().map(|&id| b.levels[id]).collect()
    }
    fn storage(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.decl_refs }
}

impl Item for Expr<AddrOf> {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        smallvec![b.levels[self.expr]]
    }
    fn storage(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.addr_ofs }
}

impl Item for Expr<Dereference> {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        smallvec![b.levels[self.expr]]
    }
    fn storage(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.derefs }
}

impl Item for Expr<If> {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        smallvec![b.levels[self.condition], b.levels[self.then_expr], b.levels[self.else_expr]]
    }
    fn storage(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.ifs }
}

impl Item for AssignedDecl {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        smallvec![b.levels[self.root_expr]]
    }
    fn storage(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.assigned_decls }
}