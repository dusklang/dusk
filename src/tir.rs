use std::ops::{Deref, DerefMut};
use std::cmp::max;
use std::marker::PhantomData;
use string_interner::{Sym, DefaultStringInterner};
use smallvec::{SmallVec, smallvec};

use crate::dep_vec::DepVec;
use crate::source_info::SourceRange;
use crate::index_vec::{Idx, IdxVec};
use crate::ty::{Type, QualType};
use crate::builder::{self, *};

newtype_index!(TreeId pub);

#[derive(Debug)]
pub struct Ret { pub expr: ExprId, pub ty: Type }
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

#[derive(Clone)]
struct LocalDecl {
    name: Sym,
    level: Level,
    decl: DeclId,
}

#[derive(Debug)]
struct GlobalDecl {
    name: Sym, 
    num_params: u32,
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
    pub int_lits: Vec<Expr<()>>,
    pub dec_lits: Vec<Expr<()>>,
    pub str_lits: Vec<Expr<()>>,
    pub char_lits: Vec<Expr<()>>,
    pub stmts: Vec<Stmt>,
    pub rets: Vec<Expr<Ret>>,
    pub implicit_rets: Vec<Ret>,
    pub casts: Vec<Expr<Cast>>,
    pub whiles: Vec<Expr<While>>,
    /// An expression to uniquely represent the void value
    pub void_expr: ExprId,

    pub global_tree: Tree,
    pub local_trees: IdxVec<Tree, TreeId>,
    pub tree_offsets: IdxVec<u32, TreeId>,

    pub source_ranges: IdxVec<SourceRange, ExprId>,
    pub decls: IdxVec<Decl, DeclId>,
    /// Each declref's overload choices
    pub overloads: IdxVec<Vec<DeclId>, DeclRefId>,
    /// Number of expressions in the entire program
    pub num_exprs: usize,
}

pub struct Builder<'src> {
    int_lits: Vec<Expr<()>>,
    dec_lits: Vec<Expr<()>>,
    str_lits: Vec<Expr<()>>,
    char_lits: Vec<Expr<()>>,
    stmts: Vec<Stmt>,
    rets: Vec<Expr<Ret>>,
    implicit_rets: Vec<Ret>,
    casts: Vec<Expr<Cast>>,
    whiles: Vec<Expr<While>>,
    // An expression to uniquely represent the void value
    void_expr: ExprId,

    global_tree: Tree,
    local_trees: IdxVec<Tree, TreeId>,
    tree_dependencies: IdxVec<TreeDependencies, TreeId>,

    source_ranges: IdxVec<SourceRange, ExprId>,
    levels: IdxVec<Level, ExprId>,
    global_decls: Vec<GlobalDecl>,
    decls: IdxVec<Decl, DeclId>,
    /// Each declref's overload choices
    overloads: IdxVec<Vec<DeclId>, DeclRefId>,

    global_decl_refs: Vec<GlobalDeclRef>,
    /// State related to each nested computed decl
    comp_decl_stack: Vec<CompDeclState>,

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
            rets: Vec::new(),
            implicit_rets: Vec::new(),
            casts: Vec::new(),
            whiles: Vec::new(),
            void_expr,
            global_tree: Tree::new(),
            local_trees: IdxVec::new(),
            tree_dependencies: IdxVec::new(),
            source_ranges,
            levels,
            global_decls: Vec::new(),
            decls: IdxVec::new(),
            overloads: IdxVec::new(),
            global_decl_refs: Vec::new(),
            comp_decl_stack: vec![CompDeclState::new(Type::Void)],
            terminal_exprs: IdxVec::new(),
            interner: DefaultStringInterner::default(),

            _phantom_src_lifetime: PhantomData,
        }
    }

    fn insert_item<T: Item>(&mut self, item: T) -> Level {
        self._insert_item(item, None, None)
    }
    fn _insert_item<T: Item>(&mut self, item: T, extra_dep: Option<Level>, decl_ref: Option<DeclRefId>) -> Level {
        macro_rules! insert_local {
            ($tree:expr, $level:expr) => {{
                Item::storage_mut(&mut self.local_trees[$tree]).insert($level, item);
                Level::Local($tree, $level)
            }};
        }
        macro_rules! insert_global {
            ($level:expr) => {{
                Item::storage_mut(&mut self.global_tree).insert($level, item);
                Level::Global($level)
            }};
        }
        let mut dependencies = item.dependencies(&self);
        if let Some(extra_dep) = extra_dep {
            dependencies.push(extra_dep);
        }
        if dependencies.is_empty() {
            insert_global!(0)
        } else {
            // Unique the dependencies by tree, choosing the max level from each
            let mut depended_trees = Vec::<Level>::new();
            for cur in dependencies {
                let mut found = false;
                for &mut prev in &mut depended_trees {
                    match (prev, cur) {
                        (Level::Global(ref mut prev), Level::Global(cur)) => {
                            *prev = max(*prev, cur);
                            found = true;
                        },
                        (Level::Local(prev_tree, ref mut prev), Level::Local(cur_tree, cur)) if prev_tree == cur_tree => {
                            *prev = max(*prev, cur);
                            found = true;
                        }
                        _ => {}
                    }
                }
                if !found {
                    depended_trees.push(cur);
                }
            }

            macro_rules! fresh_tree {
                () => {{
                    let mut global = 0;
                    let mut local = Vec::new();
                    for dep in depended_trees {
                        match dep {
                            Level::Global(level) => global = level,
                            Level::Local(tree, level) => local.push((tree, level)),
                        }
                    }
                    let new_tree = self.local_trees.push(Tree::new());
                    let dependencies = self.tree_dependencies.push(TreeDependencies { global, local, decl_ref });
                    assert_eq!(new_tree, dependencies);
                    insert_local!(new_tree, 0)
                }};
            }

            if decl_ref.is_some() {
                // If the tree depends on the deepest overload of a declref, it can't yet be related to any other item and must go
                // in a fresh tree of its own
                fresh_tree!()
            }
            else if depended_trees.len() == 1 {
                // If the item depends only on items from one tree, put it in that tree
                match depended_trees[0] {
                    Level::Global(level) => {
                        insert_global!(level + 1)
                    },
                    Level::Local(id, level) => {
                        insert_local!(id, level + 1)
                    }
                }
            } else {
                // If the item depends on items from the global tree and exactly one local tree, put it in 
                // the local tree at the max level of all the items it depends on
                if depended_trees.len() == 2 {
                    let mut max_level = 0;
                    let mut local_tree = None;
                    for t in &depended_trees {
                        match t {
                            &Level::Global(level) => max_level = max(max_level, level),
                            &Level::Local(tree, level) => {
                                if let Some(_) = local_tree {
                                    local_tree = None;
                                    break;
                                }
                                max_level = max(max_level, level);
                                local_tree = Some(tree);
                            }
                        }
                    }
                    if let Some(local_tree) = local_tree {
                        return insert_local!(local_tree, max_level + 1);
                    }
                }

                // If the item depends on items from multiple local trees, create a fresh tree to put it in
                fresh_tree!()
            }
        }
    }

    fn insert_expr<T>(&mut self, data: T) -> ExprId where Expr<T>: Item {
        self._insert_expr(data, None, None)
    }
    fn insert_decl_ref(&mut self, data: DeclRef, extra_dep: Option<Level>) -> ExprId {
        let decl_ref = data.decl_ref_id;
        self._insert_expr(data, extra_dep, Some(decl_ref))
    }
    fn _insert_expr<T>(&mut self, data: T, extra_dep: Option<Level>, decl_ref: Option<DeclRefId>) -> ExprId where Expr<T>: Item {
        let id = ExprId::new(self.levels.len());
        let level = self._insert_item(Expr { id, data }, extra_dep, decl_ref);
        self.levels.push(level);
        id
    }
}

impl Item for Expr<Do> {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        smallvec![b.levels[self.terminal_expr]]
    }
    fn storage(tree: &Tree) -> &DepVec<Self> { &tree.dos }
    fn storage_mut(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.dos }
}

impl Item for Expr<Assignment> {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        smallvec![b.levels[self.lhs], b.levels[self.rhs]]
    }
    fn storage(tree: &Tree) -> &DepVec<Self> { &tree.assignments }
    fn storage_mut(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.assignments }
}

impl Item for Expr<DeclRef> {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        self.args.iter().map(|&id| b.levels[id]).collect()
    }
    fn storage(tree: &Tree) -> &DepVec<Self> { &tree.decl_refs }
    fn storage_mut(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.decl_refs }
}

impl Item for Expr<AddrOf> {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        smallvec![b.levels[self.expr]]
    }
    fn storage(tree: &Tree) -> &DepVec<Self> { &tree.addr_ofs }
    fn storage_mut(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.addr_ofs }
}

impl Item for Expr<Dereference> {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        smallvec![b.levels[self.expr]]
    }
    fn storage(tree: &Tree) -> &DepVec<Self> { &tree.derefs }
    fn storage_mut(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.derefs }
}

impl Item for Expr<If> {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        smallvec![b.levels[self.condition], b.levels[self.then_expr], b.levels[self.else_expr]]
    }
    fn storage(tree: &Tree) -> &DepVec<Self> { &tree.ifs }
    fn storage_mut(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.ifs }
}

impl Item for AssignedDecl {
    fn dependencies<'src>(&'src self, b: &'src Builder<'src>) -> SmallVec<[Level; 3]> {
        smallvec![b.levels[self.root_expr]]
    }
    fn storage(tree: &Tree) -> &DepVec<Self> { &tree.assigned_decls }
    fn storage_mut(tree: &mut Tree) -> &mut DepVec<Self> { &mut tree.assigned_decls }
}

pub trait Item: Sized {
    fn dependencies<'src>(&'src self, builder: &'src Builder<'src>) -> SmallVec<[Level; 3]>;
    fn storage(tree: &Tree) -> &DepVec<Self>;
    fn storage_mut(tree: &mut Tree) -> &mut DepVec<Self>;
}

impl<'src> Builder<'src> {
    fn compute_tree_offset(&self, tree: TreeId, tree_offsets: &mut IdxVec<u32, TreeId>) {
        if tree_offsets[tree] != std::u32::MAX {
            return;
        }

        let deps = &self.tree_dependencies[tree];
        let mut result = deps.global;
        for &(tree, offset) in &deps.local {
            self.compute_tree_offset(tree, tree_offsets);
            let local_offset = tree_offsets[tree] + offset;
            result = max(result, local_offset);
        }
        if let Some(decl_ref) = deps.decl_ref {
            for &overload in &self.overloads[decl_ref] {
                let level = self.decls[overload].level;
                match level {
                    Level::Global(offset) => result = max(result, offset),
                    Level::Local(tree, offset) => {
                        self.compute_tree_offset(tree, tree_offsets);
                        let local_offset = tree_offsets[tree] + offset;
                        result = max(result, local_offset);
                    },
                }
            }
        }
        tree_offsets[tree] = result;
    }
}

impl<'src> builder::Builder<'src> for Builder<'src> {
    type Output = Program;

    fn output(mut self) -> Program {
        // Enumerate overloads for all the decl refs
        for decl_ref in &self.global_decl_refs {
            self.overloads[decl_ref.id] = self.global_decls.iter().filter_map(|decl| {
                if decl.name == decl_ref.name && decl.num_params == decl_ref.num_arguments {
                    Some(decl.decl)
                } else {
                    None
                }
            }).collect();
        }

        // Compute base tree offsets using dynamic programming
        let mut tree_offsets = IdxVec::<u32, TreeId>::new();
        tree_offsets.raw.resize(self.local_trees.len(), std::u32::MAX);
        for i in 0..tree_offsets.len() {
            self.compute_tree_offset(TreeId::new(i), &mut tree_offsets);
        }

        Program {
            int_lits: self.int_lits,
            dec_lits: self.dec_lits,
            str_lits: self.str_lits,
            char_lits: self.char_lits,
            stmts: self.stmts,
            rets: self.rets,
            implicit_rets: self.implicit_rets,
            casts: self.casts,
            whiles: self.whiles,
            void_expr: self.void_expr,
            global_tree: self.global_tree,
            local_trees: self.local_trees,
            tree_offsets,
            source_ranges: self.source_ranges,
            decls: self.decls,
            overloads: self.overloads,
            num_exprs: self.levels.len(),
        }
    }

    fn add_intrinsic(&mut self, intrinsic: Intrinsic, param_tys: SmallVec<[Type; 2]>, ret_ty: Type) {
        let name = self.interner.get_or_intern(intrinsic.name());
        let num_params = param_tys.len() as u32;
        let id = self.decls.push(Decl::new(param_tys, ret_ty, Level::Global(0)));
        self.global_decls.push(GlobalDecl { name, num_params, decl: id });
    }

    fn void_expr(&self) -> ExprId { self.void_expr }

    fn int_lit(&mut self, _lit: u64, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        self.int_lits.push(Expr { id, data: () });
        self.levels.push(Level::Global(0));
        self.source_ranges.push(range);
        id
    }

    fn dec_lit(&mut self, _lit: f64, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        self.dec_lits.push(Expr { id, data: () });
        self.levels.push(Level::Global(0));
        self.source_ranges.push(range);

        id
    }

    fn str_lit(&mut self, _lit: String, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        self.str_lits.push(Expr { id, data: () });
        self.levels.push(Level::Global(0));
        self.source_ranges.push(range);

        id
    }

    fn char_lit(&mut self, _lit: i8, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        self.char_lits.push(Expr { id, data: () });
        self.levels.push(Level::Global(0));
        self.source_ranges.push(range);

        id
    }

    fn bin_op(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, range: SourceRange) -> ExprId {
        self.source_ranges.push(range);
        match op {
            BinOp::Assign => self.insert_expr(Assignment { lhs, rhs }), 
            _ => {
                let decl_ref_id = self.overloads.push(Vec::new());
                self.global_decl_refs.push(GlobalDeclRef { id: decl_ref_id, name: self.interner.get_or_intern(op.symbol()), num_arguments: 2 });
                self.insert_decl_ref(DeclRef { args: smallvec![lhs, rhs], decl_ref_id }, None)
            }
        }
    }

    fn cast(&mut self, expr: ExprId, ty: Type, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        let cast_id = CastId::new(self.casts.len());
        self.casts.push(Expr { id, data: Cast { expr, ty, cast_id }});
        self.levels.push(Level::Global(0));
        self.source_ranges.push(range);

        id
    }

    fn un_op(&mut self, op: UnOp, expr: ExprId, range: SourceRange) -> ExprId {
        self.source_ranges.push(range);
        match op {
            UnOp::AddrOf => self.insert_expr(AddrOf { expr, is_mut: false }),
            UnOp::AddrOfMut => self.insert_expr(AddrOf { expr, is_mut: true }),
            UnOp::Deref => self.insert_expr(Dereference { expr }),
            _ => {
                let decl_ref_id = self.overloads.push(Vec::new());
                self.global_decl_refs.push(GlobalDeclRef { id: decl_ref_id, name: self.interner.get_or_intern(op.symbol()), num_arguments: 1 });
                self.insert_decl_ref(DeclRef { args: smallvec![expr], decl_ref_id }, None)
            }
        }
    }

    fn stored_decl(&mut self, name: &'src str, explicit_ty: Option<Type>, is_mut: bool, root_expr: ExprId, _range: SourceRange) {
        let decl_id = DeclId::new(self.decls.len());
        let level = self.insert_item(AssignedDecl { explicit_ty, root_expr, decl_id });
        self.decls.push(
            Decl::new(SmallVec::new(), QualType { ty: Type::Error, is_mut }, level),
        );
        let name = self.interner.get_or_intern(name);
        self.comp_decl_stack.last_mut().unwrap().decls.push(LocalDecl { name, level, decl: decl_id });
    }

    fn ret(&mut self, expr: ExprId, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        let ty = self.comp_decl_stack.last().unwrap().ret_ty.clone();
        self.rets.push(Expr { id, data: Ret { expr, ty }});
        self.levels.push(Level::Global(0));
        self.source_ranges.push(range);

        id
    }

    fn implicit_ret(&mut self, expr: ExprId) {
        let ty = self.comp_decl_stack.last().unwrap().ret_ty.clone();
        self.implicit_rets.push(Ret { expr, ty });
    }

    fn if_expr(&mut self, condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId>, range: SourceRange) -> ExprId {
        let then_expr = self.terminal_exprs[then_scope];
        let else_expr = else_scope.map_or_else(|| self.void_expr(), |scope| self.terminal_exprs[scope]);
        self.source_ranges.push(range);
        self.insert_expr(If { condition, then_expr, else_expr })
    }

    fn while_expr(&mut self, condition: ExprId, _scope: ScopeId, range: SourceRange) -> ExprId {
        let id = ExprId::new(self.levels.len());
        self.whiles.push(Expr { id, data: While { condition }});
        self.levels.push(Level::Global(0));
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
        self.source_ranges.push(0..0);
        self.insert_expr(Do { terminal_expr: self.terminal_exprs[scope] })
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

    fn begin_computed_decl(&mut self, name: &'src str, param_names: SmallVec<[&'src str; 2]>, param_tys: SmallVec<[Type; 2]>, ret_ty: Type, _proto_range: SourceRange) {
        assert_eq!(param_names.len(), param_tys.len());
        let decl_id = self.decls.push(Decl::new(param_tys.clone(), ret_ty.clone(), Level::Global(0)));
        let name = self.interner.get_or_intern(name);
        let local_decl = LocalDecl { name, level: Level::Global(0), decl: decl_id };
        // Add decl to enclosing scope
        self.comp_decl_stack.last_mut().unwrap().decls.push(local_decl.clone());
        let mut decl_state = CompDeclState::new(ret_ty);
        // Add decl to its own scope to enable recursion
        decl_state.decls.push(local_decl);
        // Add parameters to scope
        for (&name, ty) in param_names.iter().zip(&param_tys) {
            let name = self.interner.get_or_intern(name);
            let id = self.decls.push(Decl::new(SmallVec::new(), ty.clone(), Level::Global(0)));
            decl_state.decls.push(LocalDecl { name, level: Level::Global(0), decl: id });
        }
        self.comp_decl_stack.push(decl_state);
    }

    fn end_computed_decl(&mut self) {
        self.comp_decl_stack.pop().unwrap();
    }

    fn decl_ref(&mut self, name: &'src str, arguments: SmallVec<[ExprId; 2]>, range: SourceRange) -> ExprId {
        let mut decl: Option<(Level, DeclId)> = None;
        let name = self.interner.get_or_intern(name);
        for &LocalDecl { name: other_name, level: other_level, decl: other_decl } in self.comp_decl_stack.last().unwrap().decls.iter().rev() {
            if name == other_name {
                decl = Some((other_level, other_decl));
                break;
            }
        }

        self.source_ranges.push(range);
        if let Some((level, decl)) = decl {
            // Local decl
            let decl_ref_id = self.overloads.push(vec![decl]);
            self.insert_decl_ref(DeclRef { args: arguments, decl_ref_id }, Some(level))
        } else {
            // Global decl
            let decl_ref_id = self.overloads.push(Vec::new());
            self.global_decl_refs.push(GlobalDeclRef { id: decl_ref_id, name, num_arguments: arguments.len() as u32 });
            self.insert_decl_ref(DeclRef { args: arguments, decl_ref_id }, None)
        }
    }

    fn get_range(&self, id: ExprId) -> SourceRange {
        self.source_ranges[id].clone()
    }

    fn get_terminal_expr(&self, scope: ScopeId) -> ExprId {
        self.terminal_exprs[scope]
    }
}
