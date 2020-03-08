use std::convert::From;
use std::io::{Read, Write, Result as IoResult};
use std::process::{Command, Stdio};
use std::fs::{self, File};
use std::path::PathBuf;
use std::iter::Chain;
use std::slice::Iter as SliceIter;
use std::mem;
use std::ops::{Index, IndexMut};

use crate::builder::{ExprId, DeclId};
use crate::index_vec::{Idx, IdxVec};
use crate::driver::Driver;

newtype_index!(CompId);

#[derive(Debug, Copy, Clone)]
pub enum ItemId {
    Expr(ExprId),
    Decl(DeclId),
}

pub trait Item: Copy + Into<ItemId> {
    fn elem<T>(self, vec: &ItemIdxVec<T>) -> &T;
    fn elem_mut<T>(self, vec: &mut ItemIdxVec<T>) -> &mut T;
    fn write_node_name(self, w: &mut impl Write) -> IoResult<()>;
    fn add_to_component(self, state: &mut ComponentState);
}

impl Item for ExprId {
    fn elem<T>(self, vec: &ItemIdxVec<T>) -> &T {
        &vec.expr[self]
    }

    fn elem_mut<T>(self, vec: &mut ItemIdxVec<T>) -> &mut T {
        &mut vec.expr[self]
    }

    fn write_node_name(self, w: &mut impl Write) -> IoResult<()> {
        write!(w, "expr{}", self.idx())
    }

    fn add_to_component(self, state: &mut ComponentState) {
        state.cur_component.exprs.push(self);
    }
}

impl Item for DeclId {
    fn elem<T>(self, vec: &ItemIdxVec<T>) -> &T {
        &vec.decl[self]
    }

    fn elem_mut<T>(self, vec: &mut ItemIdxVec<T>) -> &mut T {
        &mut vec.decl[self]
    }

    fn write_node_name(self, w: &mut impl Write) -> IoResult<()> {
        write!(w, "decl{}", self.idx())
    }

    fn add_to_component(self, state: &mut ComponentState) {
        state.cur_component.decls.push(self);
    }
}

impl From<ExprId> for ItemId {
    fn from(id: ExprId) -> Self {
        Self::Expr(id)
    }
}

impl From<DeclId> for ItemId {
    fn from(id: DeclId) -> Self {
        Self::Decl(id)
    }
}

pub struct ItemIdxVec<T> {
    expr: IdxVec<T, ExprId>,
    decl: IdxVec<T, DeclId>,
}

impl<T> ItemIdxVec<T> {
    fn new() -> Self {
        Self {
            expr: IdxVec::new(),
            decl: IdxVec::new(),
        }
    }

    fn expr_len(&self) -> usize { self.expr.len() }
    fn decl_len(&self) -> usize { self.decl.len() }

    fn resize_with(&mut self, num_exprs: usize, num_decls: usize, mut f: impl FnMut() -> T) {
        self.expr.resize_with(num_exprs, || f());
        self.decl.resize_with(num_decls, || f());
    }
}

impl<I: Item, T> Index<I> for ItemIdxVec<T> {
    type Output = T;
    fn index(&self, id: I) -> &Self::Output {
        id.elem(self)
    }
}

impl<I: Item, T> IndexMut<I> for ItemIdxVec<T> {
    fn index_mut(&mut self, id: I) -> &mut Self::Output {
        id.elem_mut(self)
    }
}

pub struct Graph {
    dependees: ItemIdxVec<Vec<ItemId>>,
    t2_dependees: ItemIdxVec<Vec<ItemId>>,
    t3_dependees: ItemIdxVec<Vec<ItemId>>,
    t4_dependees: ItemIdxVec<Vec<ItemId>>,

    // Used exclusively for finding connected components
    dependers: ItemIdxVec<Vec<ItemId>>,

    item_to_components: ItemIdxVec<CompId>,

    components: Option<IdxVec<Component, CompId>>,
}

#[derive(Default)]
struct Component {
    exprs: Vec<ExprId>,
    decls: Vec<DeclId>,
}

// TODO: Make this private. Right now it's public only because it's used in the Item trait above.
// So we need a better way to abstract over exprs and decls.
pub struct ComponentState {
    // TODO: Vec of bools == gross
    visited: ItemIdxVec<bool>,

    cur_component: Component,
    components: IdxVec<Component, CompId>,
    item_to_components: ItemIdxVec<CompId>,
}

impl Graph {
    /// a and b must be in the *same* subprogram, and a must have a higher level than b
    pub fn add_type1_dep(&mut self, a: impl Item, b: impl Item) {
        let a_val = a.into();
        let b_val = b.into();
        // TODO: remove this HACK to prevent type 1 dependencies on the void expression
        if let ItemId::Expr(expr) = b_val {
            if expr.idx() == 0 { return; }
        }
        
        self.dependees[a].push(b_val);
        self.dependers[b].push(a_val);
    }

    /// a must either be in the same subprogram as b or a later subprogram, but if they are in the same subprogram, a must have a higher level than b
    pub fn add_type2_dep(&mut self, a: impl Item, b: impl Item) {
        self.t2_dependees[a].push(b.into());
    }

    /// a must either be in the same subprogram as b or a later subprogram, but their levels are independent
    pub fn add_type3_dep(&mut self, a: impl Item, b: impl Item) {
        self.t3_dependees[a].push(b.into());
    }

    /// a must be in an later subprogram than b, but their levels are independent
    pub fn add_type4_dep(&mut self, a: impl Item, b: impl Item) {
        self.t4_dependees[a].push(b.into());
    }

    fn find_subcomponent(&self, item: impl Item, state: &mut ComponentState) {
        state.visited[item] = true;
        item.add_to_component(state);
        state.item_to_components[item] = CompId::new(state.components.len());
        let adjs = self.dependees[item].iter()
            .chain(self.dependers[item].iter());
        for &adj in adjs {
            match adj {
                ItemId::Expr(expr) => if !state.visited.expr[expr] { self.find_subcomponent(expr, state) },
                ItemId::Decl(decl) => if !state.visited.decl[decl] { self.find_subcomponent(decl, state) },
            };
        }
    }

    fn find_component(&self, item: impl Item, state: &mut ComponentState) {
        if state.visited[item] { return; }
        self.find_subcomponent(item, state);
        let new_component = mem::replace(&mut state.cur_component, Component::default());
        state.components.push(new_component);
    }

    // Find the weak components of the graph
    pub fn split(&mut self) {
        let mut visited = ItemIdxVec::new();
        visited.resize_with(self.dependees.expr_len(), self.dependees.decl_len(), || false);
        let mut state = ComponentState {
            visited, cur_component: Component::default(), components: IdxVec::new(), item_to_components: ItemIdxVec::new(),
        };
        state.item_to_components.resize_with(self.dependees.expr_len(), self.dependees.decl_len(), || CompId::new(std::usize::MAX));
        assert!(self.components.is_none());
        for i in 0..self.dependees.expr_len() {
            self.find_component(ExprId::new(i), &mut state);
        }

        for i in 0..self.dependees.decl_len() {
            self.find_component(DeclId::new(i), &mut state);
        }

        self.components = Some(state.components);
        self.item_to_components = state.item_to_components;
    }
}

impl ItemId {
    fn write_node_name<T: Write>(&self, w: &mut T) -> IoResult<()> {
        match self {
            ItemId::Expr(id) => write!(w, "expr{}", id.idx())?,
            ItemId::Decl(id) => write!(w, "decl{}", id.idx())?,
        }
        Ok(())
    }
}

impl Driver {
    pub fn create_graph(&self) -> Graph {
        let mut deps = [ItemIdxVec::new(), ItemIdxVec::new(), ItemIdxVec::new(), ItemIdxVec::new(), ItemIdxVec::new()];
        for dep in &mut deps {
            dep.resize_with(self.hir.exprs.len(), self.hir.decls.len(), || Vec::new());
        }

        let mut item_to_components = ItemIdxVec::new();
        item_to_components.resize_with(self.hir.exprs.len(), self.hir.decls.len(), || CompId::new(std::usize::MAX));
        
        let [dependees, t2_dependees, t3_dependees, t4_dependees, dependers] = deps;
        Graph { dependees, t2_dependees, t3_dependees, t4_dependees, dependers, item_to_components, components: None }
    }

    fn write_dep<W: Write>(&self, w: &mut W, a: impl Item, b: ItemId, write_attribs: impl FnOnce(&mut W) -> IoResult<()>) -> IoResult<()> {
        write!(w, "    ")?;
        a.write_node_name(w)?;
        write!(w, " -> ")?;
        b.write_node_name(w)?;
        write_attribs(w)?;
        writeln!(w, ";")?;
        Ok(())
    }

    fn write_deps(&self, w: &mut impl Write, a: impl Item, graph: &Graph) -> IoResult<()> {
        for &b in &graph.dependees[a] {
            self.write_dep(w, a, b, |_| Ok(()))?;
        }
        Ok(())
    }

    fn write_non_t1_deps(&self, w: &mut impl Write, a: impl Item, graph: &Graph) -> IoResult<()> {
        for &b in &graph.t2_dependees[a] {
            self.write_dep(w, a, b, |w| write!(w, " [style=dashed]"))?;
        }
        for &b in &graph.t3_dependees[a] {
            self.write_dep(w, a, b, |w| write!(w, " [style=dashed; color=grey]"))?;
        }
        for &b in &graph.t4_dependees[a] {
            self.write_dep(w, a, b, |w| write!(w, " [color=red]"))?;
        }
        Ok(())
    }

    fn write_expr_node(&self, w: &mut impl Write, id: ExprId) -> IoResult<()> {
        let range = self.hir.source_ranges[id].clone();
        if range.start != range.end {
            writeln!(
                w,
                "    expr{} [label=\"{}\\l\"];",
                id.idx(),
                // TODO: do something more efficient than calling replace multiple times
                self.file.substring_from_range(range)
                    .replace("\\", "\\\\")
                    .replace("\"", "\\\"")
                    .replace("\n", "\\n")
                    .replace("\r", ""),
            )?;
        }
        Ok(())
    }

    /// Prints graph in Graphviz format, then opens a web browser to display the results.
    pub fn print_graph(&self, graph: &Graph) -> IoResult<()> {
        let tmp_dir = fs::read_dir(".")?.find(|entry| entry.as_ref().unwrap().file_name() == "tmp");
        if tmp_dir.is_none() {
            fs::create_dir("tmp")?;
        }
        let mut w = File::create("tmp/tc_graph.gv")?;
        writeln!(w, "digraph G {{")?;
        writeln!(w, "    node [shape=box];")?;

        if let Some(components) = &graph.components {
            for (i, component) in components.iter().enumerate() {
                writeln!(w, "    subgraph cluster{} {{", i)?;
                writeln!(w, "        label=cluster{};", i)?;
                writeln!(w, "        style=filled;")?;
                writeln!(w, "        color=lightgrey;")?;
                writeln!(w, "        node [style=filled,color=white];")?;
                for &expr in &component.exprs {
                    self.write_deps(&mut w, expr, graph)?;
                    self.write_expr_node(&mut w, expr)?;
                }
                for &decl in &component.decls {
                    self.write_deps(&mut w, decl, graph)?;
                }
                writeln!(w, "    }}")?;
            }
        } else {
            for i in 0..graph.dependees.expr_len() {
                let a = ExprId::new(i);
                self.write_deps(&mut w, a, graph)?;
                self.write_expr_node(&mut w, a)?;
            }
            for i in 0..graph.dependees.decl_len() {
                let a = DeclId::new(i);
                self.write_deps(&mut w, a, graph)?;
            }
        }
        for i in 0..graph.dependees.expr_len() {
            let a = ExprId::new(i);
            self.write_non_t1_deps(&mut w, a, graph)?;
        }
        for i in 0..graph.dependees.decl_len() {
            let a = DeclId::new(i);
            self.write_non_t1_deps(&mut w, a, graph)?;
        }
        writeln!(w, "}}")?;
        w.flush()?;

        let command = Command::new("dot")
            .args(&["-Tsvg", "tmp/tc_graph.gv", "-o", "tmp/tc_graph.svg"])
            .stdout(Stdio::inherit())
            .output()
            .expect("Failed to execute graph rendering command");

        let graph_pdf_path: PathBuf = [".", "tmp", "tc_graph.svg"].iter().collect();
        webbrowser::open(graph_pdf_path.to_str().unwrap()).unwrap();

        Ok(())
    }
}