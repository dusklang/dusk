use std::convert::From;
use std::io::{Read, Write, Result as IoResult};
use std::process::{Command, Stdio};
use std::fs::{self, File};
use std::path::PathBuf;
use std::iter::Chain;
use std::slice::Iter as SliceIter;
use std::mem;

use crate::builder::{ExprId, DeclId};
use crate::index_vec::{Idx, IdxVec};
use crate::driver::Driver;

#[derive(Debug, Copy, Clone)]
pub enum ItemId {
    Expr(ExprId),
    Decl(DeclId),
}

pub trait Item: Copy + Into<ItemId> {
    fn dependees(self, graph: &mut Graph) -> &mut Vec<ItemId>;
    fn dependers(self, graph: &mut Graph) -> &mut Vec<ItemId>;
    fn adjacents<'a>(self, graph: &'a Graph) -> Chain<SliceIter<'a, ItemId>, SliceIter<'a, ItemId>>;
    fn set_visited(self, state: &mut ComponentState);
    fn add_to_component(self, state: &mut ComponentState);
}

impl Item for ExprId {
    fn dependees(self, graph: &mut Graph) -> &mut Vec<ItemId> {
        &mut graph.expr_dependees[self]
    }

    fn dependers(self, graph: &mut Graph) -> &mut Vec<ItemId> {
        &mut graph.expr_dependers[self]
    }

    fn adjacents<'a>(self, graph: &'a Graph) -> Chain<SliceIter<'a, ItemId>, SliceIter<'a, ItemId>> {
        graph.expr_dependees[self].iter()
            .chain(graph.expr_dependers[self].iter())
    }

    fn set_visited(self, state: &mut ComponentState) {
        state.expr_visited[self] = true;
    }

    fn add_to_component(self, state: &mut ComponentState) {
        state.cur_component.exprs.push(self);
    }
}

impl Item for DeclId {
    fn dependees(self, graph: &mut Graph) -> &mut Vec<ItemId> {
        &mut graph.decl_dependees[self]
    }

    fn dependers(self, graph: &mut Graph) -> &mut Vec<ItemId> {
        &mut graph.decl_dependers[self]
    }

    fn adjacents<'a>(self, graph: &'a Graph) -> Chain<SliceIter<'a, ItemId>, SliceIter<'a, ItemId>> {
        graph.decl_dependees[self].iter()
            .chain(graph.decl_dependers[self].iter())
    }

    fn set_visited(self, state: &mut ComponentState) {
        state.decl_visited[self] = true;
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

pub struct Graph {
    // TODO: consider putting ExprIds and DeclIds into separate arrays, or one sorted one, rather than all together using an enum
    expr_dependees: IdxVec<Vec<ItemId>, ExprId>,
    decl_dependees: IdxVec<Vec<ItemId>, DeclId>,

    // Used exclusively for finding connected components
    expr_dependers: IdxVec<Vec<ItemId>, ExprId>,
    decl_dependers: IdxVec<Vec<ItemId>, DeclId>,

    components: Option<Vec<Component>>,
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
    expr_visited: IdxVec<bool, ExprId>,
    decl_visited: IdxVec<bool, DeclId>,

    cur_component: Component,
}



impl Graph {
    pub fn add_type1_dep(&mut self, a: impl Item, b: impl Item) {
        let a_val = a.into();
        let b_val = b.into();
        // TODO: remove this HACK to prevent type 1 dependencies on the void expression
        if let ItemId::Expr(expr) = b_val {
            if expr.idx() == 0 { return; }
        }
        a.dependees(self).push(b_val);
        b.dependers(self).push(a_val);
    }

    fn find_component(&self, item: impl Item, state: &mut ComponentState) {
        item.set_visited(state);
        item.add_to_component(state);
        for &adj in item.adjacents(self) {
            let visited = match adj {
                ItemId::Expr(expr) => if !state.expr_visited[expr] {
                    self.find_component(expr, state);
                },
                ItemId::Decl(decl) => if !state.decl_visited[decl] {
                    self.find_component(decl, state);
                },
            };
        }
    }

    // Find the weak components of the graph
    pub fn split(&mut self) {
        let mut expr_visited = IdxVec::new();
        expr_visited.resize_with(self.expr_dependees.len(), || false);
        let mut decl_visited = IdxVec::new();
        decl_visited.resize_with(self.decl_dependees.len(), || false);
        let mut state = ComponentState {
            expr_visited, decl_visited, cur_component: Component::default(),
        };
        assert!(self.components.is_none());
        let mut components = Vec::new();
        for i in 0..self.expr_dependees.len() {
            let id = ExprId::new(i);
            if !state.expr_visited[id] {
                self.find_component(id, &mut state);
                let new_component = mem::replace(&mut state.cur_component, Component::default());
                components.push(new_component);
            }
        }

        for i in 0..self.decl_dependees.len() {
            let id = DeclId::new(i);
            if !state.decl_visited[id] {
                self.find_component(id, &mut state);
                let new_component = mem::replace(&mut state.cur_component, Component::default());
                components.push(new_component);
            }
        }

        self.components = Some(components);
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
        let mut expr_dependees = IdxVec::new();
        expr_dependees.resize_with(self.hir.exprs.len(), || Vec::new());
        let mut decl_dependees = IdxVec::new();
        decl_dependees.resize_with(self.hir.decls.len(), || Vec::new());

        let mut expr_dependers = IdxVec::new();
        expr_dependers.resize_with(self.hir.exprs.len(), || Vec::new());
        let mut decl_dependers = IdxVec::new();
        decl_dependers.resize_with(self.hir.decls.len(), || Vec::new());

        Graph { expr_dependees, decl_dependees, expr_dependers, decl_dependers, components: None }
    }

    fn write_expr_deps(&self, w: &mut impl Write, a: ExprId, graph: &Graph) -> IoResult<()> {
        for &b in &graph.expr_dependees[a] {
            write!(w, "    ")?;
            write!(w, "expr{}", a.idx())?;
            write!(w, " -> ")?;
            b.write_node_name(w)?;
            writeln!(w, ";")?;
        }
        Ok(())
    }

    fn write_decl_deps(&self, w: &mut impl Write, a: DeclId, graph: &Graph) -> IoResult<()> {
        for &b in &graph.decl_dependees[a] {
            write!(w, "    ")?;
            write!(w, "decl{}", a.idx())?;
            write!(w, " -> ")?;
            b.write_node_name(w)?;
            writeln!(w, ";")?;
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

    // Prints graph in Graphviz format, then opens a web browser to display the results.
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
                    self.write_expr_deps(&mut w, expr, graph)?;
                    self.write_expr_node(&mut w, expr)?;
                }
                for &decl in &component.decls {
                    self.write_decl_deps(&mut w, decl, graph)?;
                }
                writeln!(w, "    }}")?;
            }
        } else {
            for i in 0..graph.expr_dependees.len() {
                let a = ExprId::new(i);
                self.write_expr_deps(&mut w, a, graph)?;
                self.write_expr_node(&mut w, a)?;
            }
            for i in 0..graph.decl_dependees.len() {
                let a = DeclId::new(i);
                self.write_decl_deps(&mut w, a, graph)?;
            }
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