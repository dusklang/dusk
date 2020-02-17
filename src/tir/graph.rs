use std::convert::From;
use std::io::{Read, Write, Result as IoResult};
use std::process::{Command, Stdio};
use std::fs::{self, File};
use std::path::PathBuf;

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
}

impl Item for ExprId {
    fn dependees(self, graph: &mut Graph) -> &mut Vec<ItemId> {
        &mut graph.expr_dependees[self]
    }

    fn dependers(self, graph: &mut Graph) -> &mut Vec<ItemId> {
        &mut graph.expr_dependers[self]
    }
}

impl Item for DeclId {
    fn dependees(self, graph: &mut Graph) -> &mut Vec<ItemId> {
        &mut graph.decl_dependees[self]
    }

    fn dependers(self, graph: &mut Graph) -> &mut Vec<ItemId> {
        &mut graph.decl_dependers[self]
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

    components: Vec<Vec<ItemId>>,
}

struct SplitState {
    // TODO: bool array == gross
    expr_visited: IdxVec<bool, ExprId>,
    decl_visited: IdxVec<bool, DeclId>,
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

    // Find the weak components of the graph
    pub fn split_graph(&mut self) {
        let mut expr_visited = IdxVec::new();
        expr_visited.resize_with(self.expr_dependees.len(), || false);
        let mut decl_visited = IdxVec::new();
        decl_visited.resize_with(self.decl_dependees.len(), || false);
        let mut state = SplitState {
            expr_visited, decl_visited
        };
        
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

        Graph { expr_dependees, decl_dependees, expr_dependers, decl_dependers, components: Vec::new() }
    }

    // Prints graph in Graphviz format, then opens a web browser to display the results.
    pub fn print_graph(&self, graph: &Graph) -> IoResult<()> {
        let tmp_dir = fs::read_dir(".")?.find(|entry| entry.as_ref().unwrap().file_name() == "tmp");
        if tmp_dir.is_none() {
            fs::create_dir("tmp")?;
        }
        let mut w = File::create("tmp/tc_graph.gv")?;
        writeln!(w, "digraph G {{")?;

        for i in 0..graph.expr_dependees.len() {
            let a = ExprId::new(i);
            for &b in &graph.expr_dependees[a] {
                write!(w, "    ")?;
                write!(w, "expr{}", i)?;
                write!(w, " -> ")?;
                b.write_node_name(&mut w)?;
                writeln!(w, ";")?;
            }
        }
        for i in 0..graph.decl_dependees.len() {
            let a = DeclId::new(i);
            for &b in &graph.decl_dependees[a] {
                write!(w, "    ")?;
                write!(w, "decl{}", i)?;
                write!(w, " -> ")?;
                b.write_node_name(&mut w)?;
                writeln!(w, ";")?;
            }
        }
        for i in 0..self.hir.exprs.len() {
            let id = ExprId::new(i as usize);
            let range = self.hir.source_ranges[id].clone();
            if range.start != range.end {
                writeln!(
                    w,
                    "    expr{} [label=\"{}\\l\"];",
                    i,
                    // TODO: do something more efficient than calling replace multiple times
                    self.file.substring_from_range(range)
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\"")
                        .replace("\n", "\\n")
                        .replace("\r", ""),
                )?;
            }
        }
        writeln!(w, "}}")?;
        w.flush()?;

        let command = Command::new("sfdp")
            .args(&["-Goverlap=scale", "-Tpdf", "tmp/tc_graph.gv", "-o", "tmp/tc_graph.pdf"])
            .stdout(Stdio::inherit())
            .output()
            .expect("Failed to execute graph rendering command");

        let graph_pdf_path: PathBuf = [".", "tmp", "tc_graph.pdf"].iter().collect();
        webbrowser::open(graph_pdf_path.to_str().unwrap()).unwrap();

        Ok(())
    }
}