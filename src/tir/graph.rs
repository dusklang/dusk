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

pub trait Item {
    fn adjacent(self, graph: &mut Graph) -> &mut Vec<ItemId>;
}

impl Item for ExprId {
    fn adjacent(self, graph: &mut Graph) -> &mut Vec<ItemId> {
        &mut graph.expr_adjacent[self]
    }
}

impl Item for DeclId {
    fn adjacent(self, graph: &mut Graph) -> &mut Vec<ItemId> {
        &mut graph.decl_adjacent[self]
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
    // TODO: consider putting ExprIds and DeclIds into separate arrays, or one sorted one, rather than all together in an enum
    expr_adjacent: IdxVec<Vec<ItemId>, ExprId>,
    decl_adjacent: IdxVec<Vec<ItemId>, DeclId>,
}

impl Graph {
    pub fn add_type1_dep(&mut self, a: impl Item, b: impl Into<ItemId>) {
        let b = b.into();
        // TODO: remove this HACK to prevent type 1 dependencies on the void expression
        if let ItemId::Expr(expr) = b {
            if expr.idx() == 0 { return; }
        }
        a.adjacent(self).push(b);
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
        let mut expr_adjacent = IdxVec::new();
        expr_adjacent.resize_with(self.hir.exprs.len(), || Vec::new());
        let mut decl_adjacent = IdxVec::new();
        decl_adjacent.resize_with(self.hir.decls.len(), || Vec::new());

        Graph { expr_adjacent, decl_adjacent }
    }

    // Prints graph in Graphviz format, then opens a web browser to display the results.
    pub fn print_graph(&self, graph: &Graph) -> IoResult<()> {
        let tmp_dir = fs::read_dir(".")?.find(|entry| entry.as_ref().unwrap().file_name() == "tmp");
        if tmp_dir.is_none() {
            fs::create_dir("tmp")?;
        }
        let mut w = File::create("tmp/tc_graph.gv")?;
        writeln!(w, "digraph G {{")?;

        for i in 0..graph.expr_adjacent.len() {
            let a = ExprId::new(i);
            for &b in &graph.expr_adjacent[a] {
                write!(w, "    ")?;
                write!(w, "expr{}", i)?;
                write!(w, " -> ")?;
                b.write_node_name(&mut w)?;
                writeln!(w, ";")?;
            }
        }
        for i in 0..graph.decl_adjacent.len() {
            let a = DeclId::new(i);
            for &b in &graph.decl_adjacent[a] {
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