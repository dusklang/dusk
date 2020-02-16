use std::convert::From;
use std::io::{Read, Write, Result as IoResult};
use std::process::{Command, Stdio};
use std::fs::{self, File};
use std::path::PathBuf;

use crate::builder::{ExprId, DeclId};
use crate::index_vec::Idx;
use crate::driver::Driver;

#[derive(Debug, Copy, Clone)]
pub enum ItemId {
    Expr(ExprId),
    Decl(DeclId),
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
    edges: Vec<(ItemId, ItemId)>,
}

impl Graph {
    pub fn new() -> Self {
        Graph {
            edges: Vec::new(),
        }
    }

    pub fn add_edge(&mut self, a: impl Into<ItemId>, b: impl Into<ItemId>) {
        self.edges.push((a.into(), b.into()));
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
    // Prints graph in Graphviz format, then opens a web browser to display the results.
    pub fn print_graph(&self, graph: &Graph) -> IoResult<()> {
        let tmp_dir = fs::read_dir(".")?.find(|entry| entry.as_ref().unwrap().file_name() == "tmp");
        if tmp_dir.is_none() {
            fs::create_dir("tmp")?;
        }
        let mut w = File::create("tmp/tc_graph.gv")?;
        writeln!(w, "digraph G {{")?;

        for (a, b) in &graph.edges {
            write!(w, "    ")?;
            a.write_node_name(&mut w)?;
            write!(w, " -> ")?;
            b.write_node_name(&mut w)?;
            writeln!(w, ";")?;
        }
        for i in 0..self.hir.exprs.len() {
            let id = ExprId::new(i as usize);
            let range = self.hir.source_ranges[id].clone();
            if range.start != range.end {
                writeln!(
                    w,
                    "    expr{} [label=\"{}\"];",
                    // TODO: do something more efficient than calling replace multiple times
                    i,
                    self.file.substring_from_range(range)
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\"")
                        .replace("\n", "\\n")
                        .replace("\r", "\\r"),
                )?;
            }
        }
        writeln!(w, "}}")?;
        w.flush()?;

        let command = Command::new("sfdp")
            .args(&["-Tpdf", "tmp/tc_graph.gv", "-o", "tmp/tc_graph.pdf"])
            .stdout(Stdio::inherit())
            .output()
            .expect("Failed to execute graph rendering command");

        let graph_pdf_path: PathBuf = [".", "tmp", "tc_graph.pdf"].iter().collect();
        webbrowser::open(graph_pdf_path.to_str().unwrap()).unwrap();

        Ok(())
    }
}