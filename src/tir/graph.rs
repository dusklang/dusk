use std::convert::From;
use std::io::{Read, Write, Result as IoResult};
use std::process::{Command, Stdio};
use std::fs::{self, File};
use std::path::PathBuf;
use std::iter::Chain;
use std::slice::Iter as SliceIter;
use std::mem;
use std::ops::{Index, IndexMut, Range};
use std::iter::FromIterator;
use std::collections::{HashMap, HashSet};
use std::cmp::max;

use bitflags::bitflags;

use crate::builder::{ExprId, DeclId};
use crate::index_vec::{Idx, IdxVec};
use crate::driver::Driver;
use crate::source_info::SourceRange;
use crate::hir;

newtype_index!(CompId);
newtype_index!(UnitId);

#[derive(Debug, Copy, Clone)]
pub enum ItemId {
    Expr(ExprId),
    Decl(DeclId),
}

pub trait Item: Copy + Into<ItemId> {
    fn elem<T>(self, vec: &ItemIdxVec<T>) -> &T;
    fn elem_mut<T>(self, vec: &mut ItemIdxVec<T>) -> &mut T;
    fn source_range(self, hir: &hir::Builder) -> &SourceRange;
    fn write_debug(self, w: &mut impl Write, hir: &hir::Builder) -> IoResult<()>;
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

    fn source_range(self, hir: &hir::Builder) -> &SourceRange {
        &hir.expr_source_ranges[self]
    }

    fn write_debug(self, w: &mut impl Write, hir: &hir::Builder) -> IoResult<()> {
        write!(w, "{:?}", hir.exprs[self])
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

    fn source_range(self, hir: &hir::Builder) -> &SourceRange {
        &hir.decl_source_ranges[self]
    }

    fn write_debug(self, w: &mut impl Write, hir: &hir::Builder) -> IoResult<()> {
        write!(w, "{:?}", hir.decls[self])
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
    units: Option<IdxVec<Unit, UnitId>>,
}

bitflags! {
    struct ComponentRelation: u8 {
        const BEFORE  = 1 << 0;
        const SAME    = 1 << 1;
        const AFTER   = 1 << 2;

        // The idea here: we bitwise & these relations together to determine what relationship each component should have with others.
        // If two mutually exclusive relations get AND'd together and it returns 0, there must be a direct cycle.
        const CYCLE = 0;

        // At the component level, the difference between type 2 and 3 dependencies doesn't matter
        const TYPE_2_3_FORWARD  = Self::BEFORE.bits | Self::SAME.bits;
        const TYPE_2_3_BACKWARD = Self::SAME.bits   | Self::AFTER.bits;

        const ALL = Self::BEFORE.bits | Self::SAME.bits | Self::AFTER.bits;
    }
}

impl Default for ComponentRelation {
    fn default() -> Self { ComponentRelation::ALL }
}


#[derive(Default)]
struct Component {
    exprs: Vec<ExprId>,
    decls: Vec<DeclId>,

    deps: HashMap<CompId, ComponentRelation>,
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

#[derive(Default)]
struct Unit {
    components: Vec<CompId>,

    expr_deps: HashMap<ExprId, ItemId>,
    decl_deps: HashMap<DeclId, ItemId>,
}

impl Graph {
    /// a and b must be in the *same* unit, and a must have a higher level than b
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

    /// a must either be in the same unit as b or a later unit, but if they are in the same unit, a must have a higher level than b
    pub fn add_type2_dep(&mut self, a: impl Item, b: impl Item) {
        self.t2_dependees[a].push(b.into());
    }

    /// a must either be in the same unit as b or a later unit, but their levels are independent
    pub fn add_type3_dep(&mut self, a: impl Item, b: impl Item) {
        self.t3_dependees[a].push(b.into());
    }

    /// a must be in an later unit than b, but their levels are independent
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

    fn transfer_item_dep_to_component(
        &self,
        comp: CompId,
        dependee: ItemId,
        forward_mask: ComponentRelation,
        backward_mask: ComponentRelation,
        state: &mut ComponentState
    ) {
        let dependee = match dependee {
            ItemId::Decl(decl) => state.item_to_components[decl],
            ItemId::Expr(expr) => state.item_to_components[expr],
        };
        *state.components[comp].deps.entry(dependee).or_default() &= forward_mask;
        *state.components[dependee].deps.entry(comp).or_default() &= backward_mask;
    }

    fn transfer_item_deps_to_component(&self, comp: CompId, item: impl Item, state: &mut ComponentState) {
        let t2_and_t3_dependendees = self.t2_dependees[item].iter().chain(self.t3_dependees[item].iter());
        for &dependee in t2_and_t3_dependendees {
            self.transfer_item_dep_to_component(comp, dependee, ComponentRelation::TYPE_2_3_FORWARD, ComponentRelation::TYPE_2_3_BACKWARD, state);
        }
        for &dependee in &self.t4_dependees[item] {
            self.transfer_item_dep_to_component(comp, dependee, ComponentRelation::BEFORE, ComponentRelation::AFTER, state);
        }
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

        for i in 0..state.components.len() {
            let comp = CompId::new(i);
            for j in 0..state.components[comp].exprs.len() {
                self.transfer_item_deps_to_component(comp, state.components[comp].exprs[j], &mut state);
            }
            for j in 0..state.components[comp].decls.len() {
                self.transfer_item_deps_to_component(comp, state.components[comp].decls[j], &mut state);
            }
        }

        self.components = Some(state.components);
        self.item_to_components = state.item_to_components;
    }

    pub fn find_units(&mut self) {
        let components = self.components.as_ref().unwrap();
        let mut component_to_units = IdxVec::<UnitId, CompId>::new();
        component_to_units.resize_with(components.len(), || UnitId::new(std::usize::MAX));
        let mut units = IdxVec::<Unit, UnitId>::new();
        let mut included_components = HashSet::<CompId>::new();
        let mut outstanding_components = HashSet::<CompId>::from_iter(
            (0..components.len())
                .map(|i| CompId::new(i))
        );

        while !outstanding_components.is_empty() {
            // Get all components that have no type 4 dependencies on outstanding components
            let mut cur_unit_comps = HashSet::<CompId>::new();
            for &comp in &outstanding_components {
                let mut should_add = true;
                for (&dependee, &relation) in &components[comp].deps {
                    if relation == ComponentRelation::BEFORE && !included_components.contains(&dependee) {
                        should_add = false;
                        break;
                    }
                }
                if should_add {
                    cur_unit_comps.insert(comp);
                }
            }
            
            // Whittle down the components to only those that have type 2 or 3 dependencies on each other, or included components
            // (and not other outstanding components)
            loop {
                // Yay borrow checker
                let cur_unit_copy = cur_unit_comps.clone();
                cur_unit_comps.retain(|&comp| {
                    for (&dependee, &relation) in &components[comp].deps {
                        if relation == ComponentRelation::TYPE_2_3_FORWARD && !cur_unit_copy.contains(&dependee) && !included_components.contains(&dependee) {
                            return false;
                        }
                    }
                    true
                });

                // If we didn't remove anything this iteration, we're done
                if cur_unit_copy.len() == cur_unit_comps.len() { break; }
            }

            let mut cur_unit = Unit::default();
            cur_unit.components.extend(cur_unit_comps.iter());
            for &comp in &cur_unit.components {
                // Add intra-unit type 2 dependencies as type 1 dependencies, because they are equivalent after resolving units
                let component = &components[comp];
                for &expr in &component.exprs {
                    for &dep in &self.t2_dependees[expr] {
                        let component = match dep {
                            ItemId::Expr(dep) => self.item_to_components[dep],
                            ItemId::Decl(dep) => self.item_to_components[dep],
                        };
                        if cur_unit_comps.contains(&component) {
                            self.dependees[expr].push(dep);
                        }
                    }
                }
                for &decl in &component.decls {
                    for &dep in &self.t2_dependees[decl] {
                        let component = match dep {
                            ItemId::Expr(dep) => self.item_to_components[dep],
                            ItemId::Decl(dep) => self.item_to_components[dep],
                        };
                        if cur_unit_comps.contains(&component) {
                            self.dependees[decl].push(dep);
                        }
                    }
                }
                included_components.insert(comp);
                outstanding_components.remove(&comp);
            }
            units.push(cur_unit);
        }

        self.units = Some(units);
    }

    fn find_level(&self, item: impl Item, levels: &mut ItemIdxVec<u32>) -> u32 {
        if levels[item] != std::u32::MAX { return levels[item]; }

        let mut max_level = 0;
        let mut offset = 0;
        for &dep in &self.dependees[item] {
            let level = match dep {
                ItemId::Expr(dep) => self.find_level(dep, levels),
                ItemId::Decl(dep) => self.find_level(dep, levels),
            };
            max_level = max(max_level, level);
            offset = 1;
        }
        let level = max_level + offset;
        levels[item] = level;
        level
    }

    pub fn solve(self) -> Levels {
        let mut levels = ItemIdxVec::<u32>::new();
        levels.resize_with(self.dependees.expr_len(), self.dependees.decl_len(), || std::u32::MAX);

        for i in 0..self.dependees.expr_len() {
            let expr = ExprId::new(i);
            self.find_level(expr, &mut levels);
        }
        for i in 0..self.dependees.decl_len() {
            let decl = DeclId::new(i);
            self.find_level(decl, &mut levels);
        }

        let mut item_to_units = ItemIdxVec::<u32>::new();
        item_to_units.resize_with(self.dependees.expr_len(), self.dependees.decl_len(), || std::u32::MAX);

        let units = self.units.unwrap();
        for (i, unit) in units.iter().enumerate() {
            for &comp in &unit.components {
                let components = &self.components.as_ref().unwrap()[comp];
                for &expr in &components.exprs {
                    item_to_units[expr] = i as u32;
                }
                for &decl in &components.decls {
                    item_to_units[decl] = i as u32;
                }
            }
        }

        Levels { expr_levels: levels.expr, decl_levels: levels.decl, num_units: units.len() as u32, expr_units: item_to_units.expr, decl_units: item_to_units.decl }
    }
}

#[derive(Debug)]
pub struct Levels {
    pub expr_levels: IdxVec<u32, ExprId>,
    pub decl_levels: IdxVec<u32, DeclId>,
    
    pub num_units: u32,

    pub expr_units: IdxVec<u32, ExprId>,
    pub decl_units: IdxVec<u32, DeclId>,
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
        Graph { dependees, t2_dependees, t3_dependees, t4_dependees, dependers, item_to_components, components: None, units: None }
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

    fn write_item(&self, w: &mut impl Write, item: impl Item) -> IoResult<()> {
        let range = item.source_range(&self.hir).clone();
        write!(w, "    ")?;
        item.write_node_name(w)?;
        if range.start != range.end {
            writeln!(
                w,
                " [label=\"{}\\l\"];",
                // TODO: do something more efficient than calling replace multiple times
                self.file.substring_from_range(range)
                    .replace("\\", "\\\\")
                    .replace("\"", "\\\"")
                    .replace("\n", "\\n")
                    .replace("\r", ""),
            )?;
        } else {
            write!(w, " [label=\"")?;
            item.write_debug(w, &self.hir)?;
            writeln!(w, "\"];")?;
        }
        Ok(())
    }
    
    fn write_component(&self, w: &mut impl Write, graph: &Graph, unit: usize, i: usize, component: &Component) -> IoResult<()> {
        writeln!(w, "    subgraph cluster{}_{} {{", unit, i)?;
        writeln!(w, "        label=\"component {}\";", i)?;
        writeln!(w, "        style=filled;")?;
        writeln!(w, "        color=lightgrey;")?;
        writeln!(w, "        node [style=filled,color=white];")?;
        for &expr in &component.exprs {
            self.write_item(w, expr)?;
        }
        for &decl in &component.decls {
            self.write_item(w, decl)?;
        }
        writeln!(w, "    }}")?;

        Ok(())
    }

    fn write_component_deps(&self, w: &mut impl Write, graph: &Graph, unit: usize, i: usize, component: &Component) -> IoResult<()> {
        for &expr in &component.exprs {
            self.write_deps(w, expr, graph)?;
        }
        for &decl in &component.decls {
            self.write_deps(w, decl, graph)?;
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

        if let Some(units) = &graph.units {
            let components = graph.components.as_ref().unwrap();
            for (i, unit) in units.iter().enumerate() {
                writeln!(w, "    subgraph cluster{} {{", i)?;
                writeln!(w, "        label=\"unit {}\";", i)?;
                writeln!(w, "        style=filled;")?;
                writeln!(w, "        color=blue;")?;
                for &comp_id in &unit.components {
                    self.write_component(&mut w, graph, i, comp_id.idx(), &components[comp_id])?;
                }
                // This is done in a separate loop because 
                for &comp_id in &unit.components {
                    self.write_component_deps(&mut w, graph, i, comp_id.idx(), &components[comp_id])?;
                }
                writeln!(w, "    }}")?;
            }
        } else if let Some(components) = &graph.components {
            for (i, component) in components.iter().enumerate() {
                self.write_component(&mut w, graph, 0, i, component)?;
                self.write_component_deps(&mut w, graph, 0, i, component)?;
            }
        } else {
            for i in 0..graph.dependees.expr_len() {
                let a = ExprId::new(i);
                self.write_item(&mut w, a)?;
                self.write_deps(&mut w, a, graph)?;
            }
            for i in 0..graph.dependees.decl_len() {
                let a = DeclId::new(i);
                self.write_item(&mut w, a)?;
                self.write_deps(&mut w, a, graph)?;
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