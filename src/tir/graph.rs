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

use crate::builder::ItemId;
use crate::index_vec::{Idx, IdxVec};
use crate::driver::Driver;
use crate::source_info::SourceRange;
use crate::hir;

newtype_index!(CompId);
newtype_index!(UnitId pub);

#[derive(Debug, Default)]
pub struct Graph {
    dependees: IdxVec<Vec<ItemId>, ItemId>,
    t2_dependees: IdxVec<Vec<ItemId>, ItemId>,
    t3_dependees: IdxVec<Vec<ItemId>, ItemId>,
    t4_dependees: IdxVec<Vec<ItemId>, ItemId>,
    meta_dependees: IdxVec<Vec<ItemId>, ItemId>,
    meta_dependers: IdxVec<Vec<ItemId>, ItemId>,

    // Used exclusively for finding connected components
    dependers: IdxVec<Vec<ItemId>, ItemId>,

    item_to_components: IdxVec<CompId, ItemId>,

    components: IdxVec<Component, CompId>,
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


#[derive(Debug, Default)]
struct Component {
    items: Vec<ItemId>,

    deps: HashMap<CompId, ComponentRelation>,

    has_meta_dependees: bool,
}

struct ComponentState {
    // TODO: Vec of bools == gross
    visited: IdxVec<bool, ItemId>,
    cur_component: Component,
}

#[derive(Debug, Default)]
struct Unit {
    components: Vec<CompId>,

    deps: HashMap<ItemId, ItemId>,
}

impl Graph {
    /// a and b must be in the *same* unit, and a must have a higher level than b
    pub fn add_type1_dep(&mut self, a: ItemId, b: ItemId) {
        // TODO: remove this HACK to prevent type 1 dependencies on the void expression
        if b.idx() == 0 { return; }
        
        self.dependees[a].push(b);
        self.dependers[b].push(a);
    }

    /// a must either be in the same unit as b or a later unit, but if they are in the same unit, a must have a higher level than b
    pub fn add_type2_dep(&mut self, a: ItemId, b: ItemId) {
        self.t2_dependees[a].push(b);
        let a_comp = self.item_to_components[a];
        self.transfer_item_dep_to_component(a_comp, b, ComponentRelation::TYPE_2_3_FORWARD, ComponentRelation::TYPE_2_3_BACKWARD);
    }

    /// a must either be in the same unit as b or a later unit, but their levels are independent
    pub fn add_type3_dep(&mut self, a: ItemId, b: ItemId) {
        self.t3_dependees[a].push(b);
        let a_comp = self.item_to_components[a];
        self.transfer_item_dep_to_component(a_comp, b, ComponentRelation::TYPE_2_3_FORWARD, ComponentRelation::TYPE_2_3_BACKWARD);
    }

    /// a must be in an later unit than b, but their levels are independent
    pub fn add_type4_dep(&mut self, a: ItemId, b: ItemId) {
        self.t4_dependees[a].push(b);
        let a_comp = self.item_to_components[a];
        self.transfer_item_dep_to_component(a_comp, b, ComponentRelation::BEFORE, ComponentRelation::AFTER);
    }

    /// in order to know the type 2-4 dependencies of a, we need to know all possible members of b
    pub fn add_meta_dep(&mut self, a: ItemId, b: ItemId) {
        let comp = self.item_to_components[a];
        self.components[comp].has_meta_dependees = true;
        self.meta_dependees[a].push(b);
        self.meta_dependers[b].push(a);
    }

    fn find_subcomponent(&mut self, item: ItemId, state: &mut ComponentState) {
        state.visited[item] = true;
        state.cur_component.items.push(item);
        self.item_to_components[item] = CompId::new(self.components.len());
        macro_rules! find_subcomponents {
            ($item_array:ident) => {{
                for i in 0..self.$item_array[item].len() {
                    let adj = self.$item_array[item][i];
                    if !state.visited[adj] { self.find_subcomponent(adj, state); }
                }
            }}
        }
        find_subcomponents!(dependees);
        find_subcomponents!(dependers);
    }

    fn find_component(&mut self, item: ItemId, state: &mut ComponentState) {
        if state.visited[item] { return; }
        self.find_subcomponent(item, state);
        let new_component = mem::replace(&mut state.cur_component, Component::default());
        self.components.push(new_component);
    }

    fn transfer_item_dep_to_component(
        &mut self,
        comp: CompId,
        dependee: ItemId,
        forward_mask: ComponentRelation,
        backward_mask: ComponentRelation,
    ) {
        let dependee = self.item_to_components[dependee];
        *self.components[comp].deps.entry(dependee).or_default() &= forward_mask;
        *self.components[dependee].deps.entry(comp).or_default() &= backward_mask;
    }

    // Find the weak components of the graph
    pub fn split(&mut self) {
        let mut visited = IdxVec::new();
        visited.resize_with(self.dependees.len(), || false);
        let mut state = ComponentState {
            visited, cur_component: Component::default(),
        };
        self.item_to_components.resize_with(self.dependees.len(), || CompId::new(usize::MAX));
        assert!(self.components.is_empty());
        for i in 0..self.dependees.len() {
            self.find_component(ItemId::new(i), &mut state);
        }
    }

    pub fn find_units(&mut self) {
        let mut units = IdxVec::<Unit, UnitId>::new();
        let mut included_components = HashSet::<CompId>::new();
        let mut outstanding_components = HashSet::<CompId>::from_iter(
            (0..self.components.len())
                .map(|i| CompId::new(i))
        );
        let mut excluded_components = HashSet::<CompId>::new();

        // Exclude all components with meta-dependencies
        outstanding_components.retain(|&id| 
            if self.components[id].has_meta_dependees {
                excluded_components.insert(id);
                false
            } else {
                true
            }
        );

        while !outstanding_components.is_empty() {
            // Get all components that have no type 4 dependencies on outstanding components
            let mut cur_unit_comps = HashSet::<CompId>::new();
            for &comp_id in &outstanding_components {
                let mut should_add = true;
                for (&dependee, &relation) in &self.components[comp_id].deps {
                    if relation == ComponentRelation::BEFORE && !included_components.contains(&dependee) {
                        should_add = false;

                        // If the current component depends on an excluded component,
                        // it should itself be excluded
                        if excluded_components.contains(&dependee) {
                            excluded_components.insert(comp_id);
                        }
                    }
                }
                if should_add {
                    cur_unit_comps.insert(comp_id);
                }
            }
            
            // Whittle down the components to only those that have type 2 or 3 dependencies on each other, or included components
            // (and not other outstanding components)
            loop {
                // Yay borrow checker
                let cur_unit_copy = cur_unit_comps.clone();
                cur_unit_comps.retain(|&comp| {
                    let mut should_retain = true;
                    for (&dependee, &relation) in &self.components[comp].deps {
                        if relation == ComponentRelation::TYPE_2_3_FORWARD && !cur_unit_copy.contains(&dependee) && !included_components.contains(&dependee) {
                            should_retain = false;

                            // If the current component depends on an excluded component,
                            // it should itself be excluded
                            if excluded_components.contains(&dependee) {
                                excluded_components.insert(comp);
                            }
                        }
                    }
                    should_retain
                });

                // If we didn't remove anything this iteration, we're done
                if cur_unit_copy.len() == cur_unit_comps.len() { break; }
            }

            let mut cur_unit = Unit::default();
            cur_unit.components.extend(cur_unit_comps.iter());
            for &comp in &cur_unit.components {
                // Add intra-unit type 2 dependencies as type 1 dependencies, because they are equivalent after resolving units
                let component = &self.components[comp];
                for &item in &component.items {
                    for &dep in &self.t2_dependees[item] {
                        if cur_unit_comps.contains(&self.item_to_components[dep]) {
                            self.dependees[item].push(dep);
                        }
                    }
                }
                included_components.insert(comp);
                outstanding_components.remove(&comp);
            }
            outstanding_components.retain(|id| !excluded_components.contains(id));
            units.push(cur_unit);
        }

        println!("excluded components: {:#?}", excluded_components);

        self.units = Some(units);
    }

    fn find_level(&self, item: ItemId, levels: &mut IdxVec<u32, ItemId>) -> u32 {
        if levels[item] != u32::MAX { return levels[item]; }

        let mut max_level = 0;
        let mut offset = 0;
        for &dep in &self.dependees[item] {
            let level = self.find_level(dep, levels);
            max_level = max(max_level, level);
            offset = 1;
        }
        let level = max_level + offset;
        levels[item] = level;
        level
    }

    pub fn solve(self) -> Levels {
        let mut item_to_levels = IdxVec::<u32, ItemId>::new();
        item_to_levels.resize_with(self.dependees.len(), || u32::MAX);

        for i in 0..self.dependees.len() {
            self.find_level(ItemId::new(i), &mut item_to_levels);
        }

        let mut item_to_units = IdxVec::<UnitId, ItemId>::new();
        item_to_units.resize_with(self.dependees.len(), || UnitId::new(usize::MAX));

        let units = self.units.unwrap();
        for (i, unit) in units.iter().enumerate() {
            for &comp in &unit.components {
                let components = &self.components[comp];
                for &item in &components.items {
                    item_to_units[item] = UnitId::new(i);
                }
            }
        }

        let components = self.components;

        Levels {
            item_to_levels,
            item_to_units,
            units: IdxVec::from(
                units.into_iter()
                    .map(|unit| {
                        unit.components.into_iter()
                            .flat_map(|comp| components[comp].items.iter().map(|comp| *comp))
                            .collect()
                    }).collect::<Vec<Vec<ItemId>>>()
            ),
            dependees: self.dependees,
        }
    }
}

#[derive(Default, Debug)]
pub struct Levels {
    pub item_to_levels: IdxVec<u32, ItemId>,
    pub item_to_units: IdxVec<UnitId, ItemId>,
    pub units: IdxVec<Vec<ItemId>, UnitId>,
    pub dependees: IdxVec<Vec<ItemId>, ItemId>,
}

struct SplitOp {
    included: HashSet<ItemId>,
    excluded: Vec<ItemId>,
}

impl SplitOp {
    fn split_recurse(&mut self, item_to_units: &mut IdxVec<UnitId, ItemId>, dependees: &IdxVec<Vec<ItemId>, ItemId>, split: ItemId) {
        let removed = self.included.remove(&split);
        assert!(removed);
        item_to_units[split] = UnitId::new(usize::MAX);
        self.excluded.push(split);
        for &dep in &dependees[split] {
            self.split_recurse(item_to_units, dependees, dep);
        }
    }
}

impl Levels {
    /// Recursively get the dependencies of `splits`, remove them from `unit`, and return them (including `splits`)
    pub fn split_unit(&mut self, unit: UnitId, splits: &[ItemId]) -> Vec<ItemId> {
        let mut split_op = SplitOp {
            included: HashSet::from_iter(self.units[unit].iter().map(|unit| *unit)),
            excluded: Vec::from_iter(splits.iter().map(|split| *split))
        };
        for &split in splits {
            split_op.split_recurse(&mut self.item_to_units, &self.dependees, split);
        }

        self.units[unit] = Vec::from_iter(split_op.included.into_iter());
        split_op.excluded
    }
}

impl ItemId {
    fn write_node_name(self, w: &mut impl Write, hir: &hir::Builder) -> IoResult<()> {
        match hir.items[self] {
            hir::Item::Expr(id) => write!(w, "expr{}", id.idx())?,
            hir::Item::Decl(id) => write!(w, "decl{}", id.idx())?,
        }
        Ok(())
    }

    fn write_debug(self, w: &mut impl Write, hir: &hir::Builder) -> IoResult<()> {
        match hir.items[self] {
            hir::Item::Expr(id) => write!(w, "{:?}", hir.exprs[id])?,
            hir::Item::Decl(id) => write!(w, "{:?}", hir.decls[id])?,
        }
        Ok(())
    }
}

impl Driver {
    pub fn initialize_graph(&mut self) {
        let mut deps = [
            &mut self.tir.graph.dependees,
            &mut self.tir.graph.t2_dependees,
            &mut self.tir.graph.t3_dependees,
            &mut self.tir.graph.t4_dependees,
            &mut self.tir.graph.meta_dependees,
            &mut self.tir.graph.meta_dependers,
            &mut self.tir.graph.dependers
        ];
        for dep in &mut deps {
            dep.resize_with(self.hir.items.len(), || Vec::new());
        }

        self.tir.graph.item_to_components.resize_with(self.hir.items.len(), || CompId::new(usize::MAX));
    }

    fn write_dep<W: Write>(&self, w: &mut W, a: ItemId, b: ItemId, write_attribs: impl FnOnce(&mut W) -> IoResult<()>) -> IoResult<()> {
        write!(w, "    ")?;
        a.write_node_name(w, &self.hir)?;
        write!(w, " -> ")?;
        b.write_node_name(w, &self.hir)?;
        write_attribs(w)?;
        writeln!(w, ";")?;
        Ok(())
    }

    fn write_deps(&self, w: &mut impl Write, a: ItemId, graph: &Graph) -> IoResult<()> {
        for &b in &graph.dependees[a] {
            self.write_dep(w, a, b, |_| Ok(()))?;
        }
        Ok(())
    }

    fn write_item(&self, w: &mut impl Write, item: ItemId) -> IoResult<()> {
        let range = self.hir.source_ranges[item].clone();
        write!(w, "    ")?;
        item.write_node_name(w, &self.hir)?;
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
        for &item in &component.items {
            self.write_item(w, item)?;
        }
        writeln!(w, "    }}")?;

        Ok(())
    }

    fn write_component_deps(&self, w: &mut impl Write, graph: &Graph, unit: usize, i: usize, component: &Component) -> IoResult<()> {
        for &item in &component.items {
            self.write_deps(w, item, graph)?;
        }

        Ok(())
    }

    /// Prints graph in Graphviz format, then opens a web browser to display the results.
    pub fn print_graph(&self) -> IoResult<()> {
        let graph = &self.tir.graph;
        let tmp_dir = fs::read_dir(".")?.find(|entry| entry.as_ref().unwrap().file_name() == "tmp");
        if tmp_dir.is_none() {
            fs::create_dir("tmp")?;
        }
        let mut w = File::create("tmp/tc_graph.gv")?;
        writeln!(w, "digraph G {{")?;
        writeln!(w, "    node [shape=box];")?;

        if let Some(units) = &graph.units {
            for (i, unit) in units.iter().enumerate() {
                writeln!(w, "    subgraph cluster{} {{", i)?;
                writeln!(w, "        label=\"unit {}\";", i)?;
                writeln!(w, "        style=filled;")?;
                writeln!(w, "        color=blue;")?;
                for &comp_id in &unit.components {
                    self.write_component(&mut w, graph, i, comp_id.idx(), &graph.components[comp_id])?;
                }
                // This is done in a separate loop because 
                for &comp_id in &unit.components {
                    self.write_component_deps(&mut w, graph, i, comp_id.idx(), &graph.components[comp_id])?;
                }
                writeln!(w, "    }}")?;
            }
        } else if !graph.components.is_empty() {
            for (i, component) in graph.components.iter().enumerate() {
                self.write_component(&mut w, graph, 0, i, component)?;
                self.write_component_deps(&mut w, graph, 0, i, component)?;
            }
        } else {
            for i in 0..graph.dependees.len() {
                let a = ItemId::new(i);
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