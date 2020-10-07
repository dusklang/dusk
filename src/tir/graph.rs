use std::io::{Write, Result as IoResult};
use std::process::{Command, Stdio};
use std::fs::{self, File};
use std::path::PathBuf;
use std::mem;
use std::iter::FromIterator;
use std::collections::{HashMap, HashSet};
use std::cmp::max;

use bitflags::bitflags;

use crate::builder::ItemId;
use crate::index_vec::{Idx, IdxVec};
use crate::driver::Driver;
use crate::hir;
use crate::TirGraphOutput;

newtype_index!(CompId);

#[derive(Debug, Default)]
pub struct Graph {
    dependees: IdxVec<ItemId, Vec<ItemId>>,
    t2_dependees: IdxVec<ItemId, Vec<ItemId>>,
    t3_dependees: IdxVec<ItemId, Vec<ItemId>>,
    t4_dependees: IdxVec<ItemId, Vec<ItemId>>,
    meta_dependees: HashMap<ItemId, Vec<ItemId>>,
    global_meta_dependees: HashSet<ItemId>,

    // Used exclusively for finding connected components
    dependers: IdxVec<ItemId, Vec<ItemId>>,

    item_to_components: IdxVec<ItemId, CompId>,

    components: IdxVec<CompId, Component>,

    /// Components that have not yet been added to a unit, or a mock unit
    outstanding_components: HashSet<CompId>,

    /// Components whose items have been added to one or more mock units, but who have not yet
    /// been added to a regular unit in their entirety
    staged_components: HashMap<CompId, CompStageState>,

    /// Components that have been added to a unit
    included_components: HashSet<CompId>,
}

#[derive(Debug)]
struct CompStageState {
    // Consider the component below. `other_file`, `std`, and `fs` can all be output as mock units
    // at the same time, and typechecked concurrently. `str` must be output from a later call to
    // solve(), because it depends on `std`. Then, we can finally put this component into a real
    // unit.
    //
    //      other_file.foo(std.str.concat("Hello, ", "world!"), fs.read("HAASDASKJFD.txt"))
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

    has_meta_dep: bool,
}

struct ComponentState {
    // TODO: Vec of bools == gross
    visited: IdxVec<ItemId, bool>,
    cur_component: Component,
}

#[derive(Debug, Default)]
struct InternalUnit {
    components: HashSet<CompId>,

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
    ///
    /// NOTE: An item meta-depending on another does not imply anything about their relative units
    ///       or levels. For this reason, I'm pretty sure that a meta-dependency should always
    ///       be paired with a type 1-4 dependency. But maybe there are exceptions I haven't
    ///       thought about.
    pub fn add_meta_dep(&mut self, a: ItemId, b: ItemId) {
        self.meta_dependees.entry(a).or_default().push(b);
        self.global_meta_dependees.insert(b);
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

    fn find_level(&self, item: ItemId, levels: &mut IdxVec<ItemId, u32>) -> u32 {
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

        self.outstanding_components = HashSet::<CompId>::from_iter(
            (0..self.components.len())
                .map(|i| CompId::new(i))
        );
    }

    fn update_meta_deps(&mut self) {
        for &comp in &self.outstanding_components {
            let has_meta_dep = self.components[comp].items.iter()
                .any(|item| self.global_meta_dependees.contains(item));
            self.components[comp].has_meta_dep = has_meta_dep;
        }
    }

    pub fn solve(&mut self) -> Levels {
        self.update_meta_deps();

        let mut excluded_components = HashSet::<CompId>::new();
        for &id in &self.outstanding_components {
            if self.components[id].has_meta_dep {
                excluded_components.insert(id);
            }
        }

        println!("{:#?}\nThere are {} total components and {} excluded components", excluded_components, self.components.len(), excluded_components.len());

        let mut units = Vec::<InternalUnit>::new();
        while !self.outstanding_components.is_empty() {
            // Get all components that have no type 4 dependencies on outstanding components
            let mut cur_unit_comps = HashSet::<CompId>::new();
            for &comp_id in &self.outstanding_components {
                let mut should_add = true;
                for (&dependee, &relation) in &self.components[comp_id].deps {
                    if relation == ComponentRelation::BEFORE && !self.included_components.contains(&dependee) {
                        should_add = false;
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
                        if
                            relation == ComponentRelation::TYPE_2_3_FORWARD &&
                            !cur_unit_copy.contains(&dependee) &&
                            !self.included_components.contains(&dependee)
                        {
                            should_retain = false;
                        }
                    }
                    should_retain
                });

                // If we didn't remove anything this iteration, we're done
                if cur_unit_copy.len() == cur_unit_comps.len() { break; }
            }

            let mut cur_unit = InternalUnit::default();
            cur_unit.components.extend(cur_unit_comps.iter());
            for &comp in &cur_unit.components {
                // Add intra-unit type 2 dependencies as type 1 dependencies, because they are equivalent after resolving units
                let component = &self.components[comp];
                for &item in &component.items {
                    for &dep in &self.t2_dependees[item] {
                        if cur_unit_comps.contains(&self.item_to_components[dep]) {
                            self.dependees[item].push(dep);

                            // NOTE: Adding to `dependers` here isn't required for ordinary operation
                            // of the compiler, but it is used by the graph output code to decide
                            // which items to cull.
                            self.dependers[dep].push(item);
                        }
                    }
                }
                self.included_components.insert(comp);
                self.outstanding_components.remove(&comp);
            }
            units.push(cur_unit);
        }

        let mut item_to_levels = IdxVec::<ItemId, u32>::new();
        item_to_levels.resize_with(self.dependees.len(), || u32::MAX);

        for i in 0..self.dependees.len() {
            self.find_level(ItemId::new(i), &mut item_to_levels);
        }

        let mut item_to_units = IdxVec::<ItemId, u32>::new();
        item_to_units.resize_with(self.dependees.len(), || u32::MAX);

        for (i, unit) in units.iter().enumerate() {
            for &comp in &unit.components {
                let components = &self.components[comp];
                for &item in &components.items {
                    item_to_units[item] = i as u32;
                }
            }
        }

        let components = &self.components;

        Levels {
            item_to_levels: item_to_levels.clone(),
            item_to_units,
            units: units.into_iter()
                .map(|unit| {
                    let mut meta_deps = HashMap::<u32, Vec<MetaDependee>>::new();
                    let items = unit.components.iter()
                        .flat_map(|&comp| components[comp].items.iter().map(|&item| item))
                        .collect();
                    for &item in &items {
                        if self.global_meta_dependees.contains(&item) {
                            let mut deps = HashSet::new();
                            self.get_deps(item, &mut deps, &unit.components);
                            meta_deps.entry(item_to_levels[item])
                                .or_default()
                                .push(MetaDependee { item, deps: deps.into_iter().collect() });
                        }
                    }
                    let mut meta_dependees = meta_deps.into_iter()
                        .map(|(level, meta_dependees)| LevelMetaDependees { level, meta_dependees })
                        .collect::<Vec<_>>();
                    meta_dependees.sort_by_key(|level| level.level);
                    Unit { 
                        items,
                        meta_dependees,
                    }
                }).collect::<Vec<Unit>>(),
        }
    }

    fn get_deps(&self, item: ItemId, out: &mut HashSet<ItemId>, components: &HashSet<CompId>) {
        for &dependee in &self.dependees[item] {
            out.insert(dependee);
            self.get_deps(dependee, out, components);
        }
        for &dependee in &self.t3_dependees[item] {
            if out.contains(&dependee) { continue; }
            let comp = self.item_to_components[dependee];
            // If the dependee is in the same unit as the depender:
            if components.contains(&comp) {
                out.insert(dependee);
                self.get_deps(dependee, out, components);
            }
        }
    }
}

#[derive(Debug)]
pub struct MetaDependee {
    pub item: ItemId,
    pub deps: Vec<ItemId>,
}

#[derive(Default, Debug)]
pub struct LevelMetaDependees {
    pub level: u32,
    pub meta_dependees: Vec<MetaDependee>,
}

#[derive(Default, Debug)]
pub struct Unit {
    pub items: Vec<ItemId>,
    pub meta_dependees: Vec<LevelMetaDependees>,
}

#[derive(Default, Debug)]
pub struct Levels {
    pub item_to_levels: IdxVec<ItemId, u32>,
    pub item_to_units: IdxVec<ItemId, u32>,
    pub units: Vec<Unit>,
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

    fn write_deps(&self, w: &mut impl Write, a: ItemId, graph: &Graph, constraint: bool) -> IoResult<()> {
        for &b in &graph.dependees[a] {
            self.write_dep(
                w,
                a,
                b,
                |w| {
                    if !constraint {
                        write!(w, " [constraint=false]")?
                    }
                    Ok(())
                })?;
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
                self.src_map.substring_from_range(range)
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
    
    fn write_component(&self, w: &mut impl Write, unit: usize, i: usize, component: &Component) -> IoResult<()> {
        writeln!(w, "    subgraph cluster{}_{} {{", unit, i)?;
        writeln!(w, "        label=\"component {}\";", i)?;
        writeln!(w, "        style=filled;")?;
        writeln!(w, "        color=lightgrey;")?;
        writeln!(w, "        node [style=filled,color=white];")?;
        for &item in &component.items {
            if self.should_exclude_item_from_output(item) { continue; }
            self.write_item(w, item)?;
        }
        writeln!(w, "    }}")?;

        Ok(())
    }

    fn write_component_deps(&self, w: &mut impl Write, graph: &Graph, component: &Component) -> IoResult<()> {
        for &item in &component.items {
            if self.should_exclude_item_from_output(item) { continue; }
            self.write_deps(w, item, graph, true)?;
        }

        Ok(())
    }

    fn should_exclude_item_from_output(&self, item: ItemId) -> bool {
        let decl = match self.hir.items[item] {
            hir::Item::Decl(decl) => decl,
            hir::Item::Expr(_) => return false,
        };
        let is_intrinsic = matches!(self.hir.decls[decl], hir::Decl::Intrinsic { .. });
        let is_not_depended_on = self.tir.graph.dependers[item].is_empty();
        is_intrinsic && is_not_depended_on
    }

    /// Prints graph in Graphviz format, then opens a web browser to display the results.
    pub fn print_graph(&self, output: TirGraphOutput, levels: &Levels) -> IoResult<()> {
        let graph = &self.tir.graph;
        let tmp_dir = fs::read_dir(".")?.find(|entry| entry.as_ref().unwrap().file_name() == "tmp");
        if tmp_dir.is_none() {
            fs::create_dir("tmp")?;
        }
        let mut w = File::create("tmp/tc_graph.gv")?;
        writeln!(w, "digraph G {{")?;
        writeln!(w, "    node [shape=box];")?;

        match output {
            TirGraphOutput::Items => {
                for i in 0..graph.dependees.len() {
                    let a = ItemId::new(i);
                    if self.should_exclude_item_from_output(a) { continue; }

                    self.write_item(&mut w, a)?;
                    self.write_deps(&mut w, a, graph, true)?;
                }
            }
            TirGraphOutput::Components => {
                for (i, component) in graph.components.iter().enumerate() {
                    self.write_component(&mut w, 0, i, component)?;
                    self.write_component_deps(&mut w, graph, component)?;
                }
            }
            TirGraphOutput::Units => {
                writeln!(w, "    splines=ortho;")?;
                writeln!(w, "    newrank=true;")?;
                writeln!(w, "    rankdir = BT;")?;
                let item_to_levels = &levels.item_to_levels;
                let max_level = levels.units.iter()
                    .flat_map(|unit| &unit.items)
                    .map(|&item| item_to_levels[item])
                    .max()
                    .unwrap_or(0);

                writeln!(w, "    subgraph cluster_levels {{")?;
                writeln!(w, "        label=\"Levels\";")?;
                writeln!(w, "        style=filled;")?;
                writeln!(w, "        color=lightgrey;")?;
                writeln!(w, "        node [style=filled,color=white];")?;
                write!(w, "    ")?;
                for i in 0..=max_level {
                    if i != 0 {
                        write!(w, " -> ")?;
                    }
                    write!(w, "level{}", i)?;
                }
                writeln!(w, " [style=invis];")?;
                writeln!(w, "    }}")?;

                for (i, unit) in levels.units.iter().enumerate() {
                    writeln!(w, "    subgraph cluster_unit{} {{", i)?;
                    writeln!(w, "        label=\"Unit {}\";", i)?;
                    writeln!(w, "        style=filled;")?;
                    writeln!(w, "        color=lightgrey;")?;
                    writeln!(w, "        node [style=filled,color=white];")?;
                    for &item in &unit.items {
                        if self.should_exclude_item_from_output(item) { continue; }

                        let level = levels.item_to_levels[item];
                        self.write_item(&mut w, item)?;
                        self.write_deps(&mut w, item, graph, false)?;

                        // Constrain items to be in the correct level
                        if level < max_level {
                            write!(w, "        ")?;
                            item.write_node_name(&mut w, &self.hir)?;
                            writeln!(w, " -> level{} [style=invis];", level + 1)?;
                        }
                        if level > 0 {
                            write!(w, "        level{} -> ", level - 1)?;
                            item.write_node_name(&mut w, &self.hir)?;
                            writeln!(w, " [style=invis];")?;
                        }
                    }
                    writeln!(w, "    }}")?;
                }
            }
        }
        writeln!(w, "}}")?;
        w.flush()?;

        Command::new("dot")
            .args(&["-Tsvg", "tmp/tc_graph.gv", "-o", "tmp/tc_graph.svg"])
            .stdout(Stdio::inherit())
            .output()
            .expect("Failed to execute graph rendering command");

        let graph_pdf_path: PathBuf = [".", "tmp", "tc_graph.svg"].iter().collect();
        webbrowser::open(graph_pdf_path.to_str().unwrap()).unwrap();

        Ok(())
    }
}