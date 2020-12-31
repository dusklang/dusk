use std::io::{Write, Result as IoResult};
use std::process::{Command, Stdio};
use std::fs::{self, File};
use std::path::PathBuf;
use std::mem;
use std::iter::FromIterator;
use std::collections::{HashMap, HashSet};
use std::cmp::max;

use bitflags::bitflags;

use index_vec::define_index_type;
use mire::hir::{self, ItemId, VOID_EXPR_ITEM};

use crate::index_vec::*;
use crate::driver::Driver;
use crate::TirGraphOutput;

define_index_type!(struct CompId = u32;);

#[derive(Debug, Default)]
pub struct Graph {
    dependees: IndexVec<ItemId, Vec<ItemId>>,
    t2_dependees: IndexVec<ItemId, Vec<ItemId>>,
    t3_dependees: IndexVec<ItemId, Vec<ItemId>>,
    t4_dependees: IndexVec<ItemId, Vec<ItemId>>,

    /// Set of all meta-dependees that are not yet ready to be added to a normal unit
    global_meta_dependees: HashSet<ItemId>,

    /// Map from all meta-dependers to their dependees that are not yet ready to be added to a normal unit
    meta_dependees: HashMap<ItemId, HashSet<ItemId>>,

    /// Map from all meta-dependees to their dependers
    meta_dependers: HashMap<ItemId, Vec<ItemId>>,

    /// Set of all items to which type 2-4 dependencies have been added
    dependencies_added: HashSet<ItemId>,

    // Used exclusively for finding connected components
    dependers: IndexVec<ItemId, Vec<ItemId>>,

    item_to_components: IndexVec<ItemId, CompId>,

    components: IndexVec<CompId, Component>,

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
    /// Each element of this Vec is a Vec of meta-dependees that can form independent mock units with no dependencies on each other.
    /// Each call to `solve()` pops another Vec off the end of the outer Vec, and adds them as mock units to the next set. When the
    /// outer Vec is emptied, it means this component can be added to the next Unit proper.
    ///
    /// Example: consider the component below. The correct initial value of `meta_deps` would be [[std.str], [other_file, std, fs]]:
    ///     other_file.foo(std.str.concat("Hello, ", "world!"), fs.read("HAASDASKJFD.txt"))
    meta_deps: Vec<Vec<ItemId>>,
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
    visited: IndexVec<ItemId, bool>,
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
        // TODO: maybe remove this hack to prevent type 1 dependencies on the void expression
        if b == VOID_EXPR_ITEM { return; }
        
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
        self.meta_dependees.entry(a).or_default().insert(b);
        self.meta_dependers.entry(b).or_default().push(a);
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

    // The whole purpose of this method existing as opposed to just having `find_level` is so we can call `find_level` with an ordinary closure (not a reference), then
    // pass that same closure on recursively without moving it.
    fn find_level_recursive(&self, item: ItemId, levels: &mut HashMap<ItemId, u32>, filter: &mut impl FnMut(ItemId) -> bool) -> u32 {
        if let Some(&level) = levels.get(&item) { return level; }

        let mut max_level = 0;
        let mut offset = 0;
        for &dep in &self.dependees[item] {
            let level = self.find_level_recursive(dep, levels, filter);
            max_level = max(max_level, level);
            // TODO: Call filter at most once for each element
            if filter(dep) {
                offset = 1;
            }
        }
        if !filter(item) { offset = 0; }
        let level = max_level + offset;
        levels.insert(item, level);
        level
    }

    fn find_level(&self, item: ItemId, levels: &mut HashMap<ItemId, u32>, mut filter: impl Clone + Fn(ItemId) -> bool) -> u32 {
        self.find_level_recursive(item, levels, &mut filter)
    }

    // Find the weak components of the graph
    pub fn split(&mut self) {
        let mut visited = IndexVec::new();
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

    pub fn has_outstanding_components(&mut self) -> bool { !self.outstanding_components.is_empty() }

    fn update_meta_deps(&mut self) {
        for &comp in &self.outstanding_components {
            let has_meta_dep = self.components[comp].items.iter()
                .any(|item| self.global_meta_dependees.contains(item));
            self.components[comp].has_meta_dep = has_meta_dep;
        }
    }

    fn find_comps_without_outstanding_type_4_deps(&self, comps: &HashSet<CompId>, output: &mut HashSet<CompId>) {
        'comps: for &comp_id in comps {
            for (&dependee, &relation) in &self.components[comp_id].deps {
                if relation == ComponentRelation::BEFORE && !self.included_components.contains(&dependee) {
                    // Found a type 4 dependency, so we wont add this component. Continue to the next component.
                    continue 'comps;
                }
            }
            output.insert(comp_id);
        }
    }

    fn remove_comps_with_outstanding_deps(&self, comps: &mut HashSet<CompId>, should_check_current_unit: bool) {
        loop {
            // Yay borrow checker
            let comps_copy = comps.clone();
            comps.retain(|&comp| {
                for (&dependee, &relation) in &self.components[comp].deps {
                    let in_current_unit = if should_check_current_unit {
                        false
                    } else {
                        comps_copy.contains(&dependee)
                    };
                    if
                        relation == ComponentRelation::TYPE_2_3_FORWARD &&
                        !in_current_unit &&
                        !self.included_components.contains(&dependee)
                    {
                        return false;
                    }
                }
                true
            });

            // If we didn't remove anything this iteration, we're done
            if comps_copy.len() == comps.len() { break; }
        }
    }

    fn stage_components(&mut self, comps: impl Iterator<Item=CompId>) {
        let mut levels: HashMap<ItemId, u32> = HashMap::new();
        for comp in comps {
            let mut max_level = 0;
            let items = &self.components[comp].items;
            for &item in items {
                let level = self.find_level(item, &mut levels, |item| self.global_meta_dependees.contains(&item));
                max_level = max(max_level, level);
            }
    
            let mut meta_deps = Vec::<Vec<ItemId>>::new();
            meta_deps.resize_with(max_level as usize + 1, Default::default);
    
            for &item in items {
                if !self.global_meta_dependees.contains(&item) { continue; }
                // Invert the level so that lower levels can be popped off the end of the array
                let level = max_level - levels[&item];
                meta_deps[level as usize].push(item);
            }

            let state = CompStageState { meta_deps };
            let old_val = self.staged_components.insert(comp, state);
            assert!(old_val.is_none());
        }
    }

    pub fn get_items_that_need_dependencies(&mut self) -> Vec<ItemId> {
        let items: Vec<ItemId> = self.outstanding_components.iter()
            .flat_map(|&comp| &self.components[comp].items)
            .copied()
            .filter(|item| !self.dependencies_added.contains(item))
            .filter(|item| !self.meta_dependees.contains_key(item))
            .collect();

        self.dependencies_added.extend(&items);

        items
    }

    pub fn solve(&mut self) -> Levels {
        self.update_meta_deps();

        // Get the outstanding components with meta-dependees in them
        let mut meta_dep_components = HashSet::<CompId>::new();
        for &id in &self.outstanding_components {
            if self.components[id].has_meta_dep {
                meta_dep_components.insert(id);
            }
        }

        // Get the components that depend (either directly or indirectly) on components with meta-dependees in them 
        let excluded_components = {
            let mut excluded_components = HashSet::new();
            let mut potentially_excluded_components: HashSet<CompId> = self.outstanding_components
                .difference(&meta_dep_components)
                .copied()
                .filter(|comp| !self.staged_components.contains_key(comp))
                .collect();
            let mut added_to_excluded_set = true;
            while added_to_excluded_set {
                added_to_excluded_set = false;
                potentially_excluded_components.retain(|&comp_id| {
                    let comp = &self.components[comp_id];
                    for (dep, &relation) in &comp.deps {
                        if !relation.contains(ComponentRelation::AFTER) && (meta_dep_components.contains(dep) || self.staged_components.contains_key(dep) || excluded_components.contains(dep)) {
                            excluded_components.insert(comp_id);
                            added_to_excluded_set = true;
                            return false;
                        }
                    }
                    true
                });
            }

            excluded_components
        };

        // Temporarily remove all excluded components. Those that don't get staged will be added back after finding the units
        self.outstanding_components.retain(|comp| !meta_dep_components.contains(comp) && !excluded_components.contains(comp));

        let mut units = Vec::<InternalUnit>::new();
        while !self.outstanding_components.is_empty() {
            // Get all components that have no type 4 dependencies on outstanding components
            let mut cur_unit_comps = HashSet::<CompId>::new();
            self.find_comps_without_outstanding_type_4_deps(&self.outstanding_components, &mut cur_unit_comps);

            // Whittle down the components to only those that have type 2 or 3 dependencies on each other, or included components
            // (and not other outstanding components)
            self.remove_comps_with_outstanding_deps(&mut cur_unit_comps, true);

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

        // Stage viable components
        {
            // Get all meta-dep components that have no type 4 dependencies on outstanding components
            let mut comps_to_stage = HashSet::<CompId>::new();
            self.find_comps_without_outstanding_type_4_deps(&meta_dep_components, &mut comps_to_stage);

            // Whittle down the components to only those that have type 2 or 3 dependencies on included components
            // (and not other outstanding components, or each other)
            self.remove_comps_with_outstanding_deps(&mut comps_to_stage, false);

            // Add excluded components back to `outstanding_components`.
            let all_excluded = meta_dep_components
                .difference(&comps_to_stage)
                .chain(&excluded_components);
            self.outstanding_components.extend(all_excluded);

            self.stage_components(comps_to_stage.into_iter());
        }

        let mut item_to_levels = HashMap::<ItemId, u32>::new();

        for i in 0..self.dependees.len() {
            self.find_level(ItemId::new(i), &mut item_to_levels, |_| true);
        }

        let mut item_to_units = HashMap::<ItemId, u32>::new();

        for (i, unit) in units.iter().enumerate() {
            for &comp in &unit.components {
                let components = &self.components[comp];
                for &item in &components.items {
                    item_to_units.insert(item, i as u32);
                }
            }
        }

        let mut mock_units = Vec::new();
        // TODO: Borrow checker :(
        let staged_comps_keys: Vec<_> = self.staged_components.keys().copied().collect();
        'mock_staged_comps: for comp_id in staged_comps_keys {
            // Check dependencies of the component before adding it to a mock unit. This is necessary because:
            //   - Meta-dependers can add dependencies to themselves after a meta-dependee is mocked
            //     (in fact, that's the whole point of all this)
            //   - Meta-dependers can be in the same component as their meta-dependee
            //   - Within a component, a second meta-dependee might exist that depends on the meta-depender (indeed,
            //     the meta-depender itself might also be a meta-dependee). Therefore, we need to typecheck the
            //     meta-depender's dependencies before we can mock it
            let comp = &self.components[comp_id];
            for (dep, &relation) in &comp.deps {
                if !relation.contains(ComponentRelation::AFTER) && !self.included_components.contains(dep) {
                    continue 'mock_staged_comps;
                }
            }

            let meta_deps = self.staged_components.get_mut(&comp_id).unwrap().meta_deps.pop().unwrap();
            for dep in meta_deps {
                let mut item_to_levels: HashMap<ItemId, u32> = HashMap::new();

                // Note: This will end up finding the levels of all items in the mock unit, because
                // `dep` is the root of the tree, and there's only one component (therefore, all
                // dependencies will be found)
                let item_level = self.find_level(dep, &mut item_to_levels, |_| true);

                let mut deps = HashSet::new();
                self.get_deps(dep, &mut deps);

                let mock_unit = MockUnit {
                    item: dep,
                    item_level,
                    item_to_levels,

                    // TODO: Remove this conversion; just make a Vec from the get-go (but first,
                    //       find out if safe)
                    deps: deps.into_iter().collect(),
                };
                mock_units.push(mock_unit);
            }

            if self.staged_components[&comp_id].meta_deps.is_empty() {
                self.staged_components.remove(&comp_id);

                // Re-register the component for being added to a unit
                self.outstanding_components.insert(comp_id);

                // Update state to reflect the fact that the component no longer has meta-dependees
                for i in 0..comp.items.len() {
                    let comp = &self.components[comp_id];
                    let item = comp.items[i];
                    self.remove_meta_dep_status(item);
                }
            }
        }

        let components = &self.components;
        Levels {
            item_to_levels: item_to_levels.clone(),
            item_to_units,
            units: units.into_iter()
                .map(|unit| {
                    let items = unit.components.iter()
                        .flat_map(|&comp| components[comp].items.iter().copied())
                        .collect();
                    Unit { items }
                }).collect::<Vec<Unit>>(),
            mock_units,
        }
    }

    fn get_deps(&self, item: ItemId, out: &mut HashSet<ItemId>) {
        for &dependee in &self.dependees[item] {
            out.insert(dependee);
            self.get_deps(dependee, out);
        }
    }

    fn remove_meta_dep_status(&mut self, item: ItemId) {
        let was_removed = self.global_meta_dependees.remove(&item);
        // Short-circuit the recursive chain
        if !was_removed { return; }

        for depender in &self.meta_dependers[&item] {
            let dependees = self.meta_dependees.get_mut(depender).unwrap();
            dependees.remove(&item);
            if dependees.is_empty() {
                self.meta_dependees.remove(depender);
            }
        }

        for i in 0..self.dependees[item].len() {
            let item = self.dependees[item][i];
            self.remove_meta_dep_status(item);
        }
    }
}

#[derive(Default, Debug)]
pub struct Unit {
    pub items: Vec<ItemId>,
}

#[derive(Default, Debug)]
pub struct Levels {
    pub item_to_levels: HashMap<ItemId, u32>,
    pub item_to_units: HashMap<ItemId, u32>,
    pub units: Vec<Unit>,
    pub mock_units: Vec<MockUnit>,
}

#[derive(Debug)]
pub struct MockUnit {
    /// Main item of the mock unit; the meta-dependee
    pub item: ItemId,
    /// Level of the main item
    pub item_level: u32,

    /// The levels of each item within this mock unit.
    pub item_to_levels: HashMap<ItemId, u32>,

    /// A collection of every item in this mock unit
    pub deps: Vec<ItemId>,

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

    fn write_node_name(&self, item: ItemId, w: &mut impl Write) -> IoResult<()> {
        match self.hir.items[item] {
            hir::Item::Expr(id) => write!(w, "item{}expr{}", item.index(), id.index())?,
            hir::Item::Decl(id) => write!(w, "item{}decl{}", item.index(), id.index())?,
        }
        Ok(())
    }

    fn write_debug(&self, item: ItemId, w: &mut impl Write) -> IoResult<()> {
        match self.hir.items[item] {
            hir::Item::Expr(id) => write!(w, "{:?}", self.hir.exprs[id])?,
            hir::Item::Decl(id) => write!(w, "{:?}", self.hir.decls[id])?,
        }
        Ok(())
    }

    fn write_dep<W: Write>(&self, w: &mut W, a: ItemId, b: ItemId, write_attribs: impl FnOnce(&mut W) -> IoResult<()>) -> IoResult<()> {
        write!(w, "    ")?;
        self.write_node_name(a, w)?;
        write!(w, " -> ")?;
        self.write_node_name(b, w)?;
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
        self.write_node_name(item, w)?;
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
            self.write_debug(item, w)?;
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
                    .map(|&item| item_to_levels[&item])
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

                        let level = levels.item_to_levels[&item];
                        self.write_item(&mut w, item)?;
                        self.write_deps(&mut w, item, graph, false)?;

                        // Constrain items to be in the correct level
                        if level < max_level {
                            write!(w, "        ")?;
                            self.write_node_name(item, &mut w)?;
                            writeln!(w, " -> level{} [style=invis];", level + 1)?;
                        }
                        if level > 0 {
                            write!(w, "        level{} -> ", level - 1)?;
                            self.write_node_name(item, &mut w)?;
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