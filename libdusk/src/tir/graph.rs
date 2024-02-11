use std::mem;
use std::collections::{HashMap, HashSet};
use std::cmp::max;

use bitflags::bitflags;

use crate::ast::{ItemId, VOID_EXPR_ITEM};
use crate::tir::CompId;
use crate::dvd::{Message as DvdMessage, self};

use crate::index_vec::*;
use crate::driver::Driver;
use crate::new_code::NewCode;

use crate::tir::TirError;

#[derive(Debug, Default)]
pub struct Graph {
    dependees: IndexVec<ItemId, Vec<ItemId>>,
    t2_dependees: IndexVec<ItemId, Vec<ItemId>>,

    // Whether each item has been visited (for the purpose of splitting into connected components)
    // TODO: Vec of bools == gross
    visited: IndexVec<ItemId, bool>,

    /// Set of all meta-dependees that are not yet ready to be added to a normal unit
    global_meta_dependees: HashSet<ItemId>,

    // Note: all suhmm dependees must be in exactly one of these three places: `suhmm_dependees`,
    // `typechecked_suhmm_dependees`, or `state.TypecheckingSuhmmDependees.current_dependees` below

    /// Set of all super ultra hyper mega meta dependees that have not yet been typechecked.
    suhmm_dependees: HashSet<ItemId>,

    /// Set of all super ultra hyper mega meta dependees that have been typechecked.
    typechecked_suhmm_dependees: HashSet<ItemId>,

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

    graph_state: GraphState,

    component_state: ComponentState,

    saved_component_state: Option<ComponentState>,
}

/// A command passed along to the typechecker, indicating what to do with current typechecking state.
#[derive(Debug)]
pub enum MockStateCommand {
    /// Save the mock type provider results with the real type provider, and destroy the mock type provider.
    Save,
    /// Discard the mock type provider results, and destroy the mock type provider
    Discard,
    /// Create a new mock type provider wrapping the base-level "real" type provider
    Mock,
}

#[derive(Debug, Default)]
enum GraphState {
    #[default]
    Initial,
    TypecheckingSuhmmDependees(SuhmmState),
    TypecheckingRestOfProgram,
}

#[derive(Debug, Default)]
struct SuhmmState {
    current_dependees: Vec<ItemId>,
    dependees_to_remove: Vec<usize>,
    current_index: usize,
    num_succeeded: usize,
}

impl SuhmmState {
    fn next_dep(&mut self, succeeded: bool) -> bool {
        self.num_succeeded += succeeded as usize;
        if self.current_index + 1 >= self.current_dependees.len() {
            if self.num_succeeded == 0 || self.num_succeeded == self.current_dependees.len() {
                return false;
            } else {
                self.current_index = 0;
                self.num_succeeded = 0;
                self.dependees_to_remove.sort();
                for &index in self.dependees_to_remove.iter().rev() {
                    self.current_dependees.remove(index);
                }
                self.dependees_to_remove.clear();
                if self.current_dependees.is_empty() {
                    return false;
                }
            }
        } else {
            self.current_index += 1;
        }
        true
    }

    fn remove_current_dep(&mut self) {
        self.dependees_to_remove.push(self.current_index);
    }
}

#[derive(Clone, Debug, Default)]
struct ComponentState {
    // note: every component in the `components` field of should always be in exactly one of the following places:

    /// Components that have not yet been added to a unit, or a mock unit
    outstanding_components: HashSet<CompId>,

    /// Components whose items have been added to one or more mock units, but who have not yet
    /// been added to a regular unit in their entirety
    staged_components: HashMap<CompId, CompStageState>,

    /// Components that have been added to a unit
    included_components: HashSet<CompId>,
}

#[derive(Debug, Clone)]
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
}

impl Driver {
    pub fn initialize_graph(&mut self) {
        let mut deps = [
            &mut self.tir.graph.dependees,
            &mut self.tir.graph.t2_dependees,
            &mut self.tir.graph.dependers
        ];
        for dep in &mut deps {
            dep.resize_with(self.code.ast.items.len(), Vec::new);
        }

        self.tir.graph.item_to_components.resize_with(self.code.ast.items.len(), || CompId::new(u32::MAX as usize));
    }
}

impl Graph {
    /// a and b must be in the *same* unit, and a must have a higher level than b
    pub fn add_type1_dep(&mut self, a: ItemId, b: ItemId) {
        // TODO: maybe remove this hack to prevent type 1 dependencies on the void expression
        if b == VOID_EXPR_ITEM { return; }
        
        self.dependees[a].push(b);
        self.dependers[b].push(a);

        dvd::send(|| DvdMessage::DidAddTirType1Dependency { depender: a, dependee: b });
    }

    /// a must either be in the same unit as b or a later unit, but if they are in the same unit, a must have a higher level than b
    pub fn add_type2_dep(&mut self, a: ItemId, b: ItemId) {
        self.t2_dependees[a].push(b);
        let a_comp = self.item_to_components[a];
        self.transfer_item_dep_to_component(a_comp, b, ComponentRelation::TYPE_2_3_FORWARD, ComponentRelation::TYPE_2_3_BACKWARD);

        dvd::send(|| DvdMessage::DidAddTirType2Dependency { depender: a, dependee: b });
    }

    /// a must either be in the same unit as b or a later unit, but their levels are independent
    pub fn add_type3_dep(&mut self, a: ItemId, b: ItemId) {
        let a_comp = self.item_to_components[a];
        self.transfer_item_dep_to_component(a_comp, b, ComponentRelation::TYPE_2_3_FORWARD, ComponentRelation::TYPE_2_3_BACKWARD);

        dvd::send(|| DvdMessage::DidAddTirType3Dependency { depender: a, dependee: b });
    }

    /// a must be in an later unit than b, but their levels are independent
    pub fn add_type4_dep(&mut self, a: ItemId, b: ItemId) {
        let a_comp = self.item_to_components[a];
        self.transfer_item_dep_to_component(a_comp, b, ComponentRelation::BEFORE, ComponentRelation::AFTER);

        dvd::send(|| DvdMessage::DidAddTirType4Dependency { depender: a, dependee: b });
    }

    /// in order to add the type 2-4 dependencies of a, we need to be able to mock-typecheck, and
    /// possibly even mock-evaluate b.
    ///
    /// NOTE: An item meta-depending on another does not imply anything about their relative units
    ///       or levels. For this reason, I'm pretty sure that a meta-dependency should always
    ///       be paired with a type 1-4 dependency. But maybe there are exceptions I haven't
    ///       thought about.
    pub fn add_meta_dep(&mut self, a: ItemId, b: ItemId) {
        self.meta_dependees.entry(a).or_default().insert(b);
        self.meta_dependers.entry(b).or_default().push(a);
        self.global_meta_dependees.insert(b);

        dvd::send(|| DvdMessage::DidAddTirMetaDependency { depender: a, dependee: b });
    }

    /// in order to add the type 2-4 dependencies of just about anything, we need to be able to
    /// typecheck, and possibly even evaluate b. I say "just about" because b might have its own
    /// dependencies of types 1-4 or meta. In such cases, there is no option but to
    /// typecheck/evaluate those dependees first. Furthermore, SUHMM deps (for short) added at the
    /// same time are allowed to be typechecked independently of one another.
    pub fn add_super_ultra_hyper_mega_meta_dep(&mut self, b: ItemId) {
        self.suhmm_dependees.insert(b);
    }

    fn find_subcomponent(&mut self, item: ItemId, cur_component: &mut Component) {
        self.visited[item] = true;
        cur_component.items.push(item);
        let component = self.components.next_idx();
        self.item_to_components[item] = component;
        dvd::send(|| DvdMessage::DidAddItemToTirComponent { component, item });
        macro_rules! find_subcomponents {
            ($item_array:ident) => {{
                for i in 0..self.$item_array[item].len() {
                    let adj = self.$item_array[item][i];
                    if !self.visited[adj] { self.find_subcomponent(adj, cur_component); }
                }
            }}
        }
        find_subcomponents!(dependees);
        find_subcomponents!(dependers);
    }

    fn find_component(&mut self, item: ItemId, cur_component: &mut Component) {
        if self.visited[item] { return; }
        self.find_subcomponent(item, cur_component);
        let new_component = mem::take(cur_component);
        let component = self.components.push(new_component);
        dvd::send(|| DvdMessage::DidFinishTirComponent(component));
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
    pub fn split(&mut self, new_code: &NewCode) {
        let before_components = self.components.next_idx();
        self.visited.resize_with(self.dependees.len(), || false);
        let mut cur_component = Component::default();
        self.item_to_components.resize_with(self.dependees.len(), || CompId::new(u32::MAX as usize));
        for id in range_iter(new_code.items.clone()) {
            self.find_component(id, &mut cur_component);
        }

        let after_components = self.components.next_idx();
        self.component_state.outstanding_components.extend(
            range_iter(before_components..after_components)
        );
    }

    pub fn has_outstanding_components(&self) -> bool { !self.component_state.outstanding_components.is_empty() }

    fn find_comps_without_outstanding_type_4_deps(&self, comps: &HashSet<CompId>, output: &mut HashSet<CompId>) {
        'comps: for &comp_id in comps {
            for (&dependee, &relation) in &self.components[comp_id].deps {
                if relation == ComponentRelation::BEFORE && !self.component_state.included_components.contains(&dependee) {
                    // Found a type 4 dependency, so we wont add this component. Continue to the next component.
                    continue 'comps;
                }
            }
            output.insert(comp_id);
        }
    }

    fn remove_comps_with_outstanding_deps(&self, comps: &mut HashSet<CompId>) {
        loop {
            // Yay borrow checker
            let comps_copy = comps.clone();
            comps.retain(|&comp| {
                for (&dependee, &relation) in &self.components[comp].deps {
                    let in_current_unit = comps_copy.contains(&dependee);
                    if
                        relation == ComponentRelation::TYPE_2_3_FORWARD &&
                        !in_current_unit &&
                        !self.component_state.included_components.contains(&dependee)
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
            let old_val = self.component_state.staged_components.insert(comp, state);
            assert!(old_val.is_none());
        }
    }

    fn item_has_meta_dependees(&self, item: ItemId) -> bool {
        self.meta_dependees.get(&item)
            .map(|dependees| !dependees.is_empty())
            .unwrap_or(false)
    }

    pub fn get_items_that_need_dependencies(&mut self) -> Vec<ItemId> {
        let items: Vec<ItemId> = self.component_state.outstanding_components.iter()
            .flat_map(|&comp| &self.components[comp].items)
            .copied()
            .filter(|item| !self.dependencies_added.contains(item))
            .filter(|&item| !self.item_has_meta_dependees(item))
            .collect();

        self.dependencies_added.extend(&items);

        items
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
        }

        for i in 0..self.dependees[item].len() {
            let item = self.dependees[item][i];
            self.remove_meta_dep_status(item);
        }
    }

    fn save_component_state(&mut self) {
        self.saved_component_state = Some(self.component_state.clone());
    }

    fn restore_component_state(&mut self) {
        if let Some(state) = mem::take(&mut self.saved_component_state) {
            self.component_state = state;
        }
    }

    fn check_for_new_suhmm_dependees(&mut self) -> Vec<MockStateCommand> {
        if !self.suhmm_dependees.is_empty() {
            let current_dependees = mem::take(&mut self.suhmm_dependees).into_iter().collect();
            let suhmm_state = SuhmmState {
                current_dependees,
                dependees_to_remove: Vec::new(),
                current_index: 0,
                num_succeeded: 0,
            };
            self.graph_state = GraphState::TypecheckingSuhmmDependees(suhmm_state);
            self.save_component_state();
            vec![MockStateCommand::Mock]
        } else {
            self.graph_state = GraphState::TypecheckingRestOfProgram;
            vec![]
        }
    }

    fn update_graph_state(&mut self, succeeded: bool) -> Vec<MockStateCommand> {
        match &mut self.graph_state {
            GraphState::Initial | GraphState::TypecheckingRestOfProgram => {
                self.check_for_new_suhmm_dependees()
            },
            GraphState::TypecheckingSuhmmDependees(suhmm_state) => {
                let current_dep = suhmm_state.current_dependees[suhmm_state.current_index];
                let current_dep_comp = self.item_to_components[current_dep];
                let finished_current_dep = self.component_state.included_components.contains(&current_dep_comp);
                let mut commands = Vec::new();
                if !succeeded || finished_current_dep {
                    if succeeded {
                        commands.push(MockStateCommand::Save);
                        suhmm_state.remove_current_dep();
                        self.typechecked_suhmm_dependees.insert(current_dep);
                    } else {
                        commands.push(MockStateCommand::Discard);
                    }
                    
                    if suhmm_state.next_dep(succeeded) {
                        commands.push(MockStateCommand::Mock);
                    } else {
                        self.typechecked_suhmm_dependees.extend(suhmm_state.current_dependees.clone());
                        commands.extend(self.check_for_new_suhmm_dependees());
                    }

                    if succeeded {
                        self.saved_component_state = None;
                    } else {
                        self.restore_component_state();
                        self.save_component_state();
                    }
                }
                commands
            },
        }
    }

    pub fn solve(&mut self, succeeded: bool) -> Result<Levels, TirError> {
        let commands = self.update_graph_state(succeeded);

        // If we are currently typechecking a SUHMM dependee, we should only consider outstanding components that are either directly or indirectly depended on by said SUHMM dependee.
        let mut outstanding_components = if let GraphState::TypecheckingSuhmmDependees(suhmm_state) = &self.graph_state {
            let current_dep = suhmm_state.current_dependees[suhmm_state.current_index];
            let current_dep_comp = self.item_to_components[current_dep];

            let mut component_stack = vec![current_dep_comp];
            let mut outstanding_components = HashSet::new();
            while let Some(comp) = component_stack.pop() {
                if outstanding_components.contains(&comp) || !self.component_state.outstanding_components.contains(&comp) {
                    continue;
                }
                outstanding_components.insert(comp);

                for (&dep, relation) in &self.components[comp].deps {
                    if relation.contains(ComponentRelation::BEFORE) {
                        if !outstanding_components.contains(&dep) {
                            component_stack.push(dep);
                        }
                    }
                }
            }
            outstanding_components
        } else {
            self.component_state.outstanding_components.clone()
        };

        dvd::send(|| {
            let mut outstanding_components: Vec<_> = outstanding_components.iter().copied().collect();
            outstanding_components.sort();
            DvdMessage::WillSolveTirGraph { outstanding_components }
        });

        // Get the outstanding components with meta-dependees in them
        let mut meta_dep_components = HashSet::<CompId>::new();
        for &comp in &outstanding_components {
            let has_meta_dep = self.components[comp].items.iter()
                .any(|item| self.global_meta_dependees.contains(item));
            if has_meta_dep {
                meta_dep_components.insert(comp);
            }
        }

        // Get the components that depend (either directly or indirectly) on components with meta-dependees in them 
        let excluded_components = {
            let mut excluded_components = HashSet::new();
            let mut potentially_excluded_components: HashSet<CompId> = outstanding_components
                .difference(&meta_dep_components)
                .copied()
                .filter(|comp| !self.component_state.staged_components.contains_key(comp))
                .collect();
            let mut added_to_excluded_set = true;
            while added_to_excluded_set {
                added_to_excluded_set = false;
                potentially_excluded_components.retain(|&comp_id| {
                    let comp = &self.components[comp_id];
                    for (dep, &relation) in &comp.deps {
                        if !relation.contains(ComponentRelation::AFTER) && (meta_dep_components.contains(dep) || self.component_state.staged_components.contains_key(dep) || excluded_components.contains(dep)) {
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

        dvd::send(|| {
            let mut excluded_components: Vec<_> = meta_dep_components.union(&excluded_components).copied().collect();
            excluded_components.sort();
            DvdMessage::DidExcludeTirComponentsFromSubprogram(excluded_components)
        });

        // Remove all excluded components from our local copy of `outstanding_components`.
        outstanding_components.retain(|comp| !meta_dep_components.contains(comp) && !excluded_components.contains(comp));
        if outstanding_components.is_empty() {
            return Err(TirError::DependencyCycle);
        }

        let mut units = Vec::<InternalUnit>::new();
        while !outstanding_components.is_empty() {
            // Get all components that have no type 4 dependencies on outstanding components
            let mut cur_unit_comps = HashSet::<CompId>::new();
            self.find_comps_without_outstanding_type_4_deps(&outstanding_components, &mut cur_unit_comps);
            // Whittle down the components to only those that have type 2 or 3 dependencies on each other, or included components
            // (and not other outstanding components)
            self.remove_comps_with_outstanding_deps(&mut cur_unit_comps);
            assert!(!cur_unit_comps.is_empty(), "no viable components to add to TIR graph :( {:?}", units);
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
                self.component_state.included_components.insert(comp);
                outstanding_components.remove(&comp);
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
            self.remove_comps_with_outstanding_deps(&mut comps_to_stage);

            self.stage_components(comps_to_stage.into_iter());
        }

        let mut item_to_levels = HashMap::<ItemId, u32>::new();

        for id in self.dependees.indices() {
            self.find_level(id, &mut item_to_levels, |_| true);
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
        let staged_comps_keys: Vec<_> = self.component_state.staged_components.keys().copied().collect();
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
                if !relation.contains(ComponentRelation::AFTER) && !self.component_state.included_components.contains(dep) {
                    continue 'mock_staged_comps;
                }
            }

            let meta_deps = self.component_state.staged_components.get_mut(&comp_id).unwrap().meta_deps.pop().unwrap();
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

            if self.component_state.staged_components[&comp_id].meta_deps.is_empty() {
                self.component_state.staged_components.remove(&comp_id);

                // Re-register the component for being added to a unit
                // Note that inserting to the real `outstanding_components` here instead of our local copy is intentional.
                // Otherwise, previously-staged components would not be added properly.
                self.component_state.outstanding_components.insert(comp_id);

                // Update state to reflect the fact that the component no longer has meta-dependees
                for i in 0..comp.items.len() {
                    let comp = &self.components[comp_id];
                    let item = comp.items[i];
                    self.remove_meta_dep_status(item);
                }
            }
        }

        // Update the real `outstanding_components`.
        self.component_state.outstanding_components.retain(|comp|
            !self.component_state.staged_components.contains_key(&comp) &&
            !self.component_state.included_components.contains(&comp)
        );

        dvd::send(|| DvdMessage::DidSolveTirGraph);

        let main_levels = Levels {
            item_to_levels,
            item_to_units,
            units: units.into_iter()
                .map(|unit| {
                    let items = unit.components.iter()
                        .flat_map(|&comp| self.components[comp].items.iter().copied())
                        .collect();
                    Unit { items }
                }).collect::<Vec<Unit>>(),
            mock_units,
            commands,
            is_suhmm: matches!(self.graph_state, GraphState::TypecheckingSuhmmDependees(_)),
        };

        Ok(
            main_levels
        )
    }
}

/// As the name implies, this representation of a unit is used internally in the graph
/// implementation, to keep track of the components in a unit. It is later converted to a normal
/// `Unit`.
#[derive(Debug, Default)]
struct InternalUnit {
    components: HashSet<CompId>,
}

#[derive(Default, Debug)]
pub struct Levels {
    pub item_to_levels: HashMap<ItemId, u32>,
    pub item_to_units: HashMap<ItemId, u32>,
    pub units: Vec<Unit>,
    pub mock_units: Vec<MockUnit>,
    pub commands: Vec<MockStateCommand>,
    pub is_suhmm: bool,
}

#[derive(Default, Debug)]
pub struct Unit {
    pub items: Vec<ItemId>,
}

#[derive(Debug,)]
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