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

use super::TirError;

#[derive(Debug, Default)]
pub struct Graph {
    dependees: IndexVec<ItemId, Vec<ItemId>>,
    t2_dependees: IndexVec<ItemId, Vec<ItemId>>,

    // Whether each item has been visited (for the purpose of splitting into connected components)
    // TODO: Vec of bools == gross
    visited: IndexVec<ItemId, bool>,

    /// Set of all meta-dependees that are not yet ready to be added to a normal unit
    global_meta_dependees: HashSet<ItemId>,

    /// Set of all super ultra hyper mega meta dependencies that are not yet ready to be added to a normal unit.
    suhmm_dependees: HashSet<ItemId>,

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
        self.outstanding_components.extend(
            range_iter(before_components..after_components)
        );
    }

    pub fn has_outstanding_components(&self) -> bool { !self.outstanding_components.is_empty() }

    fn item_has_meta_dependees(&self, item: ItemId) -> bool {
        self.meta_dependees.get(&item)
            .map(|dependees| !dependees.is_empty())
            .unwrap_or(false)
    }

    pub fn get_items_that_need_dependencies(&mut self) -> Vec<ItemId> {
        let items: Vec<ItemId> = self.outstanding_components.iter()
            .flat_map(|&comp| &self.components[comp].items)
            .copied()
            .filter(|item| !self.dependencies_added.contains(item))
            .filter(|&item| !self.item_has_meta_dependees(item))
            .collect();

        self.dependencies_added.extend(&items);

        items
    }

    pub fn solve(&mut self) -> Result<Levels, TirError> {
        let item_to_levels = HashMap::new();
        let item_to_units = HashMap::new();
        let units = Vec::<InternalUnit>::new();
        let mock_units = Vec::new();
        let components = &self.components;

        // TODO: handle SUHMM dependencies

        // TODO: handle components that don't depend on the mock results of unmocked meta-dependencies

        // TODO: handle meta-dependencies, add mock units

        let main_levels = Levels {
            item_to_levels,
            item_to_units,
            units: units.into_iter()
                .map(|unit| {
                    let items = unit.components.iter()
                        .flat_map(|&comp| components[comp].items.iter().copied())
                        .collect();
                    Unit { items }
                }).collect::<Vec<Unit>>(),
            mock_units,
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