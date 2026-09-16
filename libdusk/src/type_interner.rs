use std::{collections::HashMap, ops::Index};

use index_vec::IndexVec;

use crate::ty::{Type, TypeId};

#[derive(Default)]
pub struct TypeInterner {
    types: IndexVec<TypeId, Type>,
    type_map: HashMap<Type, TypeId>,
}

impl Index<TypeId> for TypeInterner {
    type Output = Type;

    fn index(&self, index: TypeId) -> &Self::Output {
        &self.types[index]
    }
}

impl TypeInterner {
    pub fn new() -> TypeInterner { Default::default() }

    pub fn intern(&mut self, ty: Type) -> TypeId {
        *self.type_map.entry(ty.clone()).or_insert_with(|| {
            self.types.push(ty)
        })
    }
}
