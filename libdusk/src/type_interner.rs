use std::ops::Index;

use crate::index_vec::ConcurrentIndexVec;
use crate::ty::{Type, TypeId};

#[derive(Default)]
pub struct TypeInterner {
    types: ConcurrentIndexVec<TypeId, Type>,
    type_map: papaya::HashMap<Type, TypeId>,
}

impl Index<TypeId> for TypeInterner {
    type Output = Type;

    fn index(&self, index: TypeId) -> &Self::Output {
        &self.types[index]
    }
}

impl TypeInterner {
    pub fn new() -> TypeInterner { Default::default() }

    pub fn intern(&self, ty: Type) -> TypeId {
        *self.type_map.pin()
            .get_or_insert_with(ty.clone(), || self.types.push(ty))
    }
}
