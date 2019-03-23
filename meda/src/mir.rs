#[derive(Debug)]
pub enum IntWidth {
    W8, W16, W32, W64,
}

#[derive(Debug)]
pub enum FloatWidth {
    W32, W64,
}

#[derive(Debug)]
pub enum TypeKind {
    Error,
    IntVar,
    FloatVar,
    Int {
        width: IntWidth,
        is_signed: bool,
    },
    Float(FloatWidth),
    Bool,
    Void,
}

pub type TypeVarID = usize;

#[derive(Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub id: Option<TypeVarID>,
}

pub struct TypeVar {
    pub locations: Vec<usize>,
    pub ty: Type,
}

impl TypeVar {
    pub fn new(location: usize, ty: Type) -> Self {
        Self {
            locations: vec![location],
            ty,
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self {
            kind: TypeKind::Error,
            id: None,
        }
    }
}
