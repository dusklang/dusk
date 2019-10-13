use crate::mir;

pub struct Driver<'src> {
    mir: mir::Builder<'src>,
}