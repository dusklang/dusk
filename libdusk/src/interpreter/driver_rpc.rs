use string_interner::symbol::SymbolU32;

use crate::{driver::Driver, mir::{FuncId, Function}};

pub struct DriverRpc<'driver>(&'driver Driver);

impl<'driver> DriverRpc<'driver> {
    pub fn new(driver: &'driver Driver) -> Self {
        Self(driver)
    }

    pub fn copy_mir_function(&self, func: FuncId) -> Function {
        self.0.mir.functions[func].get().unwrap().clone()
    }

    pub fn get_interned_ident(&self, ident: SymbolU32) -> String {
        let interner = self.0.interner.read().unwrap();
        interner.resolve(ident).unwrap().to_string()
    }
}
