use crate::target::Arch;
use crate::driver::Driver;
use crate::mir::FuncId;
use crate::linker::exe::Exe;
use crate::linker::dex::{AccessFlags, CodeItem, DexReturnType, DexType};
use crate::backend::Backend;

pub struct DexBackend;

impl Backend for DexBackend {
    fn arch(&self) -> Arch {
        Arch::Dex
    }

    fn generate_func(&self, d: &Driver, func_index: FuncId, is_main: bool, exe: &mut dyn Exe) {
        let exe = exe.as_dex_exe().expect("dex backend only supports writing code into dex files");

        exe.add_string("");
        exe.add_string("Hi");
        let my_class = exe.add_class_def("com.example.MyClass", AccessFlags::PUBLIC, None, None);
        let my_class_id = exe.class_defs[my_class].class_idx;
        let _other_class = exe.add_class_def("com.example.MyClass2", AccessFlags::PUBLIC, Some(my_class_id), None);
        let first_method_code_item = CodeItem {
            num_registers: 0,
            num_words_of_ins: 0,
            num_words_of_outs: 0,
            num_try_items: 0,
            debug_info_off: 0,
            insns: Vec::new(),
        };
        exe.add_virtual_method(my_class, "firstMethod", DexReturnType::Void, &[DexType::Class("com.example.MyClass").into()], AccessFlags::PUBLIC, Some(first_method_code_item));

        let string_class = exe.add_type(DexType::Class("java.lang.String"));
        exe.add_method(string_class, "charAt", DexType::Char, &[DexType::Class("java.lang.String").into()]);

        // TODO: generate an actual dex method (or multiple) here.
    }
}
