use crate::target::Arch;
use crate::driver::Driver;
use crate::mir::FuncId;
use crate::linker::exe::Exe;
use crate::linker::dex::{CodeItem, AccessFlags};
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
        exe.add_type("Z");
        exe.add_type("C");
        let my_class = exe.add_class_def("Lcom/example/MyClass;", AccessFlags::PUBLIC, None, None);
        let my_class_id = exe.class_defs[my_class].class_idx;
        let _other_class = exe.add_class_def("Lcom/example/MyClass2;", AccessFlags::PUBLIC, Some(my_class_id), None);
        let first_method_code_item = CodeItem {
            num_registers: 0,
            num_words_of_ins: 0,
            num_words_of_outs: 0,
            num_try_items: 0,
            debug_info_off: 0,
            insns: Vec::new(),
        };
        exe.add_virtual_method(my_class, "firstMethod", "V", &["Lcom/example/MyClass;"], AccessFlags::PUBLIC, Some(first_method_code_item));
        exe.add_method(my_class_id, "secondMethod", "V", &["Z", "B", "S", "C", "I", "J", "F", "D", "Lcom/example/MyClass;", "[Lcom/example/MyClass;"]);

        let string_class = exe.add_type("Ljava/lang/String;");
        exe.add_method(string_class, "charAt", "C", &["Ljava/lang/String;"]);

        // TODO: generate an actual dex method (or multiple) here.
    }
}
