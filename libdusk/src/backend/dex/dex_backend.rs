use crate::target::Arch;
use crate::driver::Driver;
use crate::mir::FuncId;
use crate::linker::exe::Exe;
use crate::linker::dex::{AccessFlags, DexReturnType, DexType};
use crate::backend::dex::DexEncoder;
use crate::backend::Backend;

pub struct DexBackend;

impl Backend for DexBackend {
    fn arch(&self) -> Arch {
        Arch::Dex
    }

    fn generate_func(&self, d: &Driver, func_index: FuncId, is_main: bool, exe: &mut dyn Exe) {
        let exe = exe.as_dex_exe().expect("dex backend only supports writing code into dex files");

        let android_activity = exe.add_type(DexType::Class("android.app.Activity"));
        let main_activity = exe.add_class_def("com.example.minimal.MainActivity", AccessFlags::PUBLIC, Some(android_activity), None);

        let super_constructor = exe.add_method(android_activity, "<init>", DexReturnType::Void, &[]);
        let mut constructor = DexEncoder::new();
        let this = constructor.alloc_input_register();
        constructor.invoke_direct(&[this], super_constructor);
        constructor.ret_void();
        exe.add_direct_method(main_activity, "<init>", DexReturnType::Void, &[], AccessFlags::PUBLIC | AccessFlags::CONSTRUCTOR, Some(constructor));
    }
}
