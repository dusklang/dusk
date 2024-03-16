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
        let main_activity_class_id = exe.class_defs[main_activity].class_idx;

        let super_constructor = exe.add_method(android_activity, "<init>", DexReturnType::Void, &[]);
        let mut constructor = DexEncoder::new();
        let this = constructor.alloc_input_register();
        constructor.invoke_direct(&[this], super_constructor);
        constructor.ret_void();
        exe.add_direct_method(main_activity, "<init>", DexReturnType::Void, &[], AccessFlags::PUBLIC | AccessFlags::CONSTRUCTOR, Some(constructor));

        let mut on_create = DexEncoder::new();
        let this = on_create.alloc_input_register();
        let saved_instance_state = on_create.alloc_input_register();
        
        // super.onCreate(saved_instance_state)
        let super_on_create = exe.add_method(android_activity, "onCreate", DexReturnType::Void, &[DexType::Class("android.os.Bundle").into()]);
        on_create.invoke_super(&[this, saved_instance_state], super_on_create);

        // var text_view = new TextView(this)
        let text_view = on_create.alloc_register();
        let text_view_class = exe.add_type(DexType::Class("android.widget.TextView"));
        on_create.new_instance(text_view, text_view_class);
        let text_view_constructor = exe.add_method(text_view_class, "<init>", DexReturnType::Void, &[DexType::Class("android.content.Context").into()]);
        on_create.invoke_direct(&[text_view, this], text_view_constructor);

        // var const_string = "..."
        let const_string = on_create.alloc_register();
        on_create.const_string(const_string, exe.add_string("Hello from Dusk!"));

        // text_view.setText(const_string)
        let set_text = exe.add_method(text_view_class, "setText", DexReturnType::Void, &[DexType::Class("java.lang.CharSequence").into()]);
        on_create.invoke_virtual(&[text_view, const_string], set_text);

        // this.setContentView(text_view)
        let set_content_view = exe.add_method(main_activity_class_id, "setContentView", DexReturnType::Void, &[DexType::Class("android.view.View").into()]);
        on_create.invoke_virtual(&[this, text_view], set_content_view);

        on_create.ret_void();
        exe.add_virtual_method(main_activity, "onCreate", DexReturnType::Void, &[DexType::Class("android.os.Bundle").into()], AccessFlags::PROTECTED, Some(on_create));
    }
}
