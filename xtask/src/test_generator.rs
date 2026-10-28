use std::{env, error::Error, fs::File, io::Write, path::PathBuf};

#[derive(Default)]
struct TestGenerator {
    buf: String,
    indentation_level: u16,
}

impl TestGenerator {
    fn begin_line(&mut self, begin: &str) {
        for _ in 0..self.indentation_level {
            self.buf.push_str("    ");
        }
        self.buf.push_str(begin);
    }
    fn end_line(&mut self, end: &str) {
        self.buf.push_str(end);
        self.buf.push('\n');
    }
    fn write_line(&mut self, line: &str) {
        self.begin_line(line);
        self.buf.push('\n');
    }
    fn write_assert(&mut self, condition: &str) {
        self.begin_line("core.assert(");
        self.buf.push_str(condition);
        self.end_line(")");
    }
    fn begin_func(&mut self, signature: &str) {
        self.begin_line(signature);
        self.buf.push_str(" {\n");
        self.indentation_level += 1;
    }
    fn end_func(&mut self) {
        self.indentation_level -= 1;
        self.write_line("}");
    }
    fn begin_test(&mut self) {
        if !self.buf.is_empty() {
            self.buf.push('\n');
        }
    }
    fn write_float_addition_test(&mut self, ty: &str) {
        self.begin_test();

        self.buf.push_str("@compiler_test(failure, 1, 2, 4)\n");
        self.buf.push_str("@compiler_test(success, 1, 2, 3)\n");
        self.buf.push_str("@compiler_test(success, 7, 10, 17)\n");

        self.begin_func(&format!("fn test_add_{ty}(a: {ty}, b: {ty}, result: {ty})"));
        self.write_assert("a + b == result");
        self.end_func();
    }
    fn write_int_addition_test(&mut self, ty: &str) {
        self.begin_test();

        assert!(ty.starts_with('i') || ty.starts_with('u'));
        let rest = &ty[1..];

        let ptr_size = rest == "size";
        let num_bits: u64 = if ptr_size {
            64
        } else {
            str::parse(rest).unwrap()
        };
        let signed = ty.starts_with('i');

        let mut max_value = u64::MAX >> (64 - num_bits);
        if signed {
            max_value >>= 1;
        }

        self.write_line("@compiler_test(failure, 1, 2, 4)");
        self.write_line(&format!("@compiler_test(success, {}, 5, {})", max_value - 5, max_value));
        self.write_line("@compiler_test(success, 7, 10, 17)");

        self.begin_func(&format!("fn test_add_{ty}(a: {ty}, b: {ty}, result: {ty})"));
        self.write_assert("a + b == result");
        self.end_func();
    }
}

pub fn generate_tests() -> Result<(), Box<dyn Error>> {
    let src_root = env::var("DUSK_SRC_ROOT")?;

    let mut test_dir_path = PathBuf::from(src_root);
    test_dir_path.extend(["tests", "integration"]);

    let mut addition_file_path = test_dir_path.clone();
    addition_file_path.push("addition.dusk");

    let mut generator = TestGenerator::default();
    generator.write_int_addition_test("i8");
    generator.write_int_addition_test("i16");
    generator.write_int_addition_test("i32");
    generator.write_int_addition_test("i64");
    generator.write_int_addition_test("isize");
    generator.write_int_addition_test("u8");
    generator.write_int_addition_test("u16");
    generator.write_int_addition_test("u32");
    generator.write_int_addition_test("u64");
    generator.write_int_addition_test("usize");
    generator.write_float_addition_test("f32");
    generator.write_float_addition_test("f64");

    let mut addition_file = File::create(&addition_file_path)?;
    addition_file.write_all(generator.buf.as_bytes())?;

    Ok(())
}