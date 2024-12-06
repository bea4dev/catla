use std::{
    fs::{self, File},
    io::{self, Write},
    ops::Range,
    path::Path,
};

use allocator_api2::vec::Vec;
use bumpalo::{collections::String, format, Bump};
use either::Either;

use super::context::TranspileModuleContext;

pub mod program;
pub mod user_type;

const INDENT: &str = "    ";

pub struct CodeBuilder<'allocator> {
    index: Option<usize>,
    code: Vec<Either<String<'allocator>, Option<CodeBuilder<'allocator>>>, &'allocator Bump>,
    indent: usize,
    allocator: &'allocator Bump,
}

impl<'allocator> CodeBuilder<'allocator> {
    pub fn new(allocator: &'allocator Bump) -> Self {
        Self {
            index: None,
            code: Vec::new_in(allocator),
            indent: 0,
            allocator,
        }
    }

    pub fn add(&mut self, str: String<'allocator>) {
        self.code.push(Either::Left(str));
    }

    pub fn push_indent(&mut self) {
        self.indent += 1;
    }

    pub fn pop_indent(&mut self) {
        self.indent -= 1;
    }

    pub fn fork(&mut self) -> CodeBuilder<'allocator> {
        let index = self.code.len();
        self.code.push(Either::Right(None));

        Self {
            index: Some(index),
            code: Vec::new_in(self.allocator),
            indent: self.indent,
            allocator: self.allocator,
        }
    }

    pub fn pull(&mut self, builder: CodeBuilder<'allocator>) {
        let index = builder.index.unwrap();
        self.code[index] = Either::Right(Some(builder));
    }

    pub fn build(self, context: &TranspileModuleContext) -> io::Result<()> {
        let path = std::format!(
            "{}/src/{}.rs",
            &context.context.settings.codegen_dir,
            context.module_name.replace("::", "/")
        );

        let dir = Path::new(path.as_str());

        fs::create_dir_all(dir.parent().unwrap())?;

        let mut file = File::create(path)?;

        self.write_all(&mut file)?;

        Ok(())
    }

    fn write_all(self, file: &mut File) -> io::Result<()> {
        for code in self.code {
            match code {
                Either::Left(str) => {
                    for _ in 0..self.indent {
                        file.write_all(INDENT.as_bytes())?;
                    }
                    file.write_all(str.as_bytes())?;
                    file.write_all("\n".as_bytes())?;
                }
                Either::Right(builder) => {
                    if let Some(builder) = builder {
                        builder.write_all(file)?;
                    }
                }
            }
        }

        Ok(())
    }
}

pub struct StackAllocCodeBuilder<'allocator> {
    builder: CodeBuilder<'allocator>,
}

impl<'allocator> StackAllocCodeBuilder<'allocator> {
    pub fn new(module_code_builder: &mut CodeBuilder<'allocator>) -> Self {
        Self {
            builder: module_code_builder.fork(),
        }
    }

    pub fn create_uninit(&mut self, create_for: Range<usize>) -> String<'allocator> {
        let name = format!(
            in self.builder.allocator,
            "uninit_{}_{}",
            create_for.start,
            create_for.end
        );
        let code = format!(
            in self.builder.allocator,
            "let {} = MaybeUninit::uninit();",
            &name
        );

        self.builder.add(code);

        name
    }
}
