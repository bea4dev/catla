use std::{
    fs::{self, File, OpenOptions},
    io::{self, Write},
    ops::Range,
    path::Path,
};

use allocator_api2::vec::Vec;
use bumpalo::{collections::String, format, Bump};
use catla_parser::parser::{Program, Spanned};
use fxhash::{FxHashMap, FxHashSet};
use program::{add_auto_import, codegen_program};

use super::{
    component::EntityID,
    context::{TranspileContext, TranspileModuleContext},
    name_resolver::FoundDefineInfo,
    optimizer::lifetime_analyzer::LifetimeAnalyzeResults,
    semantics::types::type_inference::TypeInferenceResultContainer,
    TranspileError,
};

pub mod cargo;
pub mod custom;
pub mod program;
pub mod user_type;

const INDENT: &str = "    ";

pub struct CodeBuilder<'allocator> {
    index: Option<usize>,
    code: Vec<Code<'allocator>, &'allocator Bump>,
    indent: usize,
    allocator: &'allocator Bump,
}

pub enum Code<'allocator> {
    Line(&'allocator str),
    Str(&'allocator str),
    Builder(Option<CodeBuilder<'allocator>>),
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

    pub fn add_line(&mut self, str: String<'allocator>) {
        self.code.push(Code::Line(str.into_bump_str()));
    }

    pub fn add_line_str(&mut self, str: &'allocator str) {
        self.code.push(Code::Line(str));
    }

    pub fn add_str(&mut self, str: &'allocator str) {
        self.code.push(Code::Str(str));
    }

    pub fn push_indent(&mut self) {
        self.indent += 1;
    }

    pub fn pop_indent(&mut self) {
        self.indent -= 1;
    }

    pub fn fork(&mut self) -> CodeBuilder<'allocator> {
        let index = self.code.len();
        self.code.push(Code::Builder(None));

        Self {
            index: Some(index),
            code: Vec::new_in(self.allocator),
            indent: self.indent,
            allocator: self.allocator,
        }
    }

    pub fn pull(&mut self, builder: CodeBuilder<'allocator>) {
        let index = builder.index.unwrap();
        self.code[index] = Code::Builder(Some(builder));
    }

    pub fn build(self, context: &TranspileModuleContext) -> io::Result<()> {
        let entry_name = context.module_name.split("::").next().unwrap();

        let out_entry_name = if entry_name == "std" {
            "catla_std"
        } else {
            entry_name
        };

        let path = std::format!(
            "{}/{}/src/{}.rs",
            &context.context.settings.codegen_dir,
            out_entry_name,
            (&context.module_name[(entry_name.len() + 2)..]).replace("::", "/")
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
                Code::Line(str) => {
                    file.write_all("\n".as_bytes())?;

                    for _ in 0..self.indent {
                        file.write_all(INDENT.as_bytes())?;
                    }

                    file.write_all(str.as_bytes())?;
                }
                Code::Str(str) => {
                    file.write_all(str.as_bytes())?;
                }
                Code::Builder(builder) => {
                    if let Some(builder) = builder {
                        builder.write_all(file)?;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn to_string_without_indent(&self) -> String<'allocator> {
        let length = self.length_without_indent();
        let mut string = String::with_capacity_in(length, self.allocator);

        self.write_to_str_without_indent(&mut string);

        string
    }

    pub fn write_to_str_without_indent(&self, string: &mut String<'allocator>) {
        for code in self.code.iter() {
            match code {
                Code::Line(str) | Code::Str(str) => *string += *str,
                Code::Builder(builder) => {
                    if let Some(builder) = builder {
                        builder.write_to_str_without_indent(string);
                    }
                }
            }
        }
    }

    pub fn length_without_indent(&self) -> usize {
        self.code
            .iter()
            .map(|code| match code {
                Code::Line(str) | Code::Str(str) => str.len(),
                Code::Builder(builder) => builder
                    .as_ref()
                    .map(|builder| builder.length_without_indent())
                    .unwrap_or(0),
            })
            .sum()
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

        self.builder.add_line(code);

        name
    }
}

pub fn codegen_dir_modules(context: &TranspileContext) -> io::Result<()> {
    let all_modules = context.source_code_provider.get_all_modules();
    let dir_modules = context.source_code_provider.get_dir_modules();

    for dir_module in dir_modules.iter() {
        let entry_name = dir_module.split("::").next().unwrap();

        let out_entry_name = if entry_name == "std" {
            "catla_std"
        } else {
            entry_name
        };

        let path = std::format!(
            "{}/{}/src/{}.rs",
            &context.settings.codegen_dir,
            out_entry_name,
            (&dir_module[(entry_name.len() + 2)..]).replace("::", "/")
        );

        let dir = Path::new(path.as_str());

        fs::create_dir_all(dir.parent().unwrap())?;

        let mut file = OpenOptions::new()
            .write(true)
            .append(true)
            .create(true)
            .open(path)?;

        for module in all_modules.iter() {
            if !module.starts_with(dir_module) {
                continue;
            }

            let right_name = &module[(dir_module.len() + 2)..];

            if right_name.contains("::") {
                continue;
            }

            file.write_all(std::format!("pub mod {};", right_name).as_bytes())?;
        }
    }

    let entries = context.source_code_provider.get_entries();

    for entry in entries.iter() {
        let out_entry_name = if entry.as_str() == "std" {
            "catla_std"
        } else {
            entry.as_str()
        };

        for crate_entry in ["main", "lib"] {
            let path = std::format!(
                "{}/{}/src/{}.rs",
                &context.settings.codegen_dir,
                out_entry_name,
                crate_entry
            );

            let dir = Path::new(path.as_str());

            fs::create_dir_all(dir.parent().unwrap())?;

            let mut file = OpenOptions::new()
                .write(true)
                .append(true)
                .create(true)
                .open(path)?;

            let mut generated = FxHashSet::default();

            for module in all_modules.iter().chain(dir_modules.iter()) {
                if !module.starts_with(entry) {
                    continue;
                }

                if generated.contains(&module) {
                    continue;
                }
                generated.insert(module);

                let right_name = &module[(entry.len() + 2)..];

                if right_name.contains("::") || right_name == "main" || right_name == "lib" {
                    continue;
                }

                if entries.contains(right_name) {
                    continue;
                }

                file.write_all(std::format!("pub mod {};", right_name).as_bytes())?;
            }
        }
    }

    Ok(())
}

pub fn codegen(
    ast: Program,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    let allocator = Bump::new();

    let mut code_builder = CodeBuilder::new(&allocator);

    add_auto_import(ast, &mut code_builder, &allocator, context);

    codegen_program(
        ast,
        None,
        type_inference_result,
        lifetime_analyze_results,
        import_element_map,
        name_resolved_map,
        None,
        &mut StackAllocCodeBuilder::new(&mut code_builder),
        &mut StackAllocCodeBuilder::new(&mut code_builder),
        &mut code_builder,
        &allocator,
        errors,
        context,
    );

    code_builder.build(context).unwrap();
}
