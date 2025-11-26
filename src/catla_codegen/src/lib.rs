pub mod codegen;
pub mod crates;

use std::{
    cell::{Cell, RefCell},
    path::PathBuf,
    rc::Rc,
};

use catla_parser::ast::{EntityID, Program, Spanned};
use catla_std::get_std_map;
use catla_type::types::Type;
use catla_util::module_path::ModulePath;
use hashbrown::HashMap;

use crate::codegen::codegen_for_program;

pub async fn codegen(
    ast: &Program<'_, '_>,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    settings: &CodegenSettings,
    module_path: &ModulePath,
) -> Result<(), String> {
    let string = match get_std_map().get(module_path.path_name.as_str()) {
        Some(code) => code.to_string(),
        None => {
            let builder = CodeBuilder::new();
            {
                let scope = builder.scope();

                codegen_for_program(ast, false, type_infer_results, &scope);
            }

            builder.dump()
        }
    };

    let mut out_path = settings.out_dir.clone();

    if let Some(first) = module_path.path.first() {
        if first.as_str() == "std" {
            out_path.push("catla_std");
        } else {
            out_path.push(first);
        }
        out_path.push("src");

        for (index, path) in module_path.path[1..].iter().enumerate() {
            if index == module_path.path.len() - 2 {
                out_path.push(format!("{}.rs", path));
            } else {
                out_path.push(path);
            }
        }
    }

    if let Some(parent) = out_path.parent() {
        tokio::fs::create_dir_all(parent)
            .await
            .map_err(|_| "Failed to create dirs.".to_string())?;
    }

    tokio::fs::write(out_path, string)
        .await
        .map_err(|error| format!("Failed to write rust source code : {}", error))?;

    Ok(())
}

#[derive(Debug)]
pub struct CodegenSettings {
    pub out_dir: PathBuf,
}

#[derive(Debug, Clone)]
pub struct CodeBuilder {
    code: Rc<RefCell<String>>,
    depth: Rc<Cell<usize>>,
}

#[derive(Debug)]
pub struct CodeBuilderScope {
    code: CodeBuilder,
}

impl CodeBuilder {
    pub fn new() -> Self {
        Self {
            code: Rc::new(RefCell::new(String::new())),
            depth: Rc::new(Cell::new(0)),
        }
    }

    pub fn scope(&self) -> CodeBuilderScope {
        self.depth.set(self.depth.get() + 1);
        dbg!(self.depth.get());

        CodeBuilderScope { code: self.clone() }
    }

    pub fn dump(&self) -> String {
        self.code.borrow().clone()
    }
}

impl Drop for CodeBuilderScope {
    fn drop(&mut self) {
        self.code.depth.set(self.code.depth.get() - 1);
    }
}

impl CodeBuilderScope {
    pub fn push_raw(&self, code: &str) {
        *self.code.code.borrow_mut() += code;
    }

    pub fn push_line(&self, code: &str) {
        *self.code.code.borrow_mut() +=
            format!("{}{}\n", "    ".repeat(self.code.depth.get() - 1), code).as_str();
    }

    pub fn scope(&self) -> CodeBuilderScope {
        self.code.scope()
    }
}
