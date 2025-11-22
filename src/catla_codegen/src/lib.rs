pub mod transpiler;

use std::{
    cell::{Cell, RefCell},
    path::PathBuf,
    rc::Rc,
};

use catla_parser::ast::Program;

pub fn transpile(ast: &Program, settings: &TranspilerSettings) {}

#[derive(Debug)]
pub struct TranspilerSettings {
    pub out_dir: PathBuf,
}

#[derive(Debug, Clone)]
pub struct CodeBuilder {
    code: Rc<RefCell<String>>,
    depth: Cell<usize>,
}

#[derive(Debug)]
pub struct CodeBuilderScope {
    code: CodeBuilder,
}

impl CodeBuilder {
    pub fn new() -> Self {
        Self {
            code: Rc::new(RefCell::new(String::new())),
            depth: Cell::new(0),
        }
    }

    pub fn scope(&self) -> CodeBuilderScope {
        self.depth.set(self.depth.get() + 1);

        CodeBuilderScope { code: self.clone() }
    }

    pub fn dump(self) -> String {
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
            format!("{}{}\n", "    ".repeat(self.code.depth.get()), code).as_str();
    }
}

