use std::ops::Range;

use catla_parser::ast::Spanned;
use catla_util::module_path::{ModulePath, Moduled};

#[derive(Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Range<usize>,
    pub module_path: ModulePath,
}

#[derive(Debug)]
pub enum TypeErrorKind {
    NotFoundModuleElementType,
    MissingModuleElementType,
    UnknownModuleElementType,
    UnknownThis,
    InvalidGenericsCount {
        expected: usize,
        found: usize,
        defined: Moduled<()>,
    },
    MissingStaticVariableType,
    TypeMismatch {
        left: Moduled<String>,
        right: Moduled<String>,
    },
    UnknownUserTypeElement,
    NotFieldInFieldAssign,
    NotUserTypeInNewObjectExpr,
    ExtraUserTypePath,
    ConflictedImplementInTypeInfer {
        conflicts: Vec<Moduled<String>>,
    },
    NoElement,
    MultiImplements {
        concretes: Vec<Moduled<String>>,
    },
    NotSatisfied {
        target: Moduled<String>,
        origin_target: Moduled<String>,
        origin_bound: Moduled<String>,
    },
    UnknownTypeAtThisPoint,
    NotFunctionType {
        not_function_type_from: Spanned<String>,
    },
    NotFunctionOrUserType {
        invalid_type_from: Spanned<String>,
    },
    FunctionArgumentCountMismatch {
        expected: Moduled<usize>,
        found: Moduled<usize>,
    },
    FunctionArgumentCountMismatchInInfer {
        expected: Moduled<usize>,
        found: Moduled<usize>,
    },
    NotInterfaceInImplements,
    NoElementFoundInImplements {
        element: Moduled<String>,
    },
    TypeMismatchInImplements {
        origin: Moduled<String>,
        impls: Moduled<String>,
    },
    ExtraBoundsInImplements {
        origin: Moduled<()>,
        impls: Moduled<()>,
    },
    UnknownImplementsElement {
        interface: Moduled<String>,
        impl_element: Moduled<String>,
    },
}
