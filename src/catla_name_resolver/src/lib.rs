use std::ops::Range;

use bumpalo::Bump;
use catla_parser::ast::EntityID;
use hashbrown::{DefaultHashBuilder, HashMap};

pub(crate) struct NameEnvironmentID(usize);

pub(crate) struct NameEnvironment<'input, 'name_env_alloc> {
    parent: Option<NameEnvironmentID>,
    map: HashMap<&'input str, DefineInfo, DefaultHashBuilder, &'name_env_alloc Bump>,
    separator_kind: EnvironmentSeparatorKind,
    span: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EnvironmentSeparatorKind {
    Function,
    UserTypeDefine,
    Closure,
    Loop,
}

pub struct DefineInfo {
    entity_id: EntityID,
    span: Range<usize>,
    kind: DefineKind,
}

#[derive(Debug, Clone, Copy)]
pub enum DefineKind {
    Import,
    Function,
    Variable,
    FunctionArgument,
    ClosureArgument,
    UserType,
    Generics,
}

