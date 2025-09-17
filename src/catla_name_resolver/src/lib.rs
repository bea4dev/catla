use std::ops::Range;

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_parser::ast::{EntityID, Literal, Spanned};
use hashbrown::{DefaultHashBuilder, HashMap};

pub(crate) struct NameEnvironmentID(usize);

pub(crate) struct NameEnvironment<'input, 'name_env_alloc> {
    parent: Option<NameEnvironmentID>,
    map: HashMap<&'input str, DefineInfo, DefaultHashBuilder, &'name_env_alloc Bump>,
    separator: EnvironmentSeparatorKind,
    span: Range<usize>,
}

pub(crate) struct NameEnvironmentSet<'input, 'name_env_alloc> {
    components:
        Vec<&'name_env_alloc mut NameEnvironment<'input, 'name_env_alloc>, &'name_env_alloc Bump>,
}

impl<'input, 'name_env_alloc> NameEnvironmentSet<'input, 'name_env_alloc> {
    pub fn new(allocator: &'name_env_alloc Bump) -> Self {
        Self {
            components: Vec::new_in(allocator),
        }
    }

    pub fn new_env(
        &mut self,
        parent: Option<NameEnvironmentID>,
        separator: EnvironmentSeparatorKind,
        span: Range<usize>,
    ) -> NameEnvironmentID {
        let allocator = *self.components.allocator();

        let new_env = NameEnvironment {
            parent,
            map: HashMap::new_in(allocator),
            separator,
            span,
        };
        self.components.push(allocator.alloc(new_env));

        NameEnvironmentID(self.components.len() - 1)
    }

    pub fn resolve(
        &self,
        env: NameEnvironmentID,
        name: &str,
        entity_id: EntityID,
        resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
        errors: &mut std::vec::Vec<NameResolveError>,
    ) {
        
    }

    pub fn define(
        &mut self,
        env: NameEnvironmentID,
        entity_id: EntityID,
        name: Literal<'input>,
        define: DefineInfo,
        errors: &mut std::vec::Vec<NameResolveError>,
    ) {
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EnvironmentSeparatorKind {
    Function,
    UserTypeDefine,
    Closure,
    Loop,
}

#[derive(Debug, Clone)]
pub struct DefineInfo {
    pub entity_id: EntityID,
    pub span: Range<usize>,
    pub kind: DefineKind,
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

#[derive(Debug, Clone)]
pub struct ResolvedInfo {
    pub define: DefineInfo,
    pub separators: std::vec::Vec<Spanned<EnvironmentSeparatorKind>>,
}

#[derive(Debug, Clone)]
pub struct NameResolveError {
    pub kind: NameResolveErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum NameResolveErrorKind {
    DuplicatedName { exists: Spanned<String> },
}
