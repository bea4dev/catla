pub mod resolve;

use std::{borrow::Cow, ops::Range};

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_parser::ast::{EntityID, Program, Spanned};
use hashbrown::{DefaultHashBuilder, HashMap};

use crate::resolve::resolve_name_for_program;

pub fn resolve_name<'input>(
    ast: &Program<'input, '_>,
    all_crates: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
) -> (
    HashMap<EntityID, ResolvedInfo>,
    std::vec::Vec<NameResolveError>,
) {
    let mut resolved_map = HashMap::new();
    let mut errors = std::vec::Vec::new();

    let allocator = Bump::new();
    let mut components = NameEnvironmentSet::new(&allocator);

    let environment =
        components.new_environment(None, EnvironmentSeparatorKind::Function, ast.span.clone());

    resolve_name_for_program(
        ast,
        environment,
        &mut components,
        &mut resolved_map,
        all_crates,
        wild_card_imports,
        &mut errors,
    );

    (resolved_map, errors)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct NameEnvironmentID(usize);

#[derive(Debug)]
pub(crate) struct NameEnvironment<'input, 'name_env_alloc> {
    parent: Option<NameEnvironmentID>,
    map: HashMap<Cow<'input, str>, DefineInfo, DefaultHashBuilder, &'name_env_alloc Bump>,
    separator: EnvironmentSeparatorKind,
    span: Range<usize>,
}

pub(crate) struct NameEnvironmentSet<'input, 'name_env_alloc> {
    components:
        Vec<&'name_env_alloc mut NameEnvironment<'input, 'name_env_alloc>, &'name_env_alloc Bump>,
}

impl<'input, 'name_env_alloc> Drop for NameEnvironmentSet<'input, 'name_env_alloc> {
    fn drop(&mut self) {
        // drop String in Cow<'input, str>
        for component in self.components.iter_mut() {
            component.map.clear();
        }
    }
}

impl<'input, 'name_env_alloc> NameEnvironmentSet<'input, 'name_env_alloc> {
    pub fn new(allocator: &'name_env_alloc Bump) -> Self {
        Self {
            components: Vec::new_in(allocator),
        }
    }

    pub fn new_environment(
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

    fn resolve_inner(
        &self,
        env: NameEnvironmentID,
        name: &str,
        separators: &mut Option<&mut std::vec::Vec<Spanned<EnvironmentSeparatorKind>>>,
    ) -> Option<DefineInfo> {
        let environment = &self.components[env.0];

        if let Some(define_info) = environment.map.get(name) {
            return Some(define_info.clone());
        }

        if let Some(separators) = separators {
            separators.push(Spanned::new(
                environment.separator,
                environment.span.clone(),
            ));
        }

        match environment.parent {
            Some(parent) => self.resolve_inner(parent, name, separators),
            None => None,
        }
    }

    pub fn resolve(
        &self,
        env: NameEnvironmentID,
        name: &str,
        entity_id: EntityID,
        span: Range<usize>,
        all_crates: &std::vec::Vec<String>,
        resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
        errors: &mut std::vec::Vec<NameResolveError>,
    ) {
        let mut separators = std::vec::Vec::new();

        match self.resolve_inner(env, name, &mut Some(&mut separators)) {
            Some(define) => {
                resolved_map.insert(entity_id, ResolvedInfo { define, separators });
            }
            None => {
                if !all_crates.iter().any(|crate_name| crate_name == name) {
                    let error = NameResolveError {
                        name: name.to_string(),
                        kind: NameResolveErrorKind::NotFound,
                        span,
                    };
                    errors.push(error);
                }
            }
        }
    }

    pub fn define(
        &mut self,
        env: NameEnvironmentID,
        name: Cow<'input, str>,
        name_span: Range<usize>,
        define: DefineInfo,
        errors: &mut std::vec::Vec<NameResolveError>,
    ) {
        if let Some(exists) = self.resolve_inner(env, name.as_ref(), &mut None) {
            if define.kind != DefineKind::Variable {
                let error = NameResolveError {
                    name: name.to_string(),
                    kind: NameResolveErrorKind::DuplicatedName {
                        exists: exists.span.clone(),
                    },
                    span: name_span.clone(),
                };
                errors.push(error);
            }
        }

        let environment = &mut self.components[env.0];

        environment.map.insert(name, define);
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefineKind {
    Import,
    Function,
    Variable,
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
    pub name: String,
    pub kind: NameResolveErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum NameResolveErrorKind {
    NotFound,
    DuplicatedName { exists: Range<usize> },
}

#[cfg(test)]
mod test {
    use catla_parser::CatlaAST;
    use hashbrown::HashMap;

    use crate::resolve_name;

    #[test]
    fn name_resolve() {
        let source = r#"
let aaa = 100
print(aaa)

function print() {}
"#;

        let ast = CatlaAST::parse(source.to_string(), "test.catla".to_string());

        let (resolved, errors) = resolve_name(ast.ast(), &std::vec::Vec::new(), &HashMap::new());

        dbg!(resolved);
        dbg!(errors);
    }
}
