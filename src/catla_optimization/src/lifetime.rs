use std::sync::Arc;

use catla_name_resolver::{DefineKind, EnvironmentSeparatorKind, ResolvedInfo};
use catla_parser::ast::{
    AddOrSubExpression, AndExpression, Define, EntityID, EqualsExpression, Expression, Factor,
    FunctionDefine, LessOrGreaterExpression, MulOrDivExpression, OrExpression, Primary,
    PrimaryLeftExpr, Statement, StatementAttribute, UserTypeKind, VariableBinding,
};
use catla_parser::CatlaAST;
use catla_type::types::{GlobalUserTypeID, GlobalUserTypeSet, ImplementsInfoSet, Type};
use hashbrown::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocationKind {
    Stack,
    Heap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LifetimeAnalyzeResult {
    pub allocation: AllocationKind,
    pub requires_drop: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LifetimeAnalyzeResults {
    object_results: HashMap<EntityID, LifetimeAnalyzeResult>,
    call_argument_borrow: HashMap<EntityID, bool>,
    variable_origins: HashMap<EntityID, HashSet<EntityID>>,
}

impl LifetimeAnalyzeResults {
    pub fn new() -> Self {
        Self {
            object_results: HashMap::new(),
            call_argument_borrow: HashMap::new(),
            variable_origins: HashMap::new(),
        }
    }

    pub fn object_result(&self, entity_id: EntityID) -> Option<LifetimeAnalyzeResult> {
        self.object_results.get(&entity_id).copied()
    }

    pub fn should_borrow_argument(&self, expression_id: EntityID) -> bool {
        self.call_argument_borrow
            .get(&expression_id)
            .copied()
            .unwrap_or(false)
    }

    pub fn variable_origins(&self, variable_entity_id: EntityID) -> Option<&HashSet<EntityID>> {
        self.variable_origins.get(&variable_entity_id)
    }
}

#[derive(Debug, Clone)]
pub struct ModuleLifetimeSource {
    pub module_name: String,
    pub ast: CatlaAST,
    pub type_infer_results: HashMap<EntityID, catla_parser::ast::Spanned<Type>>,
    pub module_entity_type_map: HashMap<EntityID, Type>,
    pub name_resolved_map: HashMap<EntityID, ResolvedInfo>,
}

pub fn collect_lifetime_source(
    module_name: &str,
    ast: &CatlaAST,
    type_infer_results: &HashMap<EntityID, catla_parser::ast::Spanned<Type>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
) -> ModuleLifetimeSource {
    ModuleLifetimeSource {
        module_name: module_name.to_string(),
        ast: ast.clone(),
        type_infer_results: type_infer_results.clone(),
        module_entity_type_map: module_entity_type_map.clone(),
        name_resolved_map: name_resolved_map.clone(),
    }
}

pub fn evaluate_lifetime_sources(
    module_sources: &HashMap<String, Arc<ModuleLifetimeSource>>,
    user_type_set: &GlobalUserTypeSet,
    global_implements_infos: &ImplementsInfoSet,
) -> HashMap<String, LifetimeAnalyzeResults> {
    let mut definitions = Vec::new();
    let mut symbol_by_entity = HashMap::<EntityID, FunctionSymbol>::new();
    let mut symbols_by_name = HashMap::<String, Vec<FunctionSymbol>>::new();

    for source in module_sources.values() {
        let (module_definitions, module_symbol_by_entity, module_symbols_by_name) =
            collect_function_definitions(
                source.module_name.as_str(),
                source.ast.ast(),
                &source.module_entity_type_map,
            );

        definitions.extend(module_definitions);
        symbol_by_entity.extend(module_symbol_by_entity);

        for (name, mut symbols) in module_symbols_by_name {
            symbols_by_name.entry(name).or_default().append(&mut symbols);
        }
    }

    let interface_adjacency = build_interface_adjacency(global_implements_infos, user_type_set);
    let concrete_method_symbols = collect_concrete_method_symbols(global_implements_infos);
    let drop_required_user_types = collect_drop_required_user_types_for_sources(
        module_sources,
        user_type_set,
        global_implements_infos,
    );

    let mut summaries = HashMap::<FunctionSymbol, FunctionSummary>::new();
    for definition in definitions.iter() {
        summaries
            .entry(definition.symbol.clone())
            .or_insert_with(|| FunctionSummary {
                parameter_escape: vec![false; definition.parameter_count],
                parameter_constraints: HashSet::new(),
                returns_object: false,
            });
    }

    let mut changed = true;
    let mut guard = 0usize;
    while changed && guard < 64 {
        guard += 1;
        changed = false;

        for definition in definitions.iter() {
            let Some(source) = module_sources.get(&definition.module_name) else {
                continue;
            };

            let local_summary = analyze_function_summary(
                definition,
                &source.name_resolved_map,
                &source.type_infer_results,
                &symbol_by_entity,
                &symbols_by_name,
                &source.module_entity_type_map,
                user_type_set,
                &summaries,
                &interface_adjacency,
                &concrete_method_symbols,
                &drop_required_user_types,
            );

            let entry = summaries
                .entry(definition.symbol.clone())
                .or_insert_with(|| FunctionSummary {
                    parameter_escape: vec![false; definition.parameter_count],
                    parameter_constraints: HashSet::new(),
                    returns_object: false,
                });

            if entry.parameter_escape.len() < local_summary.parameter_escape.len() {
                entry
                    .parameter_escape
                    .resize(local_summary.parameter_escape.len(), false);
                changed = true;
            }

            for (index, value) in local_summary.parameter_escape.iter().enumerate() {
                if *value && !entry.parameter_escape[index] {
                    entry.parameter_escape[index] = true;
                    changed = true;
                }
            }

            for constraint in local_summary.parameter_constraints.iter().copied() {
                if entry.parameter_constraints.insert(constraint) {
                    changed = true;
                }
            }

            if local_summary.returns_object && !entry.returns_object {
                entry.returns_object = true;
                changed = true;
            }
        }
    }

    let mut results = module_sources
        .keys()
        .map(|module_name| (module_name.clone(), LifetimeAnalyzeResults::new()))
        .collect::<HashMap<_, _>>();

    for definition in definitions.iter() {
        let Some(source) = module_sources.get(&definition.module_name) else {
            continue;
        };
        let Some(module_result) = results.get_mut(&definition.module_name) else {
            continue;
        };
        let mut origin_user_types = HashMap::new();

        let mut context = AnalyzerContext {
            name_resolved_map: &source.name_resolved_map,
            type_infer_results: &source.type_infer_results,
            module_entity_type_map: &source.module_entity_type_map,
            user_type_set,
            symbol_by_entity: &symbol_by_entity,
            _symbols_by_name: &symbols_by_name,
            summaries: &summaries,
            interface_adjacency: &interface_adjacency,
            concrete_method_symbols: &concrete_method_symbols,
            drop_required_user_types: &drop_required_user_types,
            call_argument_borrow: &mut module_result.call_argument_borrow,
            object_results: &mut module_result.object_results,
            variable_origins: &mut module_result.variable_origins,
            origin_user_types: &mut origin_user_types,
        };

        analyze_function_with_final_summary(definition, &mut context);
    }

    for (module_name, source) in module_sources.iter() {
        let Some(module_result) = results.get_mut(module_name) else {
            continue;
        };
        let mut origin_user_types = HashMap::new();

        let mut context = AnalyzerContext {
            name_resolved_map: &source.name_resolved_map,
            type_infer_results: &source.type_infer_results,
            module_entity_type_map: &source.module_entity_type_map,
            user_type_set,
            symbol_by_entity: &symbol_by_entity,
            _symbols_by_name: &symbols_by_name,
            summaries: &summaries,
            interface_adjacency: &interface_adjacency,
            concrete_method_symbols: &concrete_method_symbols,
            drop_required_user_types: &drop_required_user_types,
            call_argument_borrow: &mut module_result.call_argument_borrow,
            object_results: &mut module_result.object_results,
            variable_origins: &mut module_result.variable_origins,
            origin_user_types: &mut origin_user_types,
        };

        analyze_module_level_roots(source.ast.ast(), &mut context);
    }

    results
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FunctionSymbol {
    module_path: String,
    name: String,
    arity: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FunctionSummary {
    parameter_escape: Vec<bool>,
    parameter_constraints: HashSet<(usize, usize)>,
    returns_object: bool,
}

#[derive(Debug, Clone)]
struct ParameterBinding {
    variable_entity: EntityID,
    parameter_index: usize,
}

#[derive(Debug, Clone)]
struct FunctionDefinition<'ast, 'input, 'allocator> {
    module_name: String,
    symbol: FunctionSymbol,
    function_entity: EntityID,
    ast: &'ast FunctionDefine<'input, 'allocator>,
    parameter_bindings: Vec<ParameterBinding>,
    parameter_count: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LifetimeTreeRef(usize);

#[derive(Debug, Clone)]
struct LifetimeTreeNode {
    children: HashMap<String, LifetimeTreeRef>,
    borrow_refs: HashSet<LifetimeTreeRef>,
    alloc_origin: Option<EntityID>,
    drop_order: Option<usize>,
    has_static_lifetime: bool,
    is_argument_tree: bool,
}

impl LifetimeTreeNode {
    fn new(
        alloc_origin: Option<EntityID>,
        drop_order: Option<usize>,
        is_argument_tree: bool,
    ) -> Self {
        Self {
            children: HashMap::new(),
            borrow_refs: HashSet::new(),
            alloc_origin,
            drop_order,
            has_static_lifetime: false,
            is_argument_tree,
        }
    }
}

#[derive(Debug, Clone, Default)]
struct LifetimeTreeArena {
    nodes: Vec<LifetimeTreeNode>,
}

impl LifetimeTreeArena {
    fn create_node(
        &mut self,
        alloc_origin: Option<EntityID>,
        drop_order: Option<usize>,
        is_argument_tree: bool,
    ) -> LifetimeTreeRef {
        let node_ref = LifetimeTreeRef(self.nodes.len());
        self.nodes
            .push(LifetimeTreeNode::new(alloc_origin, drop_order, is_argument_tree));
        node_ref
    }

    fn get_or_create_child(&mut self, parent: LifetimeTreeRef, name: &str) -> LifetimeTreeRef {
        if let Some(existing) = self.nodes[parent.0].children.get(name).copied() {
            return existing;
        }

        let alloc_origin = self.nodes[parent.0].alloc_origin;
        let drop_order = self.nodes[parent.0].drop_order;
        let child = self.create_node(alloc_origin, drop_order, false);
        self.nodes[parent.0]
            .children
            .insert(name.to_string(), child);
        child
    }

    fn add_borrow(&mut self, owner: LifetimeTreeRef, borrowed: LifetimeTreeRef) {
        self.nodes[owner.0].borrow_refs.insert(borrowed);
    }

    fn mark_static_lifetime(&mut self, node_ref: LifetimeTreeRef) {
        let mut visited = HashSet::new();
        let mut stack = vec![node_ref];

        while let Some(current) = stack.pop() {
            if !visited.insert(current) {
                continue;
            }

            let node = &mut self.nodes[current.0];
            node.has_static_lifetime = true;

            stack.extend(node.children.values().copied());
            stack.extend(node.borrow_refs.iter().copied());
        }
    }

    fn set_drop_order(&mut self, node_ref: LifetimeTreeRef, drop_order: usize) {
        let node = &mut self.nodes[node_ref.0];
        match node.drop_order {
            Some(current) if current >= drop_order => {}
            _ => node.drop_order = Some(drop_order),
        }
    }

    fn drop_order(&self, node_ref: LifetimeTreeRef) -> Option<usize> {
        self.nodes.get(node_ref.0).and_then(|node| node.drop_order)
    }

    fn alloc_origin(&self, node_ref: LifetimeTreeRef) -> Option<EntityID> {
        self.nodes.get(node_ref.0).and_then(|node| node.alloc_origin)
    }

    fn has_static_lifetime(&self, node_ref: LifetimeTreeRef) -> bool {
        self.nodes
            .get(node_ref.0)
            .map(|node| node.has_static_lifetime)
            .unwrap_or(false)
    }

    fn all_expected_constraints(&self) -> Vec<(LifetimeTreeRef, LifetimeTreeRef)> {
        let mut constraints = Vec::new();

        for (index, node) in self.nodes.iter().enumerate() {
            let parent = LifetimeTreeRef(index);

            for child in node.children.values().copied() {
                constraints.push((parent, child));
            }
            for borrowed in node.borrow_refs.iter().copied() {
                constraints.push((parent, borrowed));
            }
        }

        constraints
    }

    fn collect_reachable_alloc_origins(
        &self,
        roots: &HashSet<LifetimeTreeRef>,
    ) -> HashSet<EntityID> {
        let mut origins = HashSet::new();
        let mut visited = HashSet::new();
        let mut stack = roots.iter().copied().collect::<Vec<_>>();

        while let Some(current) = stack.pop() {
            if !visited.insert(current) {
                continue;
            }

            let Some(node) = self.nodes.get(current.0) else {
                continue;
            };

            if let Some(origin) = node.alloc_origin {
                origins.insert(origin);
            }

            stack.extend(node.children.values().copied());
            stack.extend(node.borrow_refs.iter().copied());
        }

        origins
    }
}

#[derive(Debug, Default, Clone)]
struct ExprFlow {
    params: HashSet<usize>,
    origins: HashSet<EntityID>,
    tree_refs: HashSet<LifetimeTreeRef>,
}

impl ExprFlow {
    fn merge(&mut self, other: Self) {
        self.params.extend(other.params);
        self.origins.extend(other.origins);
        self.tree_refs.extend(other.tree_refs);
    }
}

#[derive(Debug, Clone)]
struct FunctionState {
    parameter_escape: Vec<bool>,
    parameter_constraints: HashSet<(usize, usize)>,
    aliases_to_params: HashMap<EntityID, HashSet<usize>>,
    aliases_to_origins: HashMap<EntityID, HashSet<EntityID>>,
    aliases_to_tree_refs: HashMap<EntityID, HashSet<LifetimeTreeRef>>,
    tree_constraints: HashSet<(LifetimeTreeRef, LifetimeTreeRef)>,
    param_to_tree_constraints: HashSet<(usize, LifetimeTreeRef)>,
    origin_tree_refs: HashMap<EntityID, LifetimeTreeRef>,
    lifetime_tree: LifetimeTreeArena,
}

impl FunctionState {
    fn new(parameter_count: usize) -> Self {
        Self {
            parameter_escape: vec![false; parameter_count],
            parameter_constraints: HashSet::new(),
            aliases_to_params: HashMap::new(),
            aliases_to_origins: HashMap::new(),
            aliases_to_tree_refs: HashMap::new(),
            tree_constraints: HashSet::new(),
            param_to_tree_constraints: HashSet::new(),
            origin_tree_refs: HashMap::new(),
            lifetime_tree: LifetimeTreeArena::default(),
        }
    }
}

#[derive(Debug)]
struct AnalyzerContext<'a> {
    name_resolved_map: &'a HashMap<EntityID, ResolvedInfo>,
    type_infer_results: &'a HashMap<EntityID, catla_parser::ast::Spanned<Type>>,
    module_entity_type_map: &'a HashMap<EntityID, Type>,
    user_type_set: &'a GlobalUserTypeSet,
    symbol_by_entity: &'a HashMap<EntityID, FunctionSymbol>,
    _symbols_by_name: &'a HashMap<String, Vec<FunctionSymbol>>,
    summaries: &'a HashMap<FunctionSymbol, FunctionSummary>,
    interface_adjacency: &'a HashMap<FunctionSymbol, HashSet<FunctionSymbol>>,
    concrete_method_symbols: &'a HashMap<GlobalUserTypeID, Vec<FunctionSymbol>>,
    drop_required_user_types: &'a HashSet<GlobalUserTypeID>,
    call_argument_borrow: &'a mut HashMap<EntityID, bool>,
    object_results: &'a mut HashMap<EntityID, LifetimeAnalyzeResult>,
    variable_origins: &'a mut HashMap<EntityID, HashSet<EntityID>>,
    origin_user_types: &'a mut HashMap<EntityID, GlobalUserTypeID>,
}

pub fn analyze_lifetime<'input, 'allocator>(
    ast: &catla_parser::ast::Program<'input, 'allocator>,
    type_infer_results: &HashMap<EntityID, catla_parser::ast::Spanned<Type>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    global_implements_infos: &ImplementsInfoSet,
) -> LifetimeAnalyzeResults {
    let (definitions, symbol_by_entity, symbols_by_name) =
        collect_function_definitions("__single__", ast, module_entity_type_map);

    let interface_adjacency = build_interface_adjacency(global_implements_infos, user_type_set);
    let concrete_method_symbols = collect_concrete_method_symbols(global_implements_infos);
    let drop_required_user_types = collect_drop_required_user_types_for_module(
        module_entity_type_map,
        user_type_set,
        global_implements_infos,
    );

    let mut summaries = HashMap::<FunctionSymbol, FunctionSummary>::new();
    for definition in definitions.iter() {
        summaries
            .entry(definition.symbol.clone())
            .or_insert_with(|| FunctionSummary {
                parameter_escape: vec![false; definition.parameter_count],
                parameter_constraints: HashSet::new(),
                returns_object: false,
            });
    }

    let mut changed = true;
    let mut guard = 0usize;
    while changed && guard < 32 {
        guard += 1;
        changed = false;

        for definition in definitions.iter() {
            let local_summary = analyze_function_summary(
                definition,
                name_resolved_map,
                type_infer_results,
                &symbol_by_entity,
                &symbols_by_name,
                module_entity_type_map,
                user_type_set,
                &summaries,
                &interface_adjacency,
                &concrete_method_symbols,
                &drop_required_user_types,
            );

            let entry = summaries
                .entry(definition.symbol.clone())
                .or_insert_with(|| FunctionSummary {
                    parameter_escape: vec![false; definition.parameter_count],
                    parameter_constraints: HashSet::new(),
                    returns_object: false,
                });

            if entry.parameter_escape.len() < local_summary.parameter_escape.len() {
                entry
                    .parameter_escape
                    .resize(local_summary.parameter_escape.len(), false);
                changed = true;
            }

            for (index, value) in local_summary.parameter_escape.iter().enumerate() {
                if *value && !entry.parameter_escape[index] {
                    entry.parameter_escape[index] = true;
                    changed = true;
                }
            }

            for constraint in local_summary.parameter_constraints.iter().copied() {
                if entry.parameter_constraints.insert(constraint) {
                    changed = true;
                }
            }

            if local_summary.returns_object && !entry.returns_object {
                entry.returns_object = true;
                changed = true;
            }
        }
    }

    let mut results = LifetimeAnalyzeResults::new();
    let mut origin_user_types = HashMap::new();

    {
        let mut context = AnalyzerContext {
            name_resolved_map,
            type_infer_results,
            module_entity_type_map,
            user_type_set,
            symbol_by_entity: &symbol_by_entity,
            _symbols_by_name: &symbols_by_name,
            summaries: &summaries,
            interface_adjacency: &interface_adjacency,
            concrete_method_symbols: &concrete_method_symbols,
            drop_required_user_types: &drop_required_user_types,
            call_argument_borrow: &mut results.call_argument_borrow,
            object_results: &mut results.object_results,
            variable_origins: &mut results.variable_origins,
            origin_user_types: &mut origin_user_types,
        };

        for definition in definitions.iter() {
            analyze_function_with_final_summary(definition, &mut context);
        }
        analyze_module_level_roots(ast, &mut context);
    }

    results
}

fn collect_function_definitions<'ast, 'input, 'allocator>(
    module_name: &str,
    program: &'ast catla_parser::ast::Program<'input, 'allocator>,
    module_entity_type_map: &HashMap<EntityID, Type>,
) -> (
    Vec<FunctionDefinition<'ast, 'input, 'allocator>>,
    HashMap<EntityID, FunctionSymbol>,
    HashMap<String, Vec<FunctionSymbol>>,
) {
    let mut definitions = Vec::new();
    let mut symbol_by_entity = HashMap::new();
    let mut symbols_by_name = HashMap::<String, Vec<FunctionSymbol>>::new();

    fn register_function<'ast, 'input, 'allocator>(
        module_name: &str,
        function_define: &'ast FunctionDefine<'input, 'allocator>,
        module_entity_type_map: &HashMap<EntityID, Type>,
        definitions: &mut Vec<FunctionDefinition<'ast, 'input, 'allocator>>,
        symbol_by_entity: &mut HashMap<EntityID, FunctionSymbol>,
        symbols_by_name: &mut HashMap<String, Vec<FunctionSymbol>>,
    ) {
        let function_entity = EntityID::from(function_define);

        let parameter_bindings = collect_parameter_bindings(function_define);
        let parameter_count = function_define
            .arguments
            .as_ref()
            .map(|arguments| {
                arguments.arguments.len() + usize::from(arguments.this_mutability.is_some())
            })
            .unwrap_or(0);

        let Some(symbol) = function_symbol_for_entity(function_entity, module_entity_type_map) else {
            return;
        };

        symbol_by_entity.insert(function_entity, symbol.clone());
        symbols_by_name
            .entry(symbol.name.clone())
            .or_default()
            .push(symbol.clone());

        definitions.push(FunctionDefinition {
            module_name: module_name.to_string(),
            symbol,
            function_entity,
            ast: function_define,
            parameter_bindings,
            parameter_count,
        });
    }

    fn walk_program<'ast, 'input, 'allocator>(
        module_name: &str,
        program: &'ast catla_parser::ast::Program<'input, 'allocator>,
        module_entity_type_map: &HashMap<EntityID, Type>,
        definitions: &mut Vec<FunctionDefinition<'ast, 'input, 'allocator>>,
        symbol_by_entity: &mut HashMap<EntityID, FunctionSymbol>,
        symbols_by_name: &mut HashMap<String, Vec<FunctionSymbol>>,
    ) {
        for statement in program.statements.iter() {
            match &statement.statement {
                Statement::DefineWithAttribute(define_with_attribute) => {
                    if let Ok(define) = &define_with_attribute.define {
                        match define {
                            Define::Function(function_define) => register_function(
                                module_name,
                                function_define,
                                module_entity_type_map,
                                definitions,
                                symbol_by_entity,
                                symbols_by_name,
                            ),
                            Define::UserType(user_type_define) => {
                                if let Ok(block) = &user_type_define.block {
                                    walk_program(
                                        module_name,
                                        block.program,
                                        module_entity_type_map,
                                        definitions,
                                        symbol_by_entity,
                                        symbols_by_name,
                                    );
                                }
                            }
                            _ => {}
                        }
                    }
                }
                Statement::Implements(implements) => {
                    if let Ok(block) = &implements.block {
                        walk_program(
                            module_name,
                            block.program,
                            module_entity_type_map,
                            definitions,
                            symbol_by_entity,
                            symbols_by_name,
                        );
                    }
                }
                _ => {}
            }
        }
    }

    walk_program(
        module_name,
        program,
        module_entity_type_map,
        &mut definitions,
        &mut symbol_by_entity,
        &mut symbols_by_name,
    );

    (definitions, symbol_by_entity, symbols_by_name)
}

fn collect_parameter_bindings(function_define: &FunctionDefine) -> Vec<ParameterBinding> {
    let mut parameter_bindings = Vec::new();

    let mut parameter_index = 0usize;
    if let Ok(arguments) = &function_define.arguments {
        if arguments.this_mutability.is_some() {
            parameter_index += 1;
        }

        for argument in arguments.arguments.iter() {
            collect_binding_literals(&argument.binding, parameter_index, &mut parameter_bindings);
            parameter_index += 1;
        }
    }

    parameter_bindings
}

fn collect_binding_literals(
    binding: &VariableBinding,
    parameter_index: usize,
    output: &mut Vec<ParameterBinding>,
) {
    match binding {
        VariableBinding::Literal(literal) => output.push(ParameterBinding {
            variable_entity: EntityID::from(literal),
            parameter_index,
        }),
        VariableBinding::Binding { bindings, span: _ } => {
            for item in bindings.iter() {
                collect_binding_literals(item, parameter_index, output);
            }
        }
    }
}

fn function_symbol_for_entity(
    function_entity: EntityID,
    module_entity_type_map: &HashMap<EntityID, Type>,
) -> Option<FunctionSymbol> {
    let ty = module_entity_type_map.get(&function_entity)?;
    function_symbol_from_type(ty)
}

fn function_symbol_from_type(ty: &Type) -> Option<FunctionSymbol> {
    let Type::Function {
        function_info,
        generics: _,
    } = ty
    else {
        return None;
    };

    let name = function_info.name.as_ref()?.value.to_string();

    Some(FunctionSymbol {
        module_path: function_info.module_path.path_name.to_string(),
        name,
        arity: function_info.arguments.len(),
    })
}

fn build_interface_adjacency(
    global_implements_infos: &ImplementsInfoSet,
    user_type_set: &GlobalUserTypeSet,
) -> HashMap<FunctionSymbol, HashSet<FunctionSymbol>> {
    let mut adjacency = HashMap::<FunctionSymbol, HashSet<FunctionSymbol>>::new();

    for implements_info in global_implements_infos.all_infos().into_iter() {
        if implements_info.is_instant {
            continue;
        }

        let Type::UserType {
            user_type_info,
            generics: _,
        } = &implements_info.interface.value
        else {
            continue;
        };

        let interface_info = user_type_set.get(*user_type_info);
        let interface_info = interface_info.read().unwrap();

        if interface_info
            .kind
            .as_ref()
            .map(|kind| kind.value != UserTypeKind::Interface)
            .unwrap_or(true)
        {
            continue;
        }

        for (element_name, impl_type) in implements_info.element_type.iter() {
            let Some(impl_symbol) = function_symbol_from_type(&impl_type.value) else {
                continue;
            };

            let Some(interface_type) = interface_info.element_types.get(element_name) else {
                continue;
            };

            let Some(interface_symbol) = function_symbol_from_type(&interface_type.value) else {
                continue;
            };

            adjacency
                .entry(interface_symbol.clone())
                .or_default()
                .insert(impl_symbol.clone());
            adjacency
                .entry(impl_symbol.clone())
                .or_default()
                .insert(interface_symbol.clone());
        }
    }

    adjacency
}

fn collect_concrete_method_symbols(
    global_implements_infos: &ImplementsInfoSet,
) -> HashMap<GlobalUserTypeID, Vec<FunctionSymbol>> {
    let mut concrete_method_symbols = HashMap::<GlobalUserTypeID, Vec<FunctionSymbol>>::new();

    for implements_info in global_implements_infos.all_infos().into_iter() {
        if implements_info.is_instant {
            continue;
        }

        let Type::UserType {
            user_type_info: concrete_user_type_id,
            generics: _,
        } = &implements_info.concrete.value
        else {
            continue;
        };

        for impl_type in implements_info.element_type.values() {
            let Some(symbol) = function_symbol_from_type(&impl_type.value) else {
                continue;
            };

            concrete_method_symbols
                .entry(*concrete_user_type_id)
                .or_default()
                .push(symbol);
        }
    }

    for symbols in concrete_method_symbols.values_mut() {
        let mut dedup = HashSet::new();
        symbols.retain(|symbol| dedup.insert(symbol.clone()));
    }

    concrete_method_symbols
}

fn analyze_function_summary<'ast, 'input, 'allocator>(
    definition: &FunctionDefinition<'ast, 'input, 'allocator>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    type_infer_results: &HashMap<EntityID, catla_parser::ast::Spanned<Type>>,
    symbol_by_entity: &HashMap<EntityID, FunctionSymbol>,
    symbols_by_name: &HashMap<String, Vec<FunctionSymbol>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    user_type_set: &GlobalUserTypeSet,
    summaries: &HashMap<FunctionSymbol, FunctionSummary>,
    interface_adjacency: &HashMap<FunctionSymbol, HashSet<FunctionSymbol>>,
    concrete_method_symbols: &HashMap<GlobalUserTypeID, Vec<FunctionSymbol>>,
    drop_required_user_types: &HashSet<GlobalUserTypeID>,
) -> FunctionSummary {
    let mut object_results = HashMap::new();
    let mut call_argument_borrow = HashMap::new();
    let mut variable_origins = HashMap::new();
    let mut origin_user_types = HashMap::new();
    let mut context = AnalyzerContext {
        name_resolved_map,
        type_infer_results,
        module_entity_type_map,
        user_type_set,
        symbol_by_entity,
        _symbols_by_name: symbols_by_name,
        summaries,
        interface_adjacency,
        concrete_method_symbols,
        drop_required_user_types,
        call_argument_borrow: &mut call_argument_borrow,
        object_results: &mut object_results,
        variable_origins: &mut variable_origins,
        origin_user_types: &mut origin_user_types,
    };

    let mut state = FunctionState::new(definition.parameter_count);
    let mut parameter_tree_refs = Vec::with_capacity(definition.parameter_count);
    for _ in 0..definition.parameter_count {
        parameter_tree_refs.push(state.lifetime_tree.create_node(None, None, true));
    }

    for binding in definition.parameter_bindings.iter() {
        state
            .aliases_to_params
            .entry(binding.variable_entity)
            .or_default()
            .insert(binding.parameter_index);
        if let Some(param_tree_ref) = parameter_tree_refs.get(binding.parameter_index).copied() {
            state
                .aliases_to_tree_refs
                .entry(binding.variable_entity)
                .or_default()
                .insert(param_tree_ref);
        }
    }

    if let Some(block) = &definition.ast.block {
        analyze_program_statements(block.program, &mut state, &mut context, false, false);
    }

    FunctionSummary {
        parameter_escape: state.parameter_escape,
        parameter_constraints: state.parameter_constraints,
        returns_object: function_returns_object(definition, module_entity_type_map, user_type_set),
    }
}

fn function_returns_object(
    definition: &FunctionDefinition,
    module_entity_type_map: &HashMap<EntityID, Type>,
    user_type_set: &GlobalUserTypeSet,
) -> bool {
    let Some(function_type) = module_entity_type_map.get(&definition.function_entity) else {
        return false;
    };
    let Type::Function { function_info, .. } = function_type else {
        return false;
    };

    is_class_type(&function_info.return_type.value, user_type_set)
}

fn analyze_function_with_final_summary<'ast, 'input, 'allocator>(
    definition: &FunctionDefinition<'ast, 'input, 'allocator>,
    context: &mut AnalyzerContext,
) {
    let mut state = FunctionState::new(definition.parameter_count);
    let mut parameter_tree_refs = Vec::with_capacity(definition.parameter_count);
    for _ in 0..definition.parameter_count {
        parameter_tree_refs.push(state.lifetime_tree.create_node(None, None, true));
    }

    for binding in definition.parameter_bindings.iter() {
        state
            .aliases_to_params
            .entry(binding.variable_entity)
            .or_default()
            .insert(binding.parameter_index);
        if let Some(param_tree_ref) = parameter_tree_refs.get(binding.parameter_index).copied() {
            state
                .aliases_to_tree_refs
                .entry(binding.variable_entity)
                .or_default()
                .insert(param_tree_ref);
        }
    }

    if let Some(block) = &definition.ast.block {
        analyze_program_statements(block.program, &mut state, context, true, false);
    }

    solve_constraints_for_function(&state, context.object_results);
}

fn analyze_module_level_roots<'input, 'allocator>(
    program: &catla_parser::ast::Program<'input, 'allocator>,
    context: &mut AnalyzerContext,
) {
    let mut state = FunctionState::new(0);
    analyze_program_statements(program, &mut state, context, true, true);
    solve_constraints_for_function(&state, context.object_results);
}

fn analyze_program_statements<'input, 'allocator>(
    program: &catla_parser::ast::Program<'input, 'allocator>,
    state: &mut FunctionState,
    context: &mut AnalyzerContext,
    enable_outputs: bool,
    analyze_nested_defines: bool,
) {
    for statement in program.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                if let Ok(right) = &assignment.right {
                    let right_flow = analyze_expression(right, state, context, false, enable_outputs);

                    if let Some((left_variable, crosses_function_boundary)) = extract_variable_entity(
                        &assignment.left,
                        &state.aliases_to_params,
                        context.name_resolved_map,
                    ) {
                        if crosses_function_boundary {
                            apply_escape(&right_flow, state, context.object_results);
                        }
                        state
                            .aliases_to_params
                            .insert(left_variable, right_flow.params.clone());
                        state
                            .aliases_to_origins
                            .insert(left_variable, right_flow.origins.clone());
                        state
                            .aliases_to_tree_refs
                            .insert(left_variable, right_flow.tree_refs.clone());
                        context
                            .variable_origins
                            .insert(left_variable, right_flow.origins.clone());
                    } else {
                        let left_flow =
                            analyze_expression(&assignment.left, state, context, false, enable_outputs);

                        if left_flow.params.is_empty() && left_flow.origins.is_empty() {
                            apply_escape(&right_flow, state, context.object_results);
                        } else {
                            add_constraint_between_flows(&left_flow, &right_flow, state);
                        }
                    }
                }
            }
            Statement::Swap(swap_statement) => {
                if let Ok(right) = &swap_statement.right {
                    let right_flow = analyze_expression(right, state, context, false, enable_outputs);
                    apply_escape(&right_flow, state, context.object_results);
                }
            }
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Variable(variable_define) => {
                            let is_static_variable = define_with_attribute
                                .attribute
                                .iter()
                                .any(|attribute| attribute.value == StatementAttribute::Static);
                            if let Ok(binding) = &variable_define.binding {
                                let expression_flow = variable_define
                                    .expression
                                    .as_ref()
                                    .map(|expression| {
                                        analyze_expression(
                                            expression,
                                            state,
                                            context,
                                            false,
                                            enable_outputs,
                                        )
                                    })
                                    .unwrap_or_default();

                                if is_static_variable {
                                    for tree_ref in expression_flow.tree_refs.iter().copied() {
                                        state.lifetime_tree.mark_static_lifetime(tree_ref);
                                    }
                                    apply_escape(&expression_flow, state, context.object_results);
                                }

                                register_binding_alias(
                                    binding,
                                    &expression_flow,
                                    state,
                                    context.variable_origins,
                                    context.name_resolved_map,
                                );
                            }
                        }
                        Define::Function(_) | Define::UserType(_) | Define::TypeAlias(_) => {
                            if analyze_nested_defines {
                                if let Define::UserType(user_type_define) = define {
                                    if let Ok(block) = &user_type_define.block {
                                        analyze_program_statements(
                                            block.program,
                                            state,
                                            context,
                                            enable_outputs,
                                            true,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Statement::Drop(drop_statement) => {
                if let Ok(expression) = &drop_statement.expression {
                    analyze_expression(expression, state, context, false, enable_outputs);
                }
            }
            Statement::Expression(expression) => {
                analyze_expression(expression, state, context, false, enable_outputs);
            }
            Statement::Implements(implements) => {
                if analyze_nested_defines {
                    if let Ok(block) = &implements.block {
                        analyze_program_statements(
                            block.program,
                            state,
                            context,
                            enable_outputs,
                            true,
                        );
                    }
                }
            }
            Statement::Import(_) => {}
        }
    }
}

fn register_binding_alias(
    binding: &VariableBinding,
    flow: &ExprFlow,
    state: &mut FunctionState,
    variable_origins: &mut HashMap<EntityID, HashSet<EntityID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
) {
    match binding {
        VariableBinding::Literal(literal) => {
            let entity = name_resolved_map
                .get(&EntityID::from(literal))
                .map(|resolved| resolved.define.entity_id)
                .unwrap_or_else(|| EntityID::from(literal));

            state.aliases_to_params.insert(entity, flow.params.clone());
            state.aliases_to_origins.insert(entity, flow.origins.clone());
            state.aliases_to_tree_refs.insert(entity, flow.tree_refs.clone());
            variable_origins.insert(entity, flow.origins.clone());

            for tree_ref in flow.tree_refs.iter().copied() {
                state
                    .lifetime_tree
                    .set_drop_order(tree_ref, literal.span.start);
            }
        }
        VariableBinding::Binding { bindings, span: _ } => {
            for item in bindings.iter() {
                register_binding_alias(item, flow, state, variable_origins, name_resolved_map);
            }
        }
    }
}

fn analyze_expression(
    expression: &Expression,
    state: &mut FunctionState,
    context: &mut AnalyzerContext,
    escape_context: bool,
    enable_outputs: bool,
) -> ExprFlow {
    let flow = match expression {
        Expression::Return(return_expression) => {
            let inner = return_expression
                .expression
                .as_ref()
                .map(|expr| analyze_expression(expr, state, context, true, enable_outputs))
                .unwrap_or_default();

            inner
        }
        Expression::Closure(_closure) => ExprFlow::default(),
        Expression::Or(or_expression) => {
            analyze_or_expression(or_expression, state, context, escape_context, enable_outputs)
        }
    };

    if escape_context {
        apply_escape(&flow, state, context.object_results);
    }

    flow
}

fn analyze_or_expression(
    expression: &OrExpression,
    state: &mut FunctionState,
    context: &mut AnalyzerContext,
    escape_context: bool,
    enable_outputs: bool,
) -> ExprFlow {
    let mut flow = analyze_and_expression(
        &expression.left,
        state,
        context,
        escape_context,
        enable_outputs,
    );

    for chain in expression.chain.iter() {
        flow.merge(analyze_and_expression(
            chain,
            state,
            context,
            escape_context,
            enable_outputs,
        ));
    }

    flow
}

fn analyze_and_expression(
    expression: &AndExpression,
    state: &mut FunctionState,
    context: &mut AnalyzerContext,
    escape_context: bool,
    enable_outputs: bool,
) -> ExprFlow {
    let mut flow = analyze_equals_expression(
        &expression.left,
        state,
        context,
        escape_context,
        enable_outputs,
    );

    for chain in expression.chain.iter() {
        flow.merge(analyze_equals_expression(
            chain,
            state,
            context,
            escape_context,
            enable_outputs,
        ));
    }

    flow
}

fn analyze_equals_expression(
    expression: &EqualsExpression,
    state: &mut FunctionState,
    context: &mut AnalyzerContext,
    escape_context: bool,
    enable_outputs: bool,
) -> ExprFlow {
    let mut flow = analyze_less_or_greater_expression(
        &expression.left,
        state,
        context,
        escape_context,
        enable_outputs,
    );

    for (_, chain) in expression.chain.iter() {
        flow.merge(analyze_less_or_greater_expression(
            chain,
            state,
            context,
            escape_context,
            enable_outputs,
        ));
    }

    flow
}

fn analyze_less_or_greater_expression(
    expression: &LessOrGreaterExpression,
    state: &mut FunctionState,
    context: &mut AnalyzerContext,
    escape_context: bool,
    enable_outputs: bool,
) -> ExprFlow {
    let mut flow = analyze_add_or_sub_expression(
        &expression.left,
        state,
        context,
        escape_context,
        enable_outputs,
    );

    for (_, chain) in expression.chain.iter() {
        flow.merge(analyze_add_or_sub_expression(
            chain,
            state,
            context,
            escape_context,
            enable_outputs,
        ));
    }

    flow
}

fn analyze_add_or_sub_expression(
    expression: &AddOrSubExpression,
    state: &mut FunctionState,
    context: &mut AnalyzerContext,
    escape_context: bool,
    enable_outputs: bool,
) -> ExprFlow {
    let mut flow = analyze_mul_or_div_expression(
        &expression.left,
        state,
        context,
        escape_context,
        enable_outputs,
    );

    for (_, chain) in expression.chain.iter() {
        flow.merge(analyze_mul_or_div_expression(
            chain,
            state,
            context,
            escape_context,
            enable_outputs,
        ));
    }

    flow
}

fn analyze_mul_or_div_expression(
    expression: &MulOrDivExpression,
    state: &mut FunctionState,
    context: &mut AnalyzerContext,
    escape_context: bool,
    enable_outputs: bool,
) -> ExprFlow {
    let mut flow = analyze_factor(
        &expression.left,
        state,
        context,
        escape_context,
        enable_outputs,
    );

    for (_, chain) in expression.chain.iter() {
        flow.merge(analyze_factor(
            chain,
            state,
            context,
            escape_context,
            enable_outputs,
        ));
    }

    flow
}

fn analyze_factor(
    factor: &Factor,
    state: &mut FunctionState,
    context: &mut AnalyzerContext,
    escape_context: bool,
    enable_outputs: bool,
) -> ExprFlow {
    if let Ok(primary) = &factor.primary {
        analyze_primary(primary, state, context, escape_context, enable_outputs)
    } else {
        ExprFlow::default()
    }
}

fn analyze_primary(
    primary: &Primary,
    state: &mut FunctionState,
    context: &mut AnalyzerContext,
    escape_context: bool,
    enable_outputs: bool,
) -> ExprFlow {
    let mut current_flow =
        analyze_primary_left(&primary.left.first, state, context, escape_context, enable_outputs);

    for chain in primary.chain.iter() {
        if let Some(second) = &chain.second {
            if let Some(function_call) = &second.function_call {
                let mut argument_flows = Vec::new();
                for argument in function_call.arguments.iter() {
                    argument_flows.push(analyze_expression(
                        argument,
                        state,
                        context,
                        false,
                        enable_outputs,
                    ));
                }

                let is_method_call = chain.separator.value == catla_parser::ast::PrimarySeparator::Dot;

                let mut call_inputs = Vec::new();
                if is_method_call {
                    call_inputs.push(current_flow.clone());
                }
                call_inputs.extend(argument_flows.iter().cloned());

                let candidates = resolve_call_candidates(
                    second.literal.value,
                    function_call.arguments.len(),
                    is_method_call,
                    Some(&second.literal),
                    if is_method_call {
                        Some(&current_flow)
                    } else {
                        None
                    },
                    context,
                );

                let parameter_escape = merged_parameter_escape(
                    &candidates,
                    call_inputs.len(),
                    context.summaries,
                    context.interface_adjacency,
                );
                let parameter_constraints = merged_parameter_constraints(
                    &candidates,
                    context.summaries,
                    context.interface_adjacency,
                );

                for (index, flow) in call_inputs.iter().enumerate() {
                    if parameter_escape[index] {
                        apply_escape(flow, state, context.object_results);
                    }
                }

                for (shorter, longer) in parameter_constraints.into_iter() {
                    let Some(shorter_flow) = call_inputs.get(shorter) else {
                        continue;
                    };
                    let Some(longer_flow) = call_inputs.get(longer) else {
                        continue;
                    };

                    add_constraint_between_flows(shorter_flow, longer_flow, state);
                }

                if enable_outputs {
                    for (index, argument) in function_call.arguments.iter().enumerate() {
                        let parameter_index = if is_method_call { index + 1 } else { index };
                        if !parameter_escape[parameter_index] && is_borrowable_expression(argument, context)
                        {
                            context
                                .call_argument_borrow
                                .insert(EntityID::from(argument), true);
                        }
                    }
                }

                let mut return_flow = ExprFlow::default();
                if merged_return_object(
                    &candidates,
                    context.summaries,
                    context.interface_adjacency,
                ) {
                    let return_origin = EntityID::from(function_call);
                    return_flow.origins.insert(return_origin);
                    context
                        .object_results
                        .entry(return_origin)
                        .or_insert(LifetimeAnalyzeResult {
                            allocation: AllocationKind::Heap,
                            requires_drop: false,
                        });
                }

                current_flow = return_flow;
            }
        }
    }

    if escape_context {
        apply_escape(&current_flow, state, context.object_results);
    }

    current_flow
}

fn analyze_primary_left(
    left: &PrimaryLeftExpr,
    state: &mut FunctionState,
    context: &mut AnalyzerContext,
    escape_context: bool,
    enable_outputs: bool,
) -> ExprFlow {
    match left {
        PrimaryLeftExpr::Simple {
            left,
            generics: _,
            function_call,
            span: _,
        } => {
            let mut base_flow = match left {
                catla_parser::ast::SimplePrimary::Tuple {
                    expressions,
                    span: _,
                } => {
                    let mut tuple_flow = ExprFlow::default();
                    for expression in expressions.iter() {
                        tuple_flow.merge(analyze_expression(
                            expression,
                            state,
                            context,
                            false,
                            enable_outputs,
                        ));
                    }
                    tuple_flow
                }
                catla_parser::ast::SimplePrimary::Literal(literal) => {
                    flow_for_literal(EntityID::from(literal), state, context.name_resolved_map)
                }
                catla_parser::ast::SimplePrimary::StringLiteral(_)
                | catla_parser::ast::SimplePrimary::NumericLiteral(_)
                | catla_parser::ast::SimplePrimary::Null(_)
                | catla_parser::ast::SimplePrimary::True(_)
                | catla_parser::ast::SimplePrimary::False(_)
                | catla_parser::ast::SimplePrimary::This(_)
                | catla_parser::ast::SimplePrimary::LargeThis(_) => ExprFlow::default(),
            };

            if let Some(function_call) = function_call {
                let callee_name = match left {
                    catla_parser::ast::SimplePrimary::Literal(literal) => Some(literal.value),
                    _ => None,
                };

                let mut argument_flows = Vec::new();
                for argument in function_call.arguments.iter() {
                    argument_flows.push(analyze_expression(
                        argument,
                        state,
                        context,
                        false,
                        enable_outputs,
                    ));
                }

                let candidates = callee_name
                    .map(|name| {
                        resolve_call_candidates(
                            name,
                            function_call.arguments.len(),
                            false,
                            match left {
                                catla_parser::ast::SimplePrimary::Literal(literal) => Some(literal),
                                _ => None,
                            },
                            None,
                            context,
                        )
                    })
                    .unwrap_or_default();

                let parameter_escape = merged_parameter_escape(
                    &candidates,
                    argument_flows.len(),
                    context.summaries,
                    context.interface_adjacency,
                );
                let parameter_constraints = merged_parameter_constraints(
                    &candidates,
                    context.summaries,
                    context.interface_adjacency,
                );

                for (index, flow) in argument_flows.iter().enumerate() {
                    if parameter_escape[index] {
                        apply_escape(flow, state, context.object_results);
                    }
                }

                for (shorter, longer) in parameter_constraints.into_iter() {
                    let Some(shorter_flow) = argument_flows.get(shorter) else {
                        continue;
                    };
                    let Some(longer_flow) = argument_flows.get(longer) else {
                        continue;
                    };

                    add_constraint_between_flows(shorter_flow, longer_flow, state);
                }

                if enable_outputs {
                    for (index, argument) in function_call.arguments.iter().enumerate() {
                        if !parameter_escape[index] && is_borrowable_expression(argument, context) {
                            context
                                .call_argument_borrow
                                .insert(EntityID::from(argument), true);
                        }
                    }
                }

                let mut return_flow = ExprFlow::default();
                if merged_return_object(
                    &candidates,
                    context.summaries,
                    context.interface_adjacency,
                ) {
                    let return_origin = EntityID::from(function_call);
                    return_flow.origins.insert(return_origin);
                    context
                        .object_results
                        .entry(return_origin)
                        .or_insert(LifetimeAnalyzeResult {
                            allocation: AllocationKind::Heap,
                            requires_drop: false,
                        });
                }

                base_flow = return_flow;
            }

            if escape_context {
                apply_escape(&base_flow, state, context.object_results);
            }

            base_flow
        }
        PrimaryLeftExpr::NewObject { new_object } => {
            let mut field_flows = Vec::new();
            for field in new_object.field_assign.elements.iter() {
                let field_flow =
                    analyze_expression(&field.expression, state, context, false, enable_outputs);
                field_flows.push((field.field.value.to_string(), field_flow));
            }

            let mut flow = ExprFlow::default();
            let new_object_entity = EntityID::from(new_object);

            if is_class_new_object(new_object, context) {
                let concrete_user_type = new_object
                    .path
                    .first()
                    .and_then(|_| resolve_new_object_user_type(new_object, context))
                    .inspect(|user_type_id| {
                        context
                            .origin_user_types
                            .insert(new_object_entity, *user_type_id);
                    });
                let requires_drop = concrete_user_type
                    .map(|user_type_id| context.drop_required_user_types.contains(&user_type_id))
                    .unwrap_or(false);

                context.object_results.entry(new_object_entity).or_insert(LifetimeAnalyzeResult {
                    allocation: AllocationKind::Stack,
                    requires_drop,
                });
                flow.origins.insert(new_object_entity);
                let owner_ref = match state.origin_tree_refs.get(&new_object_entity).copied() {
                    Some(existing) => existing,
                    None => {
                        let created = state.lifetime_tree.create_node(
                            Some(new_object_entity),
                            Some(new_object.span.start),
                            false,
                        );
                        state.origin_tree_refs.insert(new_object_entity, created);
                        created
                    }
                };
                state
                    .lifetime_tree
                    .set_drop_order(owner_ref, new_object.span.start);
                flow.tree_refs.insert(owner_ref);

                let mut owner_flow = ExprFlow::default();
                owner_flow.origins.insert(new_object_entity);
                owner_flow.tree_refs.insert(owner_ref);

                for (field_name, field_flow) in field_flows.into_iter() {
                    let child_ref = state.lifetime_tree.get_or_create_child(owner_ref, &field_name);
                    state.lifetime_tree.set_drop_order(child_ref, new_object.span.start);

                    for borrowed_ref in field_flow.tree_refs.iter().copied() {
                        state.lifetime_tree.add_borrow(child_ref, borrowed_ref);
                        state.tree_constraints.insert((child_ref, borrowed_ref));
                    }

                    add_constraint_between_flows(&owner_flow, &field_flow, state);
                }
            } else {
                for (_, field_flow) in field_flows.into_iter() {
                    flow.merge(field_flow);
                }
            }

            if escape_context {
                apply_escape(&flow, state, context.object_results);
            }

            flow
        }
        PrimaryLeftExpr::NewArray { new_array } => {
            let mut flow = ExprFlow::default();
            for expression in new_array.elements.iter() {
                flow.merge(analyze_expression(
                    expression,
                    state,
                    context,
                    false,
                    enable_outputs,
                ));
            }
            if escape_context {
                apply_escape(&flow, state, context.object_results);
            }
            flow
        }
        PrimaryLeftExpr::NewArrayInit { new_array_init } => {
            let mut flow = ExprFlow::default();
            if let Ok(init_expression) = &new_array_init.init_expression {
                flow.merge(analyze_expression(
                    init_expression,
                    state,
                    context,
                    false,
                    enable_outputs,
                ));
            }
            if let Ok(length_expression) = &new_array_init.length_expression {
                flow.merge(analyze_expression(
                    length_expression,
                    state,
                    context,
                    false,
                    enable_outputs,
                ));
            }
            if escape_context {
                apply_escape(&flow, state, context.object_results);
            }
            flow
        }
        PrimaryLeftExpr::If { if_expression } => {
            let mut flow = ExprFlow::default();
            if let Ok(condition) = &if_expression.first.condition {
                flow.merge(analyze_expression(
                    condition,
                    state,
                    context,
                    false,
                    enable_outputs,
                ));
            }
            if let Ok(block) = &if_expression.first.block {
                analyze_program_statements(block.program, state, context, enable_outputs, false);
            }
            for chain in if_expression.chain.iter() {
                match chain {
                    catla_parser::ast::ElseChain::ElseIf { if_statement } => {
                        if let Ok(condition) = &if_statement.condition {
                            flow.merge(analyze_expression(
                                condition,
                                state,
                                context,
                                false,
                                enable_outputs,
                            ));
                        }
                        if let Ok(block) = &if_statement.block {
                            analyze_program_statements(
                                block.program,
                                state,
                                context,
                                enable_outputs,
                                false,
                            );
                        }
                    }
                    catla_parser::ast::ElseChain::Else { block } => {
                        analyze_program_statements(
                            block.program,
                            state,
                            context,
                            enable_outputs,
                            false,
                        );
                    }
                }
            }
            if escape_context {
                apply_escape(&flow, state, context.object_results);
            }
            flow
        }
        PrimaryLeftExpr::Loop { loop_expression } => {
            if let Ok(block) = &loop_expression.block {
                analyze_program_statements(block.program, state, context, enable_outputs, false);
            }
            ExprFlow::default()
        }
    }
}

fn flow_for_literal(
    literal_entity: EntityID,
    state: &FunctionState,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
) -> ExprFlow {
    let mut flow = ExprFlow::default();

    let Some(resolved) = name_resolved_map.get(&literal_entity) else {
        return flow;
    };

    if resolved.define.kind != DefineKind::Variable {
        return flow;
    }

    let variable_entity = resolved.define.entity_id;

    if let Some(params) = state.aliases_to_params.get(&variable_entity) {
        flow.params.extend(params.iter().copied());
    }
    if let Some(origins) = state.aliases_to_origins.get(&variable_entity) {
        flow.origins.extend(origins.iter().copied());
    }
    if let Some(tree_refs) = state.aliases_to_tree_refs.get(&variable_entity) {
        flow.tree_refs.extend(tree_refs.iter().copied());
    }

    flow
}

fn apply_escape(
    flow: &ExprFlow,
    state: &mut FunctionState,
    object_results: &mut HashMap<EntityID, LifetimeAnalyzeResult>,
) {
    for parameter_index in flow.params.iter().copied() {
        if let Some(escape) = state.parameter_escape.get_mut(parameter_index) {
            *escape = true;
        }
    }

    for object_entity in flow.origins.iter().copied() {
        set_object_heap(object_results, object_entity);
    }

    for object_entity in state
        .lifetime_tree
        .collect_reachable_alloc_origins(&flow.tree_refs)
        .into_iter()
    {
        set_object_heap(object_results, object_entity);
    }
}

fn add_constraint_between_flows(shorter: &ExprFlow, longer: &ExprFlow, state: &mut FunctionState) {
    for shorter_param in shorter.params.iter().copied() {
        for longer_param in longer.params.iter().copied() {
            state
                .parameter_constraints
                .insert((shorter_param, longer_param));
        }

        for longer_tree_ref in longer.tree_refs.iter().copied() {
            state
                .param_to_tree_constraints
                .insert((shorter_param, longer_tree_ref));
        }
    }

    for shorter_tree_ref in shorter.tree_refs.iter().copied() {
        for longer_tree_ref in longer.tree_refs.iter().copied() {
            state
                .tree_constraints
                .insert((shorter_tree_ref, longer_tree_ref));
        }
    }
}

fn solve_constraints_for_function(
    state: &FunctionState,
    object_results: &mut HashMap<EntityID, LifetimeAnalyzeResult>,
) {
    let mut tree_constraints = state.tree_constraints.clone();
    tree_constraints.extend(state.lifetime_tree.all_expected_constraints());

    let mut changed = true;
    let mut guard = 0usize;

    while changed && guard < 64 {
        guard += 1;
        changed = false;

        for (_, longer_tree_ref) in state.param_to_tree_constraints.iter().copied() {
            if promote_tree_heap(longer_tree_ref, state, object_results) {
                changed = true;
            }
        }

        for (shorter_tree_ref, longer_tree_ref) in tree_constraints.iter().copied() {
            let is_satisfied = is_tree_constraint_satisfied(
                shorter_tree_ref,
                longer_tree_ref,
                state,
                object_results,
            );

            if !is_satisfied && promote_tree_heap(longer_tree_ref, state, object_results) {
                changed = true;
            }
        }
    }
}

fn promote_tree_heap(
    tree_ref: LifetimeTreeRef,
    state: &FunctionState,
    object_results: &mut HashMap<EntityID, LifetimeAnalyzeResult>,
) -> bool {
    let Some(origin) = state.lifetime_tree.alloc_origin(tree_ref) else {
        return false;
    };

    set_object_heap(object_results, origin)
}

fn set_object_heap(
    object_results: &mut HashMap<EntityID, LifetimeAnalyzeResult>,
    object_entity: EntityID,
) -> bool {
    let entry = object_results
        .entry(object_entity)
        .or_insert(LifetimeAnalyzeResult {
            allocation: AllocationKind::Stack,
            requires_drop: false,
        });

    if entry.allocation == AllocationKind::Heap {
        return false;
    }

    entry.allocation = AllocationKind::Heap;
    true
}

fn is_tree_constraint_satisfied(
    shorter_tree_ref: LifetimeTreeRef,
    longer_tree_ref: LifetimeTreeRef,
    state: &FunctionState,
    object_results: &HashMap<EntityID, LifetimeAnalyzeResult>,
) -> bool {
    let shorter_heap = state
        .lifetime_tree
        .alloc_origin(shorter_tree_ref)
        .and_then(|origin| object_results.get(&origin))
        .map(|result| result.allocation == AllocationKind::Heap)
        .unwrap_or(false);
    let longer_heap = state
        .lifetime_tree
        .alloc_origin(longer_tree_ref)
        .and_then(|origin| object_results.get(&origin))
        .map(|result| result.allocation == AllocationKind::Heap)
        .unwrap_or(false);

    if longer_heap {
        return true;
    }
    if state.lifetime_tree.has_static_lifetime(longer_tree_ref) {
        return true;
    }
    if shorter_heap {
        return false;
    }

    is_tree_shorter(shorter_tree_ref, longer_tree_ref, state)
}

fn is_tree_shorter(
    shorter_tree_ref: LifetimeTreeRef,
    longer_tree_ref: LifetimeTreeRef,
    state: &FunctionState,
) -> bool {
    let shorter_static = state.lifetime_tree.has_static_lifetime(shorter_tree_ref);
    let longer_static = state.lifetime_tree.has_static_lifetime(longer_tree_ref);

    if shorter_static && !longer_static {
        return false;
    }
    if !shorter_static && longer_static {
        return true;
    }
    if shorter_static && longer_static {
        return true;
    }

    let Some(shorter_order) = state.lifetime_tree.drop_order(shorter_tree_ref) else {
        return false;
    };
    let Some(longer_order) = state.lifetime_tree.drop_order(longer_tree_ref) else {
        return false;
    };

    shorter_order >= longer_order
}

fn merged_parameter_escape(
    candidates: &[FunctionSymbol],
    parameter_count: usize,
    summaries: &HashMap<FunctionSymbol, FunctionSummary>,
    interface_adjacency: &HashMap<FunctionSymbol, HashSet<FunctionSymbol>>,
) -> Vec<bool> {
    if candidates.is_empty() {
        return vec![true; parameter_count];
    }

    let mut merged = vec![false; parameter_count];

    for parameter_index in 0..parameter_count {
        let mut any_known = false;
        let mut escaped = false;

        for candidate in candidates.iter() {
            let closure = equivalent_symbols(candidate, interface_adjacency);

            for symbol in closure.iter() {
                if let Some(summary) = summaries.get(symbol) {
                    any_known = true;

                    if parameter_index >= summary.parameter_escape.len()
                        || summary.parameter_escape[parameter_index]
                    {
                        escaped = true;
                        break;
                    }
                }
            }

            if escaped {
                break;
            }
        }

        merged[parameter_index] = if any_known { escaped } else { true };
    }

    merged
}

fn merged_parameter_constraints(
    candidates: &[FunctionSymbol],
    summaries: &HashMap<FunctionSymbol, FunctionSummary>,
    interface_adjacency: &HashMap<FunctionSymbol, HashSet<FunctionSymbol>>,
) -> HashSet<(usize, usize)> {
    let mut merged = HashSet::new();

    for candidate in candidates.iter() {
        let closure = equivalent_symbols(candidate, interface_adjacency);

        for symbol in closure.iter() {
            let Some(summary) = summaries.get(symbol) else {
                continue;
            };

            merged.extend(summary.parameter_constraints.iter().copied());
        }
    }

    merged
}

fn merged_return_object(
    candidates: &[FunctionSymbol],
    summaries: &HashMap<FunctionSymbol, FunctionSummary>,
    interface_adjacency: &HashMap<FunctionSymbol, HashSet<FunctionSymbol>>,
) -> bool {
    for candidate in candidates.iter() {
        let closure = equivalent_symbols(candidate, interface_adjacency);

        for symbol in closure.iter() {
            let Some(summary) = summaries.get(symbol) else {
                continue;
            };

            if summary.returns_object {
                return true;
            }
        }
    }

    false
}

fn equivalent_symbols(
    symbol: &FunctionSymbol,
    interface_adjacency: &HashMap<FunctionSymbol, HashSet<FunctionSymbol>>,
) -> HashSet<FunctionSymbol> {
    let mut visited = HashSet::new();
    let mut stack = vec![symbol.clone()];

    while let Some(current) = stack.pop() {
        if !visited.insert(current.clone()) {
            continue;
        }

        if let Some(next) = interface_adjacency.get(&current) {
            for item in next.iter() {
                if !visited.contains(item) {
                    stack.push(item.clone());
                }
            }
        }
    }

    visited
}

fn resolve_call_candidates(
    call_name: &str,
    argument_count: usize,
    is_method_call: bool,
    literal: Option<&catla_parser::ast::Spanned<&str>>,
    receiver_flow: Option<&ExprFlow>,
    context: &AnalyzerContext,
) -> Vec<FunctionSymbol> {
    if let Some(literal) = literal {
        if let Some(resolved) = context.name_resolved_map.get(&EntityID::from(literal)) {
            if resolved.define.kind == DefineKind::Function {
                if let Some(symbol) = context.symbol_by_entity.get(&resolved.define.entity_id) {
                    return vec![symbol.clone()];
                }
            }
        }
    }

    if is_method_call {
        let mut concrete_candidates = Vec::new();

        if let Some(receiver_flow) = receiver_flow {
            let mut receiver_user_types = HashSet::<GlobalUserTypeID>::new();

            for origin in receiver_flow.origins.iter().copied() {
                if let Some(user_type_id) = context.origin_user_types.get(&origin) {
                    receiver_user_types.insert(*user_type_id);
                    continue;
                }

                let Some(inferred_type) = context.type_infer_results.get(&origin) else {
                    continue;
                };
                let Type::UserType {
                    user_type_info,
                    generics: _,
                } = &inferred_type.value
                else {
                    continue;
                };

                receiver_user_types.insert(*user_type_info);
            }

            for user_type_id in receiver_user_types.into_iter() {
                let Some(method_symbols) = context.concrete_method_symbols.get(&user_type_id) else {
                    continue;
                };

                concrete_candidates.extend(method_symbols.iter().filter(|symbol| {
                    symbol.name == call_name
                        && (symbol.arity == argument_count || symbol.arity == argument_count + 1)
                }));
            }
        }

        if !concrete_candidates.is_empty() {
            let mut dedup = HashSet::new();
            return concrete_candidates
                .into_iter()
                .cloned()
                .filter(|symbol| dedup.insert(symbol.clone()))
                .collect();
        }
    }

    Vec::new()
}

fn is_borrowable_expression(expression: &Expression, context: &AnalyzerContext) -> bool {
    let Expression::Or(or_expression) = expression else {
        return false;
    };

    if !or_expression.chain.is_empty() {
        return false;
    }

    let and_expression = &or_expression.left;
    if !and_expression.chain.is_empty() {
        return false;
    }

    let equals_expression = &and_expression.left;
    if !equals_expression.chain.is_empty() {
        return false;
    }

    let less_or_greater_expression = &equals_expression.left;
    if !less_or_greater_expression.chain.is_empty() {
        return false;
    }

    let add_or_sub_expression = &less_or_greater_expression.left;
    if !add_or_sub_expression.chain.is_empty() {
        return false;
    }

    let mul_or_div_expression = &add_or_sub_expression.left;
    if !mul_or_div_expression.chain.is_empty() {
        return false;
    }

    let factor = &mul_or_div_expression.left;
    if factor.minus.is_some() {
        return false;
    }

    let Ok(primary) = &factor.primary else {
        return false;
    };

    if !primary.chain.is_empty() {
        return false;
    }

    let PrimaryLeftExpr::Simple {
        left,
        generics: _,
        function_call,
        span: _,
    } = &primary.left.first
    else {
        return false;
    };

    if function_call.is_some() {
        return false;
    }

    let catla_parser::ast::SimplePrimary::Literal(literal) = left else {
        return false;
    };

    let literal_entity = EntityID::from(literal);

    let Some(resolved) = context.name_resolved_map.get(&literal_entity) else {
        return false;
    };
    if resolved.define.kind != DefineKind::Variable {
        return false;
    }

    let Some(literal_type) = context.type_infer_results.get(&literal_entity) else {
        return false;
    };

    is_class_type(&literal_type.value, context.user_type_set)
}

fn extract_variable_entity(
    expression: &Expression,
    _aliases_to_params: &HashMap<EntityID, HashSet<usize>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
) -> Option<(EntityID, bool)> {
    let Expression::Or(or_expression) = expression else {
        return None;
    };
    if !or_expression.chain.is_empty() {
        return None;
    }

    let and_expression = &or_expression.left;
    if !and_expression.chain.is_empty() {
        return None;
    }

    let equals_expression = &and_expression.left;
    if !equals_expression.chain.is_empty() {
        return None;
    }

    let less_or_greater_expression = &equals_expression.left;
    if !less_or_greater_expression.chain.is_empty() {
        return None;
    }

    let add_or_sub_expression = &less_or_greater_expression.left;
    if !add_or_sub_expression.chain.is_empty() {
        return None;
    }

    let mul_or_div_expression = &add_or_sub_expression.left;
    if !mul_or_div_expression.chain.is_empty() {
        return None;
    }

    let factor = &mul_or_div_expression.left;
    if factor.minus.is_some() {
        return None;
    }

    let Ok(primary) = &factor.primary else {
        return None;
    };

    if !primary.chain.is_empty() {
        return None;
    }

    let PrimaryLeftExpr::Simple {
        left,
        generics: _,
        function_call,
        span: _,
    } = &primary.left.first
    else {
        return None;
    };

    if function_call.is_some() {
        return None;
    }

    let catla_parser::ast::SimplePrimary::Literal(literal) = left else {
        return None;
    };

    let resolved = name_resolved_map.get(&EntityID::from(literal))?;
    if resolved.define.kind != DefineKind::Variable {
        return None;
    }

    let crosses_function_boundary = resolved
        .separators
        .iter()
        .any(|separator| separator.value == EnvironmentSeparatorKind::Function);

    Some((resolved.define.entity_id, crosses_function_boundary))
}

fn is_class_new_object(
    new_object: &catla_parser::ast::NewObjectExpression,
    context: &AnalyzerContext,
) -> bool {
    resolve_new_object_user_type(new_object, context)
        .map(|user_type_id| is_class_user_type(user_type_id, context.user_type_set))
        .unwrap_or(false)
}

fn resolve_new_object_user_type(
    new_object: &catla_parser::ast::NewObjectExpression,
    context: &AnalyzerContext,
) -> Option<GlobalUserTypeID> {
    let first = new_object.path.first()?;
    let resolved = context.name_resolved_map.get(&EntityID::from(first))?;
    if resolved.define.kind != DefineKind::UserType {
        return None;
    }

    let ty = context.module_entity_type_map.get(&resolved.define.entity_id)?;
    let Type::UserType {
        user_type_info,
        generics: _,
    } = ty.resolve_type_alias(context.user_type_set)
    else {
        return None;
    };

    Some(user_type_info)
}

fn collect_drop_required_user_types_for_sources(
    module_sources: &HashMap<String, Arc<ModuleLifetimeSource>>,
    user_type_set: &GlobalUserTypeSet,
    global_implements_infos: &ImplementsInfoSet,
) -> HashSet<GlobalUserTypeID> {
    let mut candidate_user_types = HashSet::new();
    for source in module_sources.values() {
        for ty in source.module_entity_type_map.values() {
            collect_user_type_ids_in_type(ty, user_type_set, &mut candidate_user_types);
        }
    }

    collect_drop_required_user_types(
        candidate_user_types,
        user_type_set,
        global_implements_infos,
    )
}

fn collect_drop_required_user_types_for_module(
    module_entity_type_map: &HashMap<EntityID, Type>,
    user_type_set: &GlobalUserTypeSet,
    global_implements_infos: &ImplementsInfoSet,
) -> HashSet<GlobalUserTypeID> {
    let mut candidate_user_types = HashSet::new();
    for ty in module_entity_type_map.values() {
        collect_user_type_ids_in_type(ty, user_type_set, &mut candidate_user_types);
    }

    collect_drop_required_user_types(
        candidate_user_types,
        user_type_set,
        global_implements_infos,
    )
}

fn collect_drop_required_user_types(
    mut candidate_user_types: HashSet<GlobalUserTypeID>,
    user_type_set: &GlobalUserTypeSet,
    global_implements_infos: &ImplementsInfoSet,
) -> HashSet<GlobalUserTypeID> {
    let mut drop_required_user_types =
        collect_direct_drop_implemented_user_types(user_type_set, global_implements_infos);

    candidate_user_types.extend(drop_required_user_types.iter().copied());

    let mut class_field_dependencies = HashMap::<GlobalUserTypeID, HashSet<GlobalUserTypeID>>::new();
    let mut pending = candidate_user_types.iter().copied().collect::<Vec<_>>();
    let mut visited = HashSet::new();

    while let Some(class_type_id) = pending.pop() {
        if !visited.insert(class_type_id) {
            continue;
        }

        if !is_class_user_type(class_type_id, user_type_set) {
            continue;
        }

        let class_info = user_type_set.get(class_type_id);
        let class_info = class_info.read().unwrap();

        let mut field_dependencies = HashSet::new();

        for element_type in class_info.element_types.values() {
            let resolved_element_type = element_type.value.resolve_type_alias(user_type_set);
            if matches!(resolved_element_type, Type::Function { .. }) {
                continue;
            }

            let mut contained_user_types = HashSet::new();
            collect_user_type_ids_in_type(
                &resolved_element_type,
                user_type_set,
                &mut contained_user_types,
            );

            for contained_type_id in contained_user_types.into_iter() {
                candidate_user_types.insert(contained_type_id);
                pending.push(contained_type_id);

                if is_class_user_type(contained_type_id, user_type_set) {
                    field_dependencies.insert(contained_type_id);
                }
            }
        }

        class_field_dependencies.insert(class_type_id, field_dependencies);
    }

    let mut changed = true;
    let mut guard = 0usize;
    while changed && guard < 128 {
        changed = false;
        guard += 1;

        for (class_type_id, field_type_ids) in class_field_dependencies.iter() {
            if drop_required_user_types.contains(class_type_id) {
                continue;
            }

            if field_type_ids
                .iter()
                .any(|field_type_id| drop_required_user_types.contains(field_type_id))
            {
                drop_required_user_types.insert(*class_type_id);
                changed = true;
            }
        }
    }

    drop_required_user_types
}

fn collect_direct_drop_implemented_user_types(
    user_type_set: &GlobalUserTypeSet,
    global_implements_infos: &ImplementsInfoSet,
) -> HashSet<GlobalUserTypeID> {
    let mut drop_implemented_user_types = HashSet::new();

    for implements_info in global_implements_infos.all_infos().into_iter() {
        if !is_std_drop_interface(&implements_info.interface.value, user_type_set) {
            continue;
        }

        let Type::UserType {
            user_type_info,
            generics: _,
        } = implements_info.concrete.value.resolve_type_alias(user_type_set)
        else {
            continue;
        };

        if is_class_user_type(user_type_info, user_type_set) {
            drop_implemented_user_types.insert(user_type_info);
        }
    }

    drop_implemented_user_types
}

fn is_std_drop_interface(interface_type: &Type, user_type_set: &GlobalUserTypeSet) -> bool {
    let Type::UserType {
        user_type_info,
        generics: _,
    } = interface_type.resolve_type_alias(user_type_set)
    else {
        return false;
    };

    let user_type_info = user_type_set.get(user_type_info);
    let user_type_info = user_type_info.read().unwrap();

    user_type_info
        .kind
        .as_ref()
        .map(|kind| kind.value == UserTypeKind::Interface)
        .unwrap_or(false)
        && user_type_info.name.value == "Drop"
        && user_type_info.module_path.path_name.as_str() == "std::dispose"
}

fn collect_user_type_ids_in_type(
    ty: &Type,
    user_type_set: &GlobalUserTypeSet,
    output: &mut HashSet<GlobalUserTypeID>,
) {
    match ty.resolve_type_alias(user_type_set) {
        Type::UserType {
            user_type_info,
            generics,
        } => {
            output.insert(user_type_info);

            for generic in generics.iter() {
                collect_user_type_ids_in_type(generic, user_type_set, output);
            }
        }
        Type::Function {
            function_info,
            generics,
        } => {
            for generic in generics.iter() {
                collect_user_type_ids_in_type(generic, user_type_set, output);
            }

            for argument in function_info.arguments.iter() {
                collect_user_type_ids_in_type(&argument.value, user_type_set, output);
            }

            collect_user_type_ids_in_type(&function_info.return_type.value, user_type_set, output);
        }
        Type::Array(base) => {
            collect_user_type_ids_in_type(base.as_ref(), user_type_set, output);
        }
        Type::Tuple(items) => {
            for item in items.iter() {
                collect_user_type_ids_in_type(item, user_type_set, output);
            }
        }
        _ => {}
    }
}

fn is_class_user_type(user_type_info: GlobalUserTypeID, user_type_set: &GlobalUserTypeSet) -> bool {
    let info = user_type_set.get(user_type_info);
    let info = info.read().unwrap();

    info.kind
        .as_ref()
        .map(|kind| kind.value == UserTypeKind::Class)
        .unwrap_or(false)
}

fn is_class_type(ty: &Type, user_type_set: &GlobalUserTypeSet) -> bool {
    match ty.resolve_type_alias(user_type_set) {
        Type::UserType {
            user_type_info,
            generics: _,
        } => {
            let info = user_type_set.get(user_type_info);
            let info = info.read().unwrap();
            info.kind
                .as_ref()
                .map(|kind| kind.value == UserTypeKind::Class)
                .unwrap_or(false)
        }
        _ => false,
    }
}
