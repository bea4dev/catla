use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_parser::parser::{
    AddOrSubExpression, AndExpression, CompareExpression, Expression, ExpressionEnum, Factor,
    FunctionCall, MappingOperator, MappingOperatorKind, MulOrDivExpression, OrExpression, Primary,
    PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, StatementAST,
};
use either::Either;
use fxhash::{FxHashMap, FxHashSet};
use hashbrown::{DefaultHashBuilder, HashMap, HashSet};

use crate::transpiler::{
    component::EntityID,
    name_resolver::{EnvironmentSeparatorKind, FoundDefineInfo},
};

pub struct VariableUserInfo {
    cannot_move_variables: FxHashSet<EntityID>,
}

impl VariableUserInfo {
    pub fn new() -> Self {
        Self {
            cannot_move_variables: FxHashSet::default(),
        }
    }

    pub fn can_move_variable(&self, user: EntityID) -> bool {
        !self.cannot_move_variables.contains(&user)
    }
}

#[must_use]
struct VariableUserEnvironment<'allocator> {
    variable_user_map:
        HashMap<EntityID, Vec<EntityID, &'allocator Bump>, DefaultHashBuilder, &'allocator Bump>,
    not_moved_variables: HashSet<EntityID, DefaultHashBuilder, &'allocator Bump>,
    allocator: &'allocator Bump,
}

impl<'allocator> VariableUserEnvironment<'allocator> {
    fn new(allocator: &'allocator Bump) -> Self {
        Self {
            variable_user_map: HashMap::new_in(allocator),
            not_moved_variables: HashSet::new_in(allocator),
            allocator,
        }
    }

    fn marge_with(&mut self, other: Self) {
        for (other_key, other_values) in other.variable_user_map.into_iter() {
            if let Some(values) = self.variable_user_map.get_mut(&other_key) {
                values.extend(other_values);
            } else {
                self.variable_user_map.insert(other_key, other_values);
            }
        }
    }

    fn update_with(&mut self, other: Self) {
        let mut not_moved = HashSet::new_in(self.allocator);
        for new_entity_id in other.variable_user_map.keys() {
            not_moved.insert(*new_entity_id);
        }

        self.variable_user_map.extend(other.variable_user_map);
        self.not_moved_variables.extend(not_moved);
    }

    fn register_use(&mut self, define: EntityID, user: EntityID) {
        self.variable_user_map
            .entry(define)
            .or_insert_with(|| Vec::new_in(self.allocator))
            .push(user);
    }

    fn export(self) -> VariableUserInfo {
        // into global allocator
        VariableUserInfo {
            cannot_move_variables: self.not_moved_variables.into_iter().collect(),
        }
    }
}

pub fn collect_variable_user_info(
    ast: Program,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
) -> VariableUserInfo {
    let allocator = Bump::new();

    let environment = collect_program(ast, name_resolved_map, &allocator);

    environment.export()
}

fn collect_program<'allocator>(
    ast: Program,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    allocator: &'allocator Bump,
) -> VariableUserEnvironment<'allocator> {
    let mut environment = VariableUserEnvironment::new(allocator);

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(_) => continue,
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                environment.update_with(collect_expression(
                    assignment.left_expr,
                    name_resolved_map,
                    allocator,
                ));

                if let Ok(expression) = assignment.right_expr {
                    environment.update_with(collect_expression(
                        expression,
                        name_resolved_map,
                        allocator,
                    ));
                }
            }
            StatementAST::Exchange(exchange) => {
                environment.update_with(collect_expression(
                    exchange.left_expr,
                    name_resolved_map,
                    allocator,
                ));

                if let Ok(expression) = exchange.right_expr {
                    environment.update_with(collect_expression(
                        expression,
                        name_resolved_map,
                        allocator,
                    ));
                }
            }
            StatementAST::Import(_) => {}
            StatementAST::StatementAttributes(_) => {}
            StatementAST::VariableDefine(variable_define) => {
                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        environment.update_with(collect_expression(
                            expression,
                            name_resolved_map,
                            allocator,
                        ));
                    }
                }
            }
            StatementAST::FunctionDefine(function_define) => {
                if let Some(block) = &function_define.block_or_semicolon.value {
                    if let Either::Right(block) = block {
                        environment.update_with(collect_program(
                            block.program,
                            name_resolved_map,
                            allocator,
                        ));
                    }
                }
            }
            StatementAST::UserTypeDefine(user_type_define) => {
                if let Some(block) = &user_type_define.block.value {
                    environment.update_with(collect_program(
                        block.program,
                        name_resolved_map,
                        allocator,
                    ));
                }
            }
            StatementAST::TypeDefine(_) => {}
            StatementAST::Implements(implements) => {
                if let Some(block) = &implements.block.value {
                    environment.update_with(collect_program(
                        block.program,
                        name_resolved_map,
                        allocator,
                    ));
                }
            }
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    environment.update_with(collect_expression(
                        expression,
                        name_resolved_map,
                        allocator,
                    ));
                }
            }
            StatementAST::Expression(expression) => {
                environment.update_with(collect_expression(
                    expression,
                    name_resolved_map,
                    allocator,
                ));

                if let ExpressionEnum::ReturnExpression(_) = expression {
                    break;
                }
            }
            StatementAST::TranspilerTag(_) => {}
        }
    }

    environment
}

fn collect_expression<'allocator>(
    ast: Expression,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    allocator: &'allocator Bump,
) -> VariableUserEnvironment<'allocator> {
    let mut environment = VariableUserEnvironment::new(allocator);

    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            environment.update_with(collect_or_expression(
                or_expression,
                name_resolved_map,
                allocator,
            ));
        }
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                environment.update_with(collect_expression(
                    expression,
                    name_resolved_map,
                    allocator,
                ));
            }
        }
        ExpressionEnum::Closure(closure) => {
            if let Some(block) = &closure.expression_or_block.value {
                if let Either::Right(block) = block {
                    environment.update_with(collect_program(
                        block.program,
                        name_resolved_map,
                        allocator,
                    ));
                }
            }
        }
    }

    environment
}

macro_rules! collect_for_op2 {
    ($function_name:ident, $ast_type:ident, $next_layer_function_name:ident) => {
        fn $function_name<'allocator>(
            ast: &$ast_type,
            name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
            allocator: &'allocator Bump,
        ) -> VariableUserEnvironment<'allocator> {
            let mut environment = VariableUserEnvironment::new(allocator);

            environment.update_with($next_layer_function_name(
                &ast.left_expr,
                name_resolved_map,
                allocator,
            ));

            for (_, right_expr) in ast.right_exprs.iter() {
                if let Ok(right_expr) = right_expr {
                    environment.update_with($next_layer_function_name(
                        right_expr,
                        name_resolved_map,
                        allocator,
                    ));
                }
            }

            environment
        }
    };
}

collect_for_op2!(collect_or_expression, OrExpression, collect_and_expression);

collect_for_op2!(
    collect_and_expression,
    AndExpression,
    collect_compare_expression
);

collect_for_op2!(
    collect_compare_expression,
    CompareExpression,
    collect_add_or_sub_expression
);

collect_for_op2!(
    collect_add_or_sub_expression,
    AddOrSubExpression,
    collect_mul_or_div_expression
);

collect_for_op2!(
    collect_mul_or_div_expression,
    MulOrDivExpression,
    collect_factor
);

fn collect_factor<'allocator>(
    ast: &Factor,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    allocator: &'allocator Bump,
) -> VariableUserEnvironment<'allocator> {
    if let Ok(primary) = &ast.primary {
        collect_primary(primary, name_resolved_map, allocator)
    } else {
        VariableUserEnvironment::new(allocator)
    }
}

fn collect_primary<'allocator>(
    ast: &Primary,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    allocator: &'allocator Bump,
) -> VariableUserEnvironment<'allocator> {
    let mut environment = VariableUserEnvironment::new(allocator);

    environment.update_with(collect_primary_left(
        &ast.left,
        name_resolved_map,
        allocator,
    ));

    for primary_right in ast.chain.iter() {
        environment.update_with(collect_primary_right(
            primary_right,
            name_resolved_map,
            allocator,
        ));
    }

    environment
}

fn collect_primary_left<'allocator>(
    ast: &PrimaryLeft,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    allocator: &'allocator Bump,
) -> VariableUserEnvironment<'allocator> {
    let mut environment = VariableUserEnvironment::new(allocator);

    match &ast.first_expr {
        PrimaryLeftExpr::Simple((simple_primary, _, function_call)) => {
            match simple_primary {
                SimplePrimary::Expressions {
                    expressions,
                    error_tokens: _,
                    span: _,
                } => {
                    for expression in expressions.iter() {
                        environment.update_with(collect_expression(
                            *expression,
                            name_resolved_map,
                            allocator,
                        ));
                    }
                }
                SimplePrimary::Identifier(spanned) => {
                    if let Some(resolved) = name_resolved_map.get(&EntityID::from(spanned)) {
                        if !resolved.has_separator(&[
                            EnvironmentSeparatorKind::Loop,
                            EnvironmentSeparatorKind::Closure,
                        ]) {
                            environment.register_use(
                                resolved.define_info.entity_id,
                                EntityID::from(spanned),
                            );
                        }
                    }
                }
                _ => {}
            }

            if let Some(function_call) = function_call {
                environment.update_with(collect_function_call(
                    function_call,
                    name_resolved_map,
                    allocator,
                ));
            }
        }
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {
            if let Ok(expression) = new_array_init_expression.length_expression {
                environment.update_with(collect_expression(
                    expression,
                    name_resolved_map,
                    allocator,
                ));
            }

            if let Ok(expression) = new_array_init_expression.init_expression {
                environment.update_with(collect_expression(
                    expression,
                    name_resolved_map,
                    allocator,
                ));
            }
        }
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {
            for expression in new_array_expression.value_expressions.iter() {
                if let Ok(expression) = expression {
                    environment.update_with(collect_expression(
                        *expression,
                        name_resolved_map,
                        allocator,
                    ));
                }
            }
        }
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(field_assigns) = &new_expression.field_assigns {
                for field_assign in field_assigns.iter() {
                    if let Ok(expression) = field_assign.expression {
                        environment.update_with(collect_expression(
                            expression,
                            name_resolved_map,
                            allocator,
                        ));
                    }
                }
            }
        }
        PrimaryLeftExpr::IfExpression(if_expression) => {
            if let Some(block) = &if_expression.if_statement.block.value {
                environment.marge_with(collect_program(
                    block.program,
                    name_resolved_map,
                    allocator,
                ));
            }

            for chain in if_expression.chain.iter() {
                if let Some(chain) = &chain.else_if_or_else.value {
                    match chain {
                        Either::Left(if_statement) => {
                            if let Some(block) = &if_statement.block.value {
                                environment.marge_with(collect_program(
                                    block.program,
                                    name_resolved_map,
                                    allocator,
                                ));
                            }
                        }
                        Either::Right(block) => {
                            environment.marge_with(collect_program(
                                block.program,
                                name_resolved_map,
                                allocator,
                            ));
                        }
                    }
                }
            }
        }
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                environment.update_with(collect_program(
                    block.program,
                    name_resolved_map,
                    allocator,
                ));
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        environment.update_with(collect_mapping_operator(
            mapping_operator,
            name_resolved_map,
            allocator,
        ));
    }

    environment
}

fn collect_function_call<'allocator>(
    ast: &FunctionCall,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    allocator: &'allocator Bump,
) -> VariableUserEnvironment<'allocator> {
    let mut environment = VariableUserEnvironment::new(allocator);

    if let Ok(args) = &ast.arg_exprs {
        for expression in args.iter() {
            environment.update_with(collect_expression(
                *expression,
                name_resolved_map,
                allocator,
            ));
        }
    }

    environment
}

fn collect_mapping_operator<'allocator>(
    ast: &MappingOperator,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    allocator: &'allocator Bump,
) -> VariableUserEnvironment<'allocator> {
    let mut environment = VariableUserEnvironment::new(allocator);

    let block = match &ast.value {
        MappingOperatorKind::NullElvisBlock(block) => &block.value,
        MappingOperatorKind::ResultElvisBlock(block) => &block.value,
        _ => return environment,
    };

    if let Some(block) = block {
        environment.update_with(collect_program(block.program, name_resolved_map, allocator));
    }

    environment
}

fn collect_primary_right<'allocator>(
    ast: &PrimaryRight,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    allocator: &'allocator Bump,
) -> VariableUserEnvironment<'allocator> {
    let mut environment = VariableUserEnvironment::new(allocator);

    if let Some((_, _, function_call)) = &ast.second_expr {
        if let Some(function_call) = function_call {
            environment.update_with(collect_function_call(
                function_call,
                name_resolved_map,
                allocator,
            ));
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        environment.update_with(collect_mapping_operator(
            mapping_operator,
            name_resolved_map,
            allocator,
        ));
    }

    environment
}
