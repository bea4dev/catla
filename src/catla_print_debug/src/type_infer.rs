use ariadne::{Color, Label, Report, ReportKind, Source};
use catla_parser::{
    CatlaAST,
    ast::{
        AddOrSubExpression, AndExpression, Define, ElseChain, EntityID, EqualsExpression,
        Expression, ExpressionOrBlock, Factor, LessOrGreaterExpression, MappingOperator,
        MulOrDivExpression, OrExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight,
        Program, SimplePrimary, Spanned, Statement, VariableBinding,
    },
};
use catla_type::types::{GlobalUserTypeSet, Type};
use hashbrown::HashMap;

pub fn print_type_infer_result(
    ast: &CatlaAST,
    entity_type_map: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
) {
    let mut results = Vec::new();

    collect_type_infer_result_for_program(
        ast.ast(),
        ast,
        entity_type_map,
        user_type_set,
        &mut results,
    );

    let mut builder = Report::build(
        ReportKind::Custom("Debug", Color::Cyan),
        (
            ast.source_code.module_path.path_name.as_str(),
            ast.ast().span.clone(),
        ),
    );

    for result in results.iter() {
        builder.add_label(
            Label::new((
                ast.source_code.module_path.path_name.as_str(),
                result.span.clone(),
            ))
            .with_message(result.value.to_display_string(user_type_set, None))
            .with_color(Color::Cyan),
        );
    }

    builder
        .finish()
        .print((
            ast.source_code.module_path.path_name.as_str(),
            Source::from(ast.source_code.code.as_str()),
        ))
        .unwrap();
}

fn collect_type_infer_result_for_program(
    ast: &Program,
    holder: &CatlaAST,
    entity_type_map: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    results: &mut Vec<Spanned<Type>>,
) {
    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                collect_type_infer_result_for_expression(
                    &assignment.left,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
                if let Ok(right) = &assignment.right {
                    collect_type_infer_result_for_expression(
                        right,
                        holder,
                        entity_type_map,
                        user_type_set,
                        results,
                    );
                }
            }
            Statement::Swap(swap_statement) => {
                collect_type_infer_result_for_expression(
                    &swap_statement.left,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
                if let Ok(right) = &swap_statement.right {
                    collect_type_infer_result_for_expression(
                        right,
                        holder,
                        entity_type_map,
                        user_type_set,
                        results,
                    );
                }
            }
            Statement::Import(_) => {}
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Function(function_define) => {
                            if let Some(block) = &function_define.block {
                                collect_type_infer_result_for_program(
                                    block.program,
                                    holder,
                                    entity_type_map,
                                    user_type_set,
                                    results,
                                );
                            }
                        }
                        Define::UserType(user_type_define) => {
                            if let Ok(block) = &user_type_define.block {
                                collect_type_infer_result_for_program(
                                    block.program,
                                    holder,
                                    entity_type_map,
                                    user_type_set,
                                    results,
                                );
                            }
                        }
                        Define::Variable(variable_define) => {
                            if let Ok(binding) = &variable_define.binding {
                                collect_type_infer_result_variable_binding(
                                    binding,
                                    holder,
                                    entity_type_map,
                                    user_type_set,
                                    results,
                                );
                            }
                            if let Some(expression) = &variable_define.expression {
                                collect_type_infer_result_for_expression(
                                    expression,
                                    holder,
                                    entity_type_map,
                                    user_type_set,
                                    results,
                                );
                            }
                        }
                        Define::TypeAlias(_) => {}
                    }
                }
            }
            Statement::Drop(drop_statement) => {
                if let Ok(expression) = &drop_statement.expression {
                    collect_type_infer_result_for_expression(
                        expression,
                        holder,
                        entity_type_map,
                        user_type_set,
                        results,
                    );
                }
            }
            Statement::Expression(expression) => {
                collect_type_infer_result_for_expression(
                    expression,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
            }
            Statement::Implements(implements) => {
                if let Ok(block) = &implements.block {
                    collect_type_infer_result_for_program(
                        block.program,
                        holder,
                        entity_type_map,
                        user_type_set,
                        results,
                    );
                }
            }
        }
    }
}

fn collect_type_infer_result_variable_binding(
    ast: &VariableBinding,
    holder: &CatlaAST,
    entity_type_map: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    results: &mut Vec<Spanned<Type>>,
) {
    match ast {
        VariableBinding::Literal(literal) => {
            if let Some(ty) = entity_type_map.get(&EntityID::from(literal)) {
                results.push(Spanned::new(ty.value.clone(), literal.span.clone()));
            }
        }
        VariableBinding::Binding { bindings, span: _ } => {
            for binding in bindings.iter() {
                collect_type_infer_result_variable_binding(
                    binding,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
            }
        }
    }
}

fn collect_type_infer_result_for_expression(
    ast: &Expression,
    holder: &CatlaAST,
    entity_type_map: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    results: &mut Vec<Spanned<Type>>,
) {
    match ast {
        Expression::Return(return_expression) => {
            if let Some(expression) = &return_expression.expression {
                collect_type_infer_result_for_expression(
                    expression,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
            }
        }
        Expression::Closure(closure) => {
            if let Ok(expression_or_block) = &closure.expression_or_block {
                match expression_or_block {
                    ExpressionOrBlock::Expression(expression) => {
                        collect_type_infer_result_for_expression(
                            expression,
                            holder,
                            entity_type_map,
                            user_type_set,
                            results,
                        );
                    }
                    ExpressionOrBlock::Block(block) => {
                        collect_type_infer_result_for_program(
                            block.program,
                            holder,
                            entity_type_map,
                            user_type_set,
                            results,
                        );
                    }
                }
            }
        }
        Expression::Or(or_expression) => {
            collect_type_infer_result_for_or_expression(
                *or_expression,
                holder,
                entity_type_map,
                user_type_set,
                results,
            );
        }
    }
}

macro_rules! collect_type_infer_result_for_2op {
    ($name:ident, $ty:ty, $next:ident) => {
        fn $name(
            ast: &$ty,
            holder: &CatlaAST,
            entity_type_map: &HashMap<EntityID, Spanned<Type>>,
            user_type_set: &GlobalUserTypeSet,
            results: &mut Vec<Spanned<Type>>,
        ) {
            $next(&ast.left, holder, entity_type_map, user_type_set, results);
            for chain in ast.chain.iter() {
                $next(chain, holder, entity_type_map, user_type_set, results);
            }
        }
    };
    (op, $name:ident, $ty:ty, $next:ident) => {
        fn $name(
            ast: &$ty,
            holder: &CatlaAST,
            entity_type_map: &HashMap<EntityID, Spanned<Type>>,
            user_type_set: &GlobalUserTypeSet,
            results: &mut Vec<Spanned<Type>>,
        ) {
            $next(&ast.left, holder, entity_type_map, user_type_set, results);
            for chain in ast.chain.iter() {
                $next(&chain.1, holder, entity_type_map, user_type_set, results);
            }
        }
    };
}

collect_type_infer_result_for_2op!(
    collect_type_infer_result_for_or_expression,
    OrExpression,
    collect_type_infer_result_for_and_expression
);

collect_type_infer_result_for_2op!(
    collect_type_infer_result_for_and_expression,
    AndExpression,
    collect_type_infer_result_for_equals_expression
);

collect_type_infer_result_for_2op!(
    op,
    collect_type_infer_result_for_equals_expression,
    EqualsExpression,
    collect_type_infer_result_for_less_or_greater_expression
);

collect_type_infer_result_for_2op!(
    op,
    collect_type_infer_result_for_less_or_greater_expression,
    LessOrGreaterExpression,
    collect_type_infer_result_for_add_or_sub_expression
);

collect_type_infer_result_for_2op!(
    op,
    collect_type_infer_result_for_add_or_sub_expression,
    AddOrSubExpression,
    collect_type_infer_result_for_mul_or_div_expression
);

collect_type_infer_result_for_2op!(
    op,
    collect_type_infer_result_for_mul_or_div_expression,
    MulOrDivExpression,
    collect_type_infer_result_for_factor
);

fn collect_type_infer_result_for_factor(
    ast: &Factor,
    holder: &CatlaAST,
    entity_type_map: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    results: &mut Vec<Spanned<Type>>,
) {
    if let Ok(primary) = &ast.primary {
        collect_type_infer_result_for_primary(
            primary,
            holder,
            entity_type_map,
            user_type_set,
            results,
        );
    }
}

fn collect_type_infer_result_for_primary(
    ast: &Primary,
    holder: &CatlaAST,
    entity_type_map: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    results: &mut Vec<Spanned<Type>>,
) {
    collect_type_infer_result_for_primary_left(
        &ast.left,
        holder,
        entity_type_map,
        user_type_set,
        results,
    );
    for chain in ast.chain.iter() {
        collect_type_infer_result_for_primary_right(
            chain,
            holder,
            entity_type_map,
            user_type_set,
            results,
        );
    }
}

fn collect_type_infer_result_for_primary_left(
    ast: &PrimaryLeft,
    holder: &CatlaAST,
    entity_type_map: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    results: &mut Vec<Spanned<Type>>,
) {
    match &ast.first {
        PrimaryLeftExpr::Simple {
            left,
            generics: _,
            function_call,
            span: _,
        } => {
            match left {
                SimplePrimary::Tuple {
                    expressions,
                    span: _,
                } => {
                    for expression in expressions.iter() {
                        collect_type_infer_result_for_expression(
                            expression,
                            holder,
                            entity_type_map,
                            user_type_set,
                            results,
                        );
                    }
                }
                _ => {}
            }
            if let Some(function_call) = function_call {
                for argument in function_call.arguments.iter() {
                    collect_type_infer_result_for_expression(
                        argument,
                        holder,
                        entity_type_map,
                        user_type_set,
                        results,
                    );
                }
            }
        }
        PrimaryLeftExpr::NewObject { new_object } => {
            for field_assign in new_object.field_assign.elements.iter() {
                collect_type_infer_result_for_expression(
                    &field_assign.expression,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
            }
        }
        PrimaryLeftExpr::NewArray { new_array } => {
            for expression in new_array.elements.iter() {
                collect_type_infer_result_for_expression(
                    expression,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
            }
        }
        PrimaryLeftExpr::NewArrayInit { new_array_init } => {
            if let Ok(expression) = &new_array_init.init_expression {
                collect_type_infer_result_for_expression(
                    expression,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
            }
            if let Ok(expression) = &new_array_init.length_expression {
                collect_type_infer_result_for_expression(
                    expression,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
            }
        }
        PrimaryLeftExpr::If { if_expression } => {
            if let Ok(condition) = &if_expression.first.condition {
                collect_type_infer_result_for_expression(
                    condition,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
            }
            if let Ok(block) = &if_expression.first.block {
                collect_type_infer_result_for_program(
                    block.program,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
            }

            for chain in if_expression.chain.iter() {
                match chain {
                    ElseChain::ElseIf { if_statement } => {
                        if let Ok(condition) = &if_statement.condition {
                            collect_type_infer_result_for_expression(
                                condition,
                                holder,
                                entity_type_map,
                                user_type_set,
                                results,
                            );
                        }
                        if let Ok(block) = &if_statement.block {
                            collect_type_infer_result_for_program(
                                block.program,
                                holder,
                                entity_type_map,
                                user_type_set,
                                results,
                            );
                        }
                    }
                    ElseChain::Else { block } => {
                        collect_type_infer_result_for_program(
                            block.program,
                            holder,
                            entity_type_map,
                            user_type_set,
                            results,
                        );
                    }
                }
            }
        }
        PrimaryLeftExpr::Loop { loop_expression } => {
            if let Ok(block) = &loop_expression.block {
                collect_type_infer_result_for_program(
                    block.program,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_type_infer_result_for_mapping_operator(
            mapping_operator,
            holder,
            entity_type_map,
            user_type_set,
            results,
        );
    }
}

fn collect_type_infer_result_for_primary_right(
    ast: &PrimaryRight,
    holder: &CatlaAST,
    entity_type_map: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    results: &mut Vec<Spanned<Type>>,
) {
    if let Some(second) = &ast.second {
        if let Some(function_call) = &second.function_call {
            for argument in function_call.arguments.iter() {
                collect_type_infer_result_for_expression(
                    argument,
                    holder,
                    entity_type_map,
                    user_type_set,
                    results,
                );
            }
        }
    }
    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_type_infer_result_for_mapping_operator(
            mapping_operator,
            holder,
            entity_type_map,
            user_type_set,
            results,
        );
    }
}

fn collect_type_infer_result_for_mapping_operator(
    ast: &MappingOperator,
    holder: &CatlaAST,
    entity_type_map: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    results: &mut Vec<Spanned<Type>>,
) {
    let block = match ast {
        MappingOperator::NullElvis { block, span: _ } => block,
        MappingOperator::ResultElvis { block, span: _ } => block,
        _ => return,
    };

    if let Ok(block) = block {
        collect_type_infer_result_for_program(
            block.program,
            holder,
            entity_type_map,
            user_type_set,
            results,
        );
    }
}
