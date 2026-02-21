use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};
use catla_name_resolver::{DefineKind, ResolvedInfo};
use catla_optimization::lifetime::{AllocationKind, LifetimeAnalyzeResults};
use catla_parser::{
    CatlaAST,
    ast::{
        AddOrSubExpression, AndExpression, ClosureArgumentsOrLiteral, Define, ElseChain, EntityID,
        EqualsExpression, Expression, ExpressionOrBlock, Factor, FunctionArgumentOrVariableBinding,
        LessOrGreaterExpression, MappingOperator, MulOrDivExpression, OrExpression, Primary,
        PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, Statement,
        VariableBinding,
    },
};
use hashbrown::HashMap;

#[derive(Debug)]
struct DebugLabel {
    span: Range<usize>,
    color: Color,
    message: String,
}

pub fn print_lifetime_optimization_result(
    ast: &CatlaAST,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    lifetime_results: &LifetimeAnalyzeResults,
) {
    let mut labels = Vec::new();

    collect_program(
        ast.ast(),
        name_resolved_map,
        lifetime_results,
        &mut labels,
    );

    if labels.is_empty() {
        return;
    }

    labels.sort_by_key(|label| (label.span.start, label.span.end));

    let source = ast.source_code.code.as_str();
    let module_name = ast.source_code.module_path.path_name.as_str();
    let root_span = byte_range_to_char_range(source, &ast.ast().span);

    let mut builder = Report::build(
        ReportKind::Custom("Optimization Debug", Color::Cyan),
        (module_name, root_span),
    );

    for label in labels.iter() {
        let span = byte_range_to_char_range(source, &label.span);
        builder.add_label(
            Label::new((module_name, span))
                .with_color(label.color)
                .with_message(label.message.as_str()),
        );
    }

    builder
        .finish()
        .print((module_name, Source::from(source)))
        .unwrap();
}

fn collect_program(
    program: &Program,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    lifetime_results: &LifetimeAnalyzeResults,
    labels: &mut Vec<DebugLabel>,
) {
    for statement in program.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                collect_expression(&assignment.left, name_resolved_map, lifetime_results, labels);
                if let Ok(right) = &assignment.right {
                    collect_expression(right, name_resolved_map, lifetime_results, labels);
                }
            }
            Statement::Swap(swap_statement) => {
                collect_expression(&swap_statement.left, name_resolved_map, lifetime_results, labels);
                if let Ok(right) = &swap_statement.right {
                    collect_expression(right, name_resolved_map, lifetime_results, labels);
                }
            }
            Statement::Import(_) => {}
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Function(function_define) => {
                            if let Ok(arguments) = &function_define.arguments {
                                for argument in arguments.arguments.iter() {
                                    collect_variable_binding(
                                        &argument.binding,
                                        name_resolved_map,
                                        lifetime_results,
                                        labels,
                                    );
                                }
                            }
                            if let Some(block) = &function_define.block {
                                collect_program(
                                    block.program,
                                    name_resolved_map,
                                    lifetime_results,
                                    labels,
                                );
                            }
                        }
                        Define::UserType(user_type_define) => {
                            if let Ok(block) = &user_type_define.block {
                                collect_program(
                                    block.program,
                                    name_resolved_map,
                                    lifetime_results,
                                    labels,
                                );
                            }
                        }
                        Define::Variable(variable_define) => {
                            if let Ok(binding) = &variable_define.binding {
                                collect_variable_binding(
                                    binding,
                                    name_resolved_map,
                                    lifetime_results,
                                    labels,
                                );
                            }
                            if let Some(expression) = &variable_define.expression {
                                collect_expression(
                                    expression,
                                    name_resolved_map,
                                    lifetime_results,
                                    labels,
                                );
                            }
                        }
                        Define::TypeAlias(_) => {}
                    }
                }
            }
            Statement::Drop(drop_statement) => {
                if let Ok(expression) = &drop_statement.expression {
                    collect_expression(expression, name_resolved_map, lifetime_results, labels);
                }
            }
            Statement::Expression(expression) => {
                collect_expression(expression, name_resolved_map, lifetime_results, labels);
            }
            Statement::Implements(implements) => {
                if let Ok(block) = &implements.block {
                    collect_program(block.program, name_resolved_map, lifetime_results, labels);
                }
            }
        }
    }
}

fn collect_variable_binding(
    binding: &VariableBinding,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    lifetime_results: &LifetimeAnalyzeResults,
    labels: &mut Vec<DebugLabel>,
) {
    match binding {
        VariableBinding::Literal(literal) => {
            let variable_entity = resolve_variable_entity(
                EntityID::from(literal),
                name_resolved_map,
            );

            if let Some((color, message)) =
                build_variable_label(variable_entity, lifetime_results)
            {
                labels.push(DebugLabel {
                    span: literal.span.clone(),
                    color,
                    message,
                });
            }
        }
        VariableBinding::Binding { bindings, span: _ } => {
            for binding in bindings.iter() {
                collect_variable_binding(binding, name_resolved_map, lifetime_results, labels);
            }
        }
    }
}

fn collect_expression(
    expression: &Expression,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    lifetime_results: &LifetimeAnalyzeResults,
    labels: &mut Vec<DebugLabel>,
) {
    if lifetime_results.should_borrow_argument(EntityID::from(expression)) {
        labels.push(DebugLabel {
            span: expression.span(),
            color: Color::Green,
            message: "borrow argument".to_string(),
        });
    }

    match expression {
        Expression::Return(return_expression) => {
            if let Some(expression) = &return_expression.expression {
                collect_expression(expression, name_resolved_map, lifetime_results, labels);
            }
        }
        Expression::Closure(closure) => {
            match &closure.arguments {
                ClosureArgumentsOrLiteral::ClosureArguments(closure_arguments) => {
                    for argument in closure_arguments.arguments.iter() {
                        match argument {
                            FunctionArgumentOrVariableBinding::FunctionArgument(argument) => {
                                collect_variable_binding(
                                    &argument.binding,
                                    name_resolved_map,
                                    lifetime_results,
                                    labels,
                                );
                            }
                            FunctionArgumentOrVariableBinding::VariableBinding(binding) => {
                                collect_variable_binding(
                                    binding,
                                    name_resolved_map,
                                    lifetime_results,
                                    labels,
                                );
                            }
                        }
                    }
                }
                ClosureArgumentsOrLiteral::Literal(_) => {}
            }

            if let Ok(expression_or_block) = &closure.expression_or_block {
                match expression_or_block {
                    ExpressionOrBlock::Expression(expression) => {
                        collect_expression(expression, name_resolved_map, lifetime_results, labels);
                    }
                    ExpressionOrBlock::Block(block) => {
                        collect_program(block.program, name_resolved_map, lifetime_results, labels);
                    }
                }
            }
        }
        Expression::Or(or_expression) => {
            collect_or_expression(*or_expression, name_resolved_map, lifetime_results, labels);
        }
    }
}

macro_rules! collect_2op_expression {
    ($name:ident, $ty:ty, $next:ident) => {
        fn $name(
            expression: &$ty,
            name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
            lifetime_results: &LifetimeAnalyzeResults,
            labels: &mut Vec<DebugLabel>,
        ) {
            $next(
                &expression.left,
                name_resolved_map,
                lifetime_results,
                labels,
            );
            for chain in expression.chain.iter() {
                $next(chain, name_resolved_map, lifetime_results, labels);
            }
        }
    };
    (op, $name:ident, $ty:ty, $next:ident) => {
        fn $name(
            expression: &$ty,
            name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
            lifetime_results: &LifetimeAnalyzeResults,
            labels: &mut Vec<DebugLabel>,
        ) {
            $next(
                &expression.left,
                name_resolved_map,
                lifetime_results,
                labels,
            );
            for chain in expression.chain.iter() {
                $next(&chain.1, name_resolved_map, lifetime_results, labels);
            }
        }
    };
}

collect_2op_expression!(
    collect_or_expression,
    OrExpression,
    collect_and_expression
);
collect_2op_expression!(
    collect_and_expression,
    AndExpression,
    collect_equals_expression
);
collect_2op_expression!(
    op,
    collect_equals_expression,
    EqualsExpression,
    collect_less_or_greater_expression
);
collect_2op_expression!(
    op,
    collect_less_or_greater_expression,
    LessOrGreaterExpression,
    collect_add_or_sub_expression
);
collect_2op_expression!(
    op,
    collect_add_or_sub_expression,
    AddOrSubExpression,
    collect_mul_or_div_expression
);
collect_2op_expression!(
    op,
    collect_mul_or_div_expression,
    MulOrDivExpression,
    collect_factor
);

fn collect_factor(
    factor: &Factor,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    lifetime_results: &LifetimeAnalyzeResults,
    labels: &mut Vec<DebugLabel>,
) {
    if let Ok(primary) = &factor.primary {
        collect_primary(primary, name_resolved_map, lifetime_results, labels);
    }
}

fn collect_primary(
    primary: &Primary,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    lifetime_results: &LifetimeAnalyzeResults,
    labels: &mut Vec<DebugLabel>,
) {
    collect_primary_left(&primary.left, name_resolved_map, lifetime_results, labels);

    for chain in primary.chain.iter() {
        collect_primary_right(chain, name_resolved_map, lifetime_results, labels);
    }
}

fn collect_primary_left(
    primary_left: &PrimaryLeft,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    lifetime_results: &LifetimeAnalyzeResults,
    labels: &mut Vec<DebugLabel>,
) {
    match &primary_left.first {
        PrimaryLeftExpr::Simple {
            left,
            generics: _,
            function_call,
            span: _,
        } => {
            if let SimplePrimary::Tuple {
                expressions,
                span: _,
            } = left
            {
                for expression in expressions.iter() {
                    collect_expression(expression, name_resolved_map, lifetime_results, labels);
                }
            }

            if let Some(function_call) = function_call {
                for argument in function_call.arguments.iter() {
                    collect_expression(argument, name_resolved_map, lifetime_results, labels);
                }
            }
        }
        PrimaryLeftExpr::NewObject { new_object } => {
            for field_assign in new_object.field_assign.elements.iter() {
                collect_expression(
                    &field_assign.expression,
                    name_resolved_map,
                    lifetime_results,
                    labels,
                );
            }

            if let Some((color, message)) =
                build_object_label(EntityID::from(new_object), lifetime_results)
            {
                labels.push(DebugLabel {
                    span: new_object.span.clone(),
                    color,
                    message,
                });
            }
        }
        PrimaryLeftExpr::NewArray { new_array } => {
            for expression in new_array.elements.iter() {
                collect_expression(expression, name_resolved_map, lifetime_results, labels);
            }
        }
        PrimaryLeftExpr::NewArrayInit { new_array_init } => {
            if let Ok(expression) = &new_array_init.init_expression {
                collect_expression(expression, name_resolved_map, lifetime_results, labels);
            }
            if let Ok(expression) = &new_array_init.length_expression {
                collect_expression(expression, name_resolved_map, lifetime_results, labels);
            }
        }
        PrimaryLeftExpr::If { if_expression } => {
            if let Ok(condition) = &if_expression.first.condition {
                collect_expression(condition, name_resolved_map, lifetime_results, labels);
            }
            if let Ok(block) = &if_expression.first.block {
                collect_program(block.program, name_resolved_map, lifetime_results, labels);
            }

            for chain in if_expression.chain.iter() {
                match chain {
                    ElseChain::ElseIf { if_statement } => {
                        if let Ok(condition) = &if_statement.condition {
                            collect_expression(condition, name_resolved_map, lifetime_results, labels);
                        }
                        if let Ok(block) = &if_statement.block {
                            collect_program(
                                block.program,
                                name_resolved_map,
                                lifetime_results,
                                labels,
                            );
                        }
                    }
                    ElseChain::Else { block } => {
                        collect_program(block.program, name_resolved_map, lifetime_results, labels);
                    }
                }
            }
        }
        PrimaryLeftExpr::Loop { loop_expression } => {
            if let Ok(block) = &loop_expression.block {
                collect_program(block.program, name_resolved_map, lifetime_results, labels);
            }
        }
    }

    if let Some(mapping_operator) = &primary_left.mapping_operator {
        collect_mapping_operator(mapping_operator, name_resolved_map, lifetime_results, labels);
    }
}

fn collect_primary_right(
    primary_right: &PrimaryRight,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    lifetime_results: &LifetimeAnalyzeResults,
    labels: &mut Vec<DebugLabel>,
) {
    if let Some(second) = &primary_right.second {
        if let Some(function_call) = &second.function_call {
            for argument in function_call.arguments.iter() {
                collect_expression(argument, name_resolved_map, lifetime_results, labels);
            }
        }
    }

    if let Some(mapping_operator) = &primary_right.mapping_operator {
        collect_mapping_operator(mapping_operator, name_resolved_map, lifetime_results, labels);
    }
}

fn collect_mapping_operator(
    mapping_operator: &MappingOperator,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    lifetime_results: &LifetimeAnalyzeResults,
    labels: &mut Vec<DebugLabel>,
) {
    let block = match mapping_operator {
        MappingOperator::NullElvis { block, span: _ } => block,
        MappingOperator::ResultElvis { block, span: _ } => block,
        _ => return,
    };

    if let Ok(block) = block {
        collect_program(block.program, name_resolved_map, lifetime_results, labels);
    }
}

fn resolve_variable_entity(
    literal_entity: EntityID,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
) -> EntityID {
    name_resolved_map
        .get(&literal_entity)
        .filter(|resolved| resolved.define.kind == DefineKind::Variable)
        .map(|resolved| resolved.define.entity_id)
        .unwrap_or(literal_entity)
}

fn build_variable_label(
    variable_entity: EntityID,
    lifetime_results: &LifetimeAnalyzeResults,
) -> Option<(Color, String)> {
    let Some(origins) = lifetime_results.variable_origins(variable_entity) else {
        return Some((Color::Blue, "var lifetime: not-object".to_string()));
    };
    if origins.is_empty() {
        return Some((Color::Blue, "var lifetime: not-object".to_string()));
    }

    let mut has_stack = false;
    let mut has_heap = false;
    let mut unresolved_count = 0usize;
    let mut requires_drop = false;

    for origin in origins.iter() {
        if let Some(result) = lifetime_results.object_result(*origin) {
            match result.allocation {
                AllocationKind::Stack => has_stack = true,
                AllocationKind::Heap => has_heap = true,
            }
            requires_drop |= result.requires_drop;
        } else {
            unresolved_count += 1;
        }
    }

    let (color, allocation_text) = if has_heap {
        (Color::Yellow, "heap")
    } else if has_stack && unresolved_count == 0 {
        (Color::Cyan, "stack")
    } else if has_stack {
        (Color::Yellow, "stack?")
    } else {
        (Color::Red, "unknown")
    };

    let message = format!(
        "var lifetime: {} [drop: {}, origins: {}, unresolved: {}]",
        allocation_text,
        requires_drop,
        origins.len(),
        unresolved_count
    );

    Some((color, message))
}

fn build_object_label(
    object_entity: EntityID,
    lifetime_results: &LifetimeAnalyzeResults,
) -> Option<(Color, String)> {
    let result = lifetime_results.object_result(object_entity)?;
    let (color, allocation_text) = match result.allocation {
        AllocationKind::Stack => (Color::Cyan, "stack"),
        AllocationKind::Heap => (Color::Yellow, "heap"),
    };

    let message = format!(
        "object allocation: {} [drop: {}]",
        allocation_text, result.requires_drop
    );
    Some((color, message))
}

fn byte_range_to_char_range(source: &str, span: &Range<usize>) -> Range<usize> {
    let start = byte_to_char_index(source, span.start);
    let end = byte_to_char_index(source, span.end.max(span.start));
    start..end.max(start)
}

fn byte_to_char_index(source: &str, byte_index: usize) -> usize {
    let byte_index = normalize_byte_index(source, byte_index);
    source[..byte_index].chars().count()
}

fn normalize_byte_index(source: &str, byte_index: usize) -> usize {
    let mut byte_index = byte_index.min(source.len());

    while byte_index > 0 && !source.is_char_boundary(byte_index) {
        byte_index -= 1;
    }

    byte_index
}
