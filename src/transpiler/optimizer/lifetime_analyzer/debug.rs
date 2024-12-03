use std::ops::{Deref, Range};

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use catla_parser::parser::{
    AddOrSubExpression, AndExpression, CompareExpression, Expression, ExpressionEnum, Factor,
    FunctionCall, MulOrDivExpression, OrExpression, Primary, PrimaryLeft, PrimaryLeftExpr,
    PrimaryRight, Program, SimplePrimary, StatementAST, VariableBinding,
};
use either::Either;

use crate::transpiler::{component::EntityID, context::TranspileModuleContext};

pub fn print_lifetime_debug_info(ast: Program, context: &TranspileModuleContext) {
    let mut info = Vec::new();

    print_program(ast, &mut info, context);

    let mut builder = Report::build(
        ReportKind::Custom("Lifetime Debug", ariadne::Color::Cyan),
        context.module_name.deref(),
        0,
    );

    if info.is_empty() {
        return;
    }

    for info in info {
        let (color, text) = match info.alloc_type {
            AllocationType::Stack => (Color::Cyan, "stack"),
            AllocationType::Heap => (Color::Yellow, "heap"),
            AllocationType::Unknown => (Color::Red, "unknown"),
        };

        let contains_static = format!(
            " [has_static : {}, has_return_value : {}]",
            info.contains_static, info.has_return_value
        );

        let mut text = text.to_string().fg(color).to_string();
        text += contains_static.as_str();

        builder.add_label(
            Label::new((context.module_name.deref(), info.span))
                .with_color(color)
                .with_message(text),
        );
    }

    let _lock = context.context.debug_print_lock.lock().unwrap();

    builder
        .finish()
        .print((
            context.module_name.deref(),
            Source::from(context.source_code.code.as_str()),
        ))
        .unwrap();
}

struct LifetimeInfo {
    span: Range<usize>,
    alloc_type: AllocationType,
    contains_static: bool,
    has_return_value: bool,
}

impl LifetimeInfo {
    pub fn new(entity_id: EntityID, span: Range<usize>, context: &TranspileModuleContext) -> Self {
        let lifetime_tree = context
            .context
            .lifetime_evaluator
            .get_lifetime_tree_info(&context.module_name, entity_id);

        let (alloc_type, contains_static, has_return_value) = match lifetime_tree {
            Some((lifetime_tree, contains_static, has_return_value)) => {
                let alloc_type = if lifetime_tree.is_alloc_point {
                    AllocationType::Stack
                } else {
                    AllocationType::Heap
                };

                (alloc_type, contains_static, has_return_value)
            }
            None => (AllocationType::Unknown, false, false),
        };

        Self {
            span,
            alloc_type,
            contains_static,
            has_return_value,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AllocationType {
    Stack,
    Heap,
    Unknown,
}

fn print_program(ast: Program, info: &mut Vec<LifetimeInfo>, context: &TranspileModuleContext) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(_) => continue,
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                if let Ok(expression) = assignment.right_expr {
                    print_expression(expression, info, context);
                }
            }
            StatementAST::VariableDefine(variable_define) => {
                if let Ok(binding) = &variable_define.binding {
                    print_variable_binding(binding, info, context);
                }

                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        print_expression(*expression, info, context);
                    }
                }
            }
            StatementAST::FunctionDefine(function_define) => {
                for argument in function_define.args.arguments.iter() {
                    print_variable_binding(&argument.binding, info, context);
                }

                if let Some(block) = &function_define.block_or_semicolon.value {
                    if let Either::Right(block) = block {
                        print_program(block.program, info, context);
                    }
                }
            }
            StatementAST::UserTypeDefine(user_type_define) => {
                if let Some(block) = &user_type_define.block.value {
                    print_program(block.program, info, context);
                }
            }
            StatementAST::Implements(implements) => {
                if let Some(block) = &implements.block.value {
                    print_program(block.program, info, context);
                }
            }
            StatementAST::DropStatement(drop_statement) => {
                // TODO
            }
            StatementAST::Expression(expression) => {
                print_expression(expression, info, context);
            }
            _ => {}
        }
    }
}

fn print_variable_binding(
    ast: &VariableBinding,
    info: &mut Vec<LifetimeInfo>,
    context: &TranspileModuleContext,
) {
    match &ast.binding {
        Either::Left(literal) => {
            info.push(LifetimeInfo::new(
                EntityID::from(literal),
                literal.span.clone(),
                context,
            ));
        }
        Either::Right(bindings) => {
            for binding in bindings.iter() {
                print_variable_binding(binding, info, context);
            }
        }
    }
}

fn print_expression(
    ast: Expression,
    info: &mut Vec<LifetimeInfo>,
    context: &TranspileModuleContext,
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            print_or_expression(or_expression, info, context);
        }
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                print_expression(expression, info, context);
            }
        }
        ExpressionEnum::Closure(closure) => {
            // TODO
        }
    }
}

macro_rules! print_2op_expr {
    ($function_name:ident, $ast_type:ident, $next_layer_function_name:ident) => {
        fn $function_name(
            ast: &$ast_type,
            info: &mut Vec<LifetimeInfo>,
            context: &TranspileModuleContext,
        ) {
            $next_layer_function_name(&ast.left_expr, info, context);

            for (_, right_expr) in ast.right_exprs.iter() {
                if let Ok(right_expr) = right_expr {
                    $next_layer_function_name(right_expr, info, context);
                }
            }
        }
    };
}

print_2op_expr!(print_or_expression, OrExpression, print_and_expression);

print_2op_expr!(
    print_and_expression,
    AndExpression,
    print_compare_expression
);

print_2op_expr!(
    print_compare_expression,
    CompareExpression,
    print_add_or_sub_expression
);

print_2op_expr!(
    print_add_or_sub_expression,
    AddOrSubExpression,
    print_mul_or_div_expression
);

print_2op_expr!(
    print_mul_or_div_expression,
    MulOrDivExpression,
    print_factor
);

fn print_factor(ast: &Factor, info: &mut Vec<LifetimeInfo>, context: &TranspileModuleContext) {
    if let Ok(primary) = &ast.primary {
        print_primary(primary, info, context);
    }
}

fn print_primary(ast: &Primary, info: &mut Vec<LifetimeInfo>, context: &TranspileModuleContext) {
    print_primary_left(&ast.left, info, context);

    for primary_right in ast.chain.iter() {
        print_primary_right(primary_right, info, context);
    }
}

fn print_primary_left(
    ast: &PrimaryLeft,
    info: &mut Vec<LifetimeInfo>,
    context: &TranspileModuleContext,
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple((simple_primary, mapping_operator, function_call)) => {
            print_simple_primary(simple_primary, info, context);

            // TODO : mapping operator

            if let Some(function_call) = function_call {
                print_function_call(function_call, info, context);
            }
        }
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {
            if let Ok(expression) = new_array_init_expression.init_expression {
                print_expression(expression, info, context);
            }
            if let Ok(expression) = new_array_init_expression.length_expression {
                print_expression(expression, info, context);
            }

            info.push(LifetimeInfo::new(
                EntityID::from(new_array_init_expression),
                new_array_init_expression.span.clone(),
                context,
            ));
        }
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {
            for expression in new_array_expression.value_expressions.iter() {
                if let Ok(expression) = expression {
                    print_expression(expression, info, context);
                }
            }

            info.push(LifetimeInfo::new(
                EntityID::from(new_array_expression),
                new_array_expression.span.clone(),
                context,
            ));
        }
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(field_assigns) = &new_expression.field_assigns {
                for field_assign in field_assigns.iter() {
                    if let Ok(expression) = field_assign.expression {
                        print_expression(expression, info, context);
                    }
                }
            }

            info.push(LifetimeInfo::new(
                EntityID::from(new_expression),
                new_expression.span.clone(),
                context,
            ));
        }
        PrimaryLeftExpr::IfExpression(if_expression) => {
            if let Ok(expression) = if_expression.if_statement.condition {
                print_expression(expression, info, context);
            }
            if let Some(block) = &if_expression.if_statement.block.value {
                print_program(block.program, info, context);
            }

            for chain in if_expression.chain.iter() {
                if let Some(chain) = &chain.else_if_or_else.value {
                    match chain {
                        Either::Left(if_statement) => {
                            if let Ok(expression) = if_statement.condition {
                                print_expression(expression, info, context);
                            }
                            if let Some(block) = &if_statement.block.value {
                                print_program(block.program, info, context);
                            }
                        }
                        Either::Right(block) => {
                            print_program(block.program, info, context);
                        }
                    }
                }
            }
        }
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            // TODO
        }
    }
}

fn print_primary_right(
    ast: &PrimaryRight,
    info: &mut Vec<LifetimeInfo>,
    context: &TranspileModuleContext,
) {
    if let Some((_, _, function_call)) = &ast.second_expr {
        if let Some(function_call) = function_call {
            print_function_call(function_call, info, context);
        }
    }

    // TODO : mapping operator
}

fn print_simple_primary(
    ast: &SimplePrimary,
    info: &mut Vec<LifetimeInfo>,
    context: &TranspileModuleContext,
) {
    match ast {
        SimplePrimary::Expressions {
            expressions,
            error_tokens: _,
            span: _,
        } => {
            for expression in expressions.iter() {
                print_expression(expression, info, context);
            }
        }
        _ => {}
    }
}

fn print_function_call(
    ast: &FunctionCall,
    info: &mut Vec<LifetimeInfo>,
    context: &TranspileModuleContext,
) {
    if let Ok(arguments) = &ast.arg_exprs {
        for expression in arguments.iter() {
            print_expression(expression, info, context);
        }
    }

    // TODO? : return value
}
