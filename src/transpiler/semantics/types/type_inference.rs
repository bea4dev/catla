use std::{mem, ops::DerefMut, sync::{Arc, Mutex}};

use ariadne::Color;
use bumpalo::Bump;
use catla_parser::parser::{AddOrSubExpression, AndExpression, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, FunctionDefine, GenericsDefine, MappingOperator, MappingOperatorKind, MemoryManageAttributeKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, Spanned, StatementAST, StatementAttributeKind, TypeAttributeEnum, TypeInfo};
use either::Either;
use fxhash::FxHashMap;

use crate::transpiler::{component::EntityID, context::TranspileModuleContext, error::SimpleError, name_resolver::{DefineKind, FoundDefineInfo}, TranspileError, TranspileWarning};

use super::type_info::{FunctionType, GenericType, Type};


#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum SubstitutionElement {
    EntityID(Spanned<EntityID>),
    Type(Spanned<Type>)
}

impl SubstitutionElement {
    
    pub fn is(&self, element: SubstitutionElement) -> Substitution {
        Substitution { left: self.clone(), right: element }
    }

}


#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Substitution {
    left: SubstitutionElement,
    right: SubstitutionElement
}



pub(crate) fn type_inference_program<'allocator>(
    ast: Program,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let mut type_environment = Vec::new_in(allocator);

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                type_inference_expression(
                    assignment.left_expr,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    &mut type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
                if let Ok(right_expr) = &assignment.right_expr {
                    type_inference_expression(
                        &right_expr,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        &mut type_environment,
                        type_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );
                }
            },
            StatementAST::Exchange(exchange) => {
                type_inference_expression(
                    exchange.left_expr,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    &mut type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
                if let Ok(right_expr) = &exchange.right_expr {
                    type_inference_expression(
                        &right_expr,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        &mut type_environment,
                        type_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if let Some(block) = &function_define.block.value {
                    type_inference_program(
                        block.program,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        type_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );
                }
            },
            StatementAST::DataStructDefine(data_struct_define) => {
                if let Ok(name) = &data_struct_define.name {
                    if let Some(block) = &data_struct_define.block.value {
                        type_inference_program(
                            block.program,
                            user_type_map,
                            import_element_map,
                            name_resolved_map, module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            generics_map,
                            type_map,
                            allocator,
                            errors,
                            warnings,
                            context
                        );
                    }
                }
            },
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    type_inference_expression(
                        expression,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        &mut type_environment,
                        type_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );
                }
            },
            StatementAST::Expression(expression) => {
                type_inference_expression(
                    &expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    &mut type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
            },
            StatementAST::VariableDefine(variable_define) => {
                let tag_type = match &variable_define.type_tag {
                    Some(type_tag) => {
                        match &type_tag.type_info {
                            Ok(type_info) => {
                                get_type(
                                    type_info,
                                    user_type_map,
                                    import_element_map,
                                    name_resolved_map,
                                    module_user_type_map,
                                    module_element_type_map,
                                    module_element_type_maps,
                                    generics_map,
                                    &mut type_environment,
                                    type_map,
                                    allocator,errors,warnings,
                                    context
                                )
                            },
                            _ => Type::Unknown
                        }
                    },
                    _ => Type::Unknown
                };

                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        type_inference_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            generics_map,
                            type_environment,
                            type_map,
                            allocator,
                            errors,
                            warnings,
                            context
                        );
                    }
                }
            }
            _ => {}
        }
    }
}

fn type_inference_expression<'allocator>(
    ast: Expression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            type_inference_and_expression(
                &or_expression.left_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
            for right_expr in or_expression.right_exprs.iter() {
                if let Ok(right_expr) = &right_expr.1 {
                    type_inference_and_expression(
                        right_expr,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        type_environment,
                        type_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );
                }
            }
        },
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                type_inference_expression(
                    expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
            }
        },
        ExpressionEnum::Closure(closure) => {
            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Either::Left(expression) => {
                        type_inference_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            generics_map,
                            type_environment,
                            type_map,
                            allocator,
                            errors,
                            warnings,
                            context
                        );
                    },
                    Either::Right(block) => {
                        type_inference_program(
                            block.program,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            generics_map,
                            type_environment,
                            type_map,
                            allocator,
                            errors,
                            warnings,
                            None,
                            context
                        );
                    }
                }
            }
        }
    }
}

fn type_inference_and_expression<'allocator>(
    ast: &AndExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    type_inference_eqne_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        type_environment,
        type_map,
        allocator,
        errors,
        warnings,
        context
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            type_inference_eqne_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}

fn type_inference_eqne_expression<'allocator>(
    ast: &EQNEExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    type_inference_compare_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        type_environment,
        type_map,
        allocator,
        errors,
        warnings,
        context
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            type_inference_compare_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}

fn type_inference_compare_expression<'allocator>(
    ast: &CompareExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    type_inference_add_or_sub_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        type_environment,
        type_map,
        allocator,
        errors,
        warnings,
        context
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            type_inference_add_or_sub_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}

fn type_inference_add_or_sub_expression<'allocator>(
    ast: &AddOrSubExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    type_inference_mul_or_div_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        type_environment,
        type_map,
        allocator,
        errors,
        warnings,
        context
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            type_inference_mul_or_div_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}

fn type_inference_mul_or_div_expression<'allocator>(
    ast: &MulOrDivExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    type_inference_factor(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        type_environment,
        type_map,
        allocator,
        errors,
        warnings,
        context
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            type_inference_factor(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}

fn type_inference_factor<'allocator>(
    ast: &Factor,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Ok(primary) = &ast.primary {
        type_inference_primary(
            primary,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            type_environment,
            type_map,
            allocator,
            errors,
            warnings,
            context
        );
    }
}

fn type_inference_primary<'allocator>(
    ast: &Primary,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    type_inference_primary_left(
        &ast.left,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        type_environment,
        type_map,
        allocator,
        errors,
        warnings,
        context
    );
    for primary_right in ast.chain.iter() {
        type_inference_primary_right(
            primary_right,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            type_environment,
            type_map,
            allocator,
            errors,
            warnings,
            context
        );
    }
}

fn type_inference_primary_left<'allocator>(
    ast: &PrimaryLeft,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expression { expression, error_tokens: _ } => {
                    if let Ok(expression) = expression {
                        type_inference_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            generics_map,
                            type_environment,
                            type_map,
                            allocator,
                            errors,
                            warnings,
                            context
                        );
                    }
                },
                _ => {}
            }

            if let Some(function_call) = &simple.1 {
                type_inference_function_call(
                    function_call,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(function_call) = &new_expression.function_call {
                type_inference_function_call(
                    function_call,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
            }
        },
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let first_statement = &if_expression.if_statement;
            if let Ok(condition) = &first_statement.condition {
                type_inference_expression(
                    &condition,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
            }
            if let Some(block) = &first_statement.block.value {
                type_inference_program(
                    block.program,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    None,
                    context
                );
            }

            for else_if_or_block in if_expression.chain.iter() {
                if let Some(else_if_or_block) = &else_if_or_block.else_if_or_else.value {
                    match else_if_or_block {
                        Either::Left(if_statement) => {
                            if let Ok(condition) = &if_statement.condition {
                                type_inference_expression(
                                    &condition,
                                    user_type_map,
                                    import_element_map,
                                    name_resolved_map,
                                    module_user_type_map,
                                    module_element_type_map,
                                    module_element_type_maps,
                                    generics_map,
                                    type_environment,
                                    type_map,
                                    allocator,
                                    errors,
                                    warnings,
                                    context
                                );
                            }
                            if let Some(block) = &if_statement.block.value {
                                type_inference_program(
                                    block.program,
                                    user_type_map,
                                    import_element_map,
                                    name_resolved_map,
                                    module_user_type_map,
                                    module_element_type_map,
                                    module_element_type_maps,
                                    generics_map,
                                    type_environment,
                                    type_map,
                                    allocator,
                                    errors,
                                    warnings,
                                    None,
                                    context
                                );
                            }
                        },
                        Either::Right(block) => {
                            type_inference_program(
                                block.program,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                module_element_type_maps,
                                generics_map,
                                type_environment,
                                type_map,
                                allocator,
                                errors,
                                warnings,
                                None,
                                context
                            );
                        }
                    }
                }
            }
        },
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                type_inference_program(
                    block.program,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    None,
                    context
                );
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        type_inference_mapping_operator(
            mapping_operator,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            type_environment,
            type_map,
            allocator,
            errors,
            warnings,
            context
        );
    }
}

fn type_inference_primary_right<'allocator>(
    ast: &PrimaryRight,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(function_call) = &second_expr.1 {
            type_inference_function_call(
                function_call,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        type_inference_mapping_operator(
            mapping_operator,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            type_environment,
            type_map,
            allocator,
            errors,
            warnings,
            context
        );
    }
}

fn type_inference_mapping_operator<'allocator>(
    ast: &MappingOperator,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let block = match &ast.value {
        MappingOperatorKind::NullElvisBlock(block) => block,
        MappingOperatorKind::ResultElvisBlock(block) => block,
        _ => return
    };

    if let Some(block) = &block.value {
        type_inference_program(
            block.program,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            type_environment,
            type_map,
            allocator,
            errors,
            warnings,
            None,
            context
        );
    }
}

fn type_inference_function_call<'allocator>(
    ast: &FunctionCall,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Ok(arg_exprs) = &ast.arg_exprs {
        for arg_expr in arg_exprs.iter() {
            type_inference_expression(
                &arg_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}

fn get_type(
    ast: &TypeInfo,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    type_environment: &mut Vec<Substitution, &'allocator Bump>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) -> Type {
    if ast.path.is_empty() {
        return Type::Unknown;
    }

    let mut type_info = if let Some(module_name) = import_element_map.get(&EntityID::from(ast)) {
        let type_map = module_user_type_map.get(module_name).unwrap();
        match type_map.get(ast.path.last().unwrap().value) {
            Some(type_info) => type_info.clone(),
            _ => {
                let span = ast.path.last().unwrap().span.clone();
                let error = SimpleError::new(
                    0030,
                    span.clone(),
                    vec![module_name.clone(), ast.path.last().unwrap().value.to_string()],
                    vec![(span, Color::Red)]
                );
                errors.push(error);
                return Type::Unknown
            }
        }
    } else {
        match name_resolved_map.get(&EntityID::from(&ast.path[0])) {
            Some(resolved) => {
                let ty = match resolved.define_info.define_kind {
                    DefineKind::UserType => user_type_map.get(ast.path[0].value).unwrap().clone(),
                    DefineKind::Generics => {
                        Type::Generic(generics_map.get(&EntityID::from(resolved.define_info.entity_id)).unwrap().clone())
                    },
                    DefineKind::Import => {
                        let module_name = import_element_map.get(&EntityID::from(resolved.define_info.entity_id)).unwrap();
                        let user_type_map = module_user_type_map.get(module_name).unwrap();
                        match user_type_map.get(ast.path[0].value) {
                            Some(user_type) => user_type.clone(),
                            None => Type::Unknown
                        }
                    },
                    _ => Type::Unknown
                };
                
                if ty == Type::Unknown {
                    let text = resolved.define_info.define_kind.get_name(&context.context.localized_text);
                    let span = ast.path[0].span.clone();

                    let error = SimpleError::new(
                        0033,
                        span.clone(),
                        vec![text],
                        vec![(span, Color::Red), (resolved.define_info.span.clone(), Color::Yellow)]
                    );
                    errors.push(error);
                }

                ty
            },
            _ => {
                match ast.path[0].value {
                    "int" => Type::Int32,
                    "int8" => Type::Int8,
                    "int16" => Type::Int16,
                    "int32" => Type::Int32,
                    "int64" => Type::Int64,
                    "uint" => Type::Uint32,
                    "uint8" => Type::Uint8,
                    "uint16" => Type::Uint16,
                    "uint32" => Type::Uint32,
                    "uint64" => Type::Uint64,
                    "float" => Type::Float32,
                    "float32" => Type::Float32,
                    "float64" => Type::Float64,
                    "bool" => Type::Bool,
                    "unit" => Type::Unit,
                    _ => Type::Unknown
                }
            }
        }
    };

    if let Some(generics_ast) = &ast.generics {
        let mut generics_types = Vec::new();
        for element in generics_ast.elements.iter() {
            let ty = get_type(
                element,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
            generics_types.push(ty);
        }

        type_info = match &type_info {
            Type::DataStruct { data_struct_info, generics: _ } => {
                Type::DataStruct { data_struct_info: data_struct_info.clone(), generics: Arc::new(generics_types) }
            },
            Type::Function { function_info, generics: _ } => {
                Type::Function { function_info: function_info.clone(), generics: Arc::new(generics_types) }
            },
            _ => {
                let span_0 = ast.path.last().unwrap().span.clone();
                let span_1 = generics_ast.span.clone();

                let error = SimpleError::new(
                    0031,
                    span_1.clone(),
                    vec![],
                    vec![(span_0, Color::Yellow), (span_1, Color::Red)]
                );
                errors.push(error);
                
                type_info.clone()
            }
        };
    }

    for attribute in ast.type_attributes.iter() {
        type_info = match &attribute.value {
            TypeAttributeEnum::Optional => Type::Option(Arc::new(type_info)),
            TypeAttributeEnum::Result(error_type) => {
                let error_type = match error_type {
                    Some(error_type) => {
                        if error_type.elements.len() == 1 {
                            get_type(
                                &error_type.elements[0],
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                module_element_type_maps,
                                generics_map,
                                type_environment,
                                type_map,
                                allocator,
                                errors,
                                warnings,
                                context
                            )
                        } else {
                            let span_0_start = ast.path.last().unwrap().span.start;
                            let span_0_end = attribute.span.end;
                            let span_0 = span_0_start..span_0_end;
                            let span_1 = error_type.span.clone();

                            let error = SimpleError::new(
                                0032,
                                span_1.clone(),
                                vec![1.to_string(), error_type.elements.len().to_string()],
                                vec![(span_0, Color::Yellow), (span_1, Color::Red)]
                            );
                            errors.push(error);
                            Type::Unknown
                        }
                    },
                    _ => Type::Unit // TODO - default error class object
                };
                Type::Result { value: Arc::new(type_info), error: Arc::new(error_type) }
            }
        };
    }

    type_info
}
