use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};
use catla_parser::parser::{AddOrSubExpression, AndExpression, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, Generics, GenericsDefine, Import, MappingOperator, MappingOperatorKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, PrimarySeparatorKind, Program, SimplePrimary, StatementAST, TypeAttributeEnum, TypeInfo, TypeTag};
use either::Either;
use fxhash::FxHashMap;

use crate::transpiler::{advice::AdviceReport, component::EntityID, context::TranspileModuleContext, error::{ErrorMessageKey, ErrorMessageType, TranspileReport}, name_resolver::FoundDefineInfo, TranspileError, TranspileWarning};


pub(crate) fn collect_import_module_program(
    ast: Program,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        if let StatementAST::Import(import) = statement {
            collect_import_module_import(import, import_element_map, errors, context);
        }
    }

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                collect_import_module_expression(assignment.left_expr, import_element_map, name_resolved_map, errors,warnings, context);
                if let Ok(right_expr) = &assignment.right_expr {
                    collect_import_module_expression(&right_expr, import_element_map, name_resolved_map, errors,warnings, context);
                }
            },
            StatementAST::Exchange(exchange) => {
                collect_import_module_expression(exchange.left_expr, import_element_map, name_resolved_map, errors,warnings, context);
                if let Ok(right_expr) = &exchange.right_expr {
                    collect_import_module_expression(&right_expr, import_element_map, name_resolved_map, errors,warnings, context);
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if let Some(block) = &function_define.block.value {
                    collect_import_module_program(block.program, import_element_map, name_resolved_map, errors,warnings, context);
                }
                if let Some(generics_define) = &function_define.generics_define {
                    collect_import_module_generics_define(generics_define, import_element_map, name_resolved_map, errors, warnings, context);
                }
                if let Some(type_tag) = &function_define.type_tag {
                    collect_import_module_type_tag(type_tag, import_element_map, name_resolved_map, errors, warnings, context);
                }
            },
            StatementAST::DataStructDefine(data_struct_define) => {
                if let Some(block) = &data_struct_define.block.value {
                    collect_import_module_program(block.program, import_element_map, name_resolved_map, errors, warnings, context);
                }
                if let Some(generics_define) = &data_struct_define.generics_define {
                    collect_import_module_generics_define(generics_define, import_element_map, name_resolved_map, errors, warnings, context);
                }
                if let Some(super_type_info) = &data_struct_define.super_type_info {
                    for type_info in super_type_info.type_infos.iter() {
                        collect_import_module_type_info(type_info, import_element_map, name_resolved_map, errors, warnings, context);
                    }
                }
            },
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    collect_import_module_expression(expression, import_element_map, name_resolved_map, errors,warnings, context);
                }
            },
            StatementAST::Expression(expression) => {
                collect_import_module_expression(&expression, import_element_map, name_resolved_map, errors,warnings, context);
            },
            StatementAST::VariableDefine(variable_define) => {
                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        collect_import_module_expression(&expression, import_element_map, name_resolved_map, errors,warnings, context);
                    }
                }
                if let Some(type_tag) = &variable_define.type_tag {
                    collect_import_module_type_tag(type_tag, import_element_map, name_resolved_map, errors, warnings, context);
                }
            },
            _ => {}
        }
    }
}

fn collect_import_module_import(
    ast: &Import,
    import_element_map: &mut FxHashMap<EntityID, String>,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext
) {
    let context = &context.context;

    let mut module_path_name = String::new();
    let size = ast.import_path.len();

    if size == 0 {
        let element = ast.elements.elements.first().unwrap();
        let module_name = element.value.to_string();

        if context.source_code_provider.exsists_source_code(&module_name) {
            import_element_map.insert(EntityID::from(element), module_name.clone());
        } else {
            let error = TranspileError::new(ModuleNotFoundError {
                module_names: Either::Left(module_name),
                span: element.span.clone(),
                advice_report: AdviceReport::new()
            });
            errors.push(error);
        }
        return;
    }

    for path in 0..size {
        module_path_name += ast.import_path[path].value;
        if path + 1 != size {
            module_path_name += "::";
        }
    }
    
    for element in ast.elements.elements.iter() {
        let module_name1 = module_path_name.clone();
        let module_name2 = module_path_name.clone() + "::" + element.value;

        let module_name = if context.source_code_provider.exsists_source_code(&module_name2) {
            module_name2
        } else if context.source_code_provider.exsists_source_code(&module_name1) {
            module_name1
        } else {
            let span = ast.import_path.first().unwrap().span.start..element.span.end;

            let error = TranspileError::new(ModuleNotFoundError {
                module_names: Either::Right([module_name1, module_name2]),
                span,
                advice_report: AdviceReport::new()
            });
            errors.push(error);
            continue
        };

        import_element_map.insert(EntityID::from(element), module_name);
    }
}

fn collect_import_module_generics_define(
    ast: &GenericsDefine,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    for element in ast.elements.iter() {
        for bound in element.bounds.iter() {
            collect_import_module_type_info(bound, import_element_map, name_resolved_map, errors, warnings, context);
        }
    }
}

fn collect_import_module_expression(
    ast: Expression,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            collect_import_module_and_expression(&or_expression.left_expr, import_element_map, name_resolved_map, errors,warnings, context);
            for right_expr in or_expression.right_exprs.iter() {
                if let Ok(right_expr) = &right_expr.1 {
                    collect_import_module_and_expression(right_expr, import_element_map, name_resolved_map, errors,warnings, context);
                }
            }
        },
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                collect_import_module_expression(expression, import_element_map, name_resolved_map, errors,warnings, context);
            }
        },
        ExpressionEnum::Closure(closure) => {
            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Either::Left(expression) => {
                        collect_import_module_expression(&expression, import_element_map, name_resolved_map, errors,warnings, context);
                    },
                    Either::Right(block) => {
                        collect_import_module_program(block.program, import_element_map, name_resolved_map, errors,warnings, context);
                    },
                }
            }
        }
    }
}

fn collect_import_module_and_expression(
    ast: &AndExpression,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_import_module_eqne_expression(&ast.left_expr, import_element_map, name_resolved_map, errors,warnings, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_import_module_eqne_expression(right_expr, import_element_map, name_resolved_map, errors,warnings, context);
        }
    }
}

fn collect_import_module_eqne_expression(
    ast: &EQNEExpression,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_import_module_compare_expression(&ast.left_expr, import_element_map, name_resolved_map, errors,warnings, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_import_module_compare_expression(right_expr, import_element_map, name_resolved_map, errors,warnings, context);
        }
    }
}

fn collect_import_module_compare_expression(
    ast: &CompareExpression,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_import_module_add_or_sub_expression(&ast.left_expr, import_element_map, name_resolved_map, errors,warnings, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_import_module_add_or_sub_expression(right_expr, import_element_map, name_resolved_map, errors,warnings, context);
        }
    }
}

fn collect_import_module_add_or_sub_expression(
    ast: &AddOrSubExpression,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_import_module_mul_or_div_expression(&ast.left_expr, import_element_map, name_resolved_map, errors,warnings, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_import_module_mul_or_div_expression(right_expr, import_element_map, name_resolved_map, errors,warnings, context);
        }
    }
}

fn collect_import_module_mul_or_div_expression(
    ast: &MulOrDivExpression,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_import_module_factor(&ast.left_expr, import_element_map, name_resolved_map, errors,warnings, context);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            collect_import_module_factor(right_expr, import_element_map, name_resolved_map, errors,warnings, context);
        }
    }
}

fn collect_import_module_factor(
    ast: &Factor,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Ok(primary) = &ast.primary {
        collect_import_module_primary(primary, import_element_map, name_resolved_map, errors,warnings, context);
    }
}

fn collect_import_module_primary(
    ast: &Primary,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    collect_import_module_primary_left(&ast.left, import_element_map, name_resolved_map, errors,warnings, context);
    let mut has_double_colon = false;
    for primary_right in ast.chain.iter() {
        if primary_right.separator.value == PrimarySeparatorKind::DoubleColon {
            has_double_colon = true;
        }
        collect_import_module_primary_right(primary_right, import_element_map, name_resolved_map, errors,warnings, context);
    }

    if ast.left.mapping_operator.is_none() && has_double_colon {
        if let PrimaryLeftExpr::Simple(simple) = &ast.left.first_expr {
            if simple.1.is_none() {
                if let SimplePrimary::Identifier(literal) = &simple.0 {
                    let mut module_name = if let Some(resolved) = name_resolved_map.get(&EntityID::from(literal)) {
                        let import_entity_id = resolved.define_info.entity_id;
                        import_element_map.get(&import_entity_id).unwrap().clone()
                    } else {
                        literal.value.to_string()
                    };
                    let mut last_name = "";

                    for i in 0..ast.chain.len() - 1 {
                        let right_primary = &ast.chain[i];

                        if right_primary.separator.value != PrimarySeparatorKind::DoubleColon {
                            break;
                        }
                        if let Some(expr) = &right_primary.second_expr {
                            if expr.1.is_some() {
                                break;
                            }
                            module_name += "::";
                            module_name += expr.0.value;
                            last_name = expr.0.value;
                        } else {
                            break;
                        }
                    }

                    if context.context.source_code_provider.exsists_source_code(&module_name) {
                        import_element_map.insert(EntityID::from(ast), module_name);
                    } else if !last_name.is_empty() {
                        let start = module_name.len() - (2 + last_name.len()) - 1;
                        let end = module_name.len() - 1;
                        module_name.replace_range(start..end, "");
                        
                        if context.context.source_code_provider.exsists_source_code(&module_name) {
                            import_element_map.insert(EntityID::from(ast), module_name);
                        }
                    }
                }
            }
        }
    }
}

fn collect_import_module_primary_left(
    ast: &PrimaryLeft,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expression { expression, error_tokens: _ } => {
                    if let Ok(expression) = expression {
                        collect_import_module_expression(&expression, import_element_map, name_resolved_map, errors,warnings, context);
                    }
                },
                _ => {}
            }

            if let Some(function_call) = &simple.1 {
                collect_import_module_function_call(function_call, import_element_map, name_resolved_map, errors,warnings, context);
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if new_expression.path.len() > 1 {
                let mut module_name = if let Some(resolved) = name_resolved_map.get(&EntityID::from(&new_expression.path[0])) {
                    let import_entity_id = resolved.define_info.entity_id;
                    import_element_map.get(&import_entity_id).unwrap().clone()
                } else {
                    new_expression.path[0].value.to_string()
                };
                
                for i in 1..new_expression.path.len() - 1 {
                    let path = &new_expression.path[i];
                    module_name += path.value;
                    if i != new_expression.path.len() - 2 {
                        module_name += "::";
                    }
                }

                if context.context.source_code_provider.exsists_source_code(&module_name) {
                    import_element_map.insert(EntityID::from(new_expression), module_name);
                }
            }
            
            if let Ok(function_call) = &new_expression.function_call {
                collect_import_module_function_call(function_call, import_element_map, name_resolved_map, errors,warnings, context);
            }
        },
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let first_statement = &if_expression.if_statement;
            if let Ok(condition) = &first_statement.condition {
                collect_import_module_expression(&condition, import_element_map, name_resolved_map, errors,warnings, context);
            }
            if let Some(block) = &first_statement.block.value {
                collect_import_module_program(block.program, import_element_map, name_resolved_map, errors,warnings, context);
            }

            for else_if_or_block in if_expression.chain.iter() {
                if let Some(else_if_or_block) = &else_if_or_block.else_if_or_else.value {
                    match else_if_or_block {
                        Either::Left(if_statement) => {
                            if let Ok(condition) = &if_statement.condition {
                                collect_import_module_expression(&condition, import_element_map, name_resolved_map, errors,warnings, context);
                            }
                            if let Some(block) = &if_statement.block.value {
                                collect_import_module_program(block.program, import_element_map, name_resolved_map, errors,warnings, context);
                            }
                        },
                        Either::Right(block) => {
                            collect_import_module_program(block.program, import_element_map, name_resolved_map, errors,warnings, context);
                        },
                    }
                }
            }
        },
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                collect_import_module_program(block.program, import_element_map, name_resolved_map, errors,warnings, context);
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_import_module_mapping_operator(mapping_operator, import_element_map, name_resolved_map, errors,warnings, context);
    }
}

fn collect_import_module_primary_right(
    ast: &PrimaryRight,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(function_call) = &second_expr.1 {
            collect_import_module_function_call(function_call, import_element_map, name_resolved_map, errors,warnings, context);
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_import_module_mapping_operator(mapping_operator, import_element_map, name_resolved_map, errors,warnings, context);
    }
}

fn collect_import_module_mapping_operator(
    ast: &MappingOperator,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
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
        collect_import_module_program(block.program, import_element_map, name_resolved_map, errors,warnings, context);
    }
}

fn collect_import_module_function_call(
    ast: &FunctionCall,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Ok(arg_exprs) = &ast.arg_exprs {
        for arg_expr in arg_exprs.iter() {
            collect_import_module_expression(&arg_expr, import_element_map, name_resolved_map, errors,warnings, context);
        }
    }
    if let Some(generics) = &ast.generics {
        if let Ok(generics) = generics {
            collect_import_module_generics(generics, import_element_map, name_resolved_map, errors, warnings, context);
        }
    }
}

fn collect_import_module_type_tag(
    ast: &TypeTag,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Ok(type_info) = &ast.type_info {
        collect_import_module_type_info(type_info, import_element_map, name_resolved_map, errors, warnings, context);
    }
}

fn collect_import_module_type_info(
    ast: &TypeInfo,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if ast.path.len() > 1 {
        let first_module_name = if let Some(resolved) = name_resolved_map.get(&EntityID::from(&ast.path[0])) {
            let import_entity_id = resolved.define_info.entity_id;
            import_element_map.get(&import_entity_id).cloned()
        } else {
            None
        };

        let mut module_name = first_module_name.unwrap_or_else(|| { ast.path[0].value.to_string() });

        if ast.path.len() > 2 {
            module_name += "::";
        }

        for i in 1..ast.path.len() - 1 {
            module_name += ast.path[i].value;
            if i != ast.path.len() - 2 {
                module_name += "::";
            }
        }

        if context.context.source_code_provider.exsists_source_code(&module_name) {
            import_element_map.insert(EntityID::from(ast), module_name);
        }
    }

    if let Some(generics) = &ast.generics {
        collect_import_module_generics(generics, import_element_map, name_resolved_map, errors, warnings, context);
    }
    
    for attribute in ast.type_attributes.iter() {
        if let TypeAttributeEnum::Result(error_type) = &attribute.value {
            if let Some(error_type) = error_type {
                collect_import_module_generics(error_type, import_element_map, name_resolved_map, errors, warnings, context);
            }
        }
    }
}

fn collect_import_module_generics(
    ast: &Generics,
    import_element_map: &mut FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    for element in ast.elements.iter() {
        collect_import_module_type_info(element, import_element_map, name_resolved_map, errors, warnings, context);
    }
}


pub(crate) struct ModuleNotFoundError {
    module_names: Either<String, [String; 2]>,
    span: Range<usize>,
    advice_report: AdviceReport
}

impl TranspileReport for ModuleNotFoundError {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = &context.module_name;
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(0027);

        let message = match &self.module_names {
            Either::Left(single) => {
                let raw_message = text.get_text("error.0027.message_single");
                raw_message.replace("%0", &single)
            },
            Either::Right(double) => {
                let raw_message = text.get_text("error.0027.message_double");
                raw_message.replace("%0", &double[0]).replace("%1", &double[1])
            },
        };

        let mut builder = Report::build(ReportKind::Error, module_name, self.span.start)
            .with_code(0027)
            .with_message(message);

        builder.add_label(
            Label::new((module_name, self.span.clone()))
                .with_color(Color::Red)
                .with_message(key.get_massage(text, ErrorMessageType::Label(0)))
        );

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        builder.finish().print((module_name, Source::from(context.source_code.code.as_str()))).unwrap();

        self.advice_report.print(context, self.span.start);
    }

    fn add_advice(&mut self, module_name: String, advice: crate::transpiler::advice::Advice) {
        self.advice_report.add_advice(module_name, advice);
    }
}