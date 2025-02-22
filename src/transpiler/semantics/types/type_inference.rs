use std::{
    any::type_name,
    ops::{Deref, Range},
    sync::{Arc, RwLock},
};

use allocator_api2::vec;
use allocator_api2::vec::Vec;
use ariadne::{sources, Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use bumpalo::{collections::CollectIn, Bump};
use catla_parser::parser::{
    AddOrSubExpression, AndExpression, Block, Closure, CompareExpression, Expression,
    ExpressionEnum, Factor, FunctionCall, FunctionDefine, Generics, MappingOperator,
    MappingOperatorKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight,
    PrimarySeparatorKind, Program, SimplePrimary, Spanned, StatementAST, StatementAttributeKind,
    StatementAttributes, UserTypeKindEnum, VariableBinding,
};
use either::Either;
use fxhash::FxHashMap;
use hashbrown::{DefaultHashBuilder, HashMap};
use indexmap::IndexMap;

use crate::transpiler::{
    advice::Advice,
    component::EntityID,
    context::TranspileModuleContext,
    error::{ErrorMessageKey, ErrorMessageType, SimpleError, TranspileReport},
    name_resolver::{DefineKind, EnvironmentSeparatorKind, FoundDefineInfo},
    semantics::types::type_info::LazyGenericTypeInferError,
    TranspileError, TranspileWarning,
};

use super::{
    import_module_collector::{get_module_name_from_new_expression, get_module_name_from_primary},
    type_info::{
        Bound, FreezableMutex, FunctionDefineInfo, FunctionType, GenericType, ImplementsInfo,
        ImplementsInfoSet, LocalGenericID, OverrideElementsEnvironment, ScopeThisType, Type,
        WhereBound, WithDefineInfo,
    },
    user_type_element_collector::{get_type, parse_primitive_type},
};

const IF_CONDITION_TYPE_ERROR: usize = 0038;
const FUNCTION_CALL_TYPE_ERROR: usize = 0039;
const MAPPING_OPERATOR_TYPE_ERROR: usize = 0041;
const BOUNDS_NOT_SATISFIED_ERROR: usize = 0046;
const INVALID_SET_GENERICS_TYPE_ERROR: usize = 0047;
const NUMBER_OF_GENERICS_MISMATCH_ERROR: usize = 0048;
const WHERE_BOUNDS_NOT_SATISFIED_ERROR: usize = 0050;
const DUPLICATED_ELEMENT_ERROR: usize = 0051;
const UNRESOLVED_INTERFACE: usize = 0052;
const INVALID_OVERRIDE_ERROR: usize = 0056;
const INVALID_ARRAY_INIT_EXPR_TYPE: usize = 0067;
const INVALID_ARRAY_LENGTH_EXPR_TYPE: usize = 0068;
const INVALID_PRIMARY_SEPARATOR: usize = 0071;
const NOT_IMPLEMENTS_OPERATOR_INTERFACE: usize = 0072;
const INVALID_LOGICAL_OPERATOR_EXPR_TYPE: usize = 0076;
const INVALID_TUPLE_LENGTH: usize = 0078;
const INVALID_TYPE_TUPLE_BINDING: usize = 0079;

pub(crate) struct TypeEnvironment<'input, 'allocator> {
    entity_type_map: HashMap<
        EntityID,
        Either<EntityID, WithDefineInfo<Type>>,
        DefaultHashBuilder,
        &'allocator Bump,
    >,
    generic_type_map: HashMap<
        LocalGenericID,
        Either<LocalGenericID, WithDefineInfo<Type>>,
        DefaultHashBuilder,
        &'allocator Bump,
    >,
    generic_bounds_checks: Vec<GenericsBoundCheck, &'allocator Bump>,
    impl_interface_generics_check: Vec<(Range<usize>, WithDefineInfo<Type>), &'allocator Bump>,
    implicit_convert_map:
        HashMap<EntityID, ImplicitConvertKind, DefaultHashBuilder, &'allocator Bump>,
    return_type: Either<EntityID, WithDefineInfo<Type>>,
    closures: Vec<&'allocator Closure<'input, 'allocator>, &'allocator Bump>,
    lazy_generic_infer_type: Vec<LazyGenericTypeInference, &'allocator Bump>,
    lazy_type_reports: Vec<Box<dyn LazyTypeReport>, &'allocator Bump>,
    current_generics_id: usize,
    var_span_and_entity_ids: Vec<(Range<usize>, EntityID), &'allocator Bump>,
    operator_function_types: Vec<(EntityID, Type), &'allocator Bump>,
    operator_return_types: Vec<(EntityID, Type), &'allocator Bump>,
    function_equals_info: Vec<(Arc<FunctionType>, Arc<FunctionType>), &'allocator Bump>,
}

impl<'input, 'allocator> TypeEnvironment<'input, 'allocator> {
    pub fn new_with_return_type(
        return_type: Either<EntityID, WithDefineInfo<Type>>,
        allocator: &'allocator Bump,
    ) -> TypeEnvironment<'input, 'allocator> {
        Self {
            entity_type_map: HashMap::new_in(allocator),
            generic_type_map: HashMap::new_in(allocator),
            generic_bounds_checks: Vec::new_in(allocator),
            impl_interface_generics_check: Vec::new_in(allocator),
            implicit_convert_map: HashMap::new_in(allocator),
            return_type,
            closures: Vec::new_in(allocator),
            lazy_generic_infer_type: Vec::new_in(allocator),
            lazy_type_reports: Vec::new_in(allocator),
            current_generics_id: 0,
            var_span_and_entity_ids: Vec::new_in(allocator),
            operator_function_types: Vec::new_in(allocator),
            operator_return_types: Vec::new_in(allocator),
            function_equals_info: Vec::new_in(allocator),
        }
    }

    pub fn new_local_generic_id(
        &mut self,
        type_span: Range<usize>,
        module_name: Arc<String>,
    ) -> LocalGenericID {
        self.current_generics_id += 1;
        let generic_id = LocalGenericID(self.current_generics_id);

        self.generic_type_map.insert(
            generic_id,
            Either::Right(WithDefineInfo {
                value: Type::Unknown,
                module_name,
                span: type_span,
            }),
        );

        generic_id
    }

    pub fn get_user_or_function_type_with_local_generic_id(
        &mut self,
        ty: WithDefineInfo<Type>,
        current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
        register_bounds_check: bool,
    ) -> WithDefineInfo<Type> {
        let generics_define = match &ty.value {
            Type::UserType {
                user_type_info,
                generics: _,
                generics_span: _,
            } => &user_type_info.generics_define,
            Type::Function {
                function_info,
                generics: _,
            } => &function_info.generics_define,
            _ => return ty.clone(),
        };

        let number_of_generics = generics_define.len();
        let mut local_generics = Vec::with_capacity(number_of_generics);
        for _ in 0..number_of_generics {
            let generic_id = self.new_local_generic_id(ty.span.clone(), ty.module_name.clone());
            local_generics.push(Type::LocalGeneric(generic_id));
        }
        let local_generics = Arc::new(local_generics);

        self.lazy_generic_infer_type.push(LazyGenericTypeInference {
            origin_type: ty.clone(),
            generics_define: generics_define.clone(),
            local_generics: local_generics.clone(),
            where_bound: ty.value.get_where_bounds().unwrap().clone(),
            current_scope_implements_info_set: current_scope_implements_info_set.clone(),
        });

        let ty = match &ty.value {
            Type::UserType {
                user_type_info,
                generics: _,
                generics_span: _,
            } => WithDefineInfo {
                value: Type::UserType {
                    user_type_info: user_type_info.clone(),
                    generics: local_generics.clone(),
                    generics_span: None,
                },
                module_name: ty.module_name.clone(),
                span: ty.span.clone(),
            },
            Type::Function {
                function_info,
                generics: _,
            } => WithDefineInfo {
                value: Type::Function {
                    function_info: function_info.clone(),
                    generics: local_generics.clone(),
                },
                module_name: ty.module_name.clone(),
                span: ty.span.clone(),
            },
            _ => unreachable!(),
        };

        for (generic_define, local_generic) in ty
            .value
            .get_generics_define_with_replaced_generic()
            .unwrap()
            .iter()
            .zip(local_generics.iter())
        {
            self.generic_bounds_checks
                .push(GenericsBoundCheck::Generics {
                    type_span: ty.span.clone(),
                    ty: WithDefineInfo {
                        value: local_generic.clone(),
                        module_name: ty.module_name.clone(),
                        span: ty.span.clone(),
                    },
                    generic_define: generic_define.clone(),
                    scope_implements_info_set: current_scope_implements_info_set.clone(),
                });
        }

        if register_bounds_check {
            let where_bounds = ty.value.get_where_bounds_with_replaced_generic().unwrap();
            for where_bound in where_bounds.iter() {
                self.generic_bounds_checks.push(GenericsBoundCheck::Where {
                    type_span: ty.span.clone(),
                    target_type: where_bound.target_type.value.clone(),
                    bounds: where_bound.bounds.clone(),
                    scope_implements_info_set: current_scope_implements_info_set.clone(),
                });
            }
        }

        ty
    }

    pub fn set_entity_type(&mut self, entity_id: EntityID, ty: WithDefineInfo<Type>) {
        let next_entity_id_or_type = match self.entity_type_map.get(&entity_id) {
            Some(value) => value,
            None => {
                self.entity_type_map.insert(entity_id, Either::Right(ty));
                return;
            }
        };

        match next_entity_id_or_type {
            Either::Left(entity_id) => self.set_entity_type(*entity_id, ty),
            Either::Right(_) => {
                self.entity_type_map.insert(entity_id, Either::Right(ty));
            }
        }
    }

    pub fn set_generic_type(&mut self, generic_id: LocalGenericID, ty: WithDefineInfo<Type>) {
        let next_generic_id_or_type = match self.generic_type_map.get(&generic_id) {
            Some(value) => value,
            None => {
                self.generic_type_map.insert(generic_id, Either::Right(ty));
                return;
            }
        };

        match next_generic_id_or_type {
            Either::Left(generic_id) => self.set_generic_type(*generic_id, ty),
            Either::Right(_) => {
                self.generic_type_map.insert(generic_id, Either::Right(ty));
            }
        }
    }

    pub fn set_entity_id_equals(&mut self, first_entity_id: EntityID, second_entity_id: EntityID) {
        if first_entity_id == second_entity_id {
            return;
        }
        self.entity_type_map
            .insert(second_entity_id, Either::Left(first_entity_id));
    }

    pub fn unify(
        &mut self,
        first_entity_id: Spanned<EntityID>,
        second_entity_id: Spanned<EntityID>,
        current_scope_this_type: &ScopeThisType,
    ) -> Result<(), TypeMismatchError> {
        let mut first_resolved = self.resolve_entity_type_with_id(first_entity_id.value);
        let mut second_resolved = self.resolve_entity_type_with_id(second_entity_id.value);
        first_resolved.1.span = first_entity_id.span.clone();
        second_resolved.1.span = second_entity_id.span.clone();

        if first_resolved.0 == second_resolved.0 {
            return Ok(());
        }

        match (
            first_resolved
                .1
                .value
                .is_replaceable_with(&second_resolved.1.value),
            second_resolved
                .1
                .value
                .is_replaceable_with(&first_resolved.1.value),
        ) {
            (true, true) => {
                self.entity_type_map
                    .insert(second_resolved.0, Either::Left(first_resolved.0));
            }
            (true, false) => {
                self.entity_type_map
                    .insert(first_resolved.0, Either::Right(second_resolved.1));
                self.entity_type_map
                    .insert(second_resolved.0, Either::Left(first_resolved.0));
            }
            (false, true) => {
                self.entity_type_map
                    .insert(second_resolved.0, Either::Left(first_resolved.0));
            }
            (false, false) => {
                return self.unify_type(
                    &first_resolved.1.value,
                    &first_resolved.1.span,
                    &first_resolved.1.module_name,
                    &second_resolved.1.value,
                    &second_resolved.1.span,
                    &second_resolved.1.module_name,
                    current_scope_this_type,
                    false,
                    true,
                );
            }
        }

        Ok(())
    }

    pub fn unify_with_implicit_convert(
        &mut self,
        first_entity_id: Spanned<EntityID>,
        second_entity_id: Spanned<EntityID>,
        first_is_expr: bool,
        current_scope_this_type: &ScopeThisType,
    ) -> Result<(), TypeMismatchError> {
        let mut first_resolved = self.resolve_entity_type_with_id(first_entity_id.value);
        let mut second_resolved = self.resolve_entity_type_with_id(second_entity_id.value);
        first_resolved.1.span = first_entity_id.span.clone();
        second_resolved.1.span = second_entity_id.span.clone();

        if first_resolved.0 == second_resolved.0 {
            return Ok(());
        }

        match (
            first_resolved
                .1
                .value
                .is_replaceable_with(&second_resolved.1.value),
            second_resolved
                .1
                .value
                .is_replaceable_with(&first_resolved.1.value),
        ) {
            (true, true) => {
                self.entity_type_map
                    .insert(second_resolved.0, Either::Left(first_resolved.0));
            }
            (true, false) => {
                self.entity_type_map
                    .insert(first_resolved.0, Either::Right(second_resolved.1));
                self.entity_type_map
                    .insert(second_resolved.0, Either::Left(first_resolved.0));
            }
            (false, true) => {
                self.entity_type_map
                    .insert(second_resolved.0, Either::Left(first_resolved.0));
            }
            (false, false) => {
                let result = self.unify_type_with_implicit_convert(
                    &first_resolved.1.value,
                    &first_resolved.1.span,
                    &first_resolved.1.module_name,
                    &second_resolved.1.value,
                    &second_resolved.1.span,
                    &second_resolved.1.module_name,
                    current_scope_this_type,
                    first_is_expr,
                )?;

                if let Some(convert_kind) = result {
                    let entity_id = if first_is_expr {
                        first_entity_id.value
                    } else {
                        second_entity_id.value
                    };
                    self.implicit_convert_map.insert(entity_id, convert_kind);
                }
            }
        }

        Ok(())
    }

    fn unify_type_with_implicit_convert(
        &mut self,
        first_type: &Type,
        first_span: &Range<usize>,
        first_module_name: &Arc<String>,
        second_type: &Type,
        second_span: &Range<usize>,
        second_module_name: &Arc<String>,
        current_scope_this_type: &ScopeThisType,
        first_is_expr: bool,
    ) -> Result<Option<ImplicitConvertKind>, TypeMismatchError> {
        if first_is_expr {
            let second_resolved_type = if let Type::LocalGeneric(generic_id) = second_type {
                self.resolve_generic_type(*generic_id).1.value
            } else {
                second_type.clone()
            };

            if second_resolved_type.is_option_or_result() {
                let first_resolved_type = if let Type::LocalGeneric(generic_id) = first_type {
                    self.resolve_generic_type(*generic_id).1.value
                } else {
                    first_type.clone()
                };

                let mut implicit_convert = None;

                let new_first_type = if let Type::Option(_) = &second_resolved_type {
                    if let Type::Option(_) = &first_resolved_type {
                        first_type.clone()
                    } else {
                        implicit_convert = Some(ImplicitConvertKind::Some);
                        Type::Option(Arc::new(first_type.clone()))
                    }
                } else if let Type::Result { value, error } = &second_resolved_type {
                    if let Type::Result { value: _, error: _ } = &first_resolved_type {
                        first_type.clone()
                    } else if let Type::Unit = &first_resolved_type {
                        // TODO - replace with std error type
                        implicit_convert = Some(ImplicitConvertKind::Error);
                        Type::Result {
                            value: value.clone(),
                            error: Arc::new(first_type.clone()),
                        }
                    } else {
                        implicit_convert = Some(ImplicitConvertKind::Ok);
                        Type::Result {
                            value: Arc::new(first_type.clone()),
                            error: error.clone(),
                        }
                    }
                } else {
                    unreachable!()
                };

                self.unify_type(
                    &new_first_type,
                    first_span,
                    first_module_name,
                    second_type,
                    second_span,
                    second_module_name,
                    current_scope_this_type,
                    false,
                    true,
                )?;
                Ok(implicit_convert)
            } else {
                self.unify_type(
                    first_type,
                    first_span,
                    first_module_name,
                    second_type,
                    second_span,
                    second_module_name,
                    current_scope_this_type,
                    false,
                    true,
                )?;
                Ok(None)
            }
        } else {
            let first_resolved_type = if let Type::LocalGeneric(generic_id) = first_type {
                self.resolve_generic_type(*generic_id).1.value
            } else {
                first_type.clone()
            };

            if first_resolved_type.is_option_or_result() {
                let second_resolved_type = if let Type::LocalGeneric(generic_id) = second_type {
                    self.resolve_generic_type(*generic_id).1.value
                } else {
                    second_type.clone()
                };

                let mut implicit_convert = None;

                let new_second_type = if let Type::Option(_) = &first_resolved_type {
                    if let Type::Option(_) = &second_resolved_type {
                        second_type.clone()
                    } else {
                        implicit_convert = Some(ImplicitConvertKind::Some);
                        Type::Option(Arc::new(second_type.clone()))
                    }
                } else if let Type::Result { value, error } = &first_resolved_type {
                    if let Type::Result { value: _, error: _ } = &second_resolved_type {
                        second_type.clone()
                    } else if let Type::Unit = &second_resolved_type {
                        // TODO - replace with std error type
                        implicit_convert = Some(ImplicitConvertKind::Error);
                        Type::Result {
                            value: value.clone(),
                            error: Arc::new(second_type.clone()),
                        }
                    } else {
                        implicit_convert = Some(ImplicitConvertKind::Ok);
                        Type::Result {
                            value: Arc::new(second_type.clone()),
                            error: error.clone(),
                        }
                    }
                } else {
                    unreachable!()
                };

                self.unify_type(
                    first_type,
                    first_span,
                    first_module_name,
                    &new_second_type,
                    second_span,
                    second_module_name,
                    current_scope_this_type,
                    false,
                    true,
                )?;
                Ok(implicit_convert)
            } else {
                self.unify_type(
                    first_type,
                    first_span,
                    first_module_name,
                    second_type,
                    second_span,
                    second_module_name,
                    current_scope_this_type,
                    false,
                    true,
                )?;
                Ok(None)
            }
        }
    }

    pub fn unify_with_return_type(
        &mut self,
        return_expr_entity_id: WithDefineInfo<EntityID>,
        current_scope_this_type: &ScopeThisType,
    ) -> Result<(), TypeMismatchError> {
        let return_type = match &self.return_type {
            Either::Left(entity_id) => self.resolve_entity_type(*entity_id),
            Either::Right(ty) => ty.clone(),
        };
        let mut return_expr_resolved = self.resolve_entity_type(return_expr_entity_id.value);
        return_expr_resolved.span = return_expr_entity_id.span.clone();

        let result = self.unify_type_with_implicit_convert(
            &return_type.value,
            &return_type.span,
            &return_type.module_name,
            &return_expr_resolved.value,
            &return_expr_resolved.span,
            &return_expr_resolved.module_name,
            current_scope_this_type,
            false,
        );

        if let Ok(convert_kind) = &result {
            if let Some(convert_kind) = convert_kind {
                self.implicit_convert_map
                    .insert(return_expr_entity_id.value, *convert_kind);
            }
        }

        result?;
        Ok(())
    }

    pub fn unify_type(
        &mut self,
        first_type: &Type,
        first_span: &Range<usize>,
        first_module_name: &Arc<String>,
        second_type: &Type,
        second_span: &Range<usize>,
        second_module_name: &Arc<String>,
        current_scope_this_type: &ScopeThisType,
        allow_unknown: bool,
        is_first_layer: bool,
    ) -> Result<(), TypeMismatchError> {
        let result = self.unify_type_recursive(
            first_type,
            first_span,
            first_module_name,
            second_type,
            second_span,
            second_module_name,
            current_scope_this_type,
            allow_unknown,
            is_first_layer,
        );

        result.map_err(|err| {
            let first = WithDefineInfo {
                value: first_type.clone(),
                module_name: first_module_name.clone(),
                span: first_span.clone(),
            };
            let second = WithDefineInfo {
                value: second_type.clone(),
                module_name: second_module_name.clone(),
                span: second_span.clone(),
            };

            //let remove = (first.clone(), second.clone());
            //err.retain(|element| { element != &remove });

            TypeMismatchError {
                type_0: first,
                type_1: second,
                generics: err,
            }
        })
    }

    fn unify_type_recursive(
        &mut self,
        first_type: &Type,
        first_span: &Range<usize>,
        first_module_name: &Arc<String>,
        second_type: &Type,
        second_span: &Range<usize>,
        second_module_name: &Arc<String>,
        current_scope_this_type: &ScopeThisType,
        allow_unknown: bool,
        is_first_layer: bool,
    ) -> Result<(), Vec<(WithDefineInfo<Type>, WithDefineInfo<Type>)>> {
        if allow_unknown {
            if first_type == &Type::Unknown || second_type == &Type::Unknown {
                return Ok(());
            }
        }

        if first_type == &Type::Unreachable || second_type == &Type::Unreachable {
            return Ok(());
        }

        if let Type::LocalGeneric(first_generic_id) = first_type {
            let first_resolved_type = self.resolve_generic_type(*first_generic_id);

            if let Type::LocalGeneric(second_generic_id) = second_type {
                let second_resolved_type = self.resolve_generic_type(*second_generic_id);

                if first_resolved_type.0 == second_resolved_type.0 {
                    return Ok(());
                }

                if let Type::NumericLiteral(compatible_types_1) = &first_resolved_type.1.value {
                    if let Type::NumericLiteral(compatible_types_2) = &second_resolved_type.1.value
                    {
                        let and = compatible_types_1
                            .iter()
                            .filter(|&ty| compatible_types_2.contains(ty))
                            .cloned()
                            .collect::<Vec<_>>();

                        if !and.is_empty() {
                            self.generic_type_map.insert(
                                second_resolved_type.0,
                                Either::Left(first_resolved_type.0),
                            );
                            self.generic_type_map.insert(
                                first_resolved_type.0,
                                Either::Right(WithDefineInfo {
                                    value: Type::NumericLiteral(and),
                                    module_name: second_resolved_type.1.module_name.clone(),
                                    span: second_resolved_type.1.span.clone(),
                                }),
                            );

                            return Ok(());
                        }
                    }
                }

                match (
                    first_resolved_type
                        .1
                        .value
                        .is_replaceable_with(&second_resolved_type.1.value),
                    second_resolved_type
                        .1
                        .value
                        .is_replaceable_with(&first_resolved_type.1.value),
                ) {
                    (true, true) => {
                        self.generic_type_map
                            .insert(second_resolved_type.0, Either::Left(first_resolved_type.0));
                    }
                    (true, false) => {
                        self.generic_type_map
                            .insert(first_resolved_type.0, Either::Right(second_resolved_type.1));
                        self.generic_type_map
                            .insert(second_resolved_type.0, Either::Left(first_resolved_type.0));
                    }
                    (false, true) => {
                        self.generic_type_map
                            .insert(second_resolved_type.0, Either::Left(first_resolved_type.0));
                    }
                    (false, false) => {
                        return self.unify_type_recursive(
                            &first_resolved_type.1.value,
                            &first_resolved_type.1.span,
                            &first_resolved_type.1.module_name,
                            &second_resolved_type.1.value,
                            &second_resolved_type.1.span,
                            &second_resolved_type.1.module_name,
                            current_scope_this_type,
                            allow_unknown,
                            is_first_layer,
                        );
                    }
                }

                return Ok(());
            } else {
                if first_resolved_type.1.value.is_replaceable_with(second_type) {
                    self.set_generic_type(
                        *first_generic_id,
                        WithDefineInfo {
                            value: second_type.clone(),
                            module_name: second_module_name.clone(),
                            span: second_span.clone(),
                        },
                    );
                    return Ok(());
                } else {
                    return self.unify_type_recursive(
                        &first_resolved_type.1.value,
                        &first_resolved_type.1.span,
                        &first_resolved_type.1.module_name,
                        second_type,
                        second_span,
                        second_module_name,
                        current_scope_this_type,
                        allow_unknown,
                        is_first_layer,
                    );
                }
            }
        } else {
            if let Type::LocalGeneric(second_generic_id) = second_type {
                let second_resolved_type = self.resolve_generic_type(*second_generic_id);

                if second_resolved_type.1.value.is_replaceable_with(first_type) {
                    self.set_generic_type(
                        *second_generic_id,
                        WithDefineInfo {
                            value: first_type.clone(),
                            module_name: first_module_name.clone(),
                            span: first_span.clone(),
                        },
                    );
                    return Ok(());
                } else {
                    return self.unify_type_recursive(
                        &first_type,
                        &first_span,
                        &first_module_name,
                        &second_resolved_type.1.value,
                        &second_resolved_type.1.span,
                        &second_resolved_type.1.module_name,
                        current_scope_this_type,
                        allow_unknown,
                        is_first_layer,
                    );
                }
            }
        }

        if &Type::This != &current_scope_this_type.ty {
            if let Type::This = first_type {
                return self.unify_type_recursive(
                    &current_scope_this_type.ty,
                    first_span,
                    first_module_name,
                    second_type,
                    second_span,
                    second_module_name,
                    current_scope_this_type,
                    allow_unknown,
                    is_first_layer,
                );
            }
            if let Type::This = second_type {
                return self.unify_type_recursive(
                    first_type,
                    first_span,
                    first_module_name,
                    &current_scope_this_type.ty,
                    second_span,
                    second_module_name,
                    current_scope_this_type,
                    allow_unknown,
                    is_first_layer,
                );
            }
        }

        let eq = match first_type {
            Type::UserType {
                user_type_info: first_info,
                generics: first_generics,
                generics_span: _,
            } => {
                if let Type::UserType {
                    user_type_info: second_info,
                    generics: second_generics,
                    generics_span: _,
                } = second_type
                {
                    if first_info == second_info {
                        self.unify_generics(
                            first_generics,
                            first_span,
                            first_module_name,
                            second_generics,
                            second_span,
                            second_module_name,
                            current_scope_this_type,
                            allow_unknown,
                        )?;
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            Type::Function {
                function_info: first_function_info,
                generics: first_generics,
            } => {
                if let Type::Function {
                    function_info: second_function_info,
                    generics: second_generics,
                } = second_type
                {
                    if first_function_info.argument_types.len()
                        != second_function_info.argument_types.len()
                    {
                        false
                    } else {
                        for (first, second) in first_function_info
                            .argument_types
                            .iter()
                            .zip(second_function_info.argument_types.iter())
                        {
                            self.unify_type_recursive(
                                first,
                                first_span,
                                first_module_name,
                                second,
                                second_span,
                                second_module_name,
                                current_scope_this_type,
                                allow_unknown,
                                false,
                            )?;
                        }

                        self.unify_type_recursive(
                            &first_function_info.return_type.value,
                            first_span,
                            first_module_name,
                            &second_function_info.return_type.value,
                            second_span,
                            second_module_name,
                            current_scope_this_type,
                            allow_unknown,
                            false,
                        )?;

                        self.unify_generics(
                            first_generics,
                            first_span,
                            first_module_name,
                            second_generics,
                            second_span,
                            second_module_name,
                            current_scope_this_type,
                            allow_unknown,
                        )?;

                        if first_function_info != second_function_info {
                            self.function_equals_info
                                .push((first_function_info.clone(), second_function_info.clone()));
                        }

                        true
                    }
                } else {
                    false
                }
            }
            Type::LocalGeneric(_) => unreachable!(),
            Type::Array(first_type) => {
                if let Type::Array(second_type) = second_type {
                    self.unify_type_recursive(
                        &first_type,
                        first_span,
                        first_module_name,
                        &second_type,
                        second_span,
                        second_module_name,
                        current_scope_this_type,
                        allow_unknown,
                        false,
                    )?;
                    true
                } else {
                    false
                }
            }
            Type::Tuple(first_types) => {
                if let Type::Tuple(second_types) = second_type {
                    if first_types.len() != second_types.len() {
                        false
                    } else {
                        for (first_type, second_type) in first_types.iter().zip(second_types.iter())
                        {
                            self.unify_type_recursive(
                                first_type,
                                first_span,
                                first_module_name,
                                second_type,
                                second_span,
                                second_module_name,
                                current_scope_this_type,
                                allow_unknown,
                                is_first_layer,
                            )?;
                        }
                        true
                    }
                } else {
                    false
                }
            }
            Type::Option(first_type) => {
                if let Type::Option(second_type) = second_type {
                    self.unify_type_recursive(
                        &first_type,
                        first_span,
                        first_module_name,
                        &second_type,
                        second_span,
                        second_module_name,
                        current_scope_this_type,
                        allow_unknown,
                        false,
                    )?;
                    true
                } else {
                    false
                }
            }
            Type::Result {
                value: first_value_type,
                error: first_error_type,
            } => {
                if let Type::Result {
                    value: second_value_type,
                    error: second_error_type,
                } = second_type
                {
                    let value_type_result = self.unify_type_recursive(
                        &first_value_type,
                        first_span,
                        first_module_name,
                        &second_value_type,
                        second_span,
                        second_module_name,
                        current_scope_this_type,
                        allow_unknown,
                        false,
                    );
                    let error_type_result = self.unify_type_recursive(
                        &first_error_type,
                        first_span,
                        first_module_name,
                        &second_error_type,
                        second_span,
                        second_module_name,
                        current_scope_this_type,
                        allow_unknown,
                        false,
                    );

                    let mut errors = Vec::new();
                    if let Err(error) = value_type_result {
                        errors.extend(error);
                    }
                    if let Err(error) = error_type_result {
                        errors.extend(error);
                    }

                    if !errors.is_empty() {
                        return Err(errors);
                    }

                    true
                } else {
                    false
                }
            }
            _ => first_type == second_type,
        };

        if eq {
            Ok(())
        } else {
            if is_first_layer {
                Err(Vec::new())
            } else {
                Err(vec![(
                    WithDefineInfo {
                        value: first_type.clone(),
                        module_name: first_module_name.clone(),
                        span: first_span.clone(),
                    },
                    WithDefineInfo {
                        value: second_type.clone(),
                        module_name: second_module_name.clone(),
                        span: second_span.clone(),
                    },
                )])
            }
        }
    }

    fn unify_generics(
        &mut self,
        first_generics: &Vec<Type>,
        first_span: &Range<usize>,
        first_module_name: &Arc<String>,
        second_generics: &Vec<Type>,
        second_span: &Range<usize>,
        second_module_name: &Arc<String>,
        current_scope_this_type: &ScopeThisType,
        allow_unknown: bool,
    ) -> Result<(), Vec<(WithDefineInfo<Type>, WithDefineInfo<Type>)>> {
        let mut errors = Vec::new();

        for i in 0..first_generics.len() {
            let result = self.unify_type_recursive(
                &first_generics[i],
                first_span,
                first_module_name,
                &second_generics[i],
                second_span,
                second_module_name,
                current_scope_this_type,
                allow_unknown,
                false,
            );

            if let Err(error) = result {
                errors.extend(error);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn resolve_entity_type(&self, entity_id: EntityID) -> WithDefineInfo<Type> {
        let mut current_id = entity_id;
        loop {
            let entity_id_or_type = self.entity_type_map.get(&current_id).expect(
                format!(
                    "Not found entity type! | EntityID: {:?}, Type : {}",
                    current_id,
                    current_id.get_type_name()
                )
                .as_str(),
            );
            match entity_id_or_type {
                Either::Left(entity_id) => current_id = *entity_id,
                Either::Right(ty) => return ty.clone(),
            }
        }
    }

    pub fn resolve_entity_type_with_id(
        &self,
        entity_id: EntityID,
    ) -> (EntityID, WithDefineInfo<Type>) {
        let mut current_id = entity_id;
        loop {
            let entity_id_or_type = self.entity_type_map.get(&current_id).expect(
                format!(
                    "Not found entity type! | EntityID: {:?}, Type : {}",
                    current_id,
                    current_id.get_type_name()
                )
                .as_str(),
            );
            match entity_id_or_type {
                Either::Left(entity_id) => current_id = *entity_id,
                Either::Right(ty) => return (current_id, ty.clone()),
            }
        }
    }

    pub fn resolve_generic_type(
        &self,
        generic_id: LocalGenericID,
    ) -> (LocalGenericID, WithDefineInfo<Type>) {
        let mut current_id = generic_id;
        loop {
            let generic_id_or_type = self.generic_type_map.get(&current_id).unwrap();
            match &generic_id_or_type {
                Either::Left(entity_id) => current_id = *entity_id,
                Either::Right(ty) => {
                    return (current_id, ty.clone());
                }
            }
        }
    }

    pub(crate) fn resolve_type_surface(&self, ty: &Type) -> Type {
        match ty {
            Type::LocalGeneric(generic_id) => self.resolve_generic_type(*generic_id).1.value,
            _ => ty.clone(),
        }
    }

    fn register_variable(&mut self, binding: &VariableBinding) {
        match &binding.binding {
            Either::Left(literal) => self
                .var_span_and_entity_ids
                .push((literal.span.clone(), EntityID::from(literal))),
            Either::Right(bindings) => {
                for binding in bindings.iter() {
                    self.register_variable(binding);
                }
            }
        }
    }

    pub fn get_type_display_string(&self, ty: &Type) -> String {
        match ty {
            Type::Int8 => "int8".to_string(),
            Type::Int16 => "int16".to_string(),
            Type::Int32 => "int32".to_string(),
            Type::Int64 => "int64".to_string(),
            Type::Uint8 => "uint8".to_string(),
            Type::Uint16 => "uint16".to_string(),
            Type::Uint32 => "uint32".to_string(),
            Type::Uint64 => "uint64".to_string(),
            Type::Float32 => "float32".to_string(),
            Type::Float64 => "float64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Unit => "unit".to_string(),
            Type::NumericLiteral(_) => {
                self.get_type_display_string(&ty.get_numeric_compatible_optimal_type())
            }
            Type::UserType {
                user_type_info,
                generics,
                generics_span: _,
            } => {
                let mut name = user_type_info.name.value.clone();
                self.add_generics_info(&mut name, generics);
                name
            }
            Type::Function {
                function_info,
                generics,
            } => {
                let mut name = "function".to_string();

                self.add_generics_info(&mut name, generics);

                let argument_types = function_info
                    .argument_types
                    .iter()
                    .map(|ty| self.get_type_display_string(ty))
                    .collect::<Vec<String>>()
                    .join(", ");

                name += format!("({})", argument_types).as_str();

                if function_info.return_type.value != Type::Unit {
                    name += " -> ";
                    name += self
                        .get_type_display_string(&function_info.return_type.value)
                        .as_str();
                }

                name
            }
            Type::Generic(generic_type) => generic_type.name.deref().clone(),
            Type::LocalGeneric(generic_id) => {
                self.get_type_display_string(&self.resolve_generic_type(*generic_id).1.value)
            }
            Type::Array(base_type) => format!("[{}]", self.get_type_display_string(base_type)),
            Type::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|ty| self.get_type_display_string(ty))
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("({})", types)
            }
            Type::Option(value_type) => format!("{}?", self.get_type_display_string(value_type)),
            Type::Result { value, error } => {
                format!(
                    "{}!<{}>",
                    self.get_type_display_string(value),
                    self.get_type_display_string(error)
                )
            }
            Type::This => "This".to_string(),
            Type::Unreachable => "$unreachable".to_string(),
            Type::Unknown => "unknown".to_string(),
        }
    }

    pub fn resolve_type(&self, ty: &Type) -> Type {
        match ty {
            Type::UserType {
                user_type_info,
                generics,
                generics_span: _,
            } => {
                let mut new_generics = Vec::new();
                for generic in generics.iter() {
                    new_generics.push(self.resolve_type(generic));
                }
                Type::UserType {
                    user_type_info: user_type_info.clone(),
                    generics: Arc::new(new_generics),
                    generics_span: None,
                }
            }
            Type::Function {
                function_info,
                generics,
            } => {
                let mut new_generics = Vec::new();
                for generic in generics.iter() {
                    new_generics.push(self.resolve_type(generic));
                }
                Type::Function {
                    function_info: function_info.clone(),
                    generics: Arc::new(new_generics),
                }
            }
            Type::LocalGeneric(generic_id) => self.resolve_generic_type(*generic_id).1.value,
            Type::Array(base_type) => Type::Array(Arc::new(self.resolve_type(&base_type))),
            Type::Tuple(types) => {
                let types = types.iter().map(|ty| self.resolve_type(ty)).collect();
                Type::Tuple(Arc::new(types))
            }
            Type::Option(value) => Type::Option(Arc::new(self.resolve_type(&value))),
            Type::Result { value, error } => Type::Result {
                value: Arc::new(self.resolve_type(&value)),
                error: Arc::new(self.resolve_type(&error)),
            },
            _ => ty.clone(),
        }
    }

    pub(crate) fn regenerate_local_generic(&mut self, ty: &Type) -> Type {
        match ty {
            Type::NumericLiteral(_) => todo!(),
            Type::UserType {
                user_type_info,
                generics,
                generics_span,
            } => Type::UserType {
                user_type_info: user_type_info.clone(),
                generics: Arc::new(
                    generics
                        .iter()
                        .map(|ty| self.regenerate_local_generic(ty))
                        .collect(),
                ),
                generics_span: generics_span.clone(),
            },
            Type::Function {
                function_info,
                generics,
            } => Type::Function {
                function_info: function_info.clone(),
                generics: Arc::new(
                    generics
                        .iter()
                        .map(|ty| self.regenerate_local_generic(ty))
                        .collect(),
                ),
            },
            Type::LocalGeneric(generic_id) => {
                let resolved = self.resolve_generic_type(*generic_id).1;

                if &resolved.value == &Type::Unknown {
                    Type::LocalGeneric(
                        self.new_local_generic_id(
                            resolved.span.clone(),
                            resolved.module_name.clone(),
                        ),
                    )
                } else {
                    resolved.value.clone()
                }
            }
            Type::Array(base_type) => {
                Type::Array(Arc::new(self.regenerate_local_generic(&base_type)))
            }
            Type::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|ty| self.regenerate_local_generic(ty))
                    .collect();
                Type::Tuple(Arc::new(types))
            }
            Type::Option(value_type) => {
                Type::Option(Arc::new(self.regenerate_local_generic(&value_type)))
            }
            Type::Result { value, error } => Type::Result {
                value: Arc::new(self.regenerate_local_generic(&value)),
                error: Arc::new(self.regenerate_local_generic(&error)),
            },
            _ => ty.clone(),
        }
    }

    fn add_generics_info(&self, name: &mut String, generics: &Vec<Type>) {
        if !generics.is_empty() {
            *name += "<";

            for i in 0..generics.len() {
                if i != 0 {
                    *name += ", ";
                }
                *name += self.get_type_display_string(&generics[i]).as_str();
            }

            *name += ">";
        }
    }

    pub(crate) fn fix_numeric_literal_type(&mut self, ty: &Type) {
        match ty {
            Type::NumericLiteral(_) => unreachable!(),
            Type::UserType {
                user_type_info: _,
                generics,
                generics_span: _,
            } => {
                for generic in generics.iter() {
                    self.fix_numeric_literal_type(generic);
                }
            }
            Type::Function {
                function_info: _,
                generics,
            } => {
                for generic in generics.iter() {
                    self.fix_numeric_literal_type(generic);
                }
            }
            Type::LocalGeneric(generic_id) => {
                let resolved_type = self.resolve_generic_type(*generic_id);

                if let Type::NumericLiteral(_) = &resolved_type.1.value {
                    self.set_generic_type(
                        resolved_type.0,
                        WithDefineInfo {
                            value: resolved_type.1.value.get_numeric_compatible_optimal_type(),
                            module_name: resolved_type.1.module_name.clone(),
                            span: resolved_type.1.span.clone(),
                        },
                    );
                }
            }
            Type::Array(base_type) => self.fix_numeric_literal_type(&base_type),
            Type::Tuple(types) => {
                for ty in types.iter() {
                    self.fix_numeric_literal_type(ty);
                }
            }
            Type::Option(value_type) => self.fix_numeric_literal_type(&value_type),
            Type::Result { value, error } => {
                self.fix_numeric_literal_type(&value);
                self.fix_numeric_literal_type(&error);
            }
            _ => {}
        }
    }

    fn add_lazy_type_error_report<T: 'static + LazyTypeReport>(&mut self, report: T) {
        self.lazy_type_reports.push(Box::new(report));
    }

    pub(crate) fn collect_info(
        &mut self,
        type_inference_results: &mut TypeInferenceResultContainer,
        global_implements_info_set: &ImplementsInfoSet,
        errors: &mut Vec<TranspileError>,
        warnings: &mut Vec<TranspileWarning>,
        context: &TranspileModuleContext,
        allocator: &'allocator Bump,
    ) {
        self.lazy_infer_type_generics(global_implements_info_set, allocator);

        type_inference_results
            .implicit_convert_map
            .extend(&self.implicit_convert_map);

        for (entity_id, ty) in self.entity_type_map.iter() {
            let ty = match ty {
                Either::Left(entity_id) => self.resolve_entity_type(*entity_id).value,
                Either::Right(ty) => ty.value.clone(),
            };

            let resolved_type = self.resolve_type(&ty);

            type_inference_results
                .entity_type_map
                .insert(*entity_id, resolved_type);
        }

        self.type_check_bounds(global_implements_info_set, errors);

        for report in self.lazy_type_reports.iter() {
            match report.build_report(self, context) {
                Either::Left(error) => errors.push(error),
                Either::Right(warning) => warnings.push(warning),
            }
        }

        self.check_impl_interface_generics(errors);

        for (entity_id, function_type) in self.operator_function_types.iter() {
            type_inference_results
                .operator_function_type_map
                .insert(*entity_id, function_type.clone());
        }

        for (entity_id, return_type) in self.operator_return_types.iter() {
            type_inference_results
                .operator_return_type_map
                .insert(*entity_id, return_type.clone());
        }

        // for lifetime analyzer
        if context.context.settings.optimization.is_required_function_equals_info() {
            context.context.function_equals_info.add_info(
                self.function_equals_info
                    .iter()
                    .filter(|(function_0, function_1)| {
                        let function_0_type_name = function_0.define_info.entity_id.get_type_name();
                        let function_1_type_name = function_1.define_info.entity_id.get_type_name();

                        (function_0_type_name == type_name::<FunctionDefine>()
                            || function_0_type_name == type_name::<Closure>())
                            && (function_1_type_name == type_name::<FunctionDefine>()
                                || function_1_type_name == type_name::<Closure>())
                    })
                    .cloned(),
            );
        }

        self.print_var_type(context);
    }

    fn print_var_type(&self, context: &TranspileModuleContext) {
        if !context.context.settings.is_transpiler_debug || self.var_span_and_entity_ids.is_empty()
        {
            return;
        }

        let mut builder = Report::build(
            ReportKind::Custom("Debug", Color::Cyan),
            &context.module_name,
            0,
        );

        for var_type_and_span in self.var_span_and_entity_ids.iter() {
            let ty = self.resolve_entity_type(var_type_and_span.1).value;

            builder.add_label(
                Label::new((&context.module_name, var_type_and_span.0.clone()))
                    .with_color(Color::Green)
                    .with_message(self.get_type_display_string(&ty)),
            );
        }

        let _lock = context.context.debug_print_lock.lock().unwrap();

        builder
            .finish()
            .print((
                &context.module_name,
                Source::from(context.source_code.code.as_str()),
            ))
            .unwrap();
    }

    fn lazy_infer_type_generics(
        &mut self,
        global_implements_info_set: &ImplementsInfoSet,
        allocator: &'allocator Bump,
    ) {
        for type_infer in self.lazy_generic_infer_type.clone() {
            let generics_define = &type_infer.generics_define;
            let local_generics = type_infer.local_generics;
            let where_bounds = type_infer.where_bound;
            let current_scope_implements_info_set = &type_infer.current_scope_implements_info_set;

            let mut errors = Vec::new();

            for generic in generics_define.iter() {
                let target_type = Type::get_type_with_replaced_generics(
                    &Type::Generic(generic.clone()),
                    generics_define,
                    &local_generics,
                );

                for bound in generic.bounds.freeze_and_get().iter() {
                    let replaced_bound_type = Type::get_type_with_replaced_generics(
                        &bound.ty,
                        generics_define,
                        &local_generics,
                    );

                    global_implements_info_set.infer_type_for_generic_bounds(
                        &bound.ty,
                        &replaced_bound_type,
                        &target_type,
                        &generic.location.span,
                        &generic.location.module_name,
                        &bound.span,
                        &bound.module_name,
                        false,
                        false,
                        self,
                        current_scope_implements_info_set,
                        &mut errors,
                        allocator,
                    );
                }
            }

            for where_bound in where_bounds.iter() {
                let target_type = Type::get_type_with_replaced_generics(
                    &where_bound.target_type.value,
                    generics_define,
                    &local_generics,
                );
                for bound in where_bound.bounds.iter() {
                    let replaced_bound_type = Type::get_type_with_replaced_generics(
                        &bound.ty,
                        generics_define,
                        &local_generics,
                    );

                    global_implements_info_set.infer_type_for_generic_bounds(
                        &bound.ty,
                        &replaced_bound_type,
                        &target_type,
                        &where_bound.target_type.span,
                        &bound.module_name,
                        &bound.span,
                        &bound.module_name,
                        false,
                        false,
                        self,
                        current_scope_implements_info_set,
                        &mut errors,
                        allocator,
                    );
                }
            }

            if !errors.is_empty() {
                let error = LazyGenericTypeInferError::new(type_infer.origin_type, errors);
                self.add_lazy_type_error_report(error);
            }
        }
    }

    fn type_check_bounds(
        &mut self,
        global_implements_info_set: &ImplementsInfoSet,
        errors: &mut Vec<TranspileError>,
    ) {
        for bounds_check in self.generic_bounds_checks.clone().iter() {
            match bounds_check {
                GenericsBoundCheck::Generics {
                    type_span,
                    ty,
                    generic_define: generics_define,
                    scope_implements_info_set,
                } => {
                    self.fix_numeric_literal_type(&ty.value);

                    let type_resolved = if let Type::LocalGeneric(generic_id) = &ty.value {
                        self.resolve_generic_type(*generic_id).1
                    } else {
                        WithDefineInfo {
                            value: ty.value.clone(),
                            module_name: ty.module_name.clone(),
                            span: ty.span.clone(),
                        }
                    };

                    let result = global_implements_info_set.is_satisfied(
                        &type_resolved.value,
                        &generics_define.bounds.freeze_and_get(),
                        self,
                        scope_implements_info_set,
                        false,
                        false,
                    );

                    if let Err(bounds) = result {
                        let contains_unknown =
                            bounds.iter().any(|bound| bound.ty.contains_unknown());
                        if type_resolved.value.contains_unknown() || contains_unknown {
                            continue;
                        }

                        let type_name = type_resolved.map(|ty| self.get_type_display_string(&ty));
                        let bounds_module_name = bounds.first().unwrap().module_name.clone();
                        let not_satisfied_bounds = bounds
                            .into_iter()
                            .map(|bound| {
                                Spanned::new(
                                    self.get_type_display_string(&bound.ty),
                                    bound.span.clone(),
                                )
                            })
                            .collect();

                        let error = TranspileError::new(BoundsNotSatisfiedError {
                            span: type_span.clone(),
                            type_name,
                            bounds_module_name,
                            not_satisfied_bounds,
                        });
                        errors.push(error);
                    }
                }
                GenericsBoundCheck::Where {
                    type_span,
                    target_type,
                    bounds,
                    scope_implements_info_set,
                } => {
                    self.fix_numeric_literal_type(target_type);

                    let result = global_implements_info_set.is_satisfied(
                        target_type,
                        bounds,
                        self,
                        scope_implements_info_set,
                        false,
                        false,
                    );

                    if let Err(bounds) = result {
                        let bounds_contain_unknown = bounds
                            .iter()
                            .any(|bound| self.resolve_type(&bound.ty).contains_unknown());
                        if self.resolve_type(target_type).contains_unknown()
                            || bounds_contain_unknown
                        {
                            continue;
                        }

                        let type_name = self.get_type_display_string(target_type);
                        let bounds_module_name = bounds.first().unwrap().module_name.clone();
                        let not_satisfied_bounds = bounds
                            .into_iter()
                            .map(|bound| {
                                Spanned::new(
                                    self.get_type_display_string(&bound.ty),
                                    bound.span.clone(),
                                )
                            })
                            .collect();

                        let error = TranspileError::new(WhereBoundsNotSatisfiedError {
                            type_name: Spanned::new(type_name, type_span.clone()),
                            bounds_module_name,
                            not_satisfied_bounds,
                        });
                        errors.push(error);
                    }
                }
            }
        }
    }

    fn check_impl_interface_generics(&self, errors: &mut Vec<TranspileError>) {
        for (reference_span, interface) in self.impl_interface_generics_check.iter() {
            if let Type::UserType {
                user_type_info: _,
                generics,
                generics_span: _,
            } = self.resolve_type(&interface.value)
            {
                let mut has_unknown = false;
                for generic in generics.iter() {
                    if generic.contains_unknown() {
                        has_unknown = true;
                        break;
                    }
                }

                if has_unknown {
                    let original_name =
                        self.get_type_display_string(&interface.value.as_original_type());
                    let resolved_name =
                        self.get_type_display_string(&self.resolve_type(&interface.value));

                    let error = UnresolvedInterface {
                        original_name,
                        resolved_name,
                        reference_span: reference_span.clone(),
                        implementation: interface.clone().map(|_| ()),
                    };
                    errors.push(TranspileError::new(error));
                }
            }
        }
    }

    fn add_check_type_info_bounds(
        &mut self,
        ty: Spanned<Type>,
        this_type: &Type,
        check_this_type_bounds: bool,
        current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
        context: &TranspileModuleContext,
    ) {
        if let Type::UserType {
            user_type_info,
            generics,
            generics_span,
        } = &ty.value
        {
            if user_type_info.generics_define.len() != generics.len() {
                return;
            }

            let generics_span = generics_span
                .as_ref()
                .map(|spans| {
                    if spans.len() == generics.len() {
                        Some(spans)
                    } else {
                        None
                    }
                })
                .flatten();

            let mut generics_define = ty
                .value
                .get_generics_define_with_replaced_generic()
                .unwrap();
            if ty.value.is_interface() {
                generics_define =
                    Type::replace_generics_define_this_type(&generics_define, this_type);
            }

            for i in 0..generics.len() {
                let generic_define = &generics_define[i];
                let generic = &generics[i];

                let generic_span = generics_span
                    .map(|spans| spans[i].clone())
                    .unwrap_or(ty.span.clone());

                self.generic_bounds_checks
                    .push(GenericsBoundCheck::Generics {
                        type_span: ty.span.clone(),
                        ty: WithDefineInfo {
                            value: generic.clone(),
                            module_name: context.module_name.clone(),
                            span: generic_span.clone(),
                        },
                        generic_define: generic_define.clone(),
                        scope_implements_info_set: current_scope_implements_info_set.clone(),
                    });

                self.add_check_type_info_bounds(
                    Spanned::new(generic.clone(), generic_span),
                    this_type,
                    check_this_type_bounds,
                    current_scope_implements_info_set,
                    context,
                );
            }

            let where_bounds = ty.value.get_where_bounds_with_replaced_generic().unwrap();
            let replaced_where_bounds = if ty.value.is_interface() {
                Some(Type::replace_where_bounds_this_type(
                    &where_bounds,
                    this_type,
                ))
            } else {
                None
            };

            for i in 0..where_bounds.len() {
                let where_bound = &where_bounds[i];
                let replaced_where_bound =
                    if let Some(replaced_where_bounds) = &replaced_where_bounds {
                        &replaced_where_bounds[i]
                    } else {
                        where_bound
                    };

                if !check_this_type_bounds {
                    if &where_bound.target_type.value == &Type::This {
                        continue;
                    }
                }

                self.generic_bounds_checks.push(GenericsBoundCheck::Where {
                    type_span: ty.span.clone(),
                    target_type: replaced_where_bound.target_type.value.clone(),
                    bounds: replaced_where_bound.bounds.clone(),
                    scope_implements_info_set: current_scope_implements_info_set.clone(),
                });
            }
        }
    }

    fn add_check_type_info_generics_define(
        &mut self,
        generics_define: &Vec<Arc<GenericType>>,
        current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
        context: &TranspileModuleContext,
    ) {
        for generic_define in generics_define.iter() {
            let target_type = Type::Generic(generic_define.clone());

            for bound in generic_define.bounds.freeze_and_get().iter() {
                let bound_type = Spanned::new(bound.ty.clone(), bound.span.clone());
                self.add_check_type_info_bounds(
                    bound_type,
                    &target_type,
                    false,
                    current_scope_implements_info_set,
                    context,
                );
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ImplicitConvertKind {
    Some,
    Ok,
    Error,
}

#[derive(Debug, Clone)]
pub(crate) enum GenericsBoundCheck {
    Generics {
        type_span: Range<usize>,
        ty: WithDefineInfo<Type>,
        generic_define: Arc<GenericType>,
        scope_implements_info_set: Option<Arc<ImplementsInfoSet>>,
    },
    Where {
        type_span: Range<usize>,
        target_type: Type,
        bounds: Vec<Arc<Bound>>,
        scope_implements_info_set: Option<Arc<ImplementsInfoSet>>,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct LazyGenericTypeInference {
    origin_type: WithDefineInfo<Type>,
    generics_define: Vec<Arc<GenericType>>,
    local_generics: Arc<Vec<Type>>,
    where_bound: Arc<Vec<WhereBound>>,
    current_scope_implements_info_set: Option<Arc<ImplementsInfoSet>>,
}

#[derive(Debug, Default)]
pub struct TypeInferenceResultContainer {
    pub implicit_convert_map: FxHashMap<EntityID, ImplicitConvertKind>,
    pub entity_type_map: FxHashMap<EntityID, Type>,
    pub operator_function_type_map: FxHashMap<EntityID, Type>,
    pub operator_return_type_map: FxHashMap<EntityID, Type>,
}

impl TypeInferenceResultContainer {
    pub fn get_entity_type(&self, entity_id: EntityID) -> &Type {
        self.entity_type_map.get(&entity_id).unwrap()
    }
}

pub(crate) fn infer_type_program<'input, 'allocator>(
    ast: Program<'input, 'allocator>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    is_interface_scope: bool,
    is_closure_scope: bool,
    implements_interfaces: &Vec<Spanned<Type>>,
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    let mut has_type = false;

    let mut override_elements_environment = OverrideElementsEnvironment::new(implements_interfaces);

    for i in 0..ast.statements.len() {
        let statement = match &ast.statements[i] {
            Ok(statement) => statement,
            _ => continue,
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                infer_type_expression(
                    assignment.left_expr,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    false,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );
                if let Ok(right_expr) = &assignment.right_expr {
                    infer_type_expression(
                        &right_expr,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        module_entity_type_map,
                        global_implements_info_set,
                        current_scope_this_type,
                        current_scope_implements_info_set,
                        true,
                        type_environment,
                        type_inference_results,
                        allocator,
                        errors,
                        warnings,
                        context,
                    );

                    let result = type_environment.unify_with_implicit_convert(
                        Spanned::new(
                            EntityID::from(assignment.left_expr),
                            assignment.left_expr.get_span(),
                        ),
                        Spanned::new(EntityID::from(*right_expr), right_expr.get_span()),
                        false,
                        current_scope_this_type,
                    );
                    add_error(result, type_environment);
                }
            }
            StatementAST::Exchange(exchange) => {
                infer_type_expression(
                    exchange.left_expr,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    false,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );
                if let Ok(right_expr) = &exchange.right_expr {
                    infer_type_expression(
                        &right_expr,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        module_entity_type_map,
                        global_implements_info_set,
                        current_scope_this_type,
                        current_scope_implements_info_set,
                        false,
                        type_environment,
                        type_inference_results,
                        allocator,
                        errors,
                        warnings,
                        context,
                    );

                    let result = type_environment.unify(
                        Spanned::new(
                            EntityID::from(exchange.left_expr),
                            exchange.left_expr.get_span(),
                        ),
                        Spanned::new(EntityID::from(*right_expr), right_expr.get_span()),
                        current_scope_this_type,
                    );
                    add_error(result, type_environment);
                }
            }
            StatementAST::FunctionDefine(function_define) => {
                let function_type = module_entity_type_map
                    .get(&EntityID::from(function_define))
                    .unwrap();

                if let Ok(name) = &function_define.name {
                    if function_define
                        .attributes
                        .contains_kind(StatementAttributeKind::Override)
                    {
                        let override_keyword_span = function_define
                            .attributes
                            .get_span(StatementAttributeKind::Override)
                            .unwrap();

                        if implements_interfaces.is_empty() {
                            let mut error = TranspileError::new(SimpleError::new(
                                INVALID_OVERRIDE_ERROR,
                                override_keyword_span.clone(),
                                vec![],
                                vec![(
                                    (context.module_name.clone(), override_keyword_span.clone()),
                                    Color::Red,
                                )],
                            ));
                            error.add_advice(
                                context.module_name.clone(),
                                Advice::Remove {
                                    span: override_keyword_span,
                                },
                            );
                            errors.push(error);
                        } else {
                            if !is_interface_scope {
                                let interface_span_start =
                                    implements_interfaces.first().unwrap().span.start;
                                let interface_span_end =
                                    implements_interfaces.last().unwrap().span.end;
                                let interface_span = interface_span_start..interface_span_end;

                                if let Err(error) = override_elements_environment.check(
                                    name.clone(),
                                    function_type,
                                    &current_scope_this_type.ty,
                                    global_implements_info_set,
                                    type_environment,
                                    context,
                                ) {
                                    error.collect(
                                        override_keyword_span,
                                        interface_span,
                                        type_environment,
                                        errors,
                                        context,
                                    );
                                }
                            }
                        }
                    }
                }

                if let Type::Function {
                    function_info,
                    generics: _,
                } = function_type
                {
                    let mut type_environment = TypeEnvironment::new_with_return_type(
                        Either::Right(WithDefineInfo {
                            value: function_info.return_type.value.clone(),
                            module_name: context.module_name.clone(),
                            span: function_info.return_type.span.clone(),
                        }),
                        allocator,
                    );

                    let argument_start_index = if function_info.is_extension { 1 } else { 0 };

                    for i in 0..function_define.args.arguments.len() {
                        let argument = &function_define.args.arguments[i];
                        let argument_type = &function_info.argument_types[i + argument_start_index];

                        bind_variable_for_tuple(
                            &argument.binding,
                            &argument_type,
                            &argument.span,
                            &mut type_environment,
                            context,
                        );
                    }

                    let current_scope_implements_info_set =
                        get_and_check_where_bounds_implements_info(
                            &function_info.where_bounds.freeze_and_get(),
                            current_scope_implements_info_set,
                            &mut type_environment,
                            context,
                        );

                    type_environment.add_check_type_info_generics_define(
                        &function_info.generics_define,
                        &current_scope_implements_info_set,
                        context,
                    );

                    if let Some(semicolon_or_block) = &function_define.block_or_semicolon.value {
                        if let Either::Right(block) = semicolon_or_block {
                            infer_type_program(
                                block.program,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                module_element_type_maps,
                                generics_map,
                                module_entity_type_map,
                                global_implements_info_set,
                                &current_scope_this_type.nest(),
                                &current_scope_implements_info_set,
                                false,
                                false,
                                &Vec::new(),
                                false,
                                &mut type_environment,
                                type_inference_results,
                                allocator,
                                errors,
                                warnings,
                                context,
                            );
                        }
                    }

                    type_environment.collect_info(
                        type_inference_results,
                        global_implements_info_set,
                        errors,
                        warnings,
                        context,
                        allocator,
                    );
                }
            }
            StatementAST::UserTypeDefine(user_type_define) => {
                let user_type = if let Ok(name) = &user_type_define.name {
                    user_type_map.get(name.value).unwrap().clone()
                } else {
                    Type::Unknown
                };

                let this_type = if user_type_define.kind.value == UserTypeKindEnum::Interface {
                    Type::This
                } else {
                    user_type.clone()
                };

                let mut current_scope_implements_info_set = match &user_type {
                    Type::UserType {
                        user_type_info,
                        generics: _,
                        generics_span: _,
                    } => get_and_check_where_bounds_implements_info(
                        &user_type_info.where_bounds.freeze_and_get(),
                        &None,
                        type_environment,
                        context,
                    ),
                    _ => None,
                };

                if let Type::UserType {
                    user_type_info,
                    generics: _,
                    generics_span: _,
                } = &user_type
                {
                    type_environment.add_check_type_info_generics_define(
                        &user_type_info.generics_define,
                        &current_scope_implements_info_set,
                        context,
                    );
                }

                if user_type_define.kind.value == UserTypeKindEnum::Interface {
                    if let Ok(name) = &user_type_define.name {
                        let mut implements_info_set = current_scope_implements_info_set
                            .as_ref()
                            .map(|implements_info_set| implements_info_set.deref().clone())
                            .unwrap_or_default();
                        implements_info_set.insert(
                            EntityID::from(user_type_define),
                            ImplementsInfo {
                                generics: Arc::new(Vec::new()),
                                interface: Spanned::new(user_type.clone(), name.span.clone()),
                                concrete: Spanned::new(Type::This, name.span.clone()),
                                module_name: context.module_name.clone(),
                                where_bounds: Arc::new(Vec::new()),
                                element_types: Arc::new(FxHashMap::default()),
                                is_bounds_info: true,
                            },
                        );
                        current_scope_implements_info_set = Some(Arc::new(implements_info_set));
                    }
                }

                let mut implements_interfaces = Vec::new();

                if let Some(super_type_info) = &user_type_define.super_type_info {
                    let mut implements_infos = current_scope_implements_info_set
                        .as_ref()
                        .map(|implements_infos| implements_infos.implements_infos.clone())
                        .unwrap_or(IndexMap::new());

                    for type_info in super_type_info.type_infos.iter() {
                        let current_scope_implements_info_set = Some(Arc::new(ImplementsInfoSet {
                            implements_infos: implements_infos.clone(),
                        }));

                        let super_type = module_entity_type_map
                            .get(&EntityID::from(type_info))
                            .unwrap();

                        implements_interfaces
                            .push(Spanned::new(super_type.clone(), type_info.get_span()));

                        type_environment.add_check_type_info_bounds(
                            Spanned::new(super_type.clone(), type_info.get_span()),
                            &user_type,
                            true,
                            &current_scope_implements_info_set,
                            context,
                        );

                        if let Ok(name) = &user_type_define.name {
                            let concrete =
                                if user_type_define.kind.value == UserTypeKindEnum::Interface {
                                    Type::This
                                } else {
                                    user_type_map.get(name.value).unwrap().clone()
                                };
                            implements_infos.insert(
                                EntityID::from(type_info),
                                ImplementsInfo {
                                    generics: Arc::new(Vec::new()),
                                    interface: Spanned::new(
                                        super_type.clone(),
                                        type_info.get_span(),
                                    ),
                                    concrete: Spanned::new(concrete, name.span.clone()),
                                    module_name: context.module_name.clone(),
                                    where_bounds: Arc::new(Vec::new()),
                                    element_types: Arc::new(FxHashMap::default()),
                                    is_bounds_info: true,
                                },
                            );
                        }
                    }

                    if user_type_define.kind.value == UserTypeKindEnum::Interface {
                        current_scope_implements_info_set =
                            Some(Arc::new(ImplementsInfoSet { implements_infos }));
                    }
                }

                if let Some(block) = &user_type_define.block.value {
                    let mut type_environment = TypeEnvironment::new_with_return_type(
                        Either::Right(WithDefineInfo {
                            value: Type::Unit,
                            module_name: context.module_name.clone(),
                            span: block.span.clone(),
                        }),
                        allocator,
                    );

                    infer_type_program(
                        block.program,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        module_entity_type_map,
                        global_implements_info_set,
                        &ScopeThisType::new(this_type),
                        &current_scope_implements_info_set,
                        user_type_define.kind.value == UserTypeKindEnum::Interface,
                        false,
                        &implements_interfaces,
                        false,
                        &mut type_environment,
                        type_inference_results,
                        allocator,
                        errors,
                        warnings,
                        context,
                    );

                    type_environment.collect_info(
                        type_inference_results,
                        global_implements_info_set,
                        errors,
                        warnings,
                        context,
                        allocator,
                    );
                }
            }
            StatementAST::Implements(implements) => {
                let concrete = if let Ok(target_type) = &implements.target_user_type {
                    module_entity_type_map
                        .get(&EntityID::from(target_type))
                        .unwrap()
                        .clone()
                } else {
                    Type::Unknown
                };

                let implements_info = global_implements_info_set.get(EntityID::from(implements));
                let current_scope_implements_info_set =
                    if let Some(implements_info) = implements_info {
                        get_and_check_where_bounds_implements_info(
                            &implements_info.where_bounds,
                            &None,
                            type_environment,
                            context,
                        )
                    } else {
                        None
                    };

                if let Some(implements_info) = implements_info {
                    let mut has_same_module_type = false;
                    if let Type::UserType {
                        user_type_info,
                        generics: _,
                        generics_span: _,
                    } = &implements_info.interface.value
                    {
                        if &context.module_name == &user_type_info.module_name {
                            has_same_module_type = true;
                        }
                    }
                    if let Type::UserType {
                        user_type_info,
                        generics: _,
                        generics_span: _,
                    } = &implements_info.concrete.value
                    {
                        if &context.module_name == &user_type_info.module_name {
                            has_same_module_type = true;
                        }
                    }

                    if !has_same_module_type {
                        let error = SimpleError::new(
                            0062,
                            implements_info.interface.span.clone(),
                            vec![],
                            vec![
                                (
                                    (
                                        context.module_name.clone(),
                                        implements_info.interface.span.clone(),
                                    ),
                                    Color::Red,
                                ),
                                (
                                    (
                                        context.module_name.clone(),
                                        implements_info.concrete.span.clone(),
                                    ),
                                    Color::Red,
                                ),
                            ],
                        );
                        errors.push(TranspileError::new(error));
                    }
                }

                if let Type::UserType {
                    user_type_info,
                    generics: _,
                    generics_span: _,
                } = &concrete
                {
                    type_environment.add_check_type_info_generics_define(
                        &user_type_info.generics_define,
                        &current_scope_implements_info_set,
                        context,
                    );
                }

                if let Ok(target_type) = &implements.target_user_type {
                    type_environment.add_check_type_info_bounds(
                        Spanned::new(concrete.clone(), target_type.get_span()),
                        &concrete,
                        true,
                        &current_scope_implements_info_set,
                        context,
                    );
                }

                let mut implements_interfaces = Vec::new();

                if let Ok(interface_info) = &implements.interface {
                    let interface = module_entity_type_map
                        .get(&EntityID::from(interface_info))
                        .unwrap();

                    implements_interfaces
                        .push(Spanned::new(interface.clone(), interface_info.get_span()));

                    type_environment.add_check_type_info_bounds(
                        Spanned::new(interface.clone(), interface_info.get_span()),
                        &concrete,
                        true,
                        &current_scope_implements_info_set,
                        context,
                    );
                }

                if let Some(block) = &implements.block.value {
                    infer_type_program(
                        block.program,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        module_entity_type_map,
                        global_implements_info_set,
                        &ScopeThisType::new(concrete),
                        &current_scope_implements_info_set,
                        false,
                        false,
                        &implements_interfaces,
                        force_be_expression,
                        type_environment,
                        type_inference_results,
                        allocator,
                        errors,
                        warnings,
                        context,
                    );
                }
            }
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    infer_type_expression(
                        expression,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        module_entity_type_map,
                        global_implements_info_set,
                        current_scope_this_type,
                        current_scope_implements_info_set,
                        false,
                        type_environment,
                        type_inference_results,
                        allocator,
                        errors,
                        warnings,
                        context,
                    );
                }
            }
            StatementAST::Expression(expression) => {
                let is_last_statement = i == ast.statements.len() - 1;
                let force_be_expression = force_be_expression && is_last_statement;

                infer_type_expression(
                    &expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    force_be_expression,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                if force_be_expression {
                    type_environment
                        .set_entity_id_equals(EntityID::from(*expression), EntityID::from(ast));

                    has_type = true;
                }
            }
            StatementAST::VariableDefine(variable_define) => {
                let tag_type = match module_entity_type_map.get(&EntityID::from(variable_define)) {
                    Some(ty) => ty.clone(),
                    _ => match &variable_define.type_tag {
                        Some(type_tag) => match &type_tag.type_info {
                            Ok(type_tag) => get_type(
                                type_tag,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                generics_map,
                                current_scope_this_type,
                                errors,
                                warnings,
                                context,
                            ),
                            Err(_) => Type::Unknown,
                        },
                        None => Type::Unknown,
                    },
                };

                if let Some(type_tag) = &variable_define.type_tag {
                    type_environment.add_check_type_info_bounds(
                        Spanned::new(tag_type.clone(), type_tag.span.clone()),
                        &Type::This,
                        false,
                        current_scope_implements_info_set,
                        context,
                    );

                    type_environment.set_entity_type(
                        EntityID::from(type_tag),
                        WithDefineInfo {
                            value: tag_type.clone(),
                            module_name: context.module_name.clone(),
                            span: type_tag.span.clone(),
                        },
                    );
                }

                if &tag_type != &Type::Unknown {
                    if let Ok(binding) = &variable_define.binding {
                        let tag_type_span = variable_define
                            .type_tag
                            .as_ref()
                            .unwrap()
                            .type_info
                            .as_ref()
                            .unwrap()
                            .get_span();

                        bind_variable_for_tuple(
                            binding,
                            &tag_type,
                            &tag_type_span,
                            type_environment,
                            context,
                        );
                    }
                }

                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        infer_type_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            generics_map,
                            module_entity_type_map,
                            global_implements_info_set,
                            current_scope_this_type,
                            current_scope_implements_info_set,
                            true,
                            type_environment,
                            type_inference_results,
                            allocator,
                            errors,
                            warnings,
                            context,
                        );

                        if &tag_type == &Type::Unknown {
                            if let Ok(binding) = &variable_define.binding {
                                match &binding.binding {
                                    Either::Left(literal) => {
                                        type_environment.set_entity_id_equals(
                                            EntityID::from(*expression),
                                            EntityID::from(literal),
                                        );
                                    }
                                    Either::Right(_) => {
                                        let expression_type = type_environment
                                            .resolve_entity_type(EntityID::from(*expression));

                                        bind_variable_for_tuple(
                                            binding,
                                            &expression_type.value,
                                            &expression.get_span(),
                                            type_environment,
                                            context,
                                        );
                                    }
                                }
                            }
                        } else {
                            if let Ok(binding) = &variable_define.binding {
                                let tag_type_span = variable_define
                                    .type_tag
                                    .as_ref()
                                    .map(|type_tag| {
                                        type_tag
                                            .type_info
                                            .as_ref()
                                            .map(|type_info| type_info.get_span())
                                            .ok()
                                    })
                                    .flatten()
                                    .unwrap_or(variable_define.span.clone());

                                match &binding.binding {
                                    Either::Left(literal) => {
                                        let result = type_environment.unify_with_implicit_convert(
                                            Spanned::new(
                                                EntityID::from(*expression),
                                                expression.get_span(),
                                            ),
                                            Spanned::new(EntityID::from(literal), tag_type_span),
                                            true,
                                            current_scope_this_type,
                                        );
                                        add_error(result, type_environment);
                                    }
                                    Either::Right(_) => {
                                        let expression_type = type_environment
                                            .resolve_entity_type(EntityID::from(*expression));

                                        unify_variable_binding(
                                            binding,
                                            &expression_type.value,
                                            &expression.get_span(),
                                            current_scope_this_type,
                                            type_environment,
                                            context,
                                        );
                                    }
                                }
                            }
                        }

                        if let Ok(binding) = &variable_define.binding {
                            if context.context.settings.is_transpiler_debug {
                                type_environment.register_variable(binding);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    if !is_closure_scope {
        for i in 0..type_environment.closures.len() {
            let closure = type_environment.closures[i];

            let closure_type = type_environment.resolve_entity_type(EntityID::from(closure));

            if let Type::Function {
                function_info,
                generics: _,
            } = closure_type.value
            {
                let return_type_temp = type_environment.return_type.clone();
                type_environment.return_type = Either::Right(WithDefineInfo {
                    value: function_info.return_type.value.clone(),
                    module_name: context.module_name.clone(),
                    span: function_info.return_type.span.clone(),
                });

                if let Some(block_or_expr) = &closure.expression_or_block.value {
                    match block_or_expr {
                        Either::Left(expression) => {
                            infer_type_expression(
                                *expression,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                module_element_type_maps,
                                generics_map,
                                module_entity_type_map,
                                global_implements_info_set,
                                current_scope_this_type,
                                current_scope_implements_info_set,
                                true,
                                type_environment,
                                type_inference_results,
                                allocator,
                                errors,
                                warnings,
                                context,
                            );

                            let expression_type =
                                type_environment.resolve_entity_type(EntityID::from(*expression));

                            if &expression_type.value != &Type::Unreachable {
                                let result = type_environment.unify_with_return_type(
                                    WithDefineInfo {
                                        value: EntityID::from(*expression),
                                        module_name: context.module_name.clone(),
                                        span: expression.get_span(),
                                    },
                                    current_scope_this_type,
                                );
                                add_error(result, type_environment);
                            }
                        }
                        Either::Right(block) => {
                            infer_type_program(
                                block.program,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                module_element_type_maps,
                                generics_map,
                                module_entity_type_map,
                                global_implements_info_set,
                                current_scope_this_type,
                                current_scope_implements_info_set,
                                false,
                                true,
                                implements_interfaces,
                                true,
                                type_environment,
                                type_inference_results,
                                allocator,
                                errors,
                                warnings,
                                context,
                            );

                            let program_type =
                                type_environment.resolve_entity_type(EntityID::from(block.program));

                            if &program_type.value != &Type::Unreachable {
                                let result = type_environment.unify_with_return_type(
                                    WithDefineInfo {
                                        value: EntityID::from(block.program),
                                        module_name: context.module_name.clone(),
                                        span: block.span.clone(),
                                    },
                                    current_scope_this_type,
                                );
                                add_error(result, type_environment);
                            }
                        }
                    }
                }

                type_environment.return_type = return_type_temp;
            } else {
                unreachable!()
            }
        }
    }

    if !has_type {
        type_environment.set_entity_type(
            EntityID::from(ast),
            WithDefineInfo {
                value: Type::Unit,
                module_name: context.module_name.clone(),
                span: ast.span.clone(),
            },
        );
    }

    if !is_interface_scope {
        override_elements_environment.collect_errors(errors, context);
    }
}

fn bind_variable_for_tuple(
    ast: &VariableBinding,
    ty: &Type,
    type_span: &Range<usize>,
    type_environment: &mut TypeEnvironment,
    context: &TranspileModuleContext,
) {
    match &ast.binding {
        Either::Left(literal) => {
            type_environment.set_entity_type(
                EntityID::from(literal),
                WithDefineInfo {
                    value: ty.clone(),
                    module_name: context.module_name.clone(),
                    span: literal.span.clone(),
                },
            );
        }
        Either::Right(bindings) => {
            let resolved_type = type_environment.resolve_type_surface(ty);

            if let Type::Tuple(types) = &resolved_type {
                if bindings.len() != types.len() {
                    let error = InvalidTupleLength {
                        ty: ty.clone(),
                        type_span: type_span.clone(),
                        binding_span: ast.span.clone(),
                        types_length: types.len(),
                        bindings_length: bindings.len(),
                    };
                    type_environment.add_lazy_type_error_report(error);

                    bind_variable_unknown(ast, type_environment, context);

                    return;
                }

                for (binding, ty) in bindings.iter().zip(types.iter()) {
                    bind_variable_for_tuple(binding, ty, type_span, type_environment, context);
                }
            } else {
                let error = InvalidTypeTupleBinding {
                    ty: ty.clone(),
                    type_span: type_span.clone(),
                    binding_span: ast.span.clone(),
                };
                type_environment.add_lazy_type_error_report(error);

                bind_variable_unknown(ast, type_environment, context);
            }
        }
    }
}

fn bind_variable_unknown(
    ast: &VariableBinding,
    type_environment: &mut TypeEnvironment,
    context: &TranspileModuleContext,
) {
    match &ast.binding {
        Either::Left(literal) => {
            type_environment.set_entity_type(
                EntityID::from(literal),
                WithDefineInfo {
                    value: Type::Unknown,
                    module_name: context.module_name.clone(),
                    span: ast.span.clone(),
                },
            );
        }
        Either::Right(bindings) => {
            for binding in bindings.iter() {
                bind_variable_unknown(binding, type_environment, context);
            }
        }
    }
}

fn unify_variable_binding(
    ast: &VariableBinding,
    ty: &Type,
    type_span: &Range<usize>,
    current_scope_this_type: &ScopeThisType,
    type_environment: &mut TypeEnvironment,
    context: &TranspileModuleContext,
) {
    match &ast.binding {
        Either::Left(literal) => {
            let literal_type = type_environment.resolve_entity_type(EntityID::from(literal));

            if let Type::Unknown = type_environment.resolve_type_surface(&literal_type.value) {
                type_environment.set_entity_type(
                    EntityID::from(literal),
                    WithDefineInfo {
                        value: ty.clone(),
                        module_name: context.module_name.clone(),
                        span: type_span.clone(),
                    },
                );
            } else {
                let result = type_environment.unify_type(
                    ty,
                    type_span,
                    &context.module_name,
                    &literal_type.value,
                    &literal_type.span,
                    &literal_type.module_name,
                    current_scope_this_type,
                    false,
                    true,
                );
                add_error(result, type_environment);
            }
        }
        Either::Right(bindings) => {
            let resolved_type = type_environment.resolve_type_surface(ty);

            if let Type::Tuple(types) = &resolved_type {
                if bindings.len() != types.len() {
                    let error = InvalidTupleLength {
                        ty: ty.clone(),
                        type_span: type_span.clone(),
                        binding_span: ast.span.clone(),
                        types_length: types.len(),
                        bindings_length: bindings.len(),
                    };
                    type_environment.add_lazy_type_error_report(error);

                    return;
                }

                for (binding, ty) in bindings.iter().zip(types.iter()) {
                    unify_variable_binding(
                        binding,
                        ty,
                        type_span,
                        current_scope_this_type,
                        type_environment,
                        context,
                    );
                }
            } else {
                let error = InvalidTypeTupleBinding {
                    ty: ty.clone(),
                    type_span: type_span.clone(),
                    binding_span: ast.span.clone(),
                };
                type_environment.add_lazy_type_error_report(error);
            }
        }
    }
}

fn get_and_check_where_bounds_implements_info(
    where_bounds: &Vec<WhereBound>,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    type_environment: &mut TypeEnvironment,
    context: &TranspileModuleContext,
) -> Option<Arc<ImplementsInfoSet>> {
    if where_bounds.is_empty() {
        return current_scope_implements_info_set.clone();
    }

    let mut implements_infos = current_scope_implements_info_set
        .as_ref()
        .map(|implements_info_set| implements_info_set.implements_infos.clone())
        .unwrap_or(IndexMap::new());

    for where_bound in where_bounds.iter() {
        let current_implements_info_set = Some(Arc::new(ImplementsInfoSet {
            implements_infos: implements_infos.clone(),
        }));

        type_environment.add_check_type_info_bounds(
            where_bound.target_type.clone(),
            &where_bound.target_type.value,
            true,
            &current_implements_info_set,
            context,
        );

        for bound in where_bound.bounds.iter() {
            type_environment.add_check_type_info_bounds(
                Spanned::new(bound.ty.clone(), bound.span.clone()),
                &where_bound.target_type.value,
                false,
                &current_implements_info_set,
                context,
            );

            implements_infos.insert(
                bound.entity_id,
                ImplementsInfo {
                    generics: Arc::new(Vec::new()),
                    interface: Spanned::new(bound.ty.clone(), bound.span.clone()),
                    concrete: where_bound.target_type.clone(),
                    module_name: context.module_name.clone(),
                    where_bounds: Arc::new(Vec::new()),
                    element_types: Arc::new(FxHashMap::default()),
                    is_bounds_info: true,
                },
            );
        }
    }

    Some(Arc::new(ImplementsInfoSet { implements_infos }))
}

fn infer_type_expression<'input, 'allocator>(
    ast: Expression<'input, 'allocator>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            if !or_expression.right_exprs.is_empty() {
                type_environment.set_entity_type(
                    EntityID::from(ast),
                    WithDefineInfo {
                        value: Type::Bool,
                        module_name: context.module_name.clone(),
                        span: ast.get_span(),
                    },
                );
            }

            force_be_expression |= !or_expression.right_exprs.is_empty();

            infer_type_and_expression(
                &or_expression.left_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                type_inference_results,
                allocator,
                errors,
                warnings,
                context,
            );

            if let Some(first_right_expr) = or_expression.right_exprs.first() {
                if type_environment
                    .unify(
                        Spanned::new(
                            EntityID::from(&or_expression.left_expr),
                            or_expression.left_expr.span.clone(),
                        ),
                        Spanned::new(EntityID::from(ast), ast.get_span()),
                        current_scope_this_type,
                    )
                    .is_err()
                {
                    let found_type = type_environment
                        .resolve_entity_type(EntityID::from(&or_expression.left_expr))
                        .value;
                    let error = LogicalOperatorTypeError {
                        operator: Spanned::new("or".to_string(), first_right_expr.0.clone()),
                        found_type: Spanned::new(found_type, or_expression.left_expr.span.clone()),
                    };
                    type_environment.add_lazy_type_error_report(error);
                }
            }

            for right_expr in or_expression.right_exprs.iter() {
                let operator_span = right_expr.0.clone();

                if let Ok(right_expr) = &right_expr.1 {
                    infer_type_and_expression(
                        right_expr,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        module_entity_type_map,
                        global_implements_info_set,
                        current_scope_this_type,
                        current_scope_implements_info_set,
                        force_be_expression,
                        type_environment,
                        type_inference_results,
                        allocator,
                        errors,
                        warnings,
                        context,
                    );

                    let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

                    if type_environment
                        .unify(
                            current.clone(),
                            Spanned::new(EntityID::from(ast), ast.get_span()),
                            current_scope_this_type,
                        )
                        .is_err()
                    {
                        let found_type = type_environment
                            .resolve_entity_type(EntityID::from(right_expr))
                            .value;
                        let error = LogicalOperatorTypeError {
                            operator: Spanned::new("or".to_string(), operator_span),
                            found_type: Spanned::new(found_type, right_expr.span.clone()),
                        };
                        type_environment.add_lazy_type_error_report(error);
                    }
                }
            }

            if or_expression.right_exprs.is_empty() {
                type_environment.set_entity_id_equals(
                    EntityID::from(&or_expression.left_expr),
                    EntityID::from(ast),
                );
            }
        }
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                infer_type_expression(
                    expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    true,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                let result = type_environment.unify_with_return_type(
                    WithDefineInfo {
                        value: EntityID::from(expression),
                        module_name: context.module_name.clone(),
                        span: expression.get_span(),
                    },
                    current_scope_this_type,
                );
                add_error(result, type_environment);
            }

            type_environment.set_entity_type(
                EntityID::from(ast),
                WithDefineInfo {
                    value: Type::Unreachable,
                    module_name: context.module_name.clone(),
                    span: return_expression.span.clone(),
                },
            );
        }
        ExpressionEnum::Closure(closure) => {
            let mut argument_types = Vec::new();

            match &closure.arguments.arguments {
                Either::Left(literal) => {
                    let ty =
                        Type::LocalGeneric(type_environment.new_local_generic_id(
                            literal.span.clone(),
                            context.module_name.clone(),
                        ));

                    type_environment.set_entity_type(
                        EntityID::from(literal),
                        WithDefineInfo {
                            value: ty.clone(),
                            module_name: context.module_name.clone(),
                            span: literal.span.clone(),
                        },
                    );

                    argument_types.push(ty);
                }
                Either::Right(arguments) => {
                    for argument in arguments.iter() {
                        match argument {
                            Either::Left(argument) => {
                                let ty = if let Ok(type_info) = &argument.type_tag.type_info {
                                    get_type(
                                        type_info,
                                        user_type_map,
                                        import_element_map,
                                        name_resolved_map,
                                        module_user_type_map,
                                        module_element_type_map,
                                        generics_map,
                                        current_scope_this_type,
                                        errors,
                                        warnings,
                                        context,
                                    )
                                } else {
                                    Type::LocalGeneric(type_environment.new_local_generic_id(
                                        argument.span.clone(),
                                        context.module_name.clone(),
                                    ))
                                };

                                bind_variable_for_tuple(
                                    &argument.binding,
                                    &ty,
                                    &argument.span,
                                    type_environment,
                                    context,
                                );

                                argument_types.push(ty);
                            }
                            Either::Right(literal) => {
                                let ty = Type::LocalGeneric(type_environment.new_local_generic_id(
                                    literal.span.clone(),
                                    context.module_name.clone(),
                                ));

                                type_environment.set_entity_type(
                                    EntityID::from(literal),
                                    WithDefineInfo {
                                        value: ty.clone(),
                                        module_name: context.module_name.clone(),
                                        span: literal.span.clone(),
                                    },
                                );

                                argument_types.push(ty);
                            }
                        }
                    }
                }
            }

            let return_type = Spanned::new(
                Type::LocalGeneric(
                    type_environment
                        .new_local_generic_id(closure.span.clone(), context.module_name.clone()),
                ),
                closure.span.clone(),
            );

            let define_info = FunctionDefineInfo {
                module_name: context.module_name.clone(),
                entity_id: EntityID::from(closure),
                generics_define_span: None,
                arguments_span: closure.arguments.span.clone(),
                is_closure: true,
                origin_function: Arc::new(RwLock::new(None)),
                span: closure.span.clone(),
            };

            let function_info = FunctionType {
                is_extension: false,
                generics_define: Vec::new(),
                argument_types,
                return_type,
                define_info,
                where_bounds: FreezableMutex::new(Vec::new()),
            };

            let closure_type = WithDefineInfo {
                value: Type::Function {
                    function_info: Arc::new(function_info),
                    generics: Arc::new(Vec::new()),
                },
                module_name: context.module_name.clone(),
                span: closure.span.clone(),
            };

            type_environment.set_entity_type(EntityID::from(closure), closure_type);

            type_environment.set_entity_id_equals(EntityID::from(closure), EntityID::from(ast));

            type_environment.closures.push(closure);
        }
    }
}

fn infer_type_operator<'input, 'allocator>(
    left_entity_id: Spanned<EntityID>,
    right_entity_id: Option<Spanned<EntityID>>,
    parent_temp_entity_id: EntityID,
    operator_module: &str,
    operator_interface: &str,
    operator_method: &str,
    operator: Spanned<&str>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    let previous_type = type_environment.resolve_entity_type(left_entity_id.value);

    let operator_function = get_element_type(
        WithDefineInfo {
            value: previous_type.value.clone(),
            module_name: context.module_name.clone(),
            span: left_entity_id.span.clone(),
        },
        Spanned::new(operator_method, operator.span.clone()),
        |ty| {
            if let Type::UserType {
                user_type_info,
                generics: _,
                generics_span: _,
            } = ty
            {
                user_type_info.module_name.as_str() == operator_module
                    && user_type_info.name.value.as_str() == operator_interface
            } else {
                false
            }
        },
        global_implements_info_set,
        current_scope_this_type,
        current_scope_implements_info_set,
        type_environment,
        allocator,
    );

    let operator_function = match operator_function {
        Ok(operator_function) => operator_function.0,
        Err(error) => {
            match error {
                Either::Left(_) => {
                    let error = SimpleError::new(
                        NOT_IMPLEMENTS_OPERATOR_INTERFACE,
                        operator.span.clone(),
                        vec![
                            (operator.value.to_string(), Color::Yellow),
                            (
                                type_environment.get_type_display_string(&previous_type.value),
                                Color::Red,
                            ),
                            (
                                format!("{}::{}", operator_module, operator_interface),
                                Color::Yellow,
                            ),
                        ],
                        vec![(
                            (context.module_name.clone(), left_entity_id.span.clone()),
                            Color::Red,
                        )],
                    );
                    errors.push(error);
                }
                Either::Right(error) => {
                    errors.push(TranspileError::new(error));
                }
            }

            type_environment.set_entity_type(
                parent_temp_entity_id,
                WithDefineInfo {
                    value: Type::Unknown,
                    module_name: context.module_name.clone(),
                    span: operator.span.clone(),
                },
            );

            return;
        }
    };

    let return_type = if let Some(right_entity_id) = &right_entity_id {
        type_environment
            .operator_function_types
            .push((right_entity_id.value, operator_function.value.clone()));

        let span = left_entity_id.span.start..right_entity_id.span.end;

        if let Some(argument_type) = operator_function
            .value
            .get_indexed_element_type_with_replaced_generic(1)
        {
            type_environment.set_entity_type(
                parent_temp_entity_id,
                WithDefineInfo {
                    value: argument_type,
                    module_name: context.module_name.clone(),
                    span: operator.span.clone(),
                },
            );

            let result = type_environment.unify_with_implicit_convert(
                Spanned::new(parent_temp_entity_id, right_entity_id.span.clone()),
                right_entity_id.clone(),
                false,
                current_scope_this_type,
            );

            add_error(result, type_environment);

            let return_type = operator_function
                .value
                .get_return_type_with_replaced_generic()
                .unwrap();

            WithDefineInfo {
                value: return_type,
                module_name: context.module_name.clone(),
                span,
            }
        } else {
            WithDefineInfo {
                value: Type::Unknown,
                module_name: context.module_name.clone(),
                span,
            }
        }
    } else {
        type_environment
            .operator_function_types
            .push((left_entity_id.value, operator_function.value.clone()));

        let span = operator.span.start..left_entity_id.span.end;

        if let Some(return_type) = operator_function
            .value
            .get_return_type_with_replaced_generic()
        {
            WithDefineInfo {
                value: return_type,
                module_name: context.module_name.clone(),
                span,
            }
        } else {
            WithDefineInfo {
                value: Type::Unknown,
                module_name: context.module_name.clone(),
                span,
            }
        }
    };

    if let Some(right_entity_id) = right_entity_id {
        type_environment
            .operator_return_types
            .push((right_entity_id.value, return_type.value.clone()));
    } else {
        type_environment
            .operator_return_types
            .push((left_entity_id.value, return_type.value.clone()));
    }

    type_environment.set_entity_type(parent_temp_entity_id, return_type);
}

fn infer_type_and_expression<'input, 'allocator>(
    ast: &'allocator AndExpression<'input, 'allocator>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    if !ast.right_exprs.is_empty() {
        type_environment.set_entity_type(
            EntityID::from(ast),
            WithDefineInfo {
                value: Type::Bool,
                module_name: context.module_name.clone(),
                span: ast.span.clone(),
            },
        );
    }

    force_be_expression |= !ast.right_exprs.is_empty();

    infer_type_compare_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        module_entity_type_map,
        global_implements_info_set,
        current_scope_this_type,
        current_scope_implements_info_set,
        force_be_expression,
        type_environment,
        type_inference_results,
        allocator,
        errors,
        warnings,
        context,
    );

    if let Some(first_right_expr) = ast.right_exprs.first() {
        if type_environment
            .unify(
                Spanned::new(EntityID::from(&ast.left_expr), ast.left_expr.span.clone()),
                Spanned::new(EntityID::from(ast), ast.span.clone()),
                current_scope_this_type,
            )
            .is_err()
        {
            let found_type = type_environment
                .resolve_entity_type(EntityID::from(&ast.left_expr))
                .value;
            let error = LogicalOperatorTypeError {
                operator: Spanned::new("and".to_string(), first_right_expr.0.clone()),
                found_type: Spanned::new(found_type, ast.left_expr.span.clone()),
            };
            type_environment.add_lazy_type_error_report(error);
        }
    }

    for right_expr in ast.right_exprs.iter() {
        let operator_span = right_expr.0.clone();

        if let Ok(right_expr) = &right_expr.1 {
            infer_type_compare_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                type_inference_results,
                allocator,
                errors,
                warnings,
                context,
            );

            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            if type_environment
                .unify(
                    current.clone(),
                    Spanned::new(EntityID::from(ast), ast.span.clone()),
                    current_scope_this_type,
                )
                .is_err()
            {
                let found_type = type_environment
                    .resolve_entity_type(EntityID::from(right_expr))
                    .value;
                let error = LogicalOperatorTypeError {
                    operator: Spanned::new("and".to_string(), operator_span),
                    found_type: Spanned::new(found_type, right_expr.span.clone()),
                };
                type_environment.add_lazy_type_error_report(error);
            }
        }
    }

    if ast.right_exprs.is_empty() {
        type_environment.set_entity_id_equals(EntityID::from(&ast.left_expr), EntityID::from(ast));
    }
}

fn infer_type_compare_expression<'input, 'allocator>(
    ast: &'allocator CompareExpression<'input, 'allocator>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    force_be_expression |= !ast.right_exprs.is_empty();

    infer_type_add_or_sub_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        module_entity_type_map,
        global_implements_info_set,
        current_scope_this_type,
        current_scope_implements_info_set,
        force_be_expression,
        type_environment,
        type_inference_results,
        allocator,
        errors,
        warnings,
        context,
    );

    let mut previous = Spanned::new(EntityID::from(&ast.left_expr), ast.left_expr.span.clone());

    for right_expr in ast.right_exprs.iter() {
        let operator = right_expr.0.clone().map(|op| op.as_str());

        if let Ok(right_expr) = &right_expr.1 {
            infer_type_add_or_sub_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                type_inference_results,
                allocator,
                errors,
                warnings,
                context,
            );

            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let (operator_module, operator_interface, operator_method) = match operator.value {
                ">" => ("std::compare::order", "PartialOrder", "greater_than"),
                ">=" => ("std::compare::order", "PartialOrder", "greater_or_equals"),
                "<" => ("std::compare::order", "PartialOrder", "less_than"),
                "<=" => ("std::compare::order", "PartialOrder", "less_or_equals"),
                "==" => ("std::compare::equal", "PartialEqual", "equals"),
                "!=" => ("std::compare::equal", "PartialEqual", "equals"),
                _ => unreachable!(),
            };

            infer_type_operator(
                previous.clone(),
                Some(current.clone()),
                EntityID::from(ast),
                operator_module,
                operator_interface,
                operator_method,
                operator,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                type_environment,
                allocator,
                errors,
                context,
            );

            previous = Spanned::new(EntityID::from(ast), ast.span.start..right_expr.span.end);
        }
    }

    type_environment.set_entity_id_equals(previous.value, EntityID::from(ast));
}

fn infer_type_add_or_sub_expression<'input, 'allocator>(
    ast: &'allocator AddOrSubExpression<'input, 'allocator>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    force_be_expression |= !ast.right_exprs.is_empty();

    infer_type_mul_or_div_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        module_entity_type_map,
        global_implements_info_set,
        current_scope_this_type,
        current_scope_implements_info_set,
        force_be_expression,
        type_environment,
        type_inference_results,
        allocator,
        errors,
        warnings,
        context,
    );

    let mut previous = Spanned::new(EntityID::from(&ast.left_expr), ast.left_expr.span.clone());

    for right_expr in ast.right_exprs.iter() {
        let operator = right_expr.0.clone().map(|op| op.as_str());

        if let Ok(right_expr) = &right_expr.1 {
            infer_type_mul_or_div_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                type_inference_results,
                allocator,
                errors,
                warnings,
                context,
            );

            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let (operator_module, operator_interface, operator_method) = match operator.value {
                "+" => ("std::operator::add", "Add", "add"),
                "-" => ("std::operator::sub", "Sub", "sub"),
                _ => unreachable!(),
            };

            infer_type_operator(
                previous.clone(),
                Some(current.clone()),
                EntityID::from(ast),
                operator_module,
                operator_interface,
                operator_method,
                operator,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                type_environment,
                allocator,
                errors,
                context,
            );

            previous = Spanned::new(EntityID::from(ast), ast.span.start..right_expr.span.end);
        }
    }

    type_environment.set_entity_id_equals(previous.value, EntityID::from(ast));
}

fn infer_type_mul_or_div_expression<'input, 'allocator>(
    ast: &'allocator MulOrDivExpression<'input, 'allocator>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    force_be_expression |= !ast.right_exprs.is_empty();

    infer_type_factor(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        module_entity_type_map,
        global_implements_info_set,
        current_scope_this_type,
        current_scope_implements_info_set,
        force_be_expression,
        type_environment,
        type_inference_results,
        allocator,
        errors,
        warnings,
        context,
    );

    let mut previous = Spanned::new(EntityID::from(&ast.left_expr), ast.left_expr.span.clone());

    for right_expr in ast.right_exprs.iter() {
        let operator = right_expr.0.clone().map(|op| op.as_str());

        if let Ok(right_expr) = &right_expr.1 {
            infer_type_factor(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                type_inference_results,
                allocator,
                errors,
                warnings,
                context,
            );

            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let (operator_module, operator_interface, operator_method) = match operator.value {
                "*" => ("std::operator::mul", "Mul", "mul"),
                "/" => ("std::operator::div", "Div", "div"),
                _ => unreachable!(),
            };

            infer_type_operator(
                previous.clone(),
                Some(current.clone()),
                EntityID::from(ast),
                operator_module,
                operator_interface,
                operator_method,
                operator,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                type_environment,
                allocator,
                errors,
                context,
            );

            previous = Spanned::new(EntityID::from(ast), ast.span.start..right_expr.span.end);
        }
    }

    type_environment.set_entity_id_equals(previous.value, EntityID::from(ast));
}

fn infer_type_factor<'input, 'allocator>(
    ast: &'allocator Factor<'input, 'allocator>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    if let Ok(primary) = &ast.primary {
        infer_type_primary(
            primary,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            global_implements_info_set,
            current_scope_this_type,
            current_scope_implements_info_set,
            force_be_expression,
            type_environment,
            type_inference_results,
            allocator,
            errors,
            warnings,
            context,
        );
        // TODO - check bounds

        type_environment.set_entity_id_equals(EntityID::from(primary), EntityID::from(ast));
    } else {
        type_environment.set_entity_type(
            EntityID::from(ast),
            WithDefineInfo {
                value: Type::Unknown,
                module_name: context.module_name.clone(),
                span: ast.span.clone(),
            },
        );
    }
}

fn infer_type_primary<'input, 'allocator>(
    ast: &'allocator Primary<'input, 'allocator>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    let module_name_result =
        get_module_name_from_primary(ast, name_resolved_map, import_element_map, context);

    if let Some((module_name, count)) = module_name_result {
        if ast.chain.is_empty() {
            let span = ast.left.span.clone();
            let next_span = span.end..span.end;

            let error = SimpleError::new(
                0043,
                next_span.clone(),
                vec![],
                vec![
                    ((context.module_name.clone(), span), Color::Yellow),
                    ((context.module_name.clone(), next_span.clone()), Color::Red),
                ],
            );
            let mut error = TranspileError::new(error);

            let advice = Advice::Add {
                add: "::element_name_here".to_string(),
                position: next_span.end,
                message_override: Some("error.0043.advice"),
            };
            error.add_advice(context.module_name.clone(), advice);

            errors.push(error);

            type_environment.set_entity_type(
                EntityID::from(ast),
                WithDefineInfo {
                    value: Type::Unknown,
                    module_name: context.module_name.clone(),
                    span: ast.span.clone(),
                },
            );

            return;
        }

        let next_primary = &ast.chain[count];

        if next_primary.separator.value != PrimarySeparatorKind::DoubleColon {
            let span = next_primary.separator.span.clone();

            let error = SimpleError::new(
                0042,
                span.clone(),
                vec![],
                vec![
                    (
                        (context.module_name.clone(), next_primary.span.clone()),
                        Color::Yellow,
                    ),
                    ((context.module_name.clone(), span), Color::Red),
                ],
            );
            errors.push(TranspileError::new(error));

            type_environment.set_entity_type(
                EntityID::from(ast),
                WithDefineInfo {
                    value: Type::Unknown,
                    module_name: context.module_name.clone(),
                    span: ast.span.clone(),
                },
            );

            return;
        }

        if let Some(second_expr) = &next_primary.second_expr {
            let import_user_type_map = module_user_type_map.get(&module_name).unwrap();
            let element_type_map = module_element_type_maps.get(&module_name).unwrap();

            let ty = if let Some(user_type) = import_user_type_map.get(second_expr.0.value) {
                user_type.clone()
            } else if let Some(element_type) = element_type_map.get(second_expr.0.value) {
                element_type.clone()
            } else {
                let span = second_expr.0.span.clone();
                let first_span_start = ast.left.span.start;
                let first_span_end = if count == 0 {
                    ast.left.span.end
                } else {
                    ast.chain[count - 1].span.end
                };

                let error = SimpleError::new(
                    0044,
                    span.clone(),
                    vec![],
                    vec![
                        (
                            (
                                context.module_name.clone(),
                                first_span_start..first_span_end,
                            ),
                            Color::Yellow,
                        ),
                        ((context.module_name.clone(), span), Color::Red),
                    ],
                );
                errors.push(error);

                Type::Unknown
            };

            let ty = type_environment.get_user_or_function_type_with_local_generic_id(
                WithDefineInfo {
                    value: ty,
                    module_name: context.module_name.clone(),
                    span: second_expr.0.span.clone(),
                },
                current_scope_implements_info_set,
                true,
            );

            type_environment.set_entity_type(EntityID::from(&second_expr.0), ty);

            if let Some(generics) = &second_expr.1 {
                if let Ok(generics) = generics {
                    infer_type_generics(
                        generics,
                        EntityID::from(&second_expr.0),
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        current_scope_this_type,
                        current_scope_implements_info_set,
                        type_environment,
                        errors,
                        warnings,
                        context,
                    );
                }
            }

            let mut mappings = Vec::new_in(allocator);

            let entity_id = if let Some(function_call) = &second_expr.2 {
                infer_type_function_call(
                    function_call,
                    EntityID::from(&second_expr.0),
                    false,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                Spanned::new(
                    EntityID::from(function_call),
                    second_expr.0.span.start..function_call.span.end,
                )
            } else {
                Spanned::new(EntityID::from(&second_expr.0), second_expr.0.span.clone())
            };

            let mut previous = if let Some(mapping_operator) = &next_primary.mapping_operator {
                infer_type_mapping_operator(
                    mapping_operator,
                    entity_id,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    type_environment,
                    type_inference_results,
                    &mut mappings,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                Spanned::new(
                    EntityID::from(&mapping_operator.value),
                    second_expr.0.span.start..mapping_operator.span.end,
                )
            } else {
                entity_id
            };

            for i in (count + 1)..ast.chain.len() {
                let primary_right = &ast.chain[i];

                infer_type_primary_right(
                    primary_right,
                    previous,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    type_environment,
                    type_inference_results,
                    &mut mappings,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                previous = Spanned::new(EntityID::from(primary_right), primary_right.span.clone());
            }

            type_environment.set_entity_id_equals(previous.value, EntityID::from(ast));
        } else {
            let span = next_primary.span.clone();
            let first_span_start = ast.left.span.start;
            let first_span_end = if count == 0 {
                ast.left.span.end
            } else {
                ast.chain[count - 1].span.end
            };

            let error = SimpleError::new(
                0043,
                span.clone(),
                vec![],
                vec![
                    (
                        (
                            context.module_name.clone(),
                            first_span_start..first_span_end,
                        ),
                        Color::Yellow,
                    ),
                    ((context.module_name.clone(), span), Color::Red),
                ],
            );
            let mut error = TranspileError::new(error);

            let advice = Advice::Add {
                add: "element_name_here".to_string(),
                position: next_primary.separator.span.end,
                message_override: Some("error.0043.advice"),
            };
            error.add_advice(context.module_name.clone(), advice);

            errors.push(error);

            type_environment.set_entity_type(
                EntityID::from(ast),
                WithDefineInfo {
                    value: Type::Unknown,
                    module_name: context.module_name.clone(),
                    span: ast.span.clone(),
                },
            );
        }
    } else {
        let mut mappings = Vec::new_in(allocator);

        infer_type_primary_left(
            &ast.left,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            global_implements_info_set,
            current_scope_this_type,
            current_scope_implements_info_set,
            force_be_expression,
            type_environment,
            type_inference_results,
            &mut mappings,
            allocator,
            errors,
            warnings,
            context,
        );

        let mut previous = Spanned::new(EntityID::from(&ast.left), ast.left.span.clone());
        for primary_right in ast.chain.iter() {
            infer_type_primary_right(
                primary_right,
                previous,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                type_environment,
                type_inference_results,
                &mut mappings,
                allocator,
                errors,
                warnings,
                context,
            );
            previous = Spanned::new(EntityID::from(primary_right), primary_right.span.clone());
        }

        type_environment.set_entity_id_equals(previous.value, EntityID::from(ast));
    }
}

enum MappingTypeKind {
    Option,
    Result { error_type: Type },
}

fn infer_type_primary_left<'input, 'allocator>(
    ast: &'allocator PrimaryLeft<'input, 'allocator>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    mappings: &mut Vec<MappingTypeKind, &'allocator Bump>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expressions {
                    expressions,
                    error_tokens: _,
                    span,
                } => match expressions.len() {
                    1 => {
                        let expression = expressions.first().unwrap();

                        infer_type_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            generics_map,
                            module_entity_type_map,
                            global_implements_info_set,
                            current_scope_this_type,
                            current_scope_implements_info_set,
                            force_be_expression,
                            type_environment,
                            type_inference_results,
                            allocator,
                            errors,
                            warnings,
                            context,
                        );
                        type_environment.set_entity_id_equals(
                            EntityID::from(*expression),
                            EntityID::from(&simple.0),
                        );
                    }
                    0 => {
                        type_environment.set_entity_type(
                            EntityID::from(&simple.0),
                            WithDefineInfo {
                                value: Type::Unit,
                                module_name: context.module_name.clone(),
                                span: span.clone(),
                            },
                        );
                    }
                    _ => {
                        let mut types = Vec::new();

                        for expression in expressions.iter() {
                            infer_type_expression(
                                &expression,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                module_element_type_maps,
                                generics_map,
                                module_entity_type_map,
                                global_implements_info_set,
                                current_scope_this_type,
                                current_scope_implements_info_set,
                                force_be_expression,
                                type_environment,
                                type_inference_results,
                                allocator,
                                errors,
                                warnings,
                                context,
                            );

                            let ty =
                                type_environment.resolve_entity_type(EntityID::from(*expression));
                            types.push(ty.value);
                        }

                        let tuple_type = Type::Tuple(Arc::new(types));

                        type_environment.set_entity_type(
                            EntityID::from(&simple.0),
                            WithDefineInfo {
                                value: tuple_type,
                                module_name: context.module_name.clone(),
                                span: span.clone(),
                            },
                        );
                    }
                },
                SimplePrimary::Identifier(identifier) => {
                    if let Some(resolved) = name_resolved_map.get(&EntityID::from(identifier)) {
                        let link = match resolved.define_info.define_kind {
                            DefineKind::Import => false,
                            DefineKind::Function => {
                                // TODO - check is method
                                true
                            }
                            DefineKind::Variable
                            | DefineKind::FunctionArgument
                            | DefineKind::ClosureArgument => {
                                if resolved.has_separator(&[
                                    EnvironmentSeparatorKind::UserTypeDefine,
                                    EnvironmentSeparatorKind::Function,
                                ]) {
                                    errors.push(OutOfEnvironmentVariable::new(
                                        identifier.span.clone(),
                                        resolved,
                                    ));
                                    false
                                } else {
                                    true
                                }
                            }
                            DefineKind::UserType => false,
                            DefineKind::Generics => false,
                        };

                        match resolved.define_info.define_kind {
                            DefineKind::Variable
                            | DefineKind::FunctionArgument
                            | DefineKind::ClosureArgument => {
                                if link {
                                    type_environment.set_entity_id_equals(
                                        resolved.define_info.entity_id,
                                        EntityID::from(&simple.0),
                                    );
                                } else {
                                    type_environment.set_entity_type(
                                        EntityID::from(&simple.0),
                                        WithDefineInfo {
                                            value: Type::Unknown,
                                            module_name: context.module_name.clone(),
                                            span: identifier.span.clone(),
                                        },
                                    );
                                }
                            }
                            DefineKind::Generics => {
                                let generic_type =
                                    generics_map.get(&resolved.define_info.entity_id).unwrap();
                                type_environment.set_entity_type(
                                    EntityID::from(&simple.0),
                                    WithDefineInfo {
                                        value: Type::Generic(generic_type.clone()),
                                        module_name: context.module_name.clone(),
                                        span: identifier.span.clone(),
                                    },
                                );
                            }
                            _ => {
                                let ty =
                                    if let Some(user_type) = user_type_map.get(identifier.value) {
                                        user_type.clone()
                                    } else if let Some(element_type) =
                                        module_element_type_map.get(identifier.value)
                                    {
                                        element_type.clone()
                                    } else {
                                        Type::Unknown
                                    };

                                let ty = type_environment
                                    .get_user_or_function_type_with_local_generic_id(
                                        WithDefineInfo {
                                            value: ty,
                                            module_name: context.module_name.clone(),
                                            span: identifier.span.clone(),
                                        },
                                        current_scope_implements_info_set,
                                        true,
                                    );

                                type_environment
                                    .set_entity_type(resolved.define_info.entity_id, ty);

                                type_environment.set_entity_id_equals(
                                    resolved.define_info.entity_id,
                                    EntityID::from(&simple.0),
                                );
                            }
                        }
                    } else {
                        let text = identifier.value;

                        let ty = if let Some(auto_import_module_name) =
                            context.context.auto_import.auto_import_elements.get(text)
                        {
                            module_element_type_maps
                                .get(auto_import_module_name)
                                .expect(
                                    format!(
                                        "Not found auto import module : {}",
                                        auto_import_module_name
                                    )
                                    .as_str(),
                                )
                                .get(text)
                                .expect(
                                    format!("Not found auto import element : {}", text).as_str(),
                                )
                                .clone()
                        } else {
                            let mut numeric_compatible_types = Vec::new();

                            if text.contains(".") {
                                if text.parse::<f32>().is_ok() {
                                    numeric_compatible_types.push(Type::Float32);
                                }
                                if text.parse::<f64>().is_ok() {
                                    numeric_compatible_types.push(Type::Float64);
                                }
                            } else {
                                if text.parse::<i8>().is_ok() {
                                    numeric_compatible_types.push(Type::Int8);
                                }
                                if text.parse::<i16>().is_ok() {
                                    numeric_compatible_types.push(Type::Int16);
                                }
                                if text.parse::<i32>().is_ok() {
                                    numeric_compatible_types.push(Type::Int32);
                                }
                                if text.parse::<i64>().is_ok() {
                                    numeric_compatible_types.push(Type::Int64);
                                }
                                if text.parse::<u8>().is_ok() {
                                    numeric_compatible_types.push(Type::Uint8);
                                }
                                if text.parse::<u16>().is_ok() {
                                    numeric_compatible_types.push(Type::Uint16);
                                }
                                if text.parse::<u32>().is_ok() {
                                    numeric_compatible_types.push(Type::Uint32);
                                }
                                if text.parse::<u64>().is_ok() {
                                    numeric_compatible_types.push(Type::Uint64);
                                }
                            }

                            if !numeric_compatible_types.is_empty() {
                                let new_generic_id = type_environment.new_local_generic_id(
                                    identifier.span.clone(),
                                    context.module_name.clone(),
                                );
                                type_environment.set_generic_type(
                                    new_generic_id,
                                    WithDefineInfo {
                                        value: Type::NumericLiteral(numeric_compatible_types),
                                        module_name: context.module_name.clone(),
                                        span: identifier.span.clone(),
                                    },
                                );
                                Type::LocalGeneric(new_generic_id)
                            } else {
                                parse_primitive_type(text, current_scope_this_type)
                            }
                        };

                        type_environment.set_entity_type(
                            EntityID::from(&simple.0),
                            WithDefineInfo {
                                value: ty,
                                module_name: context.module_name.clone(),
                                span: identifier.span.clone(),
                            },
                        );
                    }
                }
                SimplePrimary::StringLiteral(literal) => {
                    const STD_STRING_MODULE: &str = "std::string";
                    const STD_STRING: &str = "String";

                    let ty = module_user_type_map
                        .get(STD_STRING_MODULE)
                        .expect(
                            format!("Not found auto import module : {}", STD_STRING_MODULE)
                                .as_str(),
                        )
                        .get(STD_STRING)
                        .expect(format!("Not found auto import element : {}", STD_STRING).as_str())
                        .clone();

                    type_environment.set_entity_type(
                        EntityID::from(&simple.0),
                        WithDefineInfo {
                            value: ty,
                            module_name: context.module_name.clone(),
                            span: literal.span.clone(),
                        },
                    );
                }
                SimplePrimary::NullKeyword(null_keyword_span) => {
                    let generic_id = type_environment.new_local_generic_id(
                        null_keyword_span.clone(),
                        context.module_name.clone(),
                    );
                    type_environment.set_entity_type(
                        EntityID::from(&simple.0),
                        WithDefineInfo {
                            value: Type::Option(Arc::new(Type::LocalGeneric(generic_id))),
                            module_name: context.module_name.clone(),
                            span: null_keyword_span.clone(),
                        },
                    );
                }
                SimplePrimary::TrueKeyword(keyword_span) => {
                    type_environment.set_entity_type(
                        EntityID::from(&simple.0),
                        WithDefineInfo {
                            value: Type::Bool,
                            module_name: context.module_name.clone(),
                            span: keyword_span.clone(),
                        },
                    );
                }
                SimplePrimary::FalseKeyword(keyword_span) => {
                    type_environment.set_entity_type(
                        EntityID::from(&simple.0),
                        WithDefineInfo {
                            value: Type::Bool,
                            module_name: context.module_name.clone(),
                            span: keyword_span.clone(),
                        },
                    );
                }
                SimplePrimary::ThisKeyword(literal) => {
                    type_environment.set_entity_type(
                        EntityID::from(&simple.0),
                        WithDefineInfo {
                            value: current_scope_this_type.ty.clone(),
                            module_name: context.module_name.clone(),
                            span: literal.span.clone(),
                        },
                    );
                }
                SimplePrimary::LargeThisKeyword(literal) => {
                    type_environment.set_entity_type(
                        EntityID::from(&simple.0),
                        WithDefineInfo {
                            value: current_scope_this_type.ty.clone(),
                            module_name: context.module_name.clone(),
                            span: literal.span.clone(),
                        },
                    );
                }
            }

            if let Some(generics) = &simple.1 {
                if let Ok(generics) = generics {
                    infer_type_generics(
                        generics,
                        EntityID::from(&simple.0),
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        current_scope_this_type,
                        current_scope_implements_info_set,
                        type_environment,
                        errors,
                        warnings,
                        context,
                    );
                }
            }

            if let Some(function_call) = &simple.2 {
                infer_type_function_call(
                    function_call,
                    EntityID::from(&simple.0),
                    false,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                type_environment.set_entity_id_equals(
                    EntityID::from(function_call),
                    EntityID::from(&ast.first_expr),
                );
            } else {
                type_environment.set_entity_id_equals(
                    EntityID::from(&simple.0),
                    EntityID::from(&ast.first_expr),
                );
            }
        }
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {
            let base_type = if let Ok(init_expression) = new_array_init_expression.init_expression {
                infer_type_expression(
                    init_expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    force_be_expression,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                let init_expression_type =
                    type_environment.resolve_entity_type(EntityID::from(init_expression));

                if new_array_init_expression.for_keyword_span.is_some() {
                    let return_type = Type::LocalGeneric(type_environment.new_local_generic_id(
                        init_expression.get_span(),
                        context.module_name.clone(),
                    ));

                    let define_info = FunctionDefineInfo {
                        module_name: context.module_name.clone(),
                        entity_id: EntityID::from(init_expression),
                        generics_define_span: None,
                        arguments_span: init_expression.get_span(),
                        is_closure: true,
                        origin_function: Arc::new(RwLock::new(None)),
                        span: init_expression.get_span(),
                    };

                    let function_info = FunctionType {
                        is_extension: false,
                        generics_define: Vec::new(),
                        argument_types: vec![Type::Uint64],
                        return_type: Spanned::new(return_type.clone(), init_expression.get_span()),
                        define_info,
                        where_bounds: FreezableMutex::new(Vec::new()),
                    };

                    let expected_type = Type::Function {
                        function_info: Arc::new(function_info),
                        generics: Arc::new(Vec::new()),
                    };

                    if type_environment
                        .unify_type(
                            &expected_type,
                            &init_expression.get_span(),
                            &context.module_name,
                            &init_expression_type.value,
                            &init_expression_type.span,
                            &context.module_name,
                            current_scope_this_type,
                            false,
                            true,
                        )
                        .is_err()
                    {
                        let error = SimpleError::new(
                            INVALID_ARRAY_INIT_EXPR_TYPE,
                            init_expression.get_span(),
                            vec![
                                ("function<T>(uint64) -> T".to_string(), Color::Cyan),
                                (
                                    type_environment
                                        .get_type_display_string(&init_expression_type.value),
                                    Color::Red,
                                ),
                            ],
                            vec![
                                (
                                    (
                                        context.module_name.clone(),
                                        new_array_init_expression.for_keyword_span.clone().unwrap(),
                                    ),
                                    Color::Yellow,
                                ),
                                (
                                    (context.module_name.clone(), init_expression.get_span()),
                                    Color::Red,
                                ),
                            ],
                        );
                        errors.push(TranspileError::new(error));
                    }

                    return_type
                } else {
                    init_expression_type.value
                }
            } else {
                Type::Unknown
            };

            if let Ok(length_expression) = new_array_init_expression.length_expression {
                infer_type_expression(
                    length_expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    force_be_expression,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                let length_expression_type =
                    type_environment.resolve_entity_type(EntityID::from(length_expression));

                if type_environment
                    .unify_type(
                        &length_expression_type.value,
                        &length_expression_type.span,
                        &context.module_name,
                        &Type::Uint64,
                        &length_expression.get_span(),
                        &context.module_name,
                        current_scope_this_type,
                        false,
                        true,
                    )
                    .is_err()
                {
                    let error = SimpleError::new(
                        INVALID_ARRAY_LENGTH_EXPR_TYPE,
                        length_expression.get_span(),
                        vec![
                            (
                                type_environment.get_type_display_string(&Type::Uint64),
                                Color::Cyan,
                            ),
                            (
                                type_environment
                                    .get_type_display_string(&length_expression_type.value),
                                Color::Red,
                            ),
                        ],
                        vec![(
                            (context.module_name.clone(), length_expression.get_span()),
                            Color::Red,
                        )],
                    );
                    errors.push(TranspileError::new(error));
                }
            }

            type_environment.set_entity_type(
                EntityID::from(&ast.first_expr),
                WithDefineInfo {
                    value: Type::Array(Arc::new(base_type)),
                    module_name: context.module_name.clone(),
                    span: new_array_init_expression.span.clone(),
                },
            );
        }
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {
            let mut expressions = Vec::new_in(allocator);
            for value_expression in new_array_expression.value_expressions.iter() {
                if let Ok(value_expression) = value_expression {
                    expressions.push(Either::Right(*value_expression));
                }
            }

            let base_type_generic = type_environment.new_local_generic_id(
                new_array_expression.span.clone(),
                context.module_name.clone(),
            );

            let ty = Type::Array(Arc::new(Type::LocalGeneric(base_type_generic)));

            type_environment.set_entity_type(
                EntityID::from(&ast.first_expr),
                WithDefineInfo {
                    value: ty,
                    module_name: context.module_name.clone(),
                    span: new_array_expression.span.clone(),
                },
            );

            infer_type_blocks_or_expressions(
                expressions,
                Spanned::new(
                    EntityID::from(new_array_expression),
                    new_array_expression.span.clone(),
                ),
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                type_inference_results,
                allocator,
                errors,
                warnings,
                context,
            );

            let base_type =
                type_environment.resolve_entity_type(EntityID::from(new_array_expression));

            type_environment.set_generic_type(base_type_generic, base_type);
        }
        PrimaryLeftExpr::NewExpression(new_expression) => {
            let user_type = if new_expression.path.len() > 1 {
                let module_name = get_module_name_from_new_expression(
                    new_expression,
                    import_element_map,
                    name_resolved_map,
                );

                if let Some(user_type_map) = module_user_type_map.get(&module_name) {
                    if let Some(user_type) =
                        user_type_map.get(new_expression.path.last().unwrap().value)
                    {
                        user_type.clone()
                    } else {
                        Type::Unknown
                    }
                } else {
                    Type::Unknown
                }
            } else {
                if let Some(name) = new_expression.path.last() {
                    if let Some(resolved) = name_resolved_map.get(&EntityID::from(name)) {
                        match resolved.define_info.define_kind {
                            DefineKind::Import => {
                                if let Some(module_name) =
                                    import_element_map.get(&resolved.define_info.entity_id)
                                {
                                    if let Some(user_type_map) =
                                        module_user_type_map.get(&module_name.value)
                                    {
                                        if let Some(user_type) = user_type_map.get(name.value) {
                                            user_type.clone()
                                        } else {
                                            Type::Unknown
                                        }
                                    } else {
                                        Type::Unknown
                                    }
                                } else {
                                    Type::Unknown
                                }
                            }
                            DefineKind::UserType => user_type_map.get(name.value).unwrap().clone(),
                            DefineKind::Generics => Type::Generic(
                                generics_map
                                    .get(&resolved.define_info.entity_id)
                                    .unwrap()
                                    .clone(),
                            ),
                            _ => Type::Unknown,
                        }
                    } else {
                        Type::Unknown
                    }
                } else {
                    Type::Unknown
                }
            };

            let user_type_span = if new_expression.path.is_empty() {
                new_expression.span.clone()
            } else {
                let span_start = new_expression.path.first().unwrap().span.start;
                let span_end = new_expression.path.last().unwrap().span.end;
                span_start..span_end
            };

            let mut user_type = type_environment.get_user_or_function_type_with_local_generic_id(
                WithDefineInfo {
                    value: user_type,
                    module_name: context.module_name.clone(),
                    span: user_type_span,
                },
                current_scope_implements_info_set,
                true,
            );

            if user_type.value.is_renamed_type() {
                user_type.value = user_type
                    .value
                    .get_element_type_with_replaced_generic("type")
                    .unwrap()
                    .value;
            }

            if let Type::UserType {
                user_type_info: _,
                generics: _,
                generics_span: _,
            } = &user_type.value
            {
                if let Ok(field_assigns) = &new_expression.field_assigns {
                    for field_assign in field_assigns.iter() {
                        if let Some(element_type) = user_type
                            .value
                            .get_element_type_with_replaced_generic(field_assign.name.value)
                        {
                            type_environment.set_entity_type(
                                EntityID::from(field_assign),
                                WithDefineInfo {
                                    value: element_type.value,
                                    module_name: context.module_name.clone(),
                                    span: field_assign.name.span.clone(),
                                },
                            );

                            if let Ok(expression) = &field_assign.expression {
                                infer_type_expression(
                                    &expression,
                                    user_type_map,
                                    import_element_map,
                                    name_resolved_map,
                                    module_user_type_map,
                                    module_element_type_map,
                                    module_element_type_maps,
                                    generics_map,
                                    module_entity_type_map,
                                    global_implements_info_set,
                                    current_scope_this_type,
                                    current_scope_implements_info_set,
                                    true,
                                    type_environment,
                                    type_inference_results,
                                    allocator,
                                    errors,
                                    warnings,
                                    context,
                                );

                                let result = type_environment.unify_with_implicit_convert(
                                    Spanned::new(
                                        EntityID::from(field_assign),
                                        field_assign.name.span.clone(),
                                    ),
                                    Spanned::new(
                                        EntityID::from(*expression),
                                        expression.get_span(),
                                    ),
                                    false,
                                    current_scope_this_type,
                                );
                                add_error(result, type_environment);
                            }
                        } else {
                            let error = NotFoundTypeElementError {
                                user_type: user_type.value.clone(),
                                name: field_assign.name.clone().map(|name| name.to_string()),
                            };
                            type_environment.add_lazy_type_error_report(error);
                        }
                    }
                }

                type_environment.set_entity_type(
                    EntityID::from(&ast.first_expr),
                    WithDefineInfo {
                        value: user_type.value,
                        module_name: context.module_name.clone(),
                        span: new_expression.span.clone(),
                    },
                );
            } else {
                if !new_expression.path.is_empty() {
                    let span_start = new_expression.path.first().unwrap().span.start;
                    let span_end = new_expression.path.last().unwrap().span.end;
                    let span = span_start..span_end;

                    let error = SimpleError::new(
                        0036,
                        span.clone(),
                        vec![],
                        vec![((context.module_name.clone(), span), Color::Red)],
                    );
                    errors.push(error);
                }

                type_environment.set_entity_type(
                    EntityID::from(&ast.first_expr),
                    WithDefineInfo {
                        value: Type::Unknown,
                        module_name: context.module_name.clone(),
                        span: new_expression.span.clone(),
                    },
                );
            }
        }
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let mut blocks = Vec::new_in(allocator);
            let mut has_else = false;

            let first_statement = &if_expression.if_statement;
            if let Ok(condition) = &first_statement.condition {
                infer_type_expression(
                    &condition,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    force_be_expression,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                let condition_type =
                    type_environment.resolve_entity_type(EntityID::from(*condition));

                let result = type_environment.unify_type(
                    &Type::Bool,
                    &condition.get_span(),
                    &context.module_name,
                    &condition_type.value,
                    &condition_type.span,
                    &context.module_name,
                    current_scope_this_type,
                    false,
                    true,
                );
                if result.is_err() {
                    type_environment.add_lazy_type_error_report(SimpleTypeError {
                        error_code: IF_CONDITION_TYPE_ERROR,
                        ty: condition_type.to_spanned(),
                    });
                }
            }
            if let Some(block) = &first_statement.block.value {
                blocks.push(Either::Left(block));
            }

            for else_if_or_block in if_expression.chain.iter() {
                if let Some(else_if_or_block) = &else_if_or_block.else_if_or_else.value {
                    match else_if_or_block {
                        Either::Left(if_statement) => {
                            if let Ok(condition) = &if_statement.condition {
                                infer_type_expression(
                                    &condition,
                                    user_type_map,
                                    import_element_map,
                                    name_resolved_map,
                                    module_user_type_map,
                                    module_element_type_map,
                                    module_element_type_maps,
                                    generics_map,
                                    module_entity_type_map,
                                    global_implements_info_set,
                                    current_scope_this_type,
                                    current_scope_implements_info_set,
                                    force_be_expression,
                                    type_environment,
                                    type_inference_results,
                                    allocator,
                                    errors,
                                    warnings,
                                    context,
                                );

                                let condition_type = type_environment
                                    .resolve_entity_type(EntityID::from(*condition));

                                let result = type_environment.unify_type(
                                    &Type::Bool,
                                    &condition.get_span(),
                                    &context.module_name,
                                    &condition_type.value,
                                    &condition_type.span,
                                    &context.module_name,
                                    current_scope_this_type,
                                    false,
                                    true,
                                );
                                if result.is_err() {
                                    type_environment.add_lazy_type_error_report(SimpleTypeError {
                                        error_code: IF_CONDITION_TYPE_ERROR,
                                        ty: condition_type.to_spanned(),
                                    });
                                }
                            }
                            if let Some(block) = &if_statement.block.value {
                                blocks.push(Either::Left(block));
                            }
                        }
                        Either::Right(block) => {
                            has_else = true;
                            blocks.push(Either::Left(block));
                        }
                    }
                }
            }

            if force_be_expression && !has_else {
                let error = SimpleError::new(
                    0063,
                    if_expression.span.clone(),
                    vec![],
                    vec![(
                        (context.module_name.clone(), if_expression.span.clone()),
                        Color::Red,
                    )],
                );
                errors.push(TranspileError::new(error));
            }

            infer_type_blocks_or_expressions(
                blocks,
                Spanned::new(EntityID::from(&ast.first_expr), if_expression.span.clone()),
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                type_inference_results,
                allocator,
                errors,
                warnings,
                context,
            );
        }
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                infer_type_program(
                    block.program,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    false,
                    false,
                    &Vec::new(),
                    force_be_expression,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        infer_type_mapping_operator(
            mapping_operator,
            Spanned::new(EntityID::from(&ast.first_expr), ast.first_expr.get_span()),
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            global_implements_info_set,
            current_scope_this_type,
            current_scope_implements_info_set,
            type_environment,
            type_inference_results,
            mappings,
            allocator,
            errors,
            warnings,
            context,
        );

        type_environment
            .set_entity_id_equals(EntityID::from(&mapping_operator.value), EntityID::from(ast));
    } else {
        type_environment.set_entity_id_equals(EntityID::from(&ast.first_expr), EntityID::from(ast));
    }
}

fn infer_type_blocks_or_expressions<'input, 'allocator>(
    blocks_or_expressions: Vec<
        Either<&'allocator Block<'input, 'allocator>, Expression<'input, 'allocator>>,
        &'allocator Bump,
    >,
    parent_ast_entity_id: Spanned<EntityID>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    let mut first_type = match blocks_or_expressions.first() {
        Some(block_or_expression) => match block_or_expression {
            Either::Left(block) => {
                infer_type_program(
                    block.program,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    false,
                    false,
                    &Vec::new(),
                    force_be_expression,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                type_environment.resolve_entity_type(EntityID::from(block.program))
            }
            Either::Right(expression) => {
                infer_type_expression(
                    expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    force_be_expression,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                type_environment.resolve_entity_type(EntityID::from(*expression))
            }
        },
        _ => {
            type_environment.set_entity_type(
                parent_ast_entity_id.value,
                WithDefineInfo {
                    value: Type::Unknown,
                    module_name: context.module_name.clone(),
                    span: parent_ast_entity_id.span,
                },
            );
            return;
        }
    };

    type_environment.set_entity_type(parent_ast_entity_id.value, first_type.clone());

    let mut has_implicit_convert = false;

    for i in 1..blocks_or_expressions.len() {
        let block_or_expression = blocks_or_expressions[i];

        let block_or_expression_entity_id = match block_or_expression {
            Either::Left(block) => {
                infer_type_program(
                    block.program,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    false,
                    false,
                    &Vec::new(),
                    force_be_expression,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                Spanned::new(EntityID::from(block.program), block.span.clone())
            }
            Either::Right(expression) => {
                infer_type_expression(
                    expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    force_be_expression,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                Spanned::new(EntityID::from(expression), expression.get_span())
            }
        };

        if !force_be_expression {
            continue;
        }

        let result = type_environment.unify_with_implicit_convert(
            Spanned::new(parent_ast_entity_id.value, first_type.span.clone()),
            block_or_expression_entity_id.clone(),
            false,
            current_scope_this_type,
        );

        let second_type = type_environment.resolve_entity_type(block_or_expression_entity_id.value);

        let second_resolved_type = if let Type::LocalGeneric(generic_id) = &second_type.value {
            type_environment.resolve_generic_type(*generic_id).1.value
        } else {
            second_type.value.clone()
        };

        if let Err(error) = result {
            if error.generics.is_empty() && !has_implicit_convert {
                if let Type::Unit = &second_resolved_type {
                    // TODO - replace with std error type
                    first_type.value = Type::Result {
                        value: Arc::new(first_type.value.clone()),
                        error: Arc::new(Type::Unit),
                    };
                    for block_or_expression in blocks_or_expressions[0..=i].iter() {
                        let entity_id = match block_or_expression {
                            Either::Left(block) => EntityID::from(block.program),
                            Either::Right(expression) => EntityID::from(*expression),
                        };

                        type_inference_results
                            .implicit_convert_map
                            .insert(entity_id, ImplicitConvertKind::Ok);
                    }
                    has_implicit_convert = true;
                } else if second_type.value.is_option_or_result() {
                    let result = if let Type::Option(value_type) = &second_resolved_type {
                        type_environment.unify_type(
                            &first_type.value,
                            &first_type.span,
                            &first_type.module_name,
                            &value_type,
                            &second_type.span,
                            &second_type.module_name,
                            current_scope_this_type,
                            false,
                            true,
                        )
                    } else if let Type::Result { value, error: _ } = &second_resolved_type {
                        type_environment.unify_type(
                            &first_type.value,
                            &first_type.span,
                            &first_type.module_name,
                            &value,
                            &second_type.span,
                            &second_type.module_name,
                            current_scope_this_type,
                            false,
                            true,
                        )
                    } else {
                        unreachable!()
                    };

                    if let Err(_) = result {
                        type_environment.add_lazy_type_error_report(error);
                    } else {
                        first_type.value = if let Type::Option(_) = &second_resolved_type {
                            Type::Option(Arc::new(first_type.value))
                        } else if let Type::Result { value: _, error } = &second_resolved_type {
                            Type::Result {
                                value: Arc::new(first_type.value),
                                error: error.clone(),
                            }
                        } else {
                            unreachable!()
                        };

                        let convert_kind = if let Type::Option(_) = &first_type.value {
                            ImplicitConvertKind::Some
                        } else {
                            ImplicitConvertKind::Ok
                        };
                        for block_or_expression in blocks_or_expressions[0..=i].iter() {
                            let entity_id = match block_or_expression {
                                Either::Left(block) => EntityID::from(block.program),
                                Either::Right(expression) => EntityID::from(*expression),
                            };

                            type_inference_results
                                .implicit_convert_map
                                .insert(entity_id, convert_kind);
                        }
                        has_implicit_convert = true;
                    }
                } else {
                    type_environment.add_lazy_type_error_report(error);
                }
            } else {
                type_environment.add_lazy_type_error_report(error);
            }
        }
    }

    type_environment.set_entity_type(parent_ast_entity_id.value, first_type.clone());
}

fn infer_type_primary_right<'input, 'allocator>(
    ast: &PrimaryRight<'input, 'allocator>,
    previous_primary: Spanned<EntityID>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    mappings: &mut Vec<MappingTypeKind, &'allocator Bump>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    let second_expr_id = if let Some(second_expr) = &ast.second_expr {
        let parent_type = type_environment.resolve_entity_type(previous_primary.value);
        let literal_entity_id = EntityID::from(&second_expr.0);

        let element_type = get_element_type(
            WithDefineInfo {
                value: parent_type.value.clone(),
                module_name: context.module_name.clone(),
                span: previous_primary.span.clone(),
            },
            second_expr.0.clone(),
            |_| true,
            global_implements_info_set,
            current_scope_this_type,
            current_scope_implements_info_set,
            type_environment,
            allocator,
        );

        let element_type = match element_type {
            Ok((ty, is_static)) => {
                let span = ast.separator.span.start..second_expr.0.span.end;

                if is_static {
                    if ast.separator.value != PrimarySeparatorKind::DoubleColon {
                        let error = SimpleError::new(
                            INVALID_PRIMARY_SEPARATOR,
                            span.clone(),
                            vec![
                                (".".to_string(), Color::Red),
                                ("::".to_string(), Color::Cyan),
                                ("static".to_string(), Color::Yellow),
                            ],
                            vec![
                                ((context.module_name.clone(), span), Color::Red),
                                ((ty.module_name, ty.span), Color::Yellow),
                            ],
                        );
                        errors.push(error);
                    }
                } else {
                    if ast.separator.value != PrimarySeparatorKind::Dot {
                        let error = SimpleError::new(
                            INVALID_PRIMARY_SEPARATOR,
                            span.clone(),
                            vec![
                                ("::".to_string(), Color::Red),
                                (".".to_string(), Color::Cyan),
                                ("non static".to_string(), Color::Yellow),
                            ],
                            vec![
                                ((context.module_name.clone(), span), Color::Red),
                                ((ty.module_name, ty.span), Color::Yellow),
                            ],
                        );
                        errors.push(error);
                    }
                }

                ty.value
            }
            Err(error) => {
                match error {
                    Either::Left(error) => {
                        type_environment.add_lazy_type_error_report(error);
                    }
                    Either::Right(error) => {
                        errors.push(TranspileError::new(error));
                    }
                }

                Type::Unknown
            }
        };

        let element_type = WithDefineInfo {
            value: element_type,
            module_name: context.module_name.clone(),
            span: second_expr.0.span.clone(),
        };

        type_environment.set_entity_type(literal_entity_id, element_type);

        if let Some(generics) = &second_expr.1 {
            if let Ok(generics) = generics {
                infer_type_generics(
                    generics,
                    literal_entity_id,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    type_environment,
                    errors,
                    warnings,
                    context,
                );
            }
        }

        let last_entity_id = if let Some(function_call) = &second_expr.2 {
            infer_type_function_call(
                function_call,
                literal_entity_id,
                ast.separator.value == PrimarySeparatorKind::Dot,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                type_environment,
                type_inference_results,
                allocator,
                errors,
                warnings,
                context,
            );

            let span_start = second_expr.0.span.start;
            let span_end = function_call.span.end;

            Spanned::new(EntityID::from(function_call), span_start..span_end)
        } else {
            Spanned::new(literal_entity_id, second_expr.0.span.clone())
        };

        let mut ty = type_environment.resolve_entity_type(last_entity_id.value);
        for mapping in mappings.iter().rev() {
            ty.value = match mapping {
                MappingTypeKind::Option => Type::Option(Arc::new(ty.value)),
                MappingTypeKind::Result { error_type } => Type::Result {
                    value: Arc::new(ty.value),
                    error: Arc::new(error_type.clone()),
                },
            };
        }
        mappings.clear();
        type_environment.set_entity_type(last_entity_id.value, ty);

        last_entity_id
    } else {
        previous_primary
    };

    if let Some(mapping_operator) = &ast.mapping_operator {
        infer_type_mapping_operator(
            mapping_operator,
            second_expr_id,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            global_implements_info_set,
            current_scope_this_type,
            current_scope_implements_info_set,
            type_environment,
            type_inference_results,
            mappings,
            allocator,
            errors,
            warnings,
            context,
        );

        type_environment
            .set_entity_id_equals(EntityID::from(&mapping_operator.value), EntityID::from(ast));
    } else {
        type_environment.set_entity_id_equals(second_expr_id.value, EntityID::from(ast));
    }
}

fn get_element_type<'allocator, 'input, F: Fn(&Type) -> bool>(
    parent_type: WithDefineInfo<Type>,
    element_name: Spanned<&str>,
    origin_type_filter: F,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    allocator: &'allocator Bump,
) -> Result<(WithDefineInfo<Type>, bool), Either<NotFoundTypeElementError, DuplicatedElement>> {
    type_environment.fix_numeric_literal_type(&parent_type.value);

    let original_parent_type = parent_type.clone();

    let parent_type = match parent_type.value {
        Type::LocalGeneric(generic_id) => type_environment.resolve_generic_type(generic_id).1,
        Type::This => WithDefineInfo {
            value: current_scope_this_type.ty.clone(),
            module_name: parent_type.module_name.clone(),
            span: parent_type.span.clone(),
        },
        _ => parent_type,
    };

    let element_type = parent_type
        .value
        .get_element_type_with_replaced_generic(element_name.value);

    let mut pre_element_types = Vec::new_in(allocator);
    if let Some(element_type) = element_type {
        pre_element_types.push((element_type, parent_type.value.clone(), false, None));
    } else if parent_type.value.is_interface() {
        let mut super_types = Vec::new_in(allocator);
        super_types.push(parent_type.value.clone());

        loop {
            let super_type = match super_types.pop() {
                Some(ty) => ty,
                _ => break,
            };

            if let Some(element_type) =
                super_type.get_element_type_with_replaced_generic(element_name.value)
            {
                pre_element_types.push((element_type, super_type.clone(), true, None));
            }

            super_types.extend(super_type.get_super_type_with_replaced_generics());
        }
    }

    if pre_element_types.is_empty() {
        let mut implementations = Vec::new_in(allocator);

        if let Type::Generic(generic_define) = &parent_type.value {
            for bound in generic_define.bounds.freeze_and_get().iter() {
                if let Some(element_type) = bound
                    .ty
                    .get_element_type_with_replaced_generic(element_name.value)
                {
                    let element_type =
                        element_type.map(|ty| ty.replace_this_type(&parent_type.value, true, true));
                    pre_element_types.push((element_type, bound.ty.clone(), true, None));
                } else {
                    let mut super_types = Vec::new_in(allocator);
                    super_types.push(bound.ty.clone());

                    loop {
                        let super_type = match super_types.pop() {
                            Some(ty) => ty,
                            _ => break,
                        };

                        if let Some(element_type) =
                            super_type.get_element_type_with_replaced_generic(element_name.value)
                        {
                            let element_type = element_type
                                .map(|ty| ty.replace_this_type(&parent_type.value, true, true));
                            pre_element_types.push((element_type, super_type.clone(), true, None));
                        }

                        super_types.extend(super_type.get_super_type_with_replaced_generics());
                    }
                }
            }
        }

        implementations.extend(
            global_implements_info_set.collect_satisfied_implementations(
                &parent_type.value,
                type_environment,
                current_scope_implements_info_set,
                allocator,
            ),
        );

        for implementation in implementations {
            let element_type = implementation
                .implements_info
                .get_element_type(element_name.value);
            if let Some(element_type) = element_type {
                if !implementation.errors.is_empty() {
                    let error = LazyGenericTypeInferError::new(
                        original_parent_type.clone(),
                        implementation.errors,
                    );
                    type_environment.add_lazy_type_error_report(error);
                }

                let ty = Type::get_type_with_replaced_generics(
                    &element_type.value,
                    &implementation.implements_info.generics,
                    &implementation.local_generics,
                );

                let interface = implementation.implements_info.interface;

                pre_element_types.push((
                    WithDefineInfo {
                        value: ty,
                        module_name: element_type.module_name.clone(),
                        span: element_type.span.clone(),
                    },
                    interface.value.clone(),
                    implementation.implements_info.is_bounds_info,
                    Some((
                        WithDefineInfo {
                            value: interface.value,
                            module_name: implementation.implements_info.module_name,
                            span: interface.span,
                        },
                        implementation.implements_info.concrete.value,
                        implementation.implements_info.where_bounds,
                    )),
                ));
            }
        }
    }

    let pre_element_types_from_bounds = pre_element_types
        .iter()
        .filter(|(_, _, is_bound_info, _)| *is_bound_info)
        .collect_in::<bumpalo::collections::Vec<_>>(allocator);

    let pre_element_types_from_global = pre_element_types
        .iter()
        .filter(|(_, _, is_bound_info, _)| !*is_bound_info)
        .collect_in::<bumpalo::collections::Vec<_>>(allocator);

    let mut element_types = Vec::new_in(allocator);
    for i in 0..2 {
        let pre_element_types = match i {
            0 => &pre_element_types_from_bounds,
            1 => &pre_element_types_from_global,
            _ => unreachable!(),
        };

        let mut origin_types = Vec::new_in(allocator);

        'root: for (element_type, origin_type, _, interface_and_where_bounds) in pre_element_types {
            let ty = type_environment
                .get_user_or_function_type_with_local_generic_id(
                    WithDefineInfo {
                        value: element_type.value.clone(),
                        module_name: element_type.module_name.clone(),
                        span: element_name.span.clone(),
                    },
                    current_scope_implements_info_set,
                    false,
                )
                .value;

            if let Some(where_bounds) = ty.get_where_bounds_with_replaced_generic() {
                for where_bound in where_bounds.iter() {
                    for bound in where_bound.bounds.iter() {
                        if !global_implements_info_set.is_implemented(
                            &where_bound.target_type.value,
                            &bound.ty,
                            type_environment,
                            current_scope_implements_info_set,
                            true,
                            false,
                        ) {
                            continue 'root;
                        }
                    }
                }
            }

            if origin_types.contains(origin_type) {
                continue;
            }
            origin_types.push(origin_type.clone());

            if !origin_type_filter(origin_type) {
                continue;
            }

            element_types.push((
                WithDefineInfo {
                    value: ty,
                    module_name: element_type.module_name.clone(),
                    span: element_type.span.clone(),
                },
                origin_type,
                interface_and_where_bounds,
            ));
        }

        if !element_types.is_empty() {
            break;
        }
    }

    let (element_type, is_static) = if element_types.is_empty() {
        let error = NotFoundTypeElementError {
            user_type: parent_type.value.clone(),
            name: element_name.clone().map(|name| name.to_string()),
        };
        return Err(Either::Left(error));
    } else {
        if element_types.len() > 1 {
            let found_elements = element_types
                .iter()
                .map(|element| element.0.clone().map(|_| ()))
                .collect();

            let error = DuplicatedElement {
                type_name: type_environment.get_type_display_string(&parent_type.value),
                element: element_name.clone().map(|name| name.to_string()),
                count: element_types.len(),
                found_elements,
            };
            return Err(Either::Right(error));
        } else {
            let where_bounds = element_types
                .last()
                .unwrap()
                .0
                .value
                .get_where_bounds_with_replaced_generic();

            if let Some(where_bounds) = where_bounds {
                for where_bound in where_bounds.iter() {
                    type_environment
                        .generic_bounds_checks
                        .push(GenericsBoundCheck::Where {
                            type_span: element_name.span.clone(),
                            target_type: where_bound.target_type.value.clone(),
                            bounds: where_bound.bounds.clone(),
                            scope_implements_info_set: current_scope_implements_info_set.clone(),
                        });
                }
            }

            if let Some((interface, _, where_bounds)) = &element_types.last().unwrap().2 {
                type_environment
                    .impl_interface_generics_check
                    .push((element_name.span.clone(), interface.clone()));

                for where_bound in where_bounds.iter() {
                    type_environment
                        .generic_bounds_checks
                        .push(GenericsBoundCheck::Where {
                            type_span: element_name.span.clone(),
                            target_type: where_bound.target_type.value.clone(),
                            bounds: where_bound.bounds.clone(),
                            scope_implements_info_set: current_scope_implements_info_set.clone(),
                        });
                }
            }
        }

        let is_static = match element_types.last().unwrap().1 {
            Type::UserType {
                user_type_info,
                generics: _,
                generics_span: _,
            } => {
                let element_attributes = user_type_info.element_attributes.lock().unwrap();
                let attributes = element_attributes.get(element_name.value).unwrap();
                attributes.contains_kind(StatementAttributeKind::Static)
            }
            _ => unreachable!(),
        };

        (element_types.last().unwrap().0.clone(), is_static)
    };

    Ok((element_type, is_static))
}

fn infer_type_mapping_operator<'input, 'allocator>(
    ast: &MappingOperator<'input, 'allocator>,
    previous_entity_id: Spanned<EntityID>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    mappings: &mut Vec<MappingTypeKind, &'allocator Bump>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    let block = match &ast.value {
        MappingOperatorKind::NullElvisBlock(block) => block.value.as_ref(),
        MappingOperatorKind::ResultElvisBlock(block) => block.value.as_ref(),
        _ => None,
    };

    let previous_type = type_environment.resolve_entity_type(previous_entity_id.value);
    let previous_type_resolved = if let Type::LocalGeneric(generic_id) = &previous_type.value {
        type_environment.resolve_generic_type(*generic_id).1.value
    } else {
        previous_type.value.clone()
    };

    let check_result = match &ast.value {
        MappingOperatorKind::NullPropagation
        | MappingOperatorKind::NullUnwrap
        | MappingOperatorKind::NullElvisBlock(_) => {
            if let Type::Option(value_type) = &previous_type_resolved {
                Ok(value_type.as_ref().clone())
            } else {
                Err(ExpectedTypeKind::Option)
            }
        }
        MappingOperatorKind::ResultPropagation
        | MappingOperatorKind::ResultUnwrap
        | MappingOperatorKind::ResultElvisBlock(_) => {
            if let Type::Result { value, error: _ } = &previous_type_resolved {
                Ok(value.as_ref().clone())
            } else {
                Err(ExpectedTypeKind::Result)
            }
        }
    };

    if let Type::Option(_) = &previous_type_resolved {
        if let MappingOperatorKind::NullPropagation = &ast.value {
            mappings.push(MappingTypeKind::Option);
        }
    }
    if let Type::Result { value: _, error } = &previous_type_resolved {
        if let MappingOperatorKind::ResultPropagation = &ast.value {
            mappings.push(MappingTypeKind::Result {
                error_type: error.as_ref().clone(),
            });
        }
    }

    if let Err(expected) = &check_result {
        let error = UnexpectedTypeError {
            error_code: MAPPING_OPERATOR_TYPE_ERROR,
            found_type: Spanned::new(previous_type.value.clone(), previous_entity_id.span.clone()),
            expected_kind: Spanned::new(*expected, ast.span.clone()),
        };
        type_environment.add_lazy_type_error_report(error);
    }

    if let Some(block) = &block {
        infer_type_program(
            block.program,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            global_implements_info_set,
            current_scope_this_type,
            current_scope_implements_info_set,
            false,
            false,
            &Vec::new(),
            true,
            type_environment,
            type_inference_results,
            allocator,
            errors,
            warnings,
            context,
        );

        if let Ok(value_type) = check_result {
            let span = previous_entity_id.span.start..ast.span.end;
            type_environment.set_entity_type(
                EntityID::from(&ast.value),
                WithDefineInfo {
                    value: value_type,
                    module_name: context.module_name.clone(),
                    span: span.clone(),
                },
            );

            let result = type_environment.unify_with_implicit_convert(
                Spanned::new(EntityID::from(&ast.value), span),
                Spanned::new(EntityID::from(block.program), block.program.span.clone()),
                false,
                current_scope_this_type,
            );

            add_error(result, type_environment);
        } else {
            type_environment.set_entity_type(
                EntityID::from(&ast.value),
                WithDefineInfo {
                    value: Type::Unknown,
                    module_name: context.module_name.clone(),
                    span: ast.span.clone(),
                },
            );
        }
    } else {
        let span = previous_entity_id.span.start..ast.span.end;

        if let Ok(value_type) = check_result {
            type_environment.set_entity_type(
                EntityID::from(&ast.value),
                WithDefineInfo {
                    value: value_type,
                    module_name: context.module_name.clone(),
                    span,
                },
            );
        } else {
            type_environment.set_entity_type(
                EntityID::from(&ast.value),
                WithDefineInfo {
                    value: previous_type.value,
                    module_name: context.module_name.clone(),
                    span,
                },
            );
        }
    }
}

fn infer_type_generics<'input, 'allocator>(
    ast: &Generics,
    previous: EntityID,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    let ty = type_environment.resolve_entity_type(previous);
    let generics = match &ty.value {
        Type::UserType {
            user_type_info: _,
            generics,
            generics_span: _,
        } => generics,
        Type::Function {
            function_info: _,
            generics,
        } => generics,
        _ => {
            let error = InvalidSetGenericsTypeError {
                ty: ty.to_spanned(),
                set_span: ast.span.clone(),
            };
            type_environment.add_lazy_type_error_report(error);
            return;
        }
    };

    if generics.len() != ast.elements.len() {
        let error = NumberOfGenericsMismatchError {
            set_span: ast.span.clone(),
            expected: generics.len(),
            found: ast.elements.len(),
            ty: ty.to_spanned().map(|ty| ty.as_original_type()),
        };
        type_environment.add_lazy_type_error_report(error);
        return;
    }

    for i in 0..generics.len() {
        let generic = &generics[i];
        if let Type::LocalGeneric(generic_id) = generic {
            let type_info = &ast.elements[i];
            let ty = get_type(
                type_info,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                current_scope_this_type,
                errors,
                warnings,
                context,
            );

            let ty = WithDefineInfo {
                value: ty.clone(),
                module_name: context.module_name.clone(),
                span: type_info.get_span(),
            };

            type_environment.set_generic_type(*generic_id, ty.clone());

            type_environment.add_check_type_info_bounds(
                ty.clone().to_spanned(),
                &ty.value,
                true,
                current_scope_implements_info_set,
                context,
            );
        }
    }
}

fn infer_type_function_call<'input, 'allocator>(
    ast: &FunctionCall<'input, 'allocator>,
    function: EntityID,
    is_method_call: bool,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    type_environment: &mut TypeEnvironment<'input, 'allocator>,
    type_inference_results: &mut TypeInferenceResultContainer,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext,
) {
    let function_type = type_environment.resolve_entity_type(function);

    if let Type::Function {
        function_info,
        generics: _,
    } = &function_type.value
    {
        let is_method_call = is_method_call && function_info.is_extension;

        let function_type = function_type.value.clone();

        if let Ok(arg_exprs) = &ast.arg_exprs {
            let is_method_call = is_method_call && function_info.is_extension;

            let number_of_args = if is_method_call {
                (function_info.argument_types.len() - 1, arg_exprs.len())
            } else {
                (function_info.argument_types.len(), arg_exprs.len())
            };

            if number_of_args.0 != number_of_args.1 {
                let error = TranspileError::new(ArgumentCountMismatchError {
                    defined_count: number_of_args.0,
                    specified_count: number_of_args.1,
                    defined_module: function_info.define_info.module_name.clone(),
                    defined_span: function_info.define_info.arguments_span.clone(),
                    specified_span: ast.span.clone(),
                });
                errors.push(error);
            }

            let mut define_arg_index = if is_method_call { 1 } else { 0 };

            for arg_expr in arg_exprs.iter() {
                infer_type_expression(
                    &arg_expr,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    true,
                    type_environment,
                    type_inference_results,
                    allocator,
                    errors,
                    warnings,
                    context,
                );

                if let Some(defined_arg_type) =
                    function_type.get_indexed_element_type_with_replaced_generic(define_arg_index)
                {
                    type_environment.set_entity_type(
                        EntityID::from(ast),
                        WithDefineInfo {
                            value: defined_arg_type,
                            module_name: context.module_name.clone(),
                            span: arg_expr.get_span(),
                        },
                    );

                    let result = type_environment.unify_with_implicit_convert(
                        Spanned::new(EntityID::from(*arg_expr), arg_expr.get_span()),
                        Spanned::new(EntityID::from(ast), arg_expr.get_span()),
                        true,
                        current_scope_this_type,
                    );

                    if let Err(error) = result {
                        type_environment.add_lazy_type_error_report(error);
                    }
                }

                define_arg_index += 1;
            }
        } else {
            let number_of_args = if is_method_call {
                function_info.argument_types.len() - 1
            } else {
                function_info.argument_types.len()
            };

            if number_of_args != 0 {
                let error = TranspileError::new(ArgumentCountMismatchError {
                    defined_count: number_of_args,
                    specified_count: 0,
                    defined_module: function_info.define_info.module_name.clone(),
                    defined_span: function_info.define_info.arguments_span.clone(),
                    specified_span: ast.span.clone(),
                });
                errors.push(error);
            }
        }

        let return_type = function_type
            .get_return_type_with_replaced_generic()
            .unwrap();

        type_environment.set_entity_type(
            EntityID::from(ast),
            WithDefineInfo {
                value: return_type,
                module_name: context.module_name.clone(),
                span: ast.span.clone(),
            },
        );
    } else {
        type_environment.add_lazy_type_error_report(SimpleTypeError {
            error_code: FUNCTION_CALL_TYPE_ERROR,
            ty: function_type.to_spanned(),
        });

        type_environment.set_entity_type(
            EntityID::from(ast),
            WithDefineInfo {
                value: Type::Unknown,
                module_name: context.module_name.clone(),
                span: ast.span.clone(),
            },
        );
    }
}

pub(crate) trait LazyTypeReport {
    fn build_report(
        &self,
        type_environment: &TypeEnvironment,
        context: &TranspileModuleContext,
    ) -> Either<TranspileError, TranspileWarning>;
}

#[derive(Debug)]
pub(crate) struct TypeMismatchError {
    type_0: WithDefineInfo<Type>,
    type_1: WithDefineInfo<Type>,
    pub generics: Vec<(WithDefineInfo<Type>, WithDefineInfo<Type>)>,
}

impl LazyTypeReport for TypeMismatchError {
    fn build_report(
        &self,
        type_environment: &TypeEnvironment,
        _: &TranspileModuleContext,
    ) -> Either<TranspileError, TranspileWarning> {
        let func = |ty| type_environment.get_type_display_string(&ty);

        let type_0 = self.type_0.clone().map(func);
        let type_1 = self.type_1.clone().map(func);
        let generics = self
            .generics
            .iter()
            .map(|generic| (generic.0.clone().map(func), generic.1.clone().map(func)))
            .collect();

        let error = TypeMismatchErrorReport {
            type_0,
            type_1,
            generics,
        };
        Either::Left(TranspileError::new(error))
    }
}

struct TypeMismatchErrorReport {
    type_0: WithDefineInfo<String>,
    type_1: WithDefineInfo<String>,
    generics: Vec<(WithDefineInfo<String>, WithDefineInfo<String>)>,
}

impl TranspileReport for TypeMismatchErrorReport {
    fn print(&self, context: &TranspileModuleContext) {
        let text = &context.context.localized_text;
        let error_code = 0034;
        let key = ErrorMessageKey::new(error_code);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(
            ReportKind::Error,
            context.module_name.as_ref().clone(),
            self.type_0.span.start,
        )
        .with_code(error_code)
        .with_message(message);

        if &self.type_0.span == &self.type_1.span
            && self.type_0.module_name.as_str() == self.type_1.module_name.as_str()
        {
            builder.add_label(
                Label::new((
                    self.type_0.module_name.as_ref().clone(),
                    self.type_0.span.clone(),
                ))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(2))
                        .replace(
                            "%expected",
                            self.type_0
                                .value
                                .clone()
                                .fg(Color::Red)
                                .to_string()
                                .as_str(),
                        )
                        .replace(
                            "%found",
                            self.type_1
                                .value
                                .clone()
                                .fg(Color::Yellow)
                                .to_string()
                                .as_str(),
                        ),
                ),
            );
        } else {
            builder.add_label(
                Label::new((
                    self.type_0.module_name.as_ref().clone(),
                    self.type_0.span.clone(),
                ))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0)).replace(
                        "%type",
                        self.type_0
                            .value
                            .clone()
                            .fg(Color::Red)
                            .to_string()
                            .as_str(),
                    ),
                ),
            );

            builder.add_label(
                Label::new((
                    self.type_1.module_name.as_ref().clone(),
                    self.type_1.span.clone(),
                ))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0)).replace(
                        "%type",
                        self.type_1
                            .value
                            .clone()
                            .fg(Color::Red)
                            .to_string()
                            .as_str(),
                    ),
                ),
            );
        }

        let mut color_generator = ColorGenerator::new();
        let mut generic_ids = String::new();
        for i in 0..self.generics.len() {
            let generic = &self.generics[i];
            let color = color_generator.next();

            let message_0 = key
                .get_massage(text, ErrorMessageType::Label(1))
                .replace(
                    "%generic",
                    format!("'{}", i.to_string()).fg(color).to_string().as_str(),
                )
                .replace(
                    "%type",
                    generic.0.value.clone().fg(color).to_string().as_str(),
                );

            builder.add_label(
                Label::new((
                    generic.0.module_name.as_ref().clone(),
                    generic.0.span.clone(),
                ))
                .with_color(color)
                .with_message(message_0),
            );

            let message_1 = key
                .get_massage(text, ErrorMessageType::Label(1))
                .replace(
                    "%generic",
                    format!("'{}", i.to_string()).fg(color).to_string().as_str(),
                )
                .replace(
                    "%type",
                    generic.1.value.clone().fg(color).to_string().as_str(),
                );

            builder.add_label(
                Label::new((
                    generic.1.module_name.as_ref().clone(),
                    generic.1.span.clone(),
                ))
                .with_color(color)
                .with_message(message_1),
            );

            if i != 0 {
                generic_ids += " ";
            }
            generic_ids += format!("'{}", i).fg(color).to_string().as_str();
        }

        if !self.generics.is_empty() {
            builder.set_note(
                key.get_massage(text, ErrorMessageType::Note)
                    .replace("%generics", &generic_ids),
            );
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        let mut module_names = vec![
            context.module_name.as_ref().clone(),
            self.type_0.module_name.as_ref().clone(),
            self.type_1.module_name.as_ref().clone(),
        ];

        for (ty_0, ty_1) in self.generics.iter() {
            module_names.push(ty_0.module_name.as_ref().clone());
            module_names.push(ty_1.module_name.as_ref().clone());
        }

        let context = &context.context;
        let source_code_vec = module_names
            .iter()
            .map(|module_name| {
                context
                    .get_module_context(module_name)
                    .unwrap()
                    .source_code
                    .clone()
            })
            .collect::<Vec<_>>();

        let sources_vec = module_names
            .into_iter()
            .zip(source_code_vec.iter())
            .map(|(module_name, source_code)| (module_name, source_code.code.as_str()))
            .collect::<Vec<_>>();

        builder.finish().print(sources(sources_vec)).unwrap();
    }
}

fn add_error(result: Result<(), TypeMismatchError>, type_environment: &mut TypeEnvironment) {
    if let Err(error) = result {
        type_environment.lazy_type_reports.push(Box::new(error));
    }
}

struct OutOfEnvironmentVariable {
    identifier_span: Range<usize>,
    defined_span: Range<usize>,
    environment_spans: Vec<Range<usize>>,
}

impl OutOfEnvironmentVariable {
    pub fn new(identifier_span: Range<usize>, resolved: &FoundDefineInfo) -> TranspileError {
        let defined_span = resolved.define_info.span.clone();
        let mut environment_spans = Vec::new();
        for separator in resolved.separators.iter() {
            let across = match separator.value {
                EnvironmentSeparatorKind::Function => false,
                EnvironmentSeparatorKind::UserTypeDefine => false,
                EnvironmentSeparatorKind::Closure => true,
                EnvironmentSeparatorKind::Loop => true,
            };

            if !across {
                environment_spans.push(separator.span.clone());
            }
        }

        TranspileError::new(OutOfEnvironmentVariable {
            identifier_span,
            defined_span,
            environment_spans,
        })
    }
}

impl TranspileReport for OutOfEnvironmentVariable {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = &context.module_name;
        let text = &context.context.localized_text;
        let error_code = 0035;
        let key = ErrorMessageKey::new(error_code);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.identifier_span.start)
            .with_code(error_code)
            .with_message(message);

        builder.add_label(
            Label::new((module_name, self.identifier_span.clone()))
                .with_color(Color::Red)
                .with_message(key.get_massage(text, ErrorMessageType::Label(0))),
        );

        builder.add_label(
            Label::new((module_name, self.defined_span.clone()))
                .with_color(Color::Yellow)
                .with_message(key.get_massage(text, ErrorMessageType::Label(1))),
        );

        for environment_span in self.environment_spans.iter() {
            builder.add_label(
                Label::new((module_name, environment_span.clone()))
                    .with_color(Color::Yellow)
                    .with_message(key.get_massage(text, ErrorMessageType::Label(2))),
            );
        }

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        builder
            .finish()
            .print((module_name, Source::from(context.source_code.code.as_str())))
            .unwrap();
    }
}

struct NotFoundTypeElementError {
    user_type: Type,
    name: Spanned<String>,
}

impl LazyTypeReport for NotFoundTypeElementError {
    fn build_report(
        &self,
        type_environment: &TypeEnvironment,
        context: &TranspileModuleContext,
    ) -> Either<TranspileError, TranspileWarning> {
        let error = SimpleError::new(
            0037,
            self.name.span.clone(),
            vec![
                (
                    type_environment.get_type_display_string(&self.user_type),
                    Color::Yellow,
                ),
                (self.name.value.to_string(), Color::Red),
            ],
            vec![(
                (context.module_name.clone(), self.name.span.clone()),
                Color::Red,
            )],
        );
        Either::Left(error)
    }
}

struct SimpleTypeError {
    error_code: usize,
    ty: Spanned<Type>,
}

impl LazyTypeReport for SimpleTypeError {
    fn build_report(
        &self,
        type_environment: &TypeEnvironment,
        context: &TranspileModuleContext,
    ) -> Either<TranspileError, TranspileWarning> {
        let ty = self
            .ty
            .clone()
            .map(|ty| type_environment.get_type_display_string(&ty));

        let error = SimpleError::new(
            self.error_code,
            self.ty.span.clone(),
            vec![(ty.value, Color::Red)],
            vec![((context.module_name.clone(), ty.span), Color::Red)],
        );
        Either::Left(error)
    }
}

struct ArgumentCountMismatchError {
    defined_count: usize,
    specified_count: usize,
    defined_module: Arc<String>,
    defined_span: Range<usize>,
    specified_span: Range<usize>,
}

impl TranspileReport for ArgumentCountMismatchError {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = context.module_name.deref();
        let text = &context.context.localized_text;
        let error_code = 0040;
        let key = ErrorMessageKey::new(error_code);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.specified_span.start)
            .with_code(error_code)
            .with_message(message);

        builder.add_label(
            Label::new((module_name.clone(), self.specified_span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace(
                            "%defined",
                            self.defined_count
                                .to_string()
                                .fg(Color::Green)
                                .to_string()
                                .as_str(),
                        )
                        .replace(
                            "%specified",
                            self.specified_count
                                .to_string()
                                .fg(Color::Yellow)
                                .to_string()
                                .as_str(),
                        ),
                ),
        );

        builder.add_label(
            Label::new((
                self.defined_module.deref().clone(),
                self.defined_span.clone(),
            ))
            .with_color(Color::Yellow)
            .with_message(
                key.get_massage(text, ErrorMessageType::Label(1))
                    .replace(
                        "%defined",
                        self.defined_count
                            .to_string()
                            .fg(Color::Green)
                            .to_string()
                            .as_str(),
                    )
                    .replace(
                        "%specified",
                        self.specified_count
                            .to_string()
                            .fg(Color::Yellow)
                            .to_string()
                            .as_str(),
                    ),
            ),
        );

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        let defined_module_context = context
            .context
            .get_module_context(&self.defined_module)
            .unwrap();

        let source_list = vec![
            (module_name.clone(), context.source_code.code.as_str()),
            (
                self.defined_module.deref().clone(),
                defined_module_context.source_code.code.as_str(),
            ),
        ];

        builder.finish().print(sources(source_list)).unwrap();
    }
}

struct UnexpectedTypeError {
    error_code: usize,
    found_type: Spanned<Type>,
    expected_kind: Spanned<ExpectedTypeKind>,
}

impl LazyTypeReport for UnexpectedTypeError {
    fn build_report(
        &self,
        type_environment: &TypeEnvironment,
        _: &TranspileModuleContext,
    ) -> Either<TranspileError, TranspileWarning> {
        let report = UnexpectedTypeErrorReport {
            error_code: self.error_code,
            found_type: self
                .found_type
                .clone()
                .map(|ty| type_environment.get_type_display_string(&ty)),
            expected_kind: self.expected_kind.clone(),
        };
        Either::Left(TranspileError::new(report))
    }
}

#[derive(Clone, Copy)]
enum ExpectedTypeKind {
    Option,
    Result,
}

impl ExpectedTypeKind {
    fn get_key(&self) -> String {
        format!(
            "expected_type.{}",
            match self {
                ExpectedTypeKind::Option => "option",
                ExpectedTypeKind::Result => "result",
            }
        )
    }
}

struct UnexpectedTypeErrorReport {
    error_code: usize,
    found_type: Spanned<String>,
    expected_kind: Spanned<ExpectedTypeKind>,
}

impl TranspileReport for UnexpectedTypeErrorReport {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = &context.module_name;
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(self.error_code);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.found_type.span.start)
            .with_code(self.error_code)
            .with_message(message);

        let expected_text = text.get_text(self.expected_kind.value.get_key());

        builder.add_label(
            Label::new((module_name, self.found_type.span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace(
                            "%found",
                            self.found_type
                                .value
                                .clone()
                                .fg(Color::Red)
                                .to_string()
                                .as_str(),
                        )
                        .replace(
                            "%expected",
                            expected_text.as_str().fg(Color::Green).to_string().as_str(),
                        ),
                ),
        );

        builder.add_label(
            Label::new((module_name, self.expected_kind.span.clone()))
                .with_color(Color::Yellow)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(1))
                        .replace(
                            "%found",
                            self.found_type
                                .value
                                .clone()
                                .fg(Color::Red)
                                .to_string()
                                .as_str(),
                        )
                        .replace(
                            "%expected",
                            expected_text.as_str().fg(Color::Green).to_string().as_str(),
                        ),
                ),
        );

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        builder
            .finish()
            .print((module_name, Source::from(context.source_code.code.as_str())))
            .unwrap();
    }
}

struct BoundsNotSatisfiedError {
    span: Range<usize>,
    type_name: WithDefineInfo<String>,
    bounds_module_name: Arc<String>,
    not_satisfied_bounds: Vec<Spanned<String>>,
}

impl TranspileReport for BoundsNotSatisfiedError {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = context.module_name.deref();
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(BOUNDS_NOT_SATISFIED_ERROR);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.span.start)
            .with_code(BOUNDS_NOT_SATISFIED_ERROR)
            .with_message(message);

        let type_text = self.type_name.value.clone().fg(Color::Red).to_string();

        let bounds_text = self
            .not_satisfied_bounds
            .iter()
            .map(|bound| bound.value.clone().fg(Color::Yellow).to_string())
            .collect::<Vec<String>>()
            .join(", ");

        builder.add_label(
            Label::new((module_name.clone(), self.span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%type", &type_text)
                        .replace("%bounds", &bounds_text),
                ),
        );

        builder.add_label(
            Label::new((module_name.clone(), self.type_name.span.clone()))
                .with_color(Color::Yellow)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(1))
                        .replace("%type", &type_text),
                ),
        );

        for bound in self.not_satisfied_bounds.iter() {
            let bound_text = bound.value.clone().fg(Color::Yellow).to_string();

            builder.add_label(
                Label::new((self.bounds_module_name.deref().clone(), bound.span.clone()))
                    .with_color(Color::Yellow)
                    .with_message(
                        key.get_massage(text, ErrorMessageType::Label(2))
                            .replace("%bound", &bound_text),
                    ),
            );
        }

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            let note = note
                .replace("%type", &type_text)
                .replace("%bounds", &bounds_text);
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            let help = help
                .replace("%type", &type_text)
                .replace("%bounds", &bounds_text);
            builder.set_help(help);
        }

        let bounds_module_context = context
            .context
            .get_module_context(&self.bounds_module_name)
            .unwrap();

        let source_list = vec![
            (module_name.clone(), context.source_code.code.as_str()),
            (
                self.bounds_module_name.deref().clone(),
                bounds_module_context.source_code.code.as_str(),
            ),
        ];

        builder.finish().print(sources(source_list)).unwrap();
    }
}

struct WhereBoundsNotSatisfiedError {
    type_name: Spanned<String>,
    bounds_module_name: Arc<String>,
    not_satisfied_bounds: Vec<Spanned<String>>,
}

impl TranspileReport for WhereBoundsNotSatisfiedError {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = context.module_name.deref();
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(WHERE_BOUNDS_NOT_SATISFIED_ERROR);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.type_name.span.start)
            .with_code(WHERE_BOUNDS_NOT_SATISFIED_ERROR)
            .with_message(message);

        let type_text = self.type_name.value.clone().fg(Color::Red).to_string();

        let bounds_text = self
            .not_satisfied_bounds
            .iter()
            .map(|bound| bound.value.clone().fg(Color::Yellow).to_string())
            .collect::<Vec<String>>()
            .join(" + ");

        builder.add_label(
            Label::new((module_name.clone(), self.type_name.span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%type", &type_text)
                        .replace("%bounds", &bounds_text),
                ),
        );

        for bound in self.not_satisfied_bounds.iter() {
            let bound_text = bound.value.clone().fg(Color::Yellow).to_string();

            builder.add_label(
                Label::new((self.bounds_module_name.deref().clone(), bound.span.clone()))
                    .with_color(Color::Yellow)
                    .with_message(
                        key.get_massage(text, ErrorMessageType::Label(1))
                            .replace("%bound", &bound_text),
                    ),
            );
        }

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            let note = note
                .replace("%type", &type_text)
                .replace("%bounds", &bounds_text);
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            let help = help
                .replace("%type", &type_text)
                .replace("%bounds", &bounds_text);
            builder.set_help(help);
        }

        let bounds_module_context = context
            .context
            .get_module_context(&self.bounds_module_name)
            .unwrap();

        let source_list = vec![
            (module_name.clone(), context.source_code.code.as_str()),
            (
                self.bounds_module_name.deref().clone(),
                bounds_module_context.source_code.code.as_str(),
            ),
        ];

        builder.finish().print(sources(source_list)).unwrap();
    }
}

struct InvalidSetGenericsTypeError {
    ty: Spanned<Type>,
    set_span: Range<usize>,
}

impl LazyTypeReport for InvalidSetGenericsTypeError {
    fn build_report(
        &self,
        type_environment: &TypeEnvironment,
        context: &TranspileModuleContext,
    ) -> Either<TranspileError, TranspileWarning> {
        let type_name = type_environment
            .get_type_display_string(&self.ty.value)
            .fg(Color::Red)
            .to_string();

        Either::Left(TranspileError::new(SimpleError::new(
            INVALID_SET_GENERICS_TYPE_ERROR,
            self.set_span.clone(),
            vec![(type_name, Color::Yellow)],
            vec![
                (
                    (context.module_name.clone(), self.set_span.clone()),
                    Color::Red,
                ),
                (
                    (context.module_name.clone(), self.ty.span.clone()),
                    Color::Yellow,
                ),
            ],
        )))
    }
}

struct NumberOfGenericsMismatchError {
    set_span: Range<usize>,
    expected: usize,
    found: usize,
    ty: Spanned<Type>,
}

impl LazyTypeReport for NumberOfGenericsMismatchError {
    fn build_report(
        &self,
        type_environment: &TypeEnvironment,
        _: &TranspileModuleContext,
    ) -> Either<TranspileError, TranspileWarning> {
        let type_name = type_environment
            .get_type_display_string(&self.ty.value)
            .fg(Color::Yellow)
            .to_string();

        let found_text = self.found.to_string().fg(Color::Red).to_string();
        let expected_text = self.expected.to_string().fg(Color::Yellow).to_string();

        let (define_module_name, define_span) = match &self.ty.value {
            Type::UserType {
                user_type_info,
                generics: _,
                generics_span: _,
            } => (
                user_type_info.module_name.clone(),
                user_type_info
                    .generics_define_span
                    .clone()
                    .unwrap_or_else(|| user_type_info.name.span.clone()),
            ),
            Type::Function {
                function_info,
                generics: _,
            } => (
                function_info.define_info.module_name.clone(),
                function_info
                    .define_info
                    .generics_define_span
                    .clone()
                    .unwrap_or_else(|| function_info.define_info.span.clone()),
            ),
            _ => unreachable!(),
        };

        Either::Left(TranspileError::new(NumberOfGenericsMismatchErrorReport {
            set_span: self.set_span.clone(),
            type_name,
            found_text,
            expected_text,
            define_module_name,
            define_span,
        }))
    }
}

struct NumberOfGenericsMismatchErrorReport {
    set_span: Range<usize>,
    type_name: String,
    found_text: String,
    expected_text: String,
    define_module_name: Arc<String>,
    define_span: Range<usize>,
}

impl TranspileReport for NumberOfGenericsMismatchErrorReport {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = context.module_name.deref();
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(NUMBER_OF_GENERICS_MISMATCH_ERROR);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.set_span.start)
            .with_code(NUMBER_OF_GENERICS_MISMATCH_ERROR)
            .with_message(message);

        builder.add_label(
            Label::new((module_name.clone(), self.set_span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%found", &self.found_text)
                        .replace("%expected", &self.expected_text),
                ),
        );

        builder.add_label(
            Label::new((
                self.define_module_name.deref().clone(),
                self.define_span.clone(),
            ))
            .with_color(Color::Yellow)
            .with_message(
                key.get_massage(text, ErrorMessageType::Label(1))
                    .replace("%type", &self.type_name)
                    .replace("%expected", &self.expected_text),
            ),
        );

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        let bounds_module_context = context
            .context
            .get_module_context(&self.define_module_name)
            .unwrap();

        let source_list = vec![
            (module_name.clone(), context.source_code.code.as_str()),
            (
                self.define_module_name.deref().clone(),
                bounds_module_context.source_code.code.as_str(),
            ),
        ];

        builder.finish().print(sources(source_list)).unwrap();
    }
}

struct DuplicatedElement {
    type_name: String,
    element: Spanned<String>,
    count: usize,
    found_elements: Vec<WithDefineInfo<()>>,
}

impl TranspileReport for DuplicatedElement {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = context.module_name.deref();
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(DUPLICATED_ELEMENT_ERROR);

        let type_name = self.type_name.clone().fg(Color::Yellow).to_string();
        let count = self.count.to_string().fg(Color::Red).to_string();
        let element = self.element.value.clone().fg(Color::Yellow).to_string();

        let message = key
            .get_massage(text, ErrorMessageType::Message)
            .replace("%type", &type_name);

        let mut builder = Report::build(ReportKind::Error, module_name, self.element.span.start)
            .with_code(DUPLICATED_ELEMENT_ERROR)
            .with_message(message);

        builder.add_label(
            Label::new((module_name.clone(), self.element.span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%count", &count),
                ),
        );

        let mut module_names = Vec::new();
        module_names.push(module_name.clone());

        for found_element in self.found_elements.iter() {
            builder.add_label(
                Label::new((
                    found_element.module_name.deref().clone(),
                    found_element.span.clone(),
                ))
                .with_color(Color::Yellow)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(1))
                        .replace("%element", &element),
                ),
            );

            module_names.push(found_element.module_name.deref().clone());
        }

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(
                note.replace("%type", &type_name)
                    .replace("%element", &element),
            );
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        let module_list = module_names
            .iter()
            .map(|module_name| context.context.get_module_context(module_name).unwrap())
            .collect::<Vec<_>>();

        let source_list = module_list.iter().map(|context| {
            (
                context.module_name.deref().clone(),
                context.source_code.code.as_str(),
            )
        });

        builder.finish().print(sources(source_list)).unwrap();
    }
}

struct UnresolvedInterface {
    original_name: String,
    resolved_name: String,
    reference_span: Range<usize>,
    implementation: WithDefineInfo<()>,
}

impl TranspileReport for UnresolvedInterface {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = context.module_name.deref();
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(UNRESOLVED_INTERFACE);

        let original_name = self.original_name.clone().fg(Color::Yellow).to_string();
        let resolved_name = self.resolved_name.clone().fg(Color::Red).to_string();

        let message = key
            .get_massage(text, ErrorMessageType::Message)
            .replace("%original", &original_name);

        let mut builder = Report::build(ReportKind::Error, module_name, self.reference_span.start)
            .with_code(UNRESOLVED_INTERFACE)
            .with_message(message);

        builder.add_label(
            Label::new((module_name.clone(), self.reference_span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%resolved", &resolved_name),
                ),
        );

        builder.add_label(
            Label::new((
                self.implementation.module_name.deref().clone(),
                self.implementation.span.clone(),
            ))
            .with_color(Color::Yellow)
            .with_message(key.get_massage(text, ErrorMessageType::Label(1))),
        );

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        let impl_module_context = context
            .context
            .get_module_context(&self.implementation.module_name)
            .unwrap();

        let source_list = vec![
            (module_name.clone(), context.source_code.code.as_str()),
            (
                impl_module_context.module_name.deref().clone(),
                impl_module_context.source_code.code.as_str(),
            ),
        ];

        builder.finish().print(sources(source_list)).unwrap();
    }
}

struct LogicalOperatorTypeError {
    operator: Spanned<String>,
    found_type: Spanned<Type>,
}

impl LazyTypeReport for LogicalOperatorTypeError {
    fn build_report(
        &self,
        type_environment: &TypeEnvironment,
        context: &TranspileModuleContext,
    ) -> Either<TranspileError, TranspileWarning> {
        let error = SimpleError::new(
            INVALID_LOGICAL_OPERATOR_EXPR_TYPE,
            self.found_type.span.clone(),
            vec![
                (self.operator.value.clone(), Color::Yellow),
                (
                    type_environment.get_type_display_string(&self.found_type.value),
                    Color::Red,
                ),
                ("bool".to_string(), Color::Cyan),
            ],
            vec![
                (
                    (context.module_name.clone(), self.operator.span.clone()),
                    Color::Yellow,
                ),
                (
                    (context.module_name.clone(), self.found_type.span.clone()),
                    Color::Red,
                ),
            ],
        );

        Either::Left(error)
    }
}

struct InvalidTupleLength {
    ty: Type,
    type_span: Range<usize>,
    binding_span: Range<usize>,
    types_length: usize,
    bindings_length: usize,
}

impl LazyTypeReport for InvalidTupleLength {
    fn build_report(
        &self,
        type_environment: &TypeEnvironment,
        context: &TranspileModuleContext,
    ) -> Either<TranspileError, TranspileWarning> {
        let error = SimpleError::new(
            INVALID_TUPLE_LENGTH,
            self.binding_span.clone(),
            vec![
                (
                    type_environment.get_type_display_string(&self.ty),
                    Color::Yellow,
                ),
                (self.types_length.to_string(), Color::Red),
                (self.bindings_length.to_string(), Color::Red),
            ],
            vec![
                (
                    (context.module_name.clone(), self.type_span.clone()),
                    Color::Red,
                ),
                (
                    (context.module_name.clone(), self.binding_span.clone()),
                    Color::Red,
                ),
            ],
        );

        Either::Left(error)
    }
}

struct InvalidTypeTupleBinding {
    ty: Type,
    type_span: Range<usize>,
    binding_span: Range<usize>,
}

impl LazyTypeReport for InvalidTypeTupleBinding {
    fn build_report(
        &self,
        type_environment: &TypeEnvironment,
        context: &TranspileModuleContext,
    ) -> Either<TranspileError, TranspileWarning> {
        let error = SimpleError::new(
            INVALID_TYPE_TUPLE_BINDING,
            self.binding_span.clone(),
            vec![(
                type_environment.get_type_display_string(&self.ty),
                Color::Yellow,
            )],
            vec![
                (
                    (context.module_name.clone(), self.type_span.clone()),
                    Color::Yellow,
                ),
                (
                    (context.module_name.clone(), self.binding_span.clone()),
                    Color::Red,
                ),
            ],
        );

        Either::Left(error)
    }
}
