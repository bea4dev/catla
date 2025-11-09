pub mod type_infer;

#[cfg(test)]
mod test {
    use std::{path::Path, sync::Arc};

    use catla_import::resource::PackageResourceSet;
    use catla_name_resolver::resolve_name;
    use catla_parser::CatlaAST;
    use catla_type::{
        module_element_collector::collect_module_element_type_for_program,
        type_infer::infer_type,
        types::{GlobalUserTypeSet, ImplementsElementChecker, ImplementsInfoSet, Type},
        user_type_collector::collect_user_type_for_program,
    };
    use catla_util::module_path::ModulePath;
    use hashbrown::HashMap;

    use crate::type_infer::print_type_infer_result;

    #[test]
    fn infer_type_test() {
        let source = r"
interface TestInterface<T> {
    function <U> test() -> (T, U);
}

implements <F, D> TestInterface<F> for D {
    function <E> test() -> (F, E) {}
}

let a = 100.test()
        ";
        let ast = CatlaAST::parse(source.to_string(), "test.catla".to_string());

        dbg!(&ast.errors);

        let (name_resolved_map, errors) = resolve_name(ast.ast(), &Vec::new(), &HashMap::new());
        dbg!(errors);

        let mut module_entity_user_type_map = HashMap::new();
        let mut module_name_type_map = HashMap::new();
        let user_type_set = GlobalUserTypeSet::new();
        let module_path = ModulePath::new(["test"].into_iter(), Path::new("test.catla"));
        collect_user_type_for_program(
            ast.ast(),
            &mut module_entity_user_type_map,
            &mut module_name_type_map,
            &user_type_set,
            &module_path,
        );

        let mut module_element_entity_type_map = HashMap::new();
        let mut module_element_name_type_map = HashMap::new();
        let mut implements_infos = ImplementsInfoSet::new(None);
        let mut errors = Vec::new();
        let mut generics = HashMap::new();
        let import_map = HashMap::new();
        let mut moduled_name_user_type_map = HashMap::new();
        moduled_name_user_type_map.insert(
            module_path
                .path
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .join("::"),
            module_name_type_map
                .iter()
                .map(|(name, user_type_id)| {
                    (
                        name.clone(),
                        Type::UserType {
                            user_type_info: *user_type_id,
                            generics: Arc::new(Vec::new()),
                        },
                    )
                })
                .collect(),
        );

        let mut module_entity_type_map = HashMap::new();
        module_entity_type_map.extend(module_entity_user_type_map.iter().map(
            |(entity_id, user_type_id)| {
                (
                    *entity_id,
                    Type::UserType {
                        user_type_info: *user_type_id,
                        generics: Arc::new(Vec::new()),
                    },
                )
            },
        ));

        collect_module_element_type_for_program(
            ast.ast(),
            &None,
            &mut None,
            &mut generics,
            &mut module_element_entity_type_map,
            &mut module_element_name_type_map,
            &mut implements_infos,
            &import_map,
            &module_entity_type_map,
            &moduled_name_user_type_map,
            &name_resolved_map,
            &user_type_set,
            &module_path,
            &mut errors,
        );

        module_entity_type_map.extend(module_element_entity_type_map);

        let moduled_name_type_map = HashMap::new();
        let package_resource_set = PackageResourceSet::new();
        let implements_element_checker = ImplementsElementChecker::new();
        let result = infer_type(
            ast.ast(),
            &implements_element_checker,
            &mut generics,
            &implements_infos,
            &import_map,
            &module_entity_type_map,
            &moduled_name_type_map,
            &name_resolved_map,
            &user_type_set,
            &module_path,
            &package_resource_set,
            &mut errors,
        );

        implements_element_checker.register_implements(&implements_infos);
        implements_element_checker.check(&user_type_set, &implements_infos, &mut errors);

        dbg!(errors);

        print_type_infer_result(&ast, &result, &user_type_set);
    }
}
