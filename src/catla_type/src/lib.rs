pub mod error;
pub mod module_element_collector;
pub mod type_infer;
pub mod types;
pub mod user_type_collector;

#[cfg(test)]
mod test {
    use std::path::Path;

    use catla_name_resolver::resolve_name;
    use catla_parser::CatlaAST;
    use catla_util::module_path::ModulePath;
    use hashbrown::HashMap;

    use crate::{
        module_element_collector::collect_module_element_type_for_program,
        types::{GlobalUserTypeSet, ImplementsInfoSet},
        user_type_collector::collect_user_type_for_program,
    };

    #[test]
    fn collect_module_element_type() {
        let source = r"
class TestClass<T> {}

static let STATIC: TestClass<int> = new TestClass {}
        ";

        let ast = CatlaAST::parse(source.to_string(), "test.catla".to_string());

        let (name_resolved_map, errors) = resolve_name(ast.ast(), &Vec::new(), &HashMap::new());
        dbg!(errors);

        let mut module_entity_type_map = HashMap::new();
        let mut module_name_type_map = HashMap::new();
        let user_type_set = GlobalUserTypeSet::new();
        let module_path = ModulePath::new(["test"].into_iter(), Path::new("test.catla"));
        collect_user_type_for_program(
            ast.ast(),
            &mut module_entity_type_map,
            &mut module_name_type_map,
            &user_type_set,
            &module_path,
        );

        let mut module_element_entity_type_map = HashMap::new();
        let mut module_element_name_type_map = HashMap::new();
        let mut implements_infos = ImplementsInfoSet::new();
        let mut errors = Vec::new();
        collect_module_element_type_for_program(
            ast.ast(),
            &None,
            &mut None,
            &mut HashMap::new(),
            &mut module_element_entity_type_map,
            &mut module_element_name_type_map,
            &mut implements_infos,
            &HashMap::new(),
            &module_entity_type_map,
            &HashMap::new(),
            &name_resolved_map,
            &user_type_set,
            &module_path,
            &mut errors,
        );
        dbg!(errors);
    }
}
