use std::sync::Arc;

use catla_import::{
    element::collect_import_element,
    import::collect_import,
    resource::{PackageResource, PackageResourceSet},
};
use catla_name_resolver::resolve_name;
use catla_parser::CatlaAST;
use catla_print_debug::type_infer::print_type_infer_result;
use catla_type::{
    module_element_collector::collect_module_element_type_for_program,
    type_infer::infer_type,
    types::{GlobalUserTypeSet, ImplementsElementChecker, ImplementsInfoSet, Type},
    user_type_collector::collect_user_type_for_program,
};
use catla_util::{future::MultiTaskFuture, source_code::SourceCode};
use hashbrown::HashMap;
use tokio::runtime::{Builder, Runtime};

use crate::{resource::ModuleResourceSet, settings::CatlaCompilerSettings};

#[derive(Debug, Clone)]
pub struct CatlaCompiler {
    inner: Arc<CatlaCompilerInner>,
}

#[derive(Debug)]
struct CatlaCompilerInner {
    package_resource_set: PackageResourceSet,
    user_type_set: GlobalUserTypeSet,
    runtime: Runtime,
    phases: CompilePhases,
    states: CompileStates,
}

impl CatlaCompiler {
    pub fn new(package_resource_set: PackageResourceSet, settings: CatlaCompilerSettings) -> Self {
        let states = CompileStates::new(&package_resource_set);

        let inner = CatlaCompilerInner {
            package_resource_set,
            user_type_set: GlobalUserTypeSet::new(),
            runtime: Builder::new_multi_thread()
                .worker_threads(settings.threads)
                .build()
                .unwrap(),
            phases: CompilePhases::new(),
            states,
        };

        Self {
            inner: Arc::new(inner),
        }
    }

    pub fn compile(&self) {
        self.inner.runtime.block_on(async {
            self.compile_inner().await;
        });
    }

    async fn compile_inner(&self) {
        let resouces = self.inner.package_resource_set.get_all();

        for resource in resouces.into_values() {
            if let PackageResource::Module { source_code } = resource {
                let self_clone = self.clone();

                self.inner.runtime.spawn(async move {
                    self_clone.clone().compile_module(source_code).await;
                });
            }
        }
    }

    async fn compile_module(&self, source_code: SourceCode) {
        let ast = CatlaAST::parse(source_code.clone());

        dbg!(&ast.errors);

        let (name_resolved_map, module_element_names, errors) =
            resolve_name(ast.ast(), &Vec::new(), &HashMap::new());
        dbg!(errors);

        let module_element_names = Arc::new(module_element_names);

        let module_name = source_code.module_path.path_name.as_ref();

        self.inner
            .states
            .module_element_name_map
            .set(module_name, module_element_names)
            .await;

        let package_resource_set = &self.inner.package_resource_set;

        let modules = collect_import(ast.ast(), package_resource_set);

        let module_element_name_map = self
            .inner
            .states
            .module_element_name_map
            .get(&modules)
            .await;

        let (import_map, errors) = collect_import_element(
            ast.ast(),
            &package_resource_set,
            &module_element_name_map,
            &source_code.module_path,
        );

        dbg!(errors);

        let mut module_entity_user_type_map = HashMap::new();
        let mut module_name_type_map = HashMap::new();
        let user_type_set = &self.inner.user_type_set;
        collect_user_type_for_program(
            ast.ast(),
            &mut module_entity_user_type_map,
            &mut module_name_type_map,
            &user_type_set,
            &source_code.module_path,
        );

        let mut module_element_entity_type_map = HashMap::new();
        let mut module_element_name_type_map = HashMap::new();
        let mut implements_infos = ImplementsInfoSet::new(None);
        let mut errors = Vec::new();
        let mut generics = HashMap::new();
        let mut moduled_name_user_type_map = HashMap::new();
        moduled_name_user_type_map.insert(
            source_code.module_path.path_name.as_ref().clone(),
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
            &source_code.module_path,
            &mut errors,
        );

        module_entity_type_map.extend(module_element_entity_type_map);

        let moduled_name_type_map = HashMap::new();
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
            &source_code.module_path,
            &package_resource_set,
            &mut errors,
        );

        implements_element_checker.register_implements(&implements_infos);
        implements_element_checker.check(&user_type_set, &implements_infos, &mut errors);

        dbg!(errors);

        print_type_infer_result(&ast, &result, &user_type_set);
    }
}

#[derive(Debug)]
pub struct CompilePhases {}

impl CompilePhases {
    pub fn new() -> Self {
        Self {}
    }
}

#[derive(Debug)]
pub struct CompileStates {
    pub module_element_name_map: ModuleResourceSet<Vec<String>>,
}

impl CompileStates {
    pub fn new(package_resource_set: &PackageResourceSet) -> Self {
        Self {
            module_element_name_map: ModuleResourceSet::new(package_resource_set),
        }
    }
}
