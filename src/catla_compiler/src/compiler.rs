use std::sync::Arc;

use catla_codegen::{CodegenSettings, codegen, crates::codegen_cargo_toml};
use catla_crate::CrateInfoSet;
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
use catla_util::source_code::SourceCode;
use futures::{StreamExt, stream::FuturesUnordered};
use hashbrown::{HashMap, HashSet};
use tokio::runtime::{Builder, Runtime};

use crate::{resource::ModuleResourceSet, settings::CatlaCompilerSettings};

#[derive(Debug, Clone)]
pub struct CatlaCompiler {
    inner: Arc<CatlaCompilerInner>,
}

#[derive(Debug)]
struct CatlaCompilerInner {
    crate_info_set: CrateInfoSet,
    package_resource_set: PackageResourceSet,
    user_type_set: GlobalUserTypeSet,
    runtime: Runtime,
    phases: CompilePhases,
    states: CompileStates,
    codegen_settings: CodegenSettings,
}

impl CatlaCompiler {
    pub fn new(
        crate_info_set: CrateInfoSet,
        package_resource_set: PackageResourceSet,
        settings: CatlaCompilerSettings,
        codegen_settings: CodegenSettings,
    ) -> Self {
        let states = CompileStates::new(&package_resource_set);

        let inner = CatlaCompilerInner {
            crate_info_set,
            package_resource_set,
            user_type_set: GlobalUserTypeSet::new(),
            runtime: Builder::new_multi_thread()
                .worker_threads(settings.threads)
                .build()
                .unwrap(),
            phases: CompilePhases::new(),
            states,
            codegen_settings,
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

        let mut futures = FuturesUnordered::new();

        for resource in resouces.into_values() {
            if let PackageResource::Module { source_code } = resource {
                let self_clone = self.clone();

                let future = self.inner.runtime.spawn(async move {
                    self_clone.compile_module(source_code).await;
                });
                futures.push(future);
            }
        }

        while let Some(result) = futures.next().await {
            result.unwrap();
        }

        let mut futures = FuturesUnordered::new();

        for crate_info in self.inner.crate_info_set.crates.iter() {
            futures.push(codegen_cargo_toml(crate_info, &self.inner.codegen_settings));
        }

        while let Some(result) = futures.next().await {
            result.unwrap();
        }
    }

    async fn compile_module(&self, source_code: SourceCode) {
        let ast = CatlaAST::parse(source_code.clone());

        dbg!(&ast.errors);

        let all_crates = &self
            .inner
            .package_resource_set
            .get_all()
            .keys()
            .filter(|path| !path.contains("::"))
            .cloned()
            .collect();

        let (name_resolved_map, module_element_names, errors) =
            resolve_name(ast.ast(), &all_crates, &HashMap::new());
        dbg!(errors);

        let module_element_names = Arc::new(module_element_names);

        let module_name = source_code.module_path.path_name.as_ref();

        self.inner
            .states
            .module_element_name_map
            .set(module_name, module_element_names)
            .await;

        let package_resource_set = &self.inner.package_resource_set;

        let mut modules = collect_import(ast.ast(), package_resource_set)
            .into_iter()
            .filter(|module_name| package_resource_set.is_module_name(&module_name))
            .collect::<Vec<_>>();
        modules.push("std::string".to_string());
        modules.push("std::operators::add".to_string());

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

        let module_name_user_type_map: Arc<HashMap<_, _>> = Arc::new(
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

        self.inner
            .states
            .module_name_user_type_map
            .set(module_name, module_name_user_type_map.clone())
            .await;

        let mut moduled_name_user_type_map = self
            .inner
            .states
            .module_name_user_type_map
            .get(&modules)
            .await;
        moduled_name_user_type_map.insert(module_name.clone(), module_name_user_type_map.clone());

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

        let mut module_element_entity_type_map = HashMap::new();
        let mut module_element_name_type_map = HashMap::new();
        let mut implements_infos = ImplementsInfoSet::new(None);
        let mut errors = Vec::new();
        let mut generics = HashMap::new();

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

        module_element_name_type_map.extend(module_name_user_type_map.as_ref().clone());

        self.inner
            .states
            .module_name_type_map
            .set(module_name, Arc::new(module_element_name_type_map))
            .await;

        let moduled_name_type_map = self.inner.states.module_name_type_map.get(&modules).await;

        self.inner
            .states
            .module_imports
            .set(module_name, Arc::new(modules.clone()))
            .await;

        self.inner
            .states
            .module_implements_infos
            .set(module_name, Arc::new(implements_infos.clone()))
            .await;

        let global_implements_infos = ImplementsInfoSet::new(None);
        let mut merged_module = HashSet::new();
        merged_module.insert(module_name.clone());
        let mut candidate_modules = Vec::new();
        candidate_modules.push(module_name.clone());

        loop {
            let Some(candidate_module) = candidate_modules.pop() else {
                break;
            };

            let import_modules = self
                .inner
                .states
                .module_imports
                .get([&candidate_module])
                .await;
            let implements_infos = self
                .inner
                .states
                .module_implements_infos
                .get([&candidate_module])
                .await;

            global_implements_infos.merge(implements_infos.get(&candidate_module).unwrap());

            for import_module in import_modules.get(&candidate_module).unwrap().iter() {
                if merged_module.contains(import_module) {
                    continue;
                }
                merged_module.insert(import_module.clone());
            }
        }

        module_entity_type_map.extend(module_element_entity_type_map);

        let implements_element_checker = ImplementsElementChecker::new();
        let type_infer_results = infer_type(
            ast.ast(),
            &implements_element_checker,
            &mut generics,
            &global_implements_infos,
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
        implements_element_checker.check(&user_type_set, &global_implements_infos, &mut errors);

        dbg!(errors);

        print_type_infer_result(&ast, &type_infer_results, &user_type_set);

        codegen(
            ast.ast(),
            &type_infer_results,
            &self.inner.codegen_settings,
            &source_code.module_path,
        )
        .await
        .unwrap();
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
    pub module_name_user_type_map: ModuleResourceSet<HashMap<String, Type>>,
    pub module_imports: ModuleResourceSet<Vec<String>>,
    pub module_implements_infos: ModuleResourceSet<ImplementsInfoSet>,
    pub module_name_type_map: ModuleResourceSet<HashMap<String, Type>>,
}

impl CompileStates {
    pub fn new(package_resource_set: &PackageResourceSet) -> Self {
        Self {
            module_element_name_map: ModuleResourceSet::new(package_resource_set),
            module_name_user_type_map: ModuleResourceSet::new(package_resource_set),
            module_imports: ModuleResourceSet::new(package_resource_set),
            module_implements_infos: ModuleResourceSet::new(package_resource_set),
            module_name_type_map: ModuleResourceSet::new(package_resource_set),
        }
    }
}
