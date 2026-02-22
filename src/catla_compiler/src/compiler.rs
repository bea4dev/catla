use std::{
    collections::{BTreeMap, BTreeSet},
    io::ErrorKind,
    path::{Path, PathBuf},
    sync::Arc,
};

use catla_codegen::{CodegenSettings, codegen, crates::codegen_cargo_toml};
use catla_crate::CrateInfoSet;
use catla_import::{
    element::collect_import_element,
    import::collect_import,
    resource::{PackageResource, PackageResourceSet},
};
use catla_name_resolver::resolve_name;
use catla_optimization::lifetime::{
    LifetimeAnalyzeResults, ModuleLifetimeSource, collect_lifetime_source,
    evaluate_lifetime_sources,
};
use catla_parser::CatlaAST;
use catla_print_debug::{
    optimization::print_lifetime_optimization_result, type_infer::print_type_infer_result,
};
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

    pub fn print_optimization_debug(&self) {
        self.inner.runtime.block_on(async {
            self.print_optimization_debug_inner().await;
        });
    }

    pub fn print_type_infer_debug(&self) {
        self.inner.runtime.block_on(async {
            self.print_type_infer_debug_inner().await;
        });
    }

    async fn compile_inner(&self) {
        let resouces = self.inner.package_resource_set.get_all();
        let module_names = resouces
            .iter()
            .filter_map(|(module_name, resource)| match resource {
                PackageResource::Module { source_code: _ } => Some(module_name.clone()),
                PackageResource::Package => None,
            })
            .collect::<Vec<_>>();

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

        let lifetime_sources = self
            .inner
            .states
            .module_lifetime_sources
            .get(module_names.iter())
            .await;
        let module_implements_infos = self
            .inner
            .states
            .module_implements_infos
            .get(module_names.iter())
            .await;

        let global_implements_infos = ImplementsInfoSet::new(None);
        for module_name in module_names.iter() {
            if let Some(implements_infos) = module_implements_infos.get(module_name) {
                global_implements_infos.merge(implements_infos);
            }
        }

        let lifetime_results = evaluate_lifetime_sources(
            &lifetime_sources,
            &self.inner.user_type_set,
            &global_implements_infos,
        );

        for (module_name, result) in lifetime_results.iter() {
            self.inner
                .states
                .module_lifetime_analyze_results
                .set(module_name, Arc::new(result.clone()))
                .await;
        }

        let mut futures = FuturesUnordered::new();

        for module_name in module_names.iter() {
            let Some(source) = lifetime_sources.get(module_name) else {
                continue;
            };
            let Some(lifetime_result) = lifetime_results.get(module_name) else {
                continue;
            };

            futures.push(codegen(
                source.ast.ast(),
                &source.type_infer_results,
                &source.name_resolved_map,
                &source.module_entity_type_map,
                &self.inner.user_type_set,
                Some(lifetime_result),
                &self.inner.codegen_settings,
                &source.ast.source_code.module_path,
            ));
        }

        while let Some(result) = futures.next().await {
            result.unwrap();
        }

        let mut futures = FuturesUnordered::new();

        for crate_info in self.inner.crate_info_set.crates.iter() {
            futures.push(ensure_pub_mod_tree(
                crate_info.name.clone(),
                &self.inner.package_resource_set,
                &self.inner.codegen_settings,
            ));
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

    async fn print_optimization_debug_inner(&self) {
        let mut module_names = self
            .inner
            .package_resource_set
            .get_all()
            .into_iter()
            .filter_map(|(module_name, resource)| match resource {
                PackageResource::Module { source_code: _ } => Some(module_name),
                PackageResource::Package => None,
            })
            .collect::<Vec<_>>();
        module_names.sort();

        let module_lifetime_sources = self
            .inner
            .states
            .module_lifetime_sources
            .get(module_names.iter())
            .await;
        let module_lifetime_results = self
            .inner
            .states
            .module_lifetime_analyze_results
            .get(module_names.iter())
            .await;

        for module_name in module_names.iter() {
            let Some(source) = module_lifetime_sources.get(module_name) else {
                continue;
            };
            let Some(result) = module_lifetime_results.get(module_name) else {
                continue;
            };

            print_lifetime_optimization_result(
                &source.ast,
                &source.name_resolved_map,
                result.as_ref(),
            );
        }
    }

    async fn print_type_infer_debug_inner(&self) {
        let mut module_names = self
            .inner
            .package_resource_set
            .get_all()
            .into_iter()
            .filter_map(|(module_name, resource)| match resource {
                PackageResource::Module { source_code: _ } => Some(module_name),
                PackageResource::Package => None,
            })
            .collect::<Vec<_>>();
        module_names.sort();

        let module_lifetime_sources = self
            .inner
            .states
            .module_lifetime_sources
            .get(module_names.iter())
            .await;

        for module_name in module_names.iter() {
            let Some(source) = module_lifetime_sources.get(module_name) else {
                continue;
            };

            print_type_infer_result(&source.ast, &source.type_infer_results, &self.inner.user_type_set);
        }
    }

    async fn compile_module(&self, source_code: SourceCode) {
        let ast = CatlaAST::parse(source_code.clone());

        let all_crates = &self
            .inner
            .package_resource_set
            .get_all()
            .keys()
            .filter(|path| !path.contains("::"))
            .cloned()
            .collect();

        let (name_resolved_map, module_element_names, _errors) =
            resolve_name(ast.ast(), &all_crates, &HashMap::new());

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
        modules.push("std::operators::sub".to_string());

        let module_element_name_map = self
            .inner
            .states
            .module_element_name_map
            .get(&modules)
            .await;

        let (import_map, _errors) = collect_import_element(
            ast.ast(),
            &package_resource_set,
            &module_element_name_map,
            &source_code.module_path,
        );

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

                candidate_modules.push(import_module.clone());
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

        let lifetime_source = collect_lifetime_source(
            module_name,
            &ast,
            &type_infer_results,
            &module_entity_type_map,
            &name_resolved_map,
        );

        self.inner
            .states
            .module_lifetime_sources
            .set(module_name, Arc::new(lifetime_source))
            .await;
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
    pub module_lifetime_sources: ModuleResourceSet<ModuleLifetimeSource>,
    pub module_lifetime_analyze_results: ModuleResourceSet<LifetimeAnalyzeResults>,
}

impl CompileStates {
    pub fn new(package_resource_set: &PackageResourceSet) -> Self {
        Self {
            module_element_name_map: ModuleResourceSet::new(package_resource_set),
            module_name_user_type_map: ModuleResourceSet::new(package_resource_set),
            module_imports: ModuleResourceSet::new(package_resource_set),
            module_implements_infos: ModuleResourceSet::new(package_resource_set),
            module_name_type_map: ModuleResourceSet::new(package_resource_set),
            module_lifetime_sources: ModuleResourceSet::new(package_resource_set),
            module_lifetime_analyze_results: ModuleResourceSet::new(package_resource_set),
        }
    }
}

async fn ensure_pub_mod_tree(
    crate_name: String,
    package_resource_set: &PackageResourceSet,
    codegen_settings: &CodegenSettings,
) -> Result<(), String> {
    let crate_prefix = format!("{}::", crate_name);

    let module_paths = package_resource_set
        .get_all()
        .into_iter()
        .filter_map(|(module_name, resource)| match resource {
            PackageResource::Module { source_code: _ } => Some(module_name),
            PackageResource::Package => None,
        })
        .filter_map(|module_name| {
            module_name.strip_prefix(&crate_prefix).map(|module_name| {
                module_name
                    .split("::")
                    .map(|segment| segment.to_string())
                    .collect::<Vec<_>>()
            })
        })
        .collect::<Vec<_>>();

    if module_paths.is_empty() {
        return Ok(());
    }

    let module_set = module_paths.iter().cloned().collect::<BTreeSet<_>>();

    let has_lib = module_set.contains(&vec!["lib".to_string()]);
    let has_main = module_set.contains(&vec!["main".to_string()]);

    let (root_file, root_name) = if has_lib {
        ("lib.rs", Some("lib"))
    } else if has_main {
        ("main.rs", Some("main"))
    } else {
        ("lib.rs", None)
    };

    let mut parent_children = BTreeMap::<Vec<String>, BTreeSet<String>>::new();
    for module_path in module_paths.iter() {
        if module_path.is_empty() {
            continue;
        }

        for parent_length in 0..module_path.len() {
            let parent = module_path[..parent_length].to_vec();
            let child = module_path[parent_length].clone();
            parent_children.entry(parent).or_default().insert(child);
        }
    }

    if let Some(root_name) = root_name {
        if let Some(root_children) = parent_children.get_mut(&Vec::new()) {
            root_children.remove(root_name);
        }
    }

    for (parent_module, children) in parent_children.into_iter() {
        if children.is_empty() {
            continue;
        }

        let source_path = resolve_parent_source_path(
            &crate_name,
            &parent_module,
            root_file,
            &module_set,
            codegen_settings,
        );

        append_missing_pub_mods(&source_path, &children).await?;
    }

    Ok(())
}

fn resolve_parent_source_path(
    crate_name: &str,
    parent_module: &[String],
    root_file: &str,
    module_set: &BTreeSet<Vec<String>>,
    codegen_settings: &CodegenSettings,
) -> PathBuf {
    let mut source_path = crate_source_dir(crate_name, codegen_settings);

    if parent_module.is_empty() {
        source_path.push(root_file);
        return source_path;
    }

    if module_set.contains(parent_module) {
        for (index, segment) in parent_module.iter().enumerate() {
            if index + 1 == parent_module.len() {
                source_path.push(format!("{}.rs", segment));
            } else {
                source_path.push(segment);
            }
        }
    } else {
        for segment in parent_module.iter() {
            source_path.push(segment);
        }
        source_path.push("mod.rs");
    }

    source_path
}

fn crate_source_dir(crate_name: &str, codegen_settings: &CodegenSettings) -> PathBuf {
    let mut path = codegen_settings.out_dir.clone();

    if crate_name == "std" {
        path.push("catla_std");
    } else {
        path.push(crate_name);
    }

    path.push("src");
    path
}

async fn append_missing_pub_mods(
    source_path: &Path,
    children: &BTreeSet<String>,
) -> Result<(), String> {
    if let Some(parent) = source_path.parent() {
        tokio::fs::create_dir_all(parent)
            .await
            .map_err(|error| format!("Failed to create dirs '{}': {}", parent.display(), error))?;
    }

    let mut source = match tokio::fs::read_to_string(source_path).await {
        Ok(source) => source,
        Err(error) if error.kind() == ErrorKind::NotFound => String::new(),
        Err(error) => {
            return Err(format!(
                "Failed to read '{}': {}",
                source_path.display(),
                error
            ));
        }
    };

    let existing_modules = source
        .lines()
        .filter_map(|line| {
            let trimmed = line.trim();
            let module = trimmed.strip_prefix("pub mod ")?;
            let module = module.strip_suffix(';')?.trim();

            if module.is_empty() || module.contains(' ') {
                return None;
            }

            Some(module.to_string())
        })
        .collect::<BTreeSet<_>>();

    let missing_modules = children
        .iter()
        .filter(|module| !existing_modules.contains(*module))
        .collect::<Vec<_>>();

    if missing_modules.is_empty() {
        return Ok(());
    }

    if !source.is_empty() && !source.ends_with('\n') {
        source.push('\n');
    }

    for module in missing_modules.into_iter() {
        source += format!("pub mod {};\n", module).as_str();
    }

    tokio::fs::write(source_path, source)
        .await
        .map_err(|error| format!("Failed to write '{}': {}", source_path.display(), error))?;

    Ok(())
}
