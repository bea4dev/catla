use catla_import::resource::{PackageResource, PackageResourceSet};
use catla_type::types::GlobalUserTypeSet;
use catla_util::{future::MultiTaskFuture, source_code::SourceCode};
use tokio::runtime::{Builder, Runtime};

use crate::settings::CatlaCompilerSettings;

pub struct CatlaCompiler {
    pub package_resource_set: PackageResourceSet,
    pub settings: CatlaCompilerSettings,
    pub user_type_set: GlobalUserTypeSet,
    runtime: Runtime,
    phases: CompilePhases,
}

impl CatlaCompiler {
    pub fn new(package_resource_set: PackageResourceSet, settings: CatlaCompilerSettings) -> Self {
        Self {
            package_resource_set,
            settings: settings.clone(),
            user_type_set: GlobalUserTypeSet::new(),
            runtime: Builder::new_multi_thread()
                .worker_threads(settings.threads)
                .build()
                .unwrap(),
            phases: CompilePhases::new(),
        }
    }

    pub fn compile(&self) {
        self.runtime.block_on(async {
            self.compile_inner().await;
        });
    }

    async fn compile_inner(&self) {
        let resouces = self.package_resource_set.get_all();

        for resource in resouces.into_values() {
            if let PackageResource::Module { source_code } = resource {
                self.phases.parse_and_name_resolve.mark_as_started();

                self.runtime.spawn(async {
                    self.compile_module(source_code).await;
                });
            }
        }

        self.phases.parse_and_name_resolve.future().await;
    }

    async fn compile_module(&self, source_code: SourceCode) {
        self.phases.parse_and_name_resolve.mark_as_finished().await;
    }
}

pub struct CompilePhases {
    pub parse_and_name_resolve: MultiTaskFuture,
}

impl CompilePhases {
    pub fn new() -> Self {
        Self {
            parse_and_name_resolve: MultiTaskFuture::new(),
        }
    }
}
