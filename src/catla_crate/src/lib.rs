use semver::Version;

#[derive(Debug)]
pub struct CrateInfoSet {
    pub crates: Vec<CrateInfo>,
}

impl CrateInfoSet {
    pub fn new() -> Self {
        Self { crates: Vec::new() }
    }
}

#[derive(Debug)]
pub struct CrateInfo {
    pub name: String,
    pub version: Version,
    pub dependencies: Vec<Dependency>,
}

#[derive(Debug)]
pub struct Dependency {
    pub name: String,
    pub version: Version,
}

