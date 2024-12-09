use std::{
    fs::{self, File},
    io::{self, Write},
    path::Path,
};

use crate::transpiler::context::TranspileContext;

pub fn generate_cargo_toml(context: &TranspileContext) -> io::Result<()> {
    for entry in context.source_code_provider.get_entries() {
        let entry = if entry.as_str() == "std" {
            "catla_std".to_string()
        } else {
            entry
        };

        let path = format!("{}/{}/Cargo.toml", &context.settings.codegen_dir, &entry,);

        let dir = Path::new(path.as_str());

        fs::create_dir_all(dir.parent().unwrap())?;

        let mut file = File::create(path)?;

        file.write_all("[package]\n".as_bytes())?;
        file.write_all(format!("name = \"{}\"\n", &entry).as_bytes())?;
        file.write_all("version = \"0.0.1\"\n".as_bytes())?;
        file.write_all("edition = \"2021\"\n\n".as_bytes())?;

        //file.write_all("[workspace]\n".as_bytes())?;
        //file.write_all("members = [ \"catla_std\", \"catla_transpile_std\" ]\n\n".as_bytes())?;

        file.write_all("[dependencies]\n".as_bytes())?;
        file.write_all("catla_std = { path = \"../catla_std\" }\n".as_bytes())?;
        file.write_all(
            "catla_transpile_std = { path = \"../../catla_transpile_std\" }\n".as_bytes(),
        )?;
    }

    Ok(())
}
