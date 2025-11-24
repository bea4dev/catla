use catla_crate::CrateInfo;

use crate::CodegenSettings;

pub async fn codegen_cargo_toml(
    crate_info: &CrateInfo,
    settings: &CodegenSettings,
) -> Result<(), String> {
    let mut out_path = settings.out_dir.clone();

    if crate_info.name.as_str() == "std" {
        out_path.push("catla_std");
    } else {
        out_path.push(&crate_info.name);
    }

    out_path.push("Cargo.toml");

    let mut code = String::new();

    code += "[package]\n";

    let name = if crate_info.name.as_str() == "std" {
        "catla_std"
    } else {
        crate_info.name.as_str()
    };

    code += format!("name = \"{}\"\n", name).as_str();
    code += format!("version = \"{}\"\n", crate_info.version.to_string()).as_str();
    code += "edition = \"2024\"\n";
    code += "\n";

    code += "[dependencies]\n";

    for dependency in crate_info.dependencies.iter() {
        let name = if dependency.name.as_str() == "std" {
            "catla_std"
        } else {
            dependency.name.as_str()
        };

        code += format!(
            "{} = {{ path = \"../{}\", version = \"{}\" }}",
            name,
            name,
            dependency.version.to_string()
        )
        .as_str();
    }

    if let Some(parent) = out_path.parent() {
        tokio::fs::create_dir_all(parent)
            .await
            .map_err(|_| "Failed to create dirs.".to_string())?;
    }

    tokio::fs::write(out_path, code)
        .await
        .map_err(|error| format!("Failed to write Cargo.toml : {}", error))?;

    Ok(())
}
