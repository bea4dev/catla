use resource::resource_str;
use toml::Table;


pub(crate) struct LocalizedText {
    lang: String,
    toml_table: Table
}

impl LocalizedText {

    pub(crate) fn new(lang: &str) -> LocalizedText {
        let toml_str = match lang {
            "ja_JP" => resource_str!("localize/ja_JP.toml"),
            "en_US" => resource_str!("localize/en_US.toml"),
            _ => resource_str!("localize/en_US.toml")
        };
        return Self {
            lang: lang.to_string(),
            toml_table: toml_str.parse::<Table>().unwrap()
        };
    }
    
    pub(crate) fn get_text<T: ToString + Clone>(&self, key: T) -> String {
        return match self.get_text_optional(key.clone()) {
            Some(text) => text.to_string(),
            _ => format!("[Unknown text. | lang : {} | : key : {}]", self.lang, key.to_string())
        };
    }

    pub(crate) fn get_text_optional<T: ToString>(&self, key: T) -> Option<String> {
        let key = key.to_string();
        let mut keys = key.split(".").collect::<Vec<_>>();
        let mut last_key = keys.pop();

        let mut current_table = &self.toml_table;
        for key in keys {
            current_table = match current_table.get(key).map(|value| { value.as_table() }).flatten() {
                Some(table) => table,
                _ => { last_key = None; break; }
            };
        }

        return last_key.map(|key| { current_table.get(key) })
            .flatten().map(|value| { value.as_str() })
            .flatten().map(|value| { value.to_string() });
    }

}
