use catla_parser::parser::Program;
use fxhash::FxHashMap;

use crate::transpiler::{component::EntityID, name_resolver::FoundDefineInfo};

pub struct LastUsePositions {
    last_user_map: FxHashMap<EntityID, EntityID>,
}

pub struct LastUses {
    is_in_loop: bool,

}

pub fn collect_last_use(
    ast: Program,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
) -> LastUsePositions {
    todo!()
}
