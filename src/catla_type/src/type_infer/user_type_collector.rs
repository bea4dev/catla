use catla_name_resolver::ResolvedInfo;
use catla_parser::ast::{EntityID, Program};
use hashbrown::HashMap;

use crate::types::Type;

pub fn collect_user_type_for_program(
    ast: &Program,
    static_type_map: &mut HashMap<EntityID, Type>,
    name_resolved: &HashMap<EntityID, ResolvedInfo>,
) {
}
