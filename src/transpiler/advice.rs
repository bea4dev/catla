use std::{collections::BTreeMap, ops::Range};

use ariadne::{sources, Color, Label, Report, ReportKind, Source};
use indexmap::IndexMap;

use super::context::TranspileModuleContext;

pub(crate) struct AdviceReport {
    elements: IndexMap<String, Vec<Advice>>
}

impl AdviceReport {

    pub(crate) fn new() -> AdviceReport {
        return Self { elements: IndexMap::new() }
    }

    pub(crate) fn add_advice(&mut self, module_name: String, advice: Advice) {
        let elements = self.elements.entry(module_name).or_insert_with(|| vec![]);
        elements.push(advice);
    }
    
    pub(crate) fn print(&self, context: &TranspileModuleContext, offset: usize) {
        if self.elements.is_empty() {
            return;
        }

        let text = &context.context.localized_text;

        let advice_color = Color::RGB(52, 226, 226);

        let mut builder = Report::build(ReportKind::Custom("  > Advice", advice_color), &context.module_name, offset)
            .with_message(text.get_text("advice.message"));

        let mut source_list = Vec::new();

        for (module_name, elements) in self.elements.iter() {
            let module_context = context.context.get_module_context(module_name).unwrap();
            let original_source = &module_context.source_code.code;
            let mut add_elements = BTreeMap::new();

            for advice in elements.iter() {
                match advice {
                    Advice::Add { add, position, message_override } => {
                        let position = *position;

                        let is_prev_space = match original_source.get((position - 1)..position) {
                            Some(prev) => prev == " ",
                            None => false
                        };
                        let is_current_space = match original_source.get(position..(position + 1)) {
                            Some(current) => current == " ",
                            None => false
                        };
                        let prev_space = if is_prev_space { "" } else { " " };
                        let next_space = if is_current_space { "" } else { " " };

                        let add = format!("{}{}{}", prev_space, add, next_space);

                        add_elements.insert(position, Advice::Add { add, position, message_override: message_override.clone() });
                    },
                    Advice::Remove { span } => {
                        add_elements.insert(span.start, Advice::Remove { span: span.clone() });
                    }
                }
            }

            let mut advice_span = usize::MAX..0;
            for element in elements.iter() {
                let span = match element {
                    Advice::Add { add: _, position, message_override: _ } => *position..*position,
                    Advice::Remove { span } => span.clone()
                };
                advice_span.start = advice_span.start.min(span.start);
                advice_span.end = advice_span.end.max(span.end);
            }

            const CLEARANCE: usize = 50;
            /*
            advice_span.start = advice_span.start.checked_sub(CLEARANCE).unwrap_or(0);
            loop {
                if original_source.is_char_boundary(advice_span.start) {
                    break;
                }
                advice_span.start -= 1;
            }*/
            advice_span.start = 0;

            advice_span.end = original_source.len().min(advice_span.end + CLEARANCE);
            loop {
                if original_source.is_char_boundary(advice_span.end) {
                    break;
                }
                advice_span.end += 1;
            }

            let span_source = &original_source[advice_span.clone()];
            
            let mut span_source_position = 0;
            let mut advice_source_position = 0;
            let mut new_source = String::new();
            for (&position, advice) in add_elements.iter() {
                let advice_position = position - advice_span.start;
                let prev_source = &span_source[span_source_position..advice_position];
                new_source += prev_source;

                advice_source_position += advice_position - span_source_position;

                match advice {
                    Advice::Add { add, position: _, message_override } => {
                        let advice_span_end = advice_source_position + add.len();
                        let message_key = message_override.unwrap_or("advice.add_label");

                        let advice_label_span_start = advice_source_position + floor_whitespace(add, 0);
                        let advice_label_span_end = advice_span_end - (add.len() - ceil_whitespace(add, add.len()));

                        builder.add_label(
                            Label::new((module_name.clone(), advice_label_span_start..advice_label_span_end))
                                .with_message(text.get_text(message_key))
                                .with_color(advice_color)
                        );
                        
                        advice_source_position = advice_span_end;
                        new_source += add.as_str();
                    },
                    Advice::Remove { span } => {
                        builder.add_label(
                            Label::new((module_name.clone(), advice_source_position..(advice_source_position + (span.len()))))
                                .with_message(text.get_text("advice.remove_label"))
                                .with_color(Color::RGB(252, 233, 79))
                        )
                    },
                }

                span_source_position = advice_position;
            }
            new_source += &span_source[span_source_position..span_source.len()];

            source_list.push((module_context.module_name.clone(), new_source));
        }
        
        builder.set_note(text.get_text("advice.note"));

        builder.finish().print(sources(source_list)).unwrap();
    }

}


pub enum Advice {
    Add { add: String, position: usize, message_override: Option<&'static str> },
    Remove { span: Range<usize> }
}


pub(crate) fn get_column(source: &str, index: usize) -> usize {
    let source = Source::from(source);
    let line_info = source.get_offset_line(index);
    return if let Some(column) = line_info {
        column.2
    } else {
        0
    };
}

pub(crate) fn create_space_indent(amount: usize) -> String {
    let mut str = String::new();
    for i in 0..amount {
        str += " ";
    }
    return str;
}

pub(crate) fn floor_whitespace(source: &str, mut position: usize) -> usize {
    let chars = source[position..].chars();
    for char in chars {
        if !char.is_whitespace() {
            break;
        }
        position += char.len_utf8();
    }
    return position;
}

pub(crate) fn ceil_whitespace(source: &str, mut position: usize) -> usize {
    let mut chars = source[..position].chars().collect::<Vec<_>>();
    chars.reverse();
    for char in chars {
        if !char.is_whitespace() {
            break;
        }
        position -= char.len_utf8();
    }
    return position;
}