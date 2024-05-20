use bnf_rules::bnf_rules_macro::bnf_rules;
use proc_macro_regex::regex;

// This is an LR(1) parser generator, used for maintain quality.
// If the specified grammar is ambiguous, compilation is aborted with conflict.
// Usage : https://github.com/bea4dev/bnf_rules
bnf_rules!{
    source              ::= program

    program             ::= [ statement ] { end_of_statement [ statement ] }
    statement           ::= assignment | exchange_statement | import_statement |
                            define_with_attr | drop_statement | expression | impl_interface

    define_with_attr    ::= statement_attribute ( function_define | user_type_define | variable_define )

    function_define     ::= "function" [ generics_define ] ( literal | memory_manage_attr ) function_arguments
                            [ function_type_tag ] [ line_feed ] [ where_clause ] block
    function_arguments  ::= "(" [ line_feed ] [ function_argument [ line_feed ] ] { "," [ line_feed ] [ function_argument ] } ")"
    function_argument   ::= literal type_tag

    where_clause        ::= "where" [ line_feed ] [ where_element ] { "," [ line_feed ] [ where_element ] }
    where_element       ::= type_info [ line_feed ] [ ":" [ line_feed ] type_info [ line_feed ] { "+" [ line_feed ] type_info [ line_feed ] } ]

    memory_manage_attr  ::= "new" | "drop" | "mutex"

    statement_attribute ::= { "static" | "private" | "suspend" | "native" | "acyclic" | "open" }

    user_type_define    ::= ( "class" | "struct" | "interface" ) literal [ generics_define [ line_feed ] ]
                            [ super_type_info ] [ where_clause ] block
    super_type_info     ::= ":" [ line_feed ] type_info [ line_feed ] { "," [ type_info [ line_feed ] ] }
    
    impl_interface      ::= "implements" [ generics_define ] type_info [ line_feed ] "for" [ line_feed ] type_info
                            [ line_feed ] [ where_clause ] block
    
    generics_define     ::= "<" [ line_feed ] [ generics_element ] { "," [ line_feed ] [ generics_element ] } ">"
    generics_element    ::= literal [ line_feed ] [ ":" [ line_feed ] type_info [ line_feed ] { "+" [ line_feed ] type_info [ line_feed ] } ]

    import_statement    ::= "import" literal { "::" [ line_feed ] ( literal | import_elements ) }
    import_elements     ::= "{" [ line_feed ] ( [ literal [ line_feed ] ] { "," [ line_feed ] [ literal [ line_feed ] ] } | "*" ) "}"

    drop_statement      ::= "drop" [ "acyclic" ] expression

    block               ::= "{" program "}"

    variable_define     ::= ( "let" | "var" ) literal [ type_tag ] [ "=" [ line_feed ] expression ]

    assignment          ::= expression "=" [ line_feed ] expression

    exchange_statement  ::= expression "<=>" [ line_feed ] expression

    expression          ::= or_expr | return_expression | closure
    or_expr             ::= and_expr { "or" [ line_feed ] and_expr }
    and_expr            ::= equ_or_ine_expr { "and" [ line_feed ] equ_or_ine_expr }
    equ_or_ine_expr     ::= les_or_gre_expr { ( "==" | "!=" ) [ line_feed ] les_or_gre_expr }
    les_or_gre_expr     ::= add_or_sub_expr { ( "<" | ">" | "<=" | ">=" ) [ line_feed ] add_or_sub_expr }
    add_or_sub_expr     ::= mul_or_div_expr { ( "+" | "-" ) [ line_feed ] mul_or_div_expr }
    mul_or_div_expr     ::= factor { ( "*" | "/" ) [ line_feed ] factor }
    factor              ::= "-" [ line_feed ] primary | primary
    primary             ::= primary_left { primary_right }
    primary_left        ::= ( simple_primary [ ":" generics_info ] [ function_call ] | new_expression | if_expression | loop_expression ) [ mapping_operator ]
    primary_right       ::= ( "." | "::" ) [ line_feed ] ( literal [ ":" generics_info ] [ function_call ] | mapping_operator )
    simple_primary      ::= "(" expression ")" | literal | "null" | "true" | "false"
    mapping_operator    ::= "?" | "?" "!" | "!" | "!" "!" | ( "?" | "!" ) ":" block

    if_expression       ::= if_statement { "else" ( if_statement | block ) }
    if_statement        ::= "if" expression block

    loop_expression     ::= "loop" block

    closure             ::= ( closure_args | literal ) "=>" ( expression | block )
    closure_args        ::= "|" [ [ line_feed ] function_argument [ line_feed ] ] { "," [ line_feed ] [ function_argument [ line_feed ] ] } "|"

    function_call       ::= "(" [ [ line_feed ] expression [ line_feed ] ] { "," [ line_feed ] [ expression [ line_feed ] ] } ")"

    new_expression      ::= "new" [ "acyclic" ] literal { "::" [ line_feed ] literal } field_assign
    field_assign        ::= "{" [ [ line_feed ] literal ":" [ line_feed ] expression [ line_feed ] ] { "," [ line_feed ] [ literal ":" [ line_feed ] expression [ line_feed ] ] } "}"

    return_expression   ::= "return" [ expression ]

    type_tag            ::= ":" type_info
    function_type_tag   ::= "->" type_info
    type_info           ::= literal { "::" [ line_feed ] literal } [ generics_info ] { type_attribute }
    type_attribute      ::= "?" | ( "!" [ generics_info ] )
    generics_info       ::= "<" [ line_feed ] [ type_info [ line_feed ] ] { "," [ line_feed ] [ type_info [ line_feed ] ] } ">"

    literal             ::= fn (literal_tokenizer) // r"\w+"
    end_of_statement    ::= line_feed | ";"
    line_feed           ::= fn (line_feed_tokenizer) // r"\n+"
}

regex!(pub number_literal_regex r"^\d+(\.\d+)?$");


fn literal_tokenizer(source: &Vec<char>, mut current_position: usize) -> usize {
    let mut iteration_count = 0;
    loop {
        let current_char = match source.get(current_position) {
            Some(ch) => ch.clone(),
            _ => break
        };
        if !(current_char == '_' || current_char.is_alphanumeric()) {
            break;
        }
        iteration_count += 1;
        current_position += 1;
    }
    return iteration_count; 
}

fn line_feed_tokenizer(source: &Vec<char>, mut current_position: usize) -> usize {
    let mut iteration_count = 0;
    loop {
        let current_char = match source.get(current_position) {
            Some(ch) => ch.clone(),
            _ => break
        };
        if !(current_char == '\n' || current_char == '\r' ) {
            break;
        }
        iteration_count += 1;
        current_position += 1;
    }
    return iteration_count;
}