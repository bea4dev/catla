use bnf_rules::bnf_rules_macro::bnf_rules;

// This is an LR(1) parser generator, used for maintain quality.
// If the specified grammar is ambiguous, compilation is aborted with conflict.
// Usage : https://github.com/bea4dev/bnf_rules
bnf_rules! {
    #[generate_code = false]

    source              ::= program

    program             ::= [ statement ] { end_of_statement [ statement ] }

    statement           ::= documents { transpiler_tag } (
                                assignment
                                | swap_statement
                                | import_statement
                                | define_with_attr
                                | drop_statement
                                | expression
                                | impl_interface
                            )

    documents           ::= { r"///[^\n\r]*(\n|\r|\r\n)" }
    // comments         ::= r"//[^\n\r]*" | r"/\*.*\*/"  /* ignored in lexer */

    define_with_attr    ::= statement_attribute ( function_define | user_type_define | variable_define | type_alias )

    transpiler_tag      ::= "#" "[" literal "]"

    function_define     ::= "function" [ generics_define ] ( literal | memory_manage_attr ) function_arguments
                            [ function_type_tag ] [ line_feed ] [ where_clause ] ( block | ";" )

    function_arguments  ::= "(" [ line_feed ] [ ( this_mutability | function_argument ) [ line_feed ] ]
                            { "," [ line_feed ] [ function_argument ] } ")"

    this_mutability     ::= ( "var" | "let" ) "this"
    function_argument   ::= variable_binding type_tag

    where_clause        ::= "where" [ line_feed ] [ where_element ] { "," [ line_feed ] [ where_element ] }
    where_element       ::= type_info [ ":" type_info { "+" type_info } ]

    memory_manage_attr  ::= "new" | "drop" | "mutex"

    statement_attribute ::= { "static" | "private" | "suspend" | "native" | "acyclic" | "open" | "override" }

    user_type_define    ::= ( "class" | "struct" | "interface" ) literal [ generics_define [ line_feed ] ]
                            [ super_type_info ] [ where_clause ] block

    super_type_info     ::= ":" [ line_feed ] [ type_info ] { "," [ type_info ] }

    impl_interface      ::= "implements" [ generics_define ] type_info [ line_feed ] "for" [ line_feed ] type_info
                            [ line_feed ] [ where_clause ] block

    type_alias          ::= "type" literal [ generics_define ] "=" type_info

    generics_define     ::= "<" [ line_feed ] [ generics_element ] { "," [ line_feed ] [ generics_element ] } ">"
    generics_element    ::= literal [ ":" type_info { "+" type_info } ]

    import_statement    ::= "import" literal { "::" [ line_feed ] ( literal | import_elements ) }

    import_elements     ::= "{" [ line_feed ] ( [ literal [ line_feed ] ]
                            { "," [ line_feed ] [ literal [ line_feed ] ] } | "*" ) "}"

    drop_statement      ::= "drop" [ "acyclic" ] expression

    block               ::= "{" program "}"

    variable_define     ::= ( "let" | "var" ) variable_binding [ type_tag ] [ "=" [ line_feed ] expression ]

    variable_binding    ::= literal
                            | "(" [ line_feed ] variable_binding [ line_feed ]
                              { "," [ line_feed ] [ variable_binding [ line_feed ] ] } ")"

    assignment          ::= expression "=" [ line_feed ] expression

    swap_statement      ::= expression "<=>" [ line_feed ] expression

    expression          ::= return_expression | closure | or_expr
    or_expr             ::= and_expr { "or" [ line_feed ] and_expr }
    and_expr            ::= equ_or_ine_expr { "and" [ line_feed ] equ_or_ine_expr }
    equ_or_ine_expr     ::= les_or_gre_expr [ ( "==" | "!=" ) [ line_feed ] les_or_gre_expr ]
    les_or_gre_expr     ::= add_or_sub_expr [ ( "<" | ">" | "<=" | ">=" ) [ line_feed ] add_or_sub_expr ]
    add_or_sub_expr     ::= mul_or_div_expr { ( "+" | "-" ) [ line_feed ] mul_or_div_expr }
    mul_or_div_expr     ::= factor { ( "*" | "/" ) [ line_feed ] factor }
    factor              ::= "-" [ line_feed ] primary | primary
    primary             ::= primary_left { primary_right }
    primary_left        ::= (
                                simple_primary [ ":" generics_info ] [ function_call ]
                                | new_array_init_expr
                                | new_array_expr
                                | new_expression
                                | if_expression
                                | loop_expression
                            )
                            [ mapping_operator ]

    primary_right       ::= ( "." | "::" | r"(\n|\r)+\." | r"(\n|\r)+::" )
                            [ literal [ ":" generics_info ] [ function_call ] ] [ mapping_operator ]

    simple_primary      ::= "(" [ line_feed ] expression [ line_feed ] { "," [ line_feed ] [ expression [ line_feed ] ] } ")"
                            | literal
                            | string_literal
                            | "null"
                            | "true"
                            | "false"
                            | "this"
                            | "This"

    mapping_operator    ::= "?"
                            | "?" "!"
                            | "!"
                            | "!" "!"
                            | ( "?:" | "!:" ) block

    if_expression       ::= if_statement { "else" ( if_statement | block ) }
    if_statement        ::= "if" expression block

    loop_expression     ::= "loop" block

    closure             ::= ( closure_args | literal ) "=>" ( expression | block )
    closure_args        ::= "|"
                                [ [ line_feed ] ( function_argument | literal ) [ line_feed ] ]
                                { "," [ line_feed ] [ ( function_argument | literal ) [ line_feed ] ] }
                            "|"

    function_call       ::= "(" [ [ line_feed ] expression [ line_feed ] ]
                            { "," [ line_feed ] [ expression [ line_feed ] ] } ")"

    new_expression      ::= "new" [ "acyclic" ] ( literal | "This" ) { "::" literal } [ line_feed ] field_assign
    field_assign        ::= "{"
                                [ [ line_feed ] literal ":" expression { ( "," | line_feed ) [ literal ":" expression ] } ]
                            "}"

    new_array_expr      ::= "new" [ "acyclic" ] "{"
                                [ [ line_feed ] expression [ line_feed ] ]
                                { "," [ line_feed ] [ expression [ line_feed ] ] }
                            "}"

    new_array_init_expr ::= "new" [ "acyclic" ] "["
                            [ line_feed ] [ "for" ] expression [ line_feed ] ";"
                            [ line_feed ] expression [ line_feed ] "]"

    return_expression   ::= "return" [ expression ]

    type_tag            ::= ":" type_info
    function_type_tag   ::= "->" type_info

    type_info           ::= ( array_type_info | base_type_info | tuple_type_info | "This" ) { type_attribute }
    tuple_type_info     ::= "(" [ line_feed ] type_info [ line_feed ] { "," [ line_feed ] [ type_info [ line_feed ] ] } ")"
    array_type_info     ::= "[" [ line_feed ] type_info [ line_feed ] "]"
    base_type_info      ::= literal { "::" literal } [ generics_info ]
    type_attribute      ::= "?" | ( "!" [ generics_info ] )
    generics_info       ::= "<" [ line_feed ] [ type_info [ line_feed ] ]
                            { "," [ line_feed ] [ type_info [ line_feed ] ] } ">"

    literal             ::= r"\w+"
    end_of_statement    ::= line_feed | ";"
    line_feed           ::= r"(\n|\r)+"

    string_literal      ::= r#""([^"\\]|\\.)*""# | r"'([^'\\]|\\.)*'"
}
