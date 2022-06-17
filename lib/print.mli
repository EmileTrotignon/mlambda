val print_value : out_channel -> Ast.value -> unit

val string_of_value : Ast.value -> string

val print_env : out_channel -> Ast.value Env.t -> unit

val string_of_env : Ast.value Env.t -> string

val print_expr : out_channel -> Ast.expr -> unit

val string_of_expr : Ast.expr -> string

val print_pattern : out_channel -> Ast.pattern -> unit

val string_of_pattern : Ast.pattern -> string

val print_si : out_channel -> Ast.struct_item -> unit

val string_of_si : Ast.struct_item -> string

val print_program : out_channel -> Ast.program -> unit

val string_of_program : Ast.program -> string
