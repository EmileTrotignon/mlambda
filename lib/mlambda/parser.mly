%{ (* -*- tuareg -*- *)
open Ast

%}
%token<string> LOWERCASE_ID
%token<string> UPPERCASE_ID
%token<int> INT
%token<string> STRING
%token<bool> BOOL

%token LET REC AND IN
%token FUN

%token UNDERSCORE

%token IF THEN ELSE
%token MATCH WITH
%token LARROW RARROW

%token ALLOC

%token DOT
%token PIPE
%token EQUAL
%token COMMA
%token SEMI

%token LPARRPAR
%token LPAR
%token RPAR

%token EOF

(* %right SEMI *)

%start<Ast.program> program
%%

let separated_twolong_list(SEP, ELE) :=
 ~=ELE; SEP; ~=separated_nonempty_list(SEP, ELE); < (::) >

let ident ==
| ~=LOWERCASE_ID; <>

let cons ==
| ~=UPPERCASE_ID; <>

let primitive ==
| ~=STRING; <PrString>
| ~=INT; <PrInt>
| ~=BOOL; <PrBool>

let pattern :=
| ~=atomic_pattern; <>
| UNDERSCORE; {PAny}
| cons=cons; LPAR; payload=separated_list(COMMA, pattern); RPAR; {PCons {cons=Some cons; payload}}
| cons=cons; {PCons{cons=Some cons; payload=[]}}

let atomic_pattern :=
| LPAR; ~=pattern; RPAR; <>
| LPAR; payload=separated_twolong_list(COMMA, pattern); RPAR; { Pattern.tuple payload }
| ~=primitive; < PPrim >
| ~=ident; < PVar >

let expr :=
| ~=seq_expr; <>
| ~=let_expr; <>
| ~=fun_expr; <>

let let_expr :=
| LET; bds=separated_list(AND, separated_pair(pattern, EQUAL, expr)); IN; in_=expr; { Expr.let_and bds ~in_ }

let seq_expr :=
| e1=fun_expr; SEMI; e2=expr; { Expr.seq e1 e2 }

let fun_expr :=
| ~=match_expr; <>
| FUN; args=nonempty_list(ident); RARROW; body=match_expr; { EFunc {args; body} }

let match_expr :=
| ~=write_expr; <>
| MATCH; arg=expr; WITH; branches=nonempty_list(branch); { EMatch { arg; branches } }

let branch :=
| PIPE; p=pattern; RARROW; e=write_expr; { (p, e) }

let write_expr :=
| ~=if_expr; <>
| block=proj_expr; DOT; LPAR; i=proj_expr; RPAR; LARROW; to_=if_expr; { Expr.write ~block ~i ~to_ }

let if_expr :=
| ~=cons_expr; <>
| IF; cond=if_expr; THEN; then_=if_expr; ELSE; else_=cons_expr; { Expr.if_ cond ~then_ ~else_ }

let cons_expr :=
| ~=apply_expr; <>
| cons=cons; { ECons {cons=Some cons; payload=[]} }
| cons=cons; LPAR; payload=separated_nonempty_list(COMMA, expr); RPAR; { ECons {cons = Some cons; payload} }

let apply_expr :=
| ~=proj_expr; <>
| func=not_cons_expr; args=nonempty_list(proj_expr); { EApply {func; args} }
(*| cons=cons; arg=atomic_expr; { ECons {cons; payload=[arg]} }*)
| ALLOC; size=proj_expr; { Expr.alloc ~size }

let not_cons_expr :=
| ~=ident; <EVar>
| LPAR; ~=expr; RPAR; <>

let proj_expr :=
| ~=paren_expr; <>
| arr=proj_expr; DOT; LPAR; index=proj_expr; RPAR; { Expr.proj arr index }

let paren_expr :=
| ~=atomic_expr; <>
| LPAR; ~=expr; RPAR; <>
| LPAR; payload=separated_twolong_list(COMMA, expr); RPAR; { Expr.tuple payload }

let atomic_expr :=
| LPARRPAR; { EUnit }
| ~=primitive; <EPrim>
| ~=ident; <EVar>

struct_item:
| LET REC name=ident EQUAL body=expr { Binding { name; is_rec=true; body } }
| LET name=ident EQUAL body=expr { Binding { name; is_rec=true; body } }

let program :=
| ~=list(struct_item); EOF; <>