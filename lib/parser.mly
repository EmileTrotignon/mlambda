%{ (* -*- tuareg -*- *)
open Ast
open Builder

%}
%token<string> LOWERCASE_ID
%token<string> UPPERCASE_ID
%token<int> INT
%token<string> STRING
%token<bool> BOOL

%token LET REC IN
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
| cons=cons; LPAR; payload=separated_list(COMMA, pattern); RPAR; {PCons {cons; payload}}
| cons=cons; {PCons{cons; payload=[]}}

let atomic_pattern :=
| LPAR; ~=pattern; RPAR; <>
| LPAR; ~=separated_twolong_list(COMMA, pattern); RPAR; < PTuple >
| ~=primitive; < PPrim >
| ~=ident; < PVar >

let expr :=
| ~=seq_expr; <>
| ~=let_expr; <>
| ~=fun_expr; <>

let let_expr :=
| LET; var=ident; EQUAL; value=expr; IN; body_in=expr; { ELet {var; is_rec=false; value; body_in} }

let seq_expr :=
| e1=fun_expr; SEMI; e2=expr; { e_seq e1 e2 }

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
| arr=proj_expr; DOT; LPAR; index=proj_expr; RPAR; LARROW; value=if_expr; { EWrite {arr; index; value} }

let if_expr :=
| ~=cons_expr; <>
| IF; cond=if_expr; THEN; then_=if_expr; ELSE; else_=cons_expr; { e_if cond ~then_ ~else_ }

let cons_expr :=
| ~=apply_expr; <>
| cons=cons; { ECons {cons; payload=[]} }
| cons=cons; LPAR; payload=separated_nonempty_list(COMMA, expr); RPAR; { ECons {cons; payload} }

let apply_expr :=
| ~=proj_expr; <>
| func=not_cons_expr; args=nonempty_list(proj_expr); { EApply {func; args} }
(*| cons=cons; arg=atomic_expr; { ECons {cons; payload=[arg]} }*)
| ALLOC; ~=proj_expr; <EAlloc>

let not_cons_expr :=
| ~=ident; <EVar>
| LPAR; ~=expr; RPAR; <>

let proj_expr :=
| ~=paren_expr; <>
| arr=proj_expr; DOT; LPAR; index=proj_expr; RPAR; { EProj {arr; index} }

let paren_expr :=
| ~=atomic_expr; <>
| LPAR; ~=expr; RPAR; <>

let atomic_expr :=
| LPARRPAR; { EUnit }
| ~=primitive; <EPrim>
| ~=ident; <EVar>

struct_item:
| LET REC name=ident EQUAL body=expr { Binding { name; is_rec=true; body } }
| LET name=ident EQUAL body=expr { Binding { name; is_rec=true; body } }

let program :=
| ~=list(struct_item); EOF; <>