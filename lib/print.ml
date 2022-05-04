open PPrint
open OCaml
open Ast

let ( ^-^ ) a b = a ^^ space ^^ b

let arrow = !^"->"

let rec value = function
  | VInt i ->
      int i
  | VBool b ->
      bool b
  | VUnit ->
      !^"()"
  | VString str ->
      string str
  | VCons cons ->
      !^cons
  | VArray arr ->
      group
      @@ brackets
           ( arr |> Array.to_list
           |> separate_map (break 0 ^^ semi ^^ space) value )
  | VFunc {env= _; args; body= _} ->
      parens (!^"fun" ^-^ separate_map space ( !^ ) args ^^ arrow ^-^ !^"...")
  | VPrFunc (name, _func) ->
      !^name

let primitive = function
  | PrString str ->
      OCaml.string str
  | PrInt n ->
      OCaml.int n
  | PrBool b ->
      !^(if b then "true" else "false")

let nest_break doc = nest 2 (break 1 ^^ doc)

let rec pattern p =
  group
  @@
  match p with
  | PAny ->
      underscore
  | PPrim pr ->
      primitive pr
  | PVar ident ->
      !^ident
  | PTuple ps ->
      parens (separate_map (comma ^^ break 1) pattern ps)
  | PCons {cons; payload} -> (
      !^cons
      ^^
      match payload with
      | [] ->
          empty
      | _ :: _ ->
          parens (separate_map (comma ^^ break 1) pattern payload) )

let rec expr ?(context = false) e =
  group
  @@
  match e with
  | EVar ident ->
      !^ident
  | EFunc {args; body} ->
      (if context then parens else Fun.id)
      @@ !^"fun"
      ^-^ separate_map space ( !^ ) args
      ^-^ arrow
      ^^ nest_break (expr body)
  | EApply {func; args} ->
      (if context then parens else Fun.id)
      @@
      let context = true in
      expr ~context func
      ^^ group (break 1 ^^ separate_map (break 1) (expr ~context) args)
  | EAlloc n ->
      group @@ !^"alloc" ^^ nest 2 (break 1 ^^ expr ~context:true n)
  | EPrim p ->
      primitive p
  | EProj {arr; index} ->
      let context = true in
      expr ~context arr ^^ dot ^^ expr ~context index
  | EWrite {arr; index; value} ->
      (if context then parens else Fun.id)
      @@
      let context = true in
      expr ~context arr ^^ dot ^^ expr ~context index ^-^ !^"<-" ^-^ expr value
  | EMatch
      { arg= cond
      ; branches=
          [(PPrim (PrBool true), body_if); (PPrim (PrBool false), body_else)] }
    ->
      group (!^"if" ^^ (nest_break @@ expr cond) ^/^ !^"then")
      ^^ (nest_break @@ expr body_if)
      ^/^ !^"else" ^^ nest_break @@ expr body_else
  | EUnit ->
      !^"()"
  | ECons {cons; payload} -> (
      (if context then parens else Fun.id)
      @@ !^cons
      ^^
      match payload with
      | [] ->
          empty
      | [e] ->
          expr ~context:true e
      | _ :: _ :: _ ->
          parens (separate_map (comma ^^ space) expr payload) )
  | ELet {var; value; is_rec; body_in} ->
      (*if var = "_" then expr value ^-^ semi ^/^ expr body_in
        else*)
      group
        ( !^"let"
        ^-^ (if is_rec then !^"rec" ^^ space else empty)
        ^^ !^var ^-^ equals
        ^^ nest_break (expr value)
        ^/^ !^"in" )
      ^/^ expr body_in
  | EMatch {arg; branches} ->
      (if context then parens else Fun.id)
      @@ group (!^"match" ^^ nest_break (expr arg) ^/^ !^"with")
      ^^ concat_map
           (fun (p, e) ->
             (break 1 ^^ !^"|" ^-^ pattern p ^-^ arrow) ^^ nest_break (expr e)
             )
           branches
  | EPrimFunc (name, _) ->
      !^"primitive" ^^ braces !^name

let si = function
  | Binding {name; is_rec; body} ->
      group
        ( !^"let"
        ^-^ (if is_rec then !^"rec" ^^ space else empty)
        ^^ !^name ^-^ equals
        ^^ nest_break (expr body) )
  | MutualRecBindings bds -> (
    match bds with
    | [] ->
        empty
    | (ident, body) :: bds ->
        group (!^"let rec" ^-^ !^ident ^-^ equals ^^ nest_break (expr body))
        ^^ concat_map
             (fun (ident, body) ->
               hardline ^^ hardline
               ^^ group
                    (!^"and" ^-^ !^ident ^-^ equals ^^ nest_break (expr body))
               )
             bds )

let program pr = separate_map (hardline ^^ hardline) si pr

let env e =
  Env.bindings e
  |> separate_map hardline (fun (name, v) -> !^name ^-^ equals ^-^ value v)

let to_channel f channel args =
  let doc = f args in
  ToChannel.pretty 0.8 80 channel doc ;
  output_string channel "\n"

let to_string f arg =
  let doc = f arg in
  let buffer = Buffer.create 10 in
  ToBuffer.pretty 0.8 80 buffer doc ;
  Buffer.contents buffer

let print_value = to_channel value

let string_of_value = to_string value

let print_env = to_channel env

let string_of_env = to_string env

let print_expr = to_channel expr

let string_of_expr = to_string expr

let print_si = to_channel si

let string_of_si = to_string si

let print_program = to_channel program

let string_of_program = to_string program