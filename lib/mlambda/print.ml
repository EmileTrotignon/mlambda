open PPrint
open OCaml
open Ast

let or_empty p o = match o with Some v -> p v | None -> empty

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
      group @@ align
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
  | PCons {cons; payload} -> (
      or_empty ( !^ ) cons
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
  | EPrim p ->
      primitive p
  | EApply {func= EVar "alloc"; args= [n]} ->
      group @@ !^"alloc" ^^ nest 2 (break 1 ^^ expr ~context:true n)
  | EApply {func= EVar "proj"; args= [arr; index]} ->
      let context = true in
      expr ~context arr ^^ dot ^^ expr ~context index
  | EApply {func= EVar "write"; args= [arr; index; value]} ->
      (if context then parens else Fun.id)
      @@
      let context = true in
      expr ~context arr ^^ dot ^^ expr ~context index ^-^ !^"<-" ^-^ expr value
  | EApply {func; args} ->
      (if context then parens else Fun.id)
      @@
      let context = true in
      expr ~context func
      ^^ group (break 1 ^^ separate_map (break 1) (expr ~context) args)
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
      @@ or_empty ( !^ ) cons ^^ align
      @@
      match payload with
      | [] ->
          empty
      | [e] ->
          expr ~context:true e
      | _ :: _ :: _ ->
          parens (separate_map (comma ^^ space) expr payload) )
  | EMatch
      { arg= ECons {cons= None; payload= payload_e}
      ; branches= [(PCons {cons= None; payload= payload_p}, body_in)] }
    when List.length payload_p = List.length payload_e ->
      group
        ( !^"let"
        ^^ separate_map
             (break 1 ^^ !^"and")
             (fun (p, e) -> space ^^ pattern p ^-^ equals ^^ nest_break (expr e))
             (List.combine payload_p payload_e)
        ^/^ !^"in" )
      ^/^ expr_semin body_in
  | EMatch {arg; branches= [(PAny, e)]} ->
      group (expr_semin arg ^-^ semi ^/^ expr e)
  | EMatch {arg= value; branches= [(p, body_in)]} ->
      group
        (!^"let" ^-^ pattern p ^-^ equals ^^ nest_break (expr value) ^/^ !^"in")
      ^/^ expr_semin body_in
  | EMatch {arg; branches} ->
      (if context then parens else Fun.id)
      @@ group (!^"match" ^^ nest_break (expr arg) ^/^ !^"with")
      ^^ concat_map
           (fun (p, e) ->
             break 1
             ^^ group (!^"|" ^-^ pattern p ^-^ arrow ^^ nest_break (expr e)) )
           branches
  | EPrimFunc (name, _) ->
      !^"primitive" ^^ braces !^name

and expr_semin e =
  match e with
  | EMatch {arg; branches= [(PAny, e)]} ->
      expr_semin arg ^-^ semi ^/^ expr_semin e
  | EMatch
      { arg= ECons {cons= None; payload= payload_e}
      ; branches= [(PCons {cons= None; payload= payload_p}, body_in)] }
    when List.length payload_p = List.length payload_e ->
      ( List.combine payload_p payload_e
      |> List.mapi (fun i (p, e) ->
             let is_first = i = 0 in
             (if not is_first then break 1 else empty)
             ^^ group
                  ( !^(if i = 0 then "let" else "and")
                  ^^ space ^^ pattern p ^-^ equals
                  ^^ nest_break (expr e) ) )
      |> concat )
      (*
      ( !^"let"
      ^^ separate_map
           (break 1 ^^ !^"and")
           (fun (p, e) -> space ^^ pattern p ^-^ equals ^^ nest_break (expr e))
           (List.combine payload_p payload_e) *)
      ^/^ !^"in"
      ^/^ expr_semin body_in
  | EMatch {arg= value; branches= [(p, body_in)]} ->
      group
        (!^"let" ^-^ pattern p ^-^ equals ^^ nest_break (expr value) ^/^ !^"in")
      ^/^ expr_semin body_in
  | _ ->
      expr e

let expr e = expr e

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

let to_format f fmt arg = Format.fprintf fmt "%s" (to_string f arg)
(*let doc = f arg in
  ToFormatter.pretty 0.8 80 fmt doc*)

module type Printer = sig
  type a

  val output : out_channel -> a -> unit

  val print : a -> unit

  val to_string : a -> string

  val pp : Format.formatter -> a -> unit
end

module Export (P : sig
  type t

  val printer : t -> document
end) : Printer with type a = P.t = struct
  type a = P.t

  let output = to_channel P.printer

  let print = output stdout

  let to_string = to_string P.printer

  let pp = to_format P.printer
end

module Value = Export (struct
  type t = value

  let printer = value
end)

module Env = Export (struct
  type t = value Env.t

  let printer = env
end)

module Expr = Export (struct
  type t = expr

  let printer = expr
end)

module Pattern = Export (struct
  type t = pattern

  let printer = pattern
end)

module Struct_item = Export (struct
  type t = struct_item

  let printer = si
end)

module Program = Export (struct
  type t = program

  let printer = program
end)