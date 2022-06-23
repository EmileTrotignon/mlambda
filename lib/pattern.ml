open Ast

type t = Ast.pattern

let%memo rec vars (pat : pattern) =
  match pat with
  | PAny ->
      String.Set.empty
  | PPrim _ ->
      String.Set.empty
  | PVar var ->
      String.Set.singleton var
  | PCons {cons= _; payload} ->
      payload |> List.map vars |> String.Set.unions

let any = PAny

let var ident = PVar ident

let prim prim = PPrim prim

let tuple li = PCons {cons= None; payload= li}

let cons ?(payload = []) cons = PCons {cons; payload}

let rec eq p1 p2 =
  match (p1, p2) with
  | PAny, PAny ->
      true
  | PPrim prim, PPrim prim' ->
      Primitive.(prim = prim')
  | PVar var, PVar var' ->
      String.equal var var'
  | PCons {cons; payload}, PCons {cons= cons'; payload= payload'} ->
      Option.equal String.equal cons cons' && List.equal eq payload payload'
  | _, _ ->
      false

let ( = ) = eq
