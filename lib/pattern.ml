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
