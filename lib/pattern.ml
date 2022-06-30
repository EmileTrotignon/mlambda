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

let n_vars p = String.Set.cardinal (vars p)

let rec vars_numbered i pattern =
  match pattern with
  | PVar var ->
      (i + 1, Env.singleton var i)
  | PAny | PPrim _ ->
      (i, Env.empty)
  | PCons {payload; _} ->
      let i, payload_env = List.fold_left_map vars_numbered i payload in
      (i, List.fold_left Env.union Env.empty payload_env)

let vars_numbered p =
  print_string "VARS NUMBERED : " ;
  Print.print_pattern stdout p ;
  p |> vars_numbered 0 |> snd

let block_size pat =
  match pat with
  | PAny ->
      0
  | PPrim _ ->
      0
  | PVar _ ->
      0
  | PCons {cons; payload} ->
      List.length payload + if Option.is_some cons then 1 else 0

let any = PAny

let var ident = PVar ident

let prim prim = PPrim prim

let int i = prim (Primitive.int i)

let string s = prim (Primitive.string s)

let bool b = prim (Primitive.bool b)

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
