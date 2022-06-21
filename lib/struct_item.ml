open Ast

type t = struct_item

let binding ?(_rec = false) name body = Binding {name; is_rec= _rec; body}

let mutualrec bds = MutualRecBindings bds

let prim_func_def name func = binding name (Expr.prim_func name func)

let prim_func_def_ar1 name func =
  prim_func_def name (function
    | [v] ->
        func v
    | _ ->
        failwith (Printf.sprintf "%s expects 1 arguments" name) )

let prim_func_def_ar2 name func =
  prim_func_def name (function
    | [v1; v2] ->
        func v1 v2
    | _ ->
        failwith (Printf.sprintf "%s expects 2 arguments" name) )