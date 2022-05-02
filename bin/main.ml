open Mlambda
open Builder

let program_maps = Parse.file "exemples/list_maps.mlambda"

let e_list e_ele li =
  List.fold_left
    (fun acc ele -> e_cons "::" ~payload:[e_ele ele; acc])
    (e_cons "[]") li

let e_int_list = e_list (fun i -> e_prim (pr_int i))

let e_double =
  Ast.(
    e_prim_func "double" (function
      | [v] ->
          let v = get_int v in
          VInt (2 * v)
      | _ ->
          failwith "double expects 1 arguments" ))

let my_list = e_int_list [1; 2; 3; 4]
