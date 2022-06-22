open Mlambda
open Helpers

let env = Parse.file "maps.mlambda" |> Tmc.program |.> print_program stdout

let tailrecness () =
  List.iter (tailrecness env)
    ["map"; "map_let"; "map_double"; "map_double_cond"; "map_double_cond_let"]

let map () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 6]
    ( Eval.expr ~env
        Expr.(apply (var "map") [var "double"; list [int 1; int 2; int 3]])
    |> int_list_of_value )

let map_let () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 6]
    ( Eval.expr ~env
        Expr.(apply (var "map_let") [var "double"; list [int 1; int 2; int 3]])
    |> int_list_of_value )

let map_double () =
  Alcotest.(check (list int))
    "same lists" [2; 2; 4; 4; 6; 6]
    ( Eval.expr ~env
        Expr.(
          apply (var "map_double") [var "double"; list [int 1; int 2; int 3]])
    |> int_list_of_value )

let map_double_cond () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 4; 6]
    ( Eval.expr ~env
        Expr.(
          apply (var "map_double_cond")
            [var "double"; var "is_pair"; list [int 1; int 2; int 3]])
    |> int_list_of_value )

let map_double_cond_let () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 4; 6]
    ( Eval.expr ~env
        Expr.(
          apply
            (var "map_double_cond_let")
            [var "double"; var "is_pair"; list [int 1; int 2; int 3]])
    |> int_list_of_value )
