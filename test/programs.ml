open Mlambda

let env = Parse.file "maps.mlambda" (*|.> print_program stdout*)

let map () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 6]
    ( Eval.expr ~env
        Expr.(apply (var "map") [var "double"; list [int 1; int 2; int 3]])
    |> Value.int_list_exn )

let map_double () =
  Alcotest.(check (list int))
    "same lists" [2; 2; 4; 4; 6; 6]
    ( Eval.expr ~env
        Expr.(
          apply (var "map_double") [var "double"; list [int 1; int 2; int 3]])
    |> Value.int_list_exn )

let map_double_cond () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 4; 6]
    ( Eval.expr ~env
        Expr.(
          apply (var "map_double_cond")
            [var "double"; var "is_pair"; list [int 1; int 2; int 3]])
    |> Value.int_list_exn )
