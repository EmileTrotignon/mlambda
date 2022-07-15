open Mlambda
open Helpers

let log = open_out "tmc.log"

let env =
  Parse.file "tmc.mlambda" |> Inline.program |> Tmc.program
  |.> Program.output log

let tailrecness () =
  List.iter (tailrecness env)
    ["map"; "map_let"; "map_double"; "map_double_cond"; "map_double_cond_let"]

let map () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 6]
    ( Eval.expr ~env
        Expr.(apply (var "map") [var "double"; list [int 1; int 2; int 3]])
    |> Value.int_list_exn )

let map_let () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 6]
    ( Eval.expr ~env
        Expr.(apply (var "map_let") [var "double"; list [int 1; int 2; int 3]])
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

let map_double_cond_let () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 4; 6]
    ( Eval.expr ~env
        Expr.(
          apply
            (var "map_double_cond_let")
            [var "double"; var "is_pair"; list [int 1; int 2; int 3]])
    |> Value.int_list_exn )

let tests =
  Alcotest.
    [ test_case "Tmc tailrecness" `Quick tailrecness
    ; test_case "Map tmc" `Quick map
    ; test_case "Map with let tmc" `Quick map_let
    ; test_case "Map double tmc" `Quick map_double
    ; test_case "Map double cond tmc" `Quick map_double_cond
    ; test_case "Map double cond let tmc" `Quick map_double_cond_let ]
  
