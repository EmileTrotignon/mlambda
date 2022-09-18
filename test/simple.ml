open Mlambda
open Helpers

module Tests (A : Test_params) : Tests = struct
  open A

  let log = open_out log

  let env = "simple.mlambda" |> Parse.file |> transform |.> Program.output log

  let () = close_out log

  let tailrecness () =
    List.iter (tailrecness env)
      ["map"; "map_let"; "map_double";(* "map_double_cond";*) "map_double_cond_let"]

  let map () =
    value_test "same lists"
      (Value.int_list [2; 4; 6])
      (Eval.expr ~env
         Expr.(apply (var "map") [var "double"; int_list [1; 2; 3]]) )

  let map_let () =
    value_test "same lists"
      (Value.int_list [2; 4; 6])
      (Eval.expr ~env
         Expr.(apply (var "map_let") [var "double"; int_list [1; 2; 3]]) )

  let map_double () =
    value_test "same lists"
      (Value.int_list [2; 2; 4; 4; 6; 6])
      (Eval.expr ~env
         Expr.(apply (var "map_double") [var "double"; int_list [1; 2; 3]]) )

  (*let map_double_cond () =
    value_test "same lists"
      (Value.int_list [2; 4; 4; 6])
      (Eval.expr ~env
         Expr.(
           apply (var "map_double_cond")
             [var "double"; var "is_pair"; list [int 1; int 2; int 3]]) )*)

  let map_double_cond_let () =
    value_test "same lists"
      (Value.int_list [2; 4; 4; 6])
      (Eval.expr ~env
         Expr.(
           apply
             (var "map_double_cond_let")
             [var "double"; var "is_pair"; int_list [1; 2; 3]]) )

  let tests =
    Alcotest.(
      ( if expects_tailrec then
        [test_case "simple tailrecness" `Quick tailrecness]
      else [] )
      @ [ test_case "map" `Quick map
        ; test_case "map_let" `Quick map_let
        ; test_case "map_double" `Quick map_double
        (* ; test_case "map_double_cond" `Quick map_double_cond *)
        ; test_case "map_double_cond_let" `Quick map_double_cond_let ])
end

include Tests (struct
  let log = "simple.log"

  let transform = Fun.id

  let expects_tailrec = false
end)
