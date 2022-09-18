open Mlambda
open Helpers

module Tests (A : Test_params) : Tests = struct
  open A

  let log = open_out log

  let env = "murmus.mlambda" |> Parse.file |> transform |.> Program.output log

  let tailrecness () =
    List.iter (tailrecness env) ["complete_tree"; "double_tree"]

  let complete_tree_3_value =
    Value.(
      array
        [| cons "Node"
         ; int 3
         ; array
             [| cons "Node"
              ; int 2
              ; array [|cons "Node"; int 1; cons "Leaf"; cons "Leaf"|]
              ; array [|cons "Node"; int 1; cons "Leaf"; cons "Leaf"|] |]
         ; array
             [| cons "Node"
              ; int 2
              ; array [|cons "Node"; int 1; cons "Leaf"; cons "Leaf"|]
              ; array [|cons "Node"; int 1; cons "Leaf"; cons "Leaf"|] |] |])

  let double_tree_3_value =
    Value.(
      array
        [| array
             [| cons "Node"
              ; int 3
              ; array
                  [| cons "Node"
                   ; int 2
                   ; array [|cons "Node"; int 1; cons "Leaf"; cons "Leaf"|]
                   ; array [|cons "Node"; int 3; cons "Leaf"; cons "Leaf"|] |]
              ; array
                  [| cons "Node"
                   ; int 2
                   ; array [|cons "Node"; int 3; cons "Leaf"; cons "Leaf"|]
                   ; array [|cons "Node"; int 1; cons "Leaf"; cons "Leaf"|] |]
             |]
         ; array
             [| cons "Node"
              ; int 1
              ; array
                  [| cons "Node"
                   ; int 2
                   ; array [|cons "Node"; int 3; cons "Leaf"; cons "Leaf"|]
                   ; array [|cons "Node"; int 1; cons "Leaf"; cons "Leaf"|] |]
              ; array
                  [| cons "Node"
                   ; int 2
                   ; array [|cons "Node"; int 1; cons "Leaf"; cons "Leaf"|]
                   ; array [|cons "Node"; int 3; cons "Leaf"; cons "Leaf"|] |]
             |] |])

  let tree () =
    value_test "same tree" complete_tree_3_value
      (Expr.(apply (var "complete_tree") [int 3]) |> Eval.expr ~env)

  let double_tree () =
    value_test "same tree" double_tree_3_value
      (Expr.(apply (var "double_tree") [int 3]) |> Eval.expr ~env)

  let tests =
    Alcotest.(
      ( if expects_tailrec then [test_case "tailrecness" `Quick tailrecness]
      else [] )
      @ [ test_case "complete_tree" `Quick tree
        ; test_case "double_tree" `Quick double_tree ])
end

include Tests (struct
  let log = "murnus.log"

  let transform = Fun.id

  let expects_tailrec = false
end)