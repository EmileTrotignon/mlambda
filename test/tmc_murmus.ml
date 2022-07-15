open Helpers

let log = open_out "tmc_murmus.log"

module Regression = Tmc_mur.T (struct
  let env =
    Mlambda.(
      "tmc_mur.mlambda" |> Parse.file |> Tmc_murmus.program
      |.> Program.output log)
end)

open Mlambda

let env =
  "tmc_murmus.mlambda" |> Parse.file |> Tmc_murmus.program
  |.> Program.output log

let tailrecness () = List.iter (tailrecness env) ["tree"]

let tree_value =
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

let tree () =
  Alcotest.(
    check
      (Alcotest.testable Value.pp Value.( = ))
      "same tree" tree_value
      (Expr.(apply (var "tree") [int 3]) |> Eval.expr ~env))

let tests =
  Alcotest.
    [ test_case "Tmc murmus tailrecness" `Quick tailrecness
    ; test_case "Complete tree" `Quick tree ]
  @ Regression.tests
