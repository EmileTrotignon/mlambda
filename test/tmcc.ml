open Mlambda
open Helpers

let env =
  "partition_map.mlambda" |> Parse.file |> Tmc.program_cons
  |.> print_program stdout

let tailrecness () =
  List.iter (tailrecness env)
    ["partition_map"]

let test_partition_map () =
  Alcotest.(
    check
      (pair (list int) (list int))
      "same lists pair"
      ([2], [1; 3])
      ( Eval.expr ~env
          Expr.(
            apply (var "partition_map")
              [ func ~args:["ele"]
                  ~body:
                    (if_
                       (apply (var "is_pair") [var "ele"])
                       ~then_:(cons "Left" ~payload:[var "ele"])
                       ~else_:(cons "Right" ~payload:[var "ele"]) )
              ; list [int 1; int 2; int 3] ])
      |> int_list_pair_of_value ))
