open Mlambda
open Helpers

(* open Result_monad *)
let log = open_out "tmcc.log"

let env =
  "partition_map.mlambda" |> Parse.file |> Tmcc.program |.> print_program log

let tailrecness () = List.iter (tailrecness env) ["partition_map"]

let partition_map () =
  Alcotest.(
    check
      (pair (list int) (list int))
      "same lists pair"
      ([2; 2; 4; 4], [1; 1; 3; 3])
      ( Eval.expr ~env
          Expr.(
            apply (var "partition_map")
              [ func ~args:["ele"]
                  ~body:
                    (if_
                       (apply (var "is_pair") [var "ele"])
                       ~then_:(cons "Left" ~payload:[var "ele"])
                       ~else_:(cons "Right" ~payload:[var "ele"]) )
              ; [1; 1; 2; 2; 3; 3; 4; 4] |> List.map int |> list ])
      |> Value.int_list_pair_exn ))

let partition_map_twister () =
  Alcotest.(
    check
      (pair (list int) (list int))
      "same lists pair"
      ([1; 2; 3; 4], [1; 2; 3; 4])
      ( Eval.expr ~env
          Expr.(
            apply
              (var "partition_map_twister")
              [ func ~args:["ele"]
                  ~body:
                    (if_
                       (apply (var "is_pair") [var "ele"])
                       ~then_:(cons "Left" ~payload:[var "ele"])
                       ~else_:(cons "Right" ~payload:[var "ele"]) )
              ; [1; 1; 2; 2; 3; 3; 4; 4] |> List.map int |> list ])
      |> Value.int_list_pair_exn ))

(* let test_partition_map () =
   Alcotest.(
     check
       (pair (list int) (pair (list int) (list int)))
       "same lists pair"
       ([0; 3; 6; 9], ([1; 4; 7], [2; 5; 8]))
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
       |> Value.(
            (fun v ->
              let* v1, v2 = to_pair v in
              let* li1 = to_int_list v1 in
              let+ li2 = to_int_list_pair v2 in
              (li1, li2) )
            |> to_exn) )) *)
