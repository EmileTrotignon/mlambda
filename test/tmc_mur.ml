open Mlambda
open Helpers
open Result_monad

let log = open_out "tmc_mur.log"

module T (A : sig
  val env : Ast.program
end) =
struct
  let env = A.env

  let tailrecness () =
    List.iter (tailrecness env)
      [ "partition_map"
      ; "partition_map_twister"
      ; "partition_map_deep"
      ; "fold_left_map" ]

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

  let partition_map_deep () =
    Alcotest.(
      check
        (pair (list int) (pair (list int) (list int)))
        "same lists pair"
        ([0; 3; 6; 9], ([1; 4; 7], [2; 5; 8]))
        ( Eval.expr ~env
            Expr.(
              apply (var "partition_map_deep")
                [ func ~args:["ele"]
                    ~body:
                      (match_
                         (apply (var "mod") [var "ele"; int 3])
                         ~with_:
                           [ (Pattern.int 0, cons "Left" ~payload:[var "ele"])
                           ; (Pattern.int 1, cons "Center" ~payload:[var "ele"])
                           ; (Pattern.int 2, cons "Right" ~payload:[var "ele"])
                           ] )
                ; [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] |> List.map int |> list ])
        |> Value.(
             (fun v ->
               let* v1, v2 = to_pair v in
               let* li1 = to_int_list v1 in
               let+ li2 = to_int_list_pair v2 in
               (li1, li2) )
             |> to_exn) ))

  let fold_left_map () =
    Alcotest.(
      check
        (pair int (list int))
        "same acc and list"
        (30, [0; 2; 4; 6; 8; 10])
        ( Eval.expr ~env
            Expr.(
              apply (var "fold_left_map")
                [ func ~args:["acc"; "ele"]
                    ~body:
                      (let_var "ele"
                         ~equal:(apply (var "mult") [var "ele"; int 2])
                         ~in_:
                           (tuple
                              [ apply (var "add") [var "acc"; var "ele"]
                              ; var "ele" ] ) )
                ; int 0
                ; [0; 1; 2; 3; 4; 5] |> List.map int |> list ])
        |> Value.(
             to_exn
             @@ fun v ->
             let* v1, v2 = to_pair v in
             let* sum = to_int v1 in
             let+ li = to_int_list v2 in
             (sum, li)) ))

  let tests =
    Alcotest.
      [ test_case "Tmc mur tailrecness" `Quick tailrecness
      ; test_case "partition_map" `Quick partition_map
      ; test_case "partition_map_twister" `Quick partition_map_twister
      ; test_case "partition_map_deep" `Quick partition_map_deep
      ; test_case "fold_left_map" `Quick fold_left_map ]
end

include T (struct
  let env =
    "tmc_mur.mlambda" |> Parse.file |> Tmc_mur.program |.> Program.output log
end)
