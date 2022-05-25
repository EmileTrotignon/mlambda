open Mlambda
open Builder
open Printf
open Option_monad

let maps = Parse.file "maps.mlambda"

let maps_tmc = Tmc.program maps

(* let () = print_program stdout maps_tmc *)

let rec int_list_of_value v =
  (* print_value stdout v ; *)
  Ast.(
    match v with
    | VCons "[]" ->
        Some []
    | VArray [|VCons "::"; VInt ele; value|] ->
        let+ v = int_list_of_value value in
        ele :: v
    | _ ->
        None)

let int_list_of_value v =
  match int_list_of_value v with
  | Some li ->
      li
  | None ->
      failwith
        (sprintf "This value is not a list of integers : %s" (string_of_value v))

let test_is_pair i () =
  Alcotest.(check bool)
    "same bools"
    (i mod 2 = 0)
    (Eval.expr (e_apply (e_var "is_pair") [e_int i]) |> Ast.get_bool)

let test_map () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 6]
    ( Eval.expr ~env:maps
        (e_apply (e_var "map")
           [e_var "double"; e_list [e_int 1; e_int 2; e_int 3]] )
    |> int_list_of_value )

let test_map_double () =
  Alcotest.(check (list int))
    "same lists" [2; 2; 4; 4; 6; 6]
    ( Eval.expr ~env:maps
        (e_apply (e_var "map_double")
           [e_var "double"; e_list [e_int 1; e_int 2; e_int 3]] )
    |> int_list_of_value )

let test_map_double_cond () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 4; 6]
    ( Eval.expr ~env:maps
        (e_apply (e_var "map_double_cond")
           [e_var "double"; e_var "is_pair"; e_list [e_int 1; e_int 2; e_int 3]] )
    |> int_list_of_value )

let test_write () =
  Alcotest.(check int)
    "same int" 1
    ( Eval.expr
        (e_let "arr"
           ~equal:(e_alloc ~size:(e_int 2))
           ~in_:
             (e_seqs
                [e_write ~block:(e_var "arr") ~i:(e_int 0) ~to_:(e_int 1)]
                (e_proj (e_var "arr") (e_int 0)) ) )
    |> Ast.get_int )

let test_map_tmc () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 6]
    ( Eval.expr ~env:maps_tmc
        (e_apply (e_var "map")
           [e_var "double"; e_list [e_int 1; e_int 2; e_int 3]] )
    |> int_list_of_value )

let test_map_let_tmc () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 6]
    ( Eval.expr ~env:maps_tmc
        (e_apply (e_var "map_let")
           [e_var "double"; e_list [e_int 1; e_int 2; e_int 3]] )
    |> int_list_of_value )

let test_map_double_tmc () =
  Alcotest.(check (list int))
    "same lists" [2; 2; 4; 4; 6; 6]
    ( Eval.expr ~env:maps_tmc
        (e_apply (e_var "map_double")
           [e_var "double"; e_list [e_int 1; e_int 2; e_int 3]] )
    |> int_list_of_value )

let test_map_double_cond_tmc () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 4; 6]
    ( Eval.expr ~env:maps_tmc
        (e_apply (e_var "map_double_cond")
           [e_var "double"; e_var "is_pair"; e_list [e_int 1; e_int 2; e_int 3]] )
    |> int_list_of_value )

let test_map_double_cond_let_tmc () =
  Alcotest.(check (list int))
    "same lists" [2; 4; 4; 6]
    ( Eval.expr ~env:maps_tmc
        (e_apply
           (e_var "map_double_cond_let")
           [e_var "double"; e_var "is_pair"; e_list [e_int 1; e_int 2; e_int 3]] )
    |> int_list_of_value )

let () =
  let open Alcotest in
  run "Tests"
    [ ( "Maps"
      , [ test_case "Test is_pair" `Quick (test_is_pair 1)
        ; test_case "Test is_pair" `Quick (test_is_pair 2)
        ; test_case "Test is_pair" `Quick (test_is_pair 3)
        ; test_case "Test is_pair" `Quick (test_is_pair 4)
        ; test_case "Test is_pair" `Quick (test_is_pair 5)
        ; test_case "Test is_pair" `Quick (test_is_pair 6)
        ; test_case "Test write" `Quick test_write
        ; test_case "Test map" `Quick test_map
        ; test_case "Test map double" `Quick test_map_double
        ; test_case "Test map double cond" `Quick test_map_double_cond
        ; test_case "Test map tmc" `Quick test_map_tmc
        ; test_case "Test map_let tmc" `Quick test_map_let_tmc
        ; test_case "Test map double tmc" `Quick test_map_double_tmc
        ; test_case "Test map double cond tmc" `Quick test_map_double_cond_tmc
        ; test_case "Test map double cond let tmc" `Quick
            test_map_double_cond_let_tmc ] ) ]
