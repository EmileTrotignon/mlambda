open Mlambda
open Option_monad
open Printf

let ( |.> ) v f = f v ; v

let dps_name name = name ^ "_dps"

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

let int_list_pair_of_value v =
  Ast.(
    match v with
    | VArray [|v1; v2|] ->
        let* v1 = int_list_of_value v1 in
        let+ v2 = int_list_of_value v2 in
        (v1, v2)
    | _ ->
        None)

let int_list_of_value v =
  match int_list_of_value v with
  | Some li ->
      li
  | None ->
      failwith
        (sprintf "This value is not a list of integers : %s" (string_of_value v))

let int_list_pair_of_value v =
  match int_list_pair_of_value v with
  | Some li ->
      li
  | None ->
      failwith
        (sprintf "This value is not a pair of list of integers : %s"
           (string_of_value v) )

let tailrecness env funcname =
  Alcotest.(check bool)
    "is tailrec" true
    (let is_rec, body =
       funcname |> dps_name |> Program.find_def env |> Option.get
     in
     assert is_rec ;
     Expr.is_tailrec funcname body )
