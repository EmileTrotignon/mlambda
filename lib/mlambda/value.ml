open Ast
open Printf
open Result_monad

type t = value

include Print.Value

type value_coertion_error = {message: string; value: t}

let rec equals v1 v2 =
  match (v1, v2) with
  | VUnit, VUnit ->
      true
  | VInt i, VInt i' ->
      i = i'
  | VCons cons, VCons cons' ->
      cons = cons'
  | VString str, VString str' ->
      str = str'
  | VBool b, VBool b' ->
      b = b'
  | VArray arr, VArray arr' ->
      Array.for_all2 equals arr arr'
  | _, _ ->
      false

let ( = ) = equals

let unit = VUnit

let int i = VInt i

let cons cons = VCons cons

let string str = VString str

let bool b = VBool b

let array arr = VArray arr

let to_int = function
  | VInt i ->
      Ok i
  | value ->
      Error {message= "Not an integer"; value}

let to_array = function
  | VArray arr ->
      Ok arr
  | value ->
      Error {message= "Not an array"; value}

let to_bool = function
  | VBool b ->
      Ok b
  | value ->
      Error {message= "Not a bool"; value}

let to_unit = function
  | VUnit ->
      Ok ()
  | value ->
      Error {message= "Not a unit"; value}

type func =
  | Prim of string * (value list -> value)
  | Lang of (value Env.t * ident list * expr)

let to_func = function
  | VFunc {env; args; body} ->
      Ok (Lang (env, args, body))
  | VPrFunc (name, func) ->
      Ok (Prim (name, func))
  | value ->
      Error {message= "Not a function"; value}

let to_pair = function
  | VArray [|v1; v2|] ->
      Ok (v1, v2)
  | value ->
      Error {message= "Not a pair"; value}

let to_triple = function
  | VArray [|v1; v2; v3|] ->
      Ok (v1, v2, v3)
  | value ->
      Error {message= "Not a triple"; value}

let rec to_list = function
  | VCons "[]" ->
      Ok []
  | VArray [|VCons "::"; ele; li|] ->
      let+ li = to_list li in
      ele :: li
  | value ->
      Error {message= "Not a list"; value}

let to_int_list v =
  let* li = to_list v in
  result_list_map to_int li

let to_int_list_pair v =
  let* v1, v2 = to_pair v in
  let* v1 = to_int_list v1 in
  let+ v2 = to_int_list v2 in
  (v1, v2)

let to_exn f v =
  match f v with
  | Ok v ->
      v
  | Error {message; value} ->
      failwith
        (sprintf "Getting from:\n%s.\n%s : %s" (to_string v) message
           (to_string value) )

let list_exn = to_exn to_list

let pair_exn = to_exn to_pair

let int_list_exn = to_exn to_int_list

let int_list_pair_exn = to_exn to_int_list_pair

let int_exn = to_exn to_int

let array_exn = to_exn to_array

let bool_exn = to_exn to_bool

let unit_exn = to_exn to_unit

let func_exn = to_exn to_func
