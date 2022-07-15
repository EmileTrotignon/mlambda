open Ast

type t = primitive

let eq pr1 pr2 =
  match (pr1, pr2) with
  | PrString str, PrString str' ->
      String.equal str str'
  | PrInt i, PrInt i' ->
      Int.equal i i'
  | PrBool b, PrBool b' ->
      Bool.equal b b'
  | _, _ ->
      false

let ( = ) = eq

let string str = PrString str

let int i = PrInt i

let bool b = PrBool b
