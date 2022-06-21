open Ast

type t = primitive

let string str = PrString str

let int i = PrInt i

let bool b = PrBool b
