type ident = string

type cons = string

type primitive = PrString of string | PrInt of int | PrBool of bool

and pattern =
  | PAny
  | PPrim of primitive
  | PVar of ident
  | PCons of {cons: cons option; payload: pattern list}

and expr =
  | EVar of ident
  | EFunc of {args: ident list; body: expr}
  | EApply of {func: expr; args: expr list}
  | EPrim of primitive
  | EUnit
  | ECons of {cons: cons option; payload: expr list}
  | EMatch of {arg: expr; branches: (pattern * expr) list}
  | EPrimFunc of string * (value list -> value)

and value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VString of string
  | VCons of cons
  | VArray of value array
  | VFunc of {mutable env: value Env.t; args: ident list; body: expr}
  | VPrFunc of string * (value list -> value)

type struct_item =
  | Binding of {name: ident; is_rec: bool; body: expr}
  | MutualRecBindings of (ident * expr) list

type program = struct_item list
