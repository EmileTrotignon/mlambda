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
  | EAlloc of expr
  | EPrim of primitive
  | EProj of {arr: expr; index: expr}
  | EWrite of {arr: expr; index: expr; value: expr}
  | EUnit
 (* cons is optionnal because tuples do not have a cons *)
  | ECons of {cons: cons option; payload: expr list}
  (* | ELet of {var: ident; is_rec: bool; value: expr; body_in: expr} *)
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

and struct_item =
  | Binding of {name: ident; is_rec: bool; body: expr}
  | MutualRecBindings of (ident * expr) list

type program = struct_item list

