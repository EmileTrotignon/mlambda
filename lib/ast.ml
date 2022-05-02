type ident = string

type cons = string

type primitive = PrString of string | PrInt of int | PrBool of bool

and pattern =
  | PAny
  | PPrim of primitive
  | PVar of ident
  | PTuple of pattern list
  | PCons of {cons: cons; payload: pattern list}

and expr =
  | EVar of ident
  | EFunc of {args: ident list; body: expr}
  | EApply of {func: expr; args: expr list}
  | EAlloc of expr
  | EPrim of primitive
  | EProj of {arr: expr; index: expr}
  | EWrite of {arr: expr; index: expr; value: expr}
  | EIf of {cond: expr; body_if: expr; body_else: expr}
  | EUnit
  | ECons of {cons: cons; payload: expr list}
  | ELet of {var: ident; is_rec: bool; value: expr; body_in: expr}
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

let get_int = function VInt i -> i | _ -> failwith "Not an integer"

let get_array = function VArray arr -> arr | _ -> failwith "Not an array"

let get_bool = function VBool b -> b | _ -> failwith "Not a bool"

let get_unit = function VUnit -> () | _ -> failwith "Not a unit"

type func =
  | Prim of string * (value list -> value)
  | Lang of (value Env.t * ident list * expr)

let get_func = function
  | VFunc {env; args; body} ->
      Lang (env, args, body)
  | VPrFunc (name, func) ->
      Prim (name, func)
  | _ ->
      failwith "Not a function"
