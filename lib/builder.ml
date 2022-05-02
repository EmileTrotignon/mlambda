open Ast

let pr_string str = PrString str

let pr_int i = PrInt i

let pr_bool b = PrBool b

let p_any = PAny

let p_var ident = PVar ident

let p_prim prim = PPrim prim

let p_tuple li = PTuple li

let p_cons ?(payload = []) cons = PCons {cons; payload}

let e_var ident = EVar ident

let e_func ~args ~body = EFunc {args; body}

let e_apply func args = EApply {func; args}

let e_alloc ~size = EAlloc size

let e_prim prim = EPrim prim

let e_prim_func name func = EPrimFunc (name, func)

let e_proj arr index = EProj {arr; index}

let e_write ~block ~i ~to_ = EWrite {arr= block; index= i; value= to_}

let e_if cond ~then_ ~else_ = EIf {cond; body_if= then_; body_else= else_}

let e_match arg ~with_ = EMatch {arg; branches= with_}

let e_let ?(is_rec = false) var ~equal ~in_ =
  ELet {var; is_rec; value= equal; body_in= in_}

let e_seq e1 e2 = e_let "_" ~equal:e1 ~in_:e2

let rec e_seqs actions finally =
  match actions with
  | [] ->
      finally
  | action :: actions ->
      e_seq action (e_seqs actions finally)

let rec seq_of_list li =
  match li with
  | [] ->
      assert false
  | [e] ->
      e
  | e :: li ->
      e_seq e (seq_of_list li)

let e_unit = EUnit

let e_true = e_prim (pr_bool true)

let e_false = e_prim (pr_bool false)

let e_bool b = e_prim (pr_bool b)

let e_cons ?(payload = []) cons = ECons {cons; payload}

let rec e_list li =
  match li with
  | [] ->
      e_cons "[]"
  | ele :: li ->
      e_cons "::" ~payload:[ele; e_list li]

let e_int i = e_prim (pr_int i)

let si_binding ?(_rec = false) name body = Binding {name; is_rec= _rec; body}

let si_mutualrec bds = MutualRecBindings bds

let prim_func_def name func = si_binding name (e_prim_func name func)

let prim_func_def_ar1 name func =
  prim_func_def name (function
    | [v] ->
        func v
    | _ ->
        failwith (Printf.sprintf "%s expects 1 arguments" name) )

let prim_func_def_ar2 name func =
  prim_func_def name (function
    | [v1; v2] ->
        func v1 v2
    | _ ->
        failwith (Printf.sprintf "%s expects 2 arguments" name) )