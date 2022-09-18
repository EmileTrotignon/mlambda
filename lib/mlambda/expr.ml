open Ast

type t = Ast.expr

let%memo rec fv (expr : t) =
  String.Set.(
    match expr with
    | EVar var ->
        singleton var
    | EFunc {args; body} ->
        fv body - (args |> List.map singleton |> unions)
    | EApply {func; args} ->
        fv func + (args |> List.map fv |> unions)
    | EPrim _ ->
        empty
    | EUnit ->
        empty
    | ECons {cons= _; payload} ->
        payload |> List.map fv |> unions
    | EMatch {arg; branches} ->
        fv arg
        + ( branches
          |> List.map (fun (pat, body) -> fv body - Pattern.vars pat)
          |> unions )
    | EPrimFunc (_, _) ->
        empty)

let rec eq_pat p e =
  match (p, e) with
  | PVar v_p, EVar v_e when v_p = v_e ->
      true
  | ( PCons {cons= cons_p; payload= payload_p}
    , ECons {cons= cons_e; payload= payload_e} )
    when cons_p = cons_e && List.for_all2 eq_pat payload_p payload_e ->
      true
  | PAny, _ ->
      true
  | _, _ ->
      false

let var ident = EVar ident

let func ~args ~body = EFunc {args; body}

let apply func args = EApply {func; args}

let alloc ~size = apply (var "alloc") [size]

let prim prim = EPrim prim

let prim_func name func = EPrimFunc (name, func)

let proj arr index = apply (var "proj") [arr; index]

let unit = EUnit

let true_ = prim (Primitive.bool true)

let false_ = prim (Primitive.bool false)

let bool b = prim (Primitive.bool b)

let cons ?(payload = []) cons = ECons {cons= Some cons; payload}

let tuple payload = ECons {cons= None; payload}

let write ~block ~i ~to_ = apply (var "write") [block; i; to_]

let match_ arg ~with_ = EMatch {arg; branches= with_}

let if_ cond ~then_ ~else_ =
  match_ cond
    ~with_:
      [ (Pattern.prim (Primitive.bool true), then_)
      ; (Pattern.prim (Primitive.bool false), else_) ]

let rec is_tailrec self (body : t) =
  match body with
  | EVar _ ->
      true
  | EFunc {args; body} ->
      if List.mem self args then true else is_tailrec self body
  | EApply {args; _} ->
      List.for_all (fun expr -> not (String.Set.mem self (fv expr))) args
  | EPrim _ ->
      true
  | EUnit ->
      true
  | ECons {payload; _} ->
      List.for_all (fun expr -> not (String.Set.mem self (fv expr))) payload
  | EMatch {arg; branches} ->
      (not (String.Set.mem self (fv arg)))
      && List.for_all
           (fun (pat, expr) ->
             String.Set.mem self (Pattern.vars pat) || is_tailrec self expr )
           branches
  | EPrimFunc _ ->
      true

let let_ pat ~equal ~in_ = match_ equal ~with_:[(pat, in_)]

let let_var var ~equal ~in_ = let_ (PVar var) ~equal ~in_

let let_and bindings ~in_ =
  match bindings with
  | [] ->
      failwith "Empty binding"
  | [(pat, equal)] ->
      let_ pat ~equal ~in_
  | _ :: _ ->
      let pats, exprs = List.split bindings in
      let_ Pattern.(tuple pats) ~equal:(tuple exprs) ~in_

let seq e1 e2 = let_ PAny ~equal:e1 ~in_:e2

let rec seqs actions finally =
  match actions with
  | [] ->
      finally
  | action :: actions ->
      seq action (seqs actions finally)

let rec seq_of_list li =
  match li with
  | [] ->
      assert false
  | [e] ->
      e
  | e :: li ->
      seq e (seq_of_list li)

let int i = prim (Primitive.int i)

let string s = prim (Primitive.string s)

let rec list li =
  match li with
  | [] ->
      cons "[]"
  | ele :: li ->
      cons "::" ~payload:[ele; list li]

let int_list li = li |> List.map int |> list

let for_ i ~equals ~to_ ~do_ =
  apply (var "for_loop") [func ~args:[i] ~body:do_; equals; to_]

include Print.Expr