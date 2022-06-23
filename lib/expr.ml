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
    | EAlloc e ->
        fv e
    | EPrim _ ->
        empty
    | EProj {arr; index} ->
        union (fv arr) (fv index)
    | EWrite {arr; index; value} ->
        union (fv arr) @@ union (fv index) (fv value)
    | EUnit ->
        empty
    | ECons {cons= _; payload} ->
        payload |> List.map fv |> unions
    | ELet {var; is_rec; value; body_in} ->
        if is_rec then remove var (fv value + fv body_in)
        else fv value + remove var (fv body_in)
    | EMatch {arg; branches} ->
        fv arg
        + ( branches
          |> List.map (fun (pat, body) -> fv body - Pattern.vars pat)
          |> unions )
    | EPrimFunc (_, _) ->
        empty)

let var ident = EVar ident

let func ~args ~body = EFunc {args; body}

let apply func args = EApply {func; args}

let alloc ~size = EAlloc size

let prim prim = EPrim prim

let prim_func name func = EPrimFunc (name, func)

let proj arr index = EProj {arr; index}

let write ~block ~i ~to_ = EWrite {arr= block; index= i; value= to_}

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
  | EAlloc i ->
      not (String.Set.mem self (fv i))
  | EPrim _ ->
      true
  | EProj {arr; index} ->
      (not (String.Set.mem self (fv arr)))
      && not (String.Set.mem self (fv index))
  | EWrite {arr; index; value} ->
      (not (String.Set.mem self (fv arr)))
      && (not (String.Set.mem self (fv index)))
      && not (String.Set.mem self (fv value))
  | EUnit ->
      true
  | ECons {payload; _} ->
      List.for_all (fun expr -> not (String.Set.mem self (fv expr))) payload
  | ELet {var; is_rec= true; _} when var = self ->
      true
  | ELet {var; is_rec= _; value; body_in} ->
      (not (String.Set.mem self (fv value)))
      && if var = self then true else is_tailrec self body_in
  | EMatch {arg; branches} ->
      (not (String.Set.mem self (fv arg)))
      && List.for_all
           (fun (pat, expr) ->
             String.Set.mem self (Pattern.vars pat) || is_tailrec self expr )
           branches
  | EPrimFunc _ ->
      true

let let_ ?(is_rec = false) var ~equal ~in_ =
  ELet {var; is_rec; value= equal; body_in= in_}

let seq e1 e2 = let_ "_" ~equal:e1 ~in_:e2

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

let unit = EUnit

let true_ = prim (Primitive.bool true)

let false_ = prim (Primitive.bool false)

let bool b = prim (Primitive.bool b)

let cons ?(payload = []) cons = ECons {cons= Some cons; payload}

let tuple payload = ECons {cons= None; payload}

let rec list li =
  match li with
  | [] ->
      cons "[]"
  | ele :: li ->
      cons "::" ~payload:[ele; list li]

let int i = prim (Primitive.int i)

let string s = prim (Primitive.string s)