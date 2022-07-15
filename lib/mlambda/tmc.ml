open Ast
open Option_monad

let rec transformable_expr self expr =
  match expr with
  | EApply {func= EVar func_name; args= _} when func_name = self ->
      true
  | ECons {cons= _; payload} ->
      List.exists (transformable_expr self) payload
  | EMatch {arg= _; branches} ->
      List.exists (fun (_pat, expr) -> transformable_expr self expr) branches
  | EFunc {args= _; body} ->
      transformable_expr self body
  | EApply _
  | EVar _
  | EAlloc _
  | EPrim _
  | EProj _
  | EWrite _
  | EPrimFunc _
  | EUnit ->
      false

let _ = transformable_expr

let is_rec_call self n_args expr =
  match expr with
  | EApply {func= EVar func_name; args}
    when func_name = self && n_args = List.length args ->
      true
  | _ ->
      false

let rec_call_args self n_args expr =
  match expr with
  | EApply {func= EVar func_name; args}
    when func_name = self && n_args = List.length args ->
      args
  | _ ->
      failwith "not a rec call"

(* type index = Int of int | Variable of string *)

(* [transform_expr_dps self n_args index expr] can be :
   - [None] if the transformation fails.
   - [Some expr'] with [expr'] an expression that write the result of the
     evaluation of [expr] in [e_var "dst"] and satisfies some other constraints :
   - [expr] exists in the context of a recursive function of name [self] that
     takes [n_args] arguments. [expr'] is such that recursive calls to [self] in
     tail modulo cons position are replaced to calls to [self ^ "dps"] in tail
     position. *)
let rec expr_dps self n_args index expr =
  (* - [self] is the name of the recursive function we are transforming.
     - [index] is [None] if the index we need to write to is [e_var "index"],
       and [Some i] if it is [e_int i].
     - [let_cands] is the set of identifiers that are bound to a recursive call,
       are not ever used in that branch.
     - [expr] is the expression we are transforming. *)
  let e_index =
    Expr.(
      match index with Some index -> int (index + 1) | None -> var "index")
  in
  match expr with
  (*| EVar var ->
      (* If [var] was bound to a recursive call earlier in the code, and wasn't
         ever used in that branch, then we replace it with its arguments.
      *)
      let+ args = Env.find_opt var let_cands in
      ( String.Set.singleton var
      , EApply
          {func= EVar (self ^ "_dps"); args= e_var "dst'" :: e_index :: args} )*)
  | e when is_rec_call self n_args e ->
      let args = rec_call_args self n_args e in
      Some
        (EApply
           {func= EVar (self ^ "_dps"); args= Expr.var "dst'" :: e_index :: args}
        )
  | EMatch {arg= value; branches= [(PVar var, body_in)]} ->
      (* We then transform the code after the binding. *)
      let+ body_in = expr_dps self n_args index body_in in
      (* And we just restore the binding with the transformed [body_in] *)
      EMatch {arg= value; branches= [(PVar var, body_in)]}
  | ECons {cons; payload} ->
      let+ expr, payload =
        List.find_and_replace_i
          (fun index expr ->
            (* Here we use the current index, because if we manage to transform
               [expr] into [expr'], [expr'] should write in [dst.index]. This
               does not affect success *)
            (*  *)
            let+ expr' = expr_dps self n_args (Some index) expr in
            (* We enter this code if transforming the payload was successful.
               Here, [expr'] is an expression that write the result of the
               evaluation of [expr] in the destination (by induction hypothesis).
               In that case, we replace the payload by [e_unit] and we return the
               transformed expression along with the set of optimised lets. *)
            (expr', Expr.unit) )
          payload
      in
      (* This code is only executed if the above was succesful, that is if one
         of the payloads was successfuly transformed (we pick the first one).
         In that case :
         - We return the set of optimised away let-bindings directly.
         - We then output the following code :
           [[ let dst' = Cons (..., (), ...) in
              dst.index <- dst' ;
              let dst = dst' in
              expr ]]
           where expr is the expression that will write in the hole of the
           destination. *)
      let cons_ = cons in
      Expr.(
        let_var "dst'"
          ~equal:(ECons {cons= cons_; payload})
          ~in_:
            ( seqs [write ~block:(var "dst") ~i:e_index ~to_:(var "dst'")]
            @@ let_var "dst" ~equal:(var "dst'") ~in_:expr ))
  | EMatch {arg; branches} ->
      (* We need to transform the branches. Each branch can be either
         succesfully transformed, or plugged as-is in a write statement.
         We consider that the transformation is a success if at least one branch
         was succesfully transformed. *)
      let had_a_succes, branches =
        List.fold_left_map
          (fun had_a_success (pat, expr) ->
            match expr_dps self n_args index expr with
            | Some expr ->
                (true, (pat, expr))
            | None ->
                ( had_a_success
                , Expr.(pat, write ~block:(var "dst") ~i:e_index ~to_:expr) ) )
          false branches
      in
      if had_a_succes then Some (Expr.match_ arg ~with_:branches) else None
  | EFunc _
  | EApply _
  | EAlloc _
  | EPrim _
  | EProj _
  | EWrite _
  | EPrimFunc _
  | EVar _
  | EUnit ->
      (* In all these cases, we fail. *)
      None

let expr_dps self n_args expr = expr |> expr_dps self n_args None

(** [transform_expr self expr] is either [None] if no transformation was found,
    or [Some (entry, dps)] where a call to [entry] has the same semantics as
    call to [self] (in the context [let rec self = expr]). And [dps] is [expr]
    in destination passing style, that is [dps dst i arg] writes [self arg] in
    [dst.i]. *)
let expr self e =
  match e with
  | EFunc {args; body} ->
      let+ transformed = expr_dps self (List.length args) body in
      let name_dps = self ^ "_dps" in
      let dps =
        let body =
          Expr.(
            seqs
              [ apply (var "print") [string "dst ="]
              ; apply (var "print") [var "dst"] ])
          @@ transformed
        in
        let args = ["dst"; "index"] @ args in
        EFunc {args; body}
      in
      let entry_point =
        Expr.(
          func ~args
            ~body:
              (let_var "dst"
                 ~equal:(alloc ~size:(int 1))
                 ~in_:
                   ( seqs
                       [ apply (var "print") [string "entry dst ="]
                       ; apply (var "print") [var "dst"] ]
                   @@ seqs
                        [ apply (var name_dps)
                            ([var "dst"; int 0] @ (args |> List.map var)) ]
                        (proj (var "dst") (int 0)) ) ))
      in
      (entry_point, dps)
  | _ ->
      None

let si si =
  match si with
  | Binding {name; body; is_rec} -> (
      if not is_rec then si
      else
        match expr name body with
        | Some (entry, dps) ->
            MutualRecBindings [(name, entry); (name ^ "_dps", dps)]
        | None ->
            si )
  | MutualRecBindings _ ->
      si

let program program = List.map si program
