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
  | EApply _ | EVar _ | EPrim _ | EPrimFunc _ | EUnit ->
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
let rec expr_dps self n_args ~dst ~index expr =
  (* - [self] is the name of the recursive function we are transforming.
     - [index] is [None] if the index we need to write to is [e_var "index"],
       and [Some i] if it is [e_int i].
     - [let_cands] is the set of identifiers that are bound to a recursive call,
       are not ever used in that branch.
     - [expr] is the expression we are transforming. *)
  match expr with
  | e when is_rec_call self n_args e ->
      let args = rec_call_args self n_args e in
      Some (EApply {func= EVar (self ^ "_dps"); args= dst :: index :: args})
  | ECons {cons; payload} ->
      let block_name = Fresh_vars.block () in
      let+ expr, payload =
        List.find_and_replace_i
          (fun index expr ->
            (* Here we use the current index, because if we manage to transform
               [expr] into [expr'], [expr'] should write in [dst.index]. This
               does not affect success *)
            let index = if Option.is_some cons then index + 1 else index in
            let+ expr' =
              expr_dps self n_args ~dst:(Expr.var block_name)
                ~index:(Expr.int index) expr
            in
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
        let_var block_name
          ~equal:(ECons {cons= cons_; payload})
          ~in_:(seqs [write ~block:dst ~i:index ~to_:(var block_name)] @@ expr))
  | EMatch {arg; branches} ->
      (* We need to transform the branches. Each branch can be either
         succesfully transformed, or plugged as-is in a write statement.
         We consider that the transformation is a success if at least one branch
         was succesfully transformed. *)
      let had_a_succes, branches =
        List.fold_left_map
          (fun had_a_success (pat, expr) ->
            match expr_dps self n_args ~dst ~index expr with
            | Some expr ->
                (true, (pat, expr))
            | None ->
                (had_a_success, Expr.(pat, write ~block:dst ~i:index ~to_:expr))
            )
          false branches
      in
      if had_a_succes then Some (Expr.match_ arg ~with_:branches) else None
  | EFunc _ | EApply _ | EPrim _ | EPrimFunc _ | EVar _ | EUnit ->
      (* In all these cases, we fail. *)
      None

let expr_dps self n_args expr =
  Fresh_vars.block_reset () ;
  expr_dps self n_args ~dst:Expr.(var "dst") ~index:Expr.(var "i") expr

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
        let args = ["dst"; "i"] @ args in
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
