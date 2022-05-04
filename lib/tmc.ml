open Ast
open Builder
open Option_monad

let rec transformable_expr self expr =
  match expr with
  | EApply {func= EVar func_name; args= _} when func_name = self ->
      true
  | ELet {body_in; _} ->
      transformable_expr self body_in
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

let _transformable si =
  match si with
  | Binding {name; body; is_rec} ->
      is_rec && transformable_expr name body
  | MutualRecBindings _ ->
      false

module StringSet = struct
  include Set.Make (String)

  let env_domain map = Env.fold (fun elt _ set -> add elt set) map empty

  let to_env ~f set =
    fold (fun elt map -> Env.add elt (f elt) map) set Env.empty

  let ( + ) = union

  let ( - ) = diff

  let unions sets = List.fold_left union empty sets
end

let rec pat_vars (pat : pattern) =
  match pat with
  | PAny ->
      StringSet.empty
  | PPrim _ ->
      StringSet.empty
  | PVar var ->
      StringSet.singleton var
  | PTuple pats ->
      pats |> List.map pat_vars |> StringSet.unions
  | PCons {cons= _; payload} ->
      payload |> List.map pat_vars |> StringSet.unions

let rec fv (expr : expr) =
  StringSet.(
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
          |> List.map (fun (pat, body) -> fv body - pat_vars pat)
          |> unions )
    | EPrimFunc (_, _) ->
        empty)

let rec transformable_expr_lets rec_calls_bindings self expr =
  match expr with
  | EVar var ->
      StringSet.mem var rec_calls_bindings
  | EApply {func= EVar func_name; args= _} when func_name = self ->
      true
  | ELet {var; value= EApply {func= EVar func_name; _}; body_in; is_rec= false}
    when func_name = self ->
      let rec_calls_bindings = StringSet.add var rec_calls_bindings in
      transformable_expr_lets rec_calls_bindings self body_in
  | ELet {var; is_rec; value; body_in} ->
      assert (not is_rec) ;
      let fv_value = fv value in
      let rec_calls_bindings =
        StringSet.(remove var rec_calls_bindings - fv_value)
      in
      transformable_expr_lets rec_calls_bindings self body_in
  | ECons {cons= _; payload} ->
      List.exists (transformable_expr_lets rec_calls_bindings self) payload
  | EMatch {arg; branches} ->
      let rec_calls_bindings = StringSet.(rec_calls_bindings - fv arg) in
      List.exists
        (fun (_pat, expr) ->
          transformable_expr_lets rec_calls_bindings self expr )
        branches
  | EFunc {args= _; body} ->
      transformable_expr_lets rec_calls_bindings self body
  | EApply _ | EAlloc _ | EPrim _ | EProj _ | EWrite _ | EPrimFunc _ | EUnit ->
      false

let _transformable_expr_lets rec_calls_bindings self expr =
  transformable_expr_lets (StringSet.env_domain rec_calls_bindings) self expr

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

let rec transform_expr_dps self n_args first_cons index let_cands expr =
  (* - [self] is the name of the recursive function we are transforming.
     - [index] is [None] if the index we need to write to is [e_var "index"],
       and [Some i] if it is [e_int i].
     - [let_cands] is the set of identifiers that are bound to a recursive call,
       are not ever used in that branch.
     - [expr] is the expression we are transforming. *)
  let e_index =
    match index with Some index -> e_int (index + 1) | None -> e_var "index"
  in
  match expr with
  | EVar var ->
      (* If [var] was bound to a recursive call earlier in the code, and wasn't
         ever used in that branch a identifier bound to a recursive call earlier in the code,
      *)
      let+ args = Env.find_opt var let_cands in
      ( StringSet.singleton var
      , EApply
          {func= EVar (self ^ "_dps"); args= e_var "dst'" :: e_index :: args} )
  | e when is_rec_call self n_args e ->
      let args = rec_call_args self n_args e in
      Some
        ( StringSet.empty
        , EApply
            {func= EVar (self ^ "_dps"); args= e_var "dst'" :: e_index :: args}
        )
  | ELet {var; value; body_in; is_rec= false} when is_rec_call self n_args value
    ->
      let args = rec_call_args self n_args value in
      let let_cands = Env.add var args let_cands in
      let+ optimised_lets, body_in =
        transform_expr_dps self n_args first_cons index let_cands body_in
      in
      if StringSet.mem var optimised_lets then
        (StringSet.remove var optimised_lets, body_in)
      else (optimised_lets, body_in)
  | ELet {var; is_rec; value; body_in} ->
      assert (not is_rec) ;
      let fv_value = fv value in
      let let_cands =
        StringSet.(
          let_cands |> env_domain |> Fun.flip diff fv_value |> remove var
          |> to_env ~f:(Fun.flip Env.find let_cands))
      in
      let+ optimised_lets, body_in =
        transform_expr_dps self n_args first_cons index let_cands body_in
      in
      let expr = ELet {var; is_rec; value; body_in} in
      (optimised_lets, expr)
  | ECons {cons; payload} ->
      let+ (optimised_lets, expr), payload =
        List.find_and_replace_i
          (fun index expr ->
            let+ optimised_lets, expr =
              transform_expr_dps self n_args false (Some index) let_cands expr
            in
            ((optimised_lets, expr), e_unit) )
          payload
      in
      ( optimised_lets
      , e_let "dst'"
          ~equal:(ECons {cons; payload})
          ~in_:
            ( e_seqs
                [e_write ~block:(e_var "dst") ~i:e_index ~to_:(e_var "dst'")]
            @@ e_let "dst" ~equal:(e_var "dst'") ~in_:expr ) )
  | EMatch {arg; branches} ->
      let (had_a_succes, optimised_lets), branches =
        List.fold_left_map
          (fun (had_a_success, lets) (pat, expr) ->
            match
              transform_expr_dps self n_args first_cons index let_cands expr
            with
            | Some (optimised_lets, expr) ->
                ((true, StringSet.(optimised_lets + lets)), (pat, expr))
            | None ->
                ( (had_a_success, lets)
                , (pat, e_write ~block:(e_var "dst") ~i:e_index ~to_:expr) ) )
          (false, StringSet.empty) branches
      in
      if had_a_succes then Some (optimised_lets, e_match arg ~with_:branches)
      else None
  | EFunc _
  | EApply _
  | EAlloc _
  | EPrim _
  | EProj _
  | EWrite _
  | EPrimFunc _
  | EUnit ->
      None

let transform_expr_dps_let self n_args expr =
  Option.map snd @@ transform_expr_dps self n_args true None Env.empty expr

let transform_expr self expr =
  match expr with
  | EFunc {args; body} ->
      let+ transformed = transform_expr_dps_let self (List.length args) body in
      let name_dps = self ^ "_dps" in
      let dps =
        let body =
          e_seqs
            [ e_apply (e_var "print") [e_prim @@ pr_string "dst ="]
            ; e_apply (e_var "print") [e_var "dst"] ]
          @@ transformed
        in
        let args = ["dst"; "index"] @ args in
        EFunc {args; body}
      in
      let entry_point =
        e_func ~args
          ~body:
            (e_let "dst"
               ~equal:(e_alloc ~size:(e_int 1))
               ~in_:
                 ( e_seqs
                     [ e_apply (e_var "print")
                         [e_prim @@ pr_string "entry dst ="]
                     ; e_apply (e_var "print") [e_var "dst"] ]
                 @@ e_seqs
                      [ e_apply (e_var name_dps)
                          ([e_var "dst"; e_int 0] @ (args |> List.map e_var)) ]
                      (e_proj (e_var "dst") (e_int 0)) ) )
      in
      (entry_point, dps)
  | _ ->
      None

let si si =
  match si with
  | Binding {name; body; is_rec} -> (
      if not is_rec then si
      else
        match transform_expr name body with
        | Some (entry, dps) ->
            MutualRecBindings [(name, entry); (name ^ "_dps", dps)]
        | None ->
            si )
  | MutualRecBindings _ ->
      si

let program program = List.map si program
