open Ast

let rec var v bindings =
  match Env.find_opt v bindings with
  | Some (EVar var') when Env.mem var' bindings ->
      let optimised, result = var var' bindings in
      (String.Set.(singleton v + optimised), result)
  | Some expr ->
      (String.Set.singleton v, expr)
  | None ->
      (String.Set.empty, EVar v)

let rec expr e bindings =
  match e with
  | EVar v ->
      var v bindings
  | EFunc {args; body} ->
      let bindings =
        String.Set.remove_from_env bindings (String.Set.of_list args)
      in
      let optimised, body = expr body bindings in
      (optimised, EFunc {args; body})
  | EApply _ | EPrim _ | EPrimFunc _ | EUnit ->
      (String.Set.empty, e)
  | ECons {cons; payload} ->
      let optimised, payload =
        List.fold_left_mapi
          (fun i optimised e ->
            let fv_others =
              List.fold_lefti
                (fun i' fv_other e ->
                  String.Set.(fv_other + if i = i' then empty else Expr.fv e) )
                String.Set.empty payload
            in
            let bindings = String.Set.remove_from_env bindings fv_others in
            let optimised', e = expr e bindings in
            (String.Set.(optimised + optimised'), e) )
          String.Set.empty payload
      in
      (optimised, ECons {cons; payload})
  (*let var = arg in body_in *)
  | EMatch {arg; branches= [(PVar var, body_in)]} ->
      let bindings_arg =
        String.Set.(remove_from_env bindings (Expr.fv body_in))
      in
      let optimised_arg, arg = expr arg bindings_arg in
      let bindings = String.Set.(remove_from_env bindings (Expr.fv arg)) in
      let bindings = Env.add var arg bindings in
      let optimised_body_in, body_in = expr body_in bindings in
      ( String.Set.(optimised_arg + optimised_body_in)
      , if String.Set.mem var optimised_body_in then body_in
        else EMatch {arg= arg; branches= [(PVar var, body_in)]} )
  | EMatch {arg; branches} ->
      let fv_branches  =
        branches |> List.map snd |> List.map Expr.fv |> String.Set.unions
      in
      let bindings_arg = String.Set.(remove_from_env bindings fv_branches) in
      let optimised, arg = expr arg bindings_arg in
      let bindings = String.Set.(remove_from_env bindings (Expr.fv arg)) in
      let bindings = String.Set.(remove_from_env bindings optimised) in
      let optimised, branches =
        List.fold_left_map
          (fun  optimised (pat, e) ->
            let bindings =
              (* shadowed *)
              String.Set.(remove_from_env bindings (Pattern.vars pat))
            in
            let optimised', e = expr e bindings in
            (String.Set.(optimised + optimised'), (pat, e)) )
          optimised branches
      in
      (optimised, EMatch {arg; branches})

let expr e = expr e Env.empty |> snd

let si (si : struct_item) =
  match si with
  | Binding {name; is_rec; body} ->
      Binding {name; is_rec; body= expr body}
  | MutualRecBindings bds ->
      MutualRecBindings (List.map (fun (name, body) -> (name, expr body)) bds)

let program = List.map si