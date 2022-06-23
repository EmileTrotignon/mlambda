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
  | EAlloc i ->
      let optimised, i = expr i bindings in
      (optimised, EAlloc i)
  | EProj {arr; index} ->
      let fv_arr = Expr.fv arr in
      let fv_index = Expr.fv index in
      let optimised_arr, arr =
        expr arr (String.Set.remove_from_env bindings fv_index)
      in
      let optimised_index, index =
        expr arr (String.Set.remove_from_env bindings fv_arr)
      in
      (String.Set.(optimised_arr + optimised_index), EProj {arr; index})
  | EWrite {arr; index; value} ->
      let fv_arr = Expr.fv arr in
      let fv_index = Expr.fv index in
      let fv_value = Expr.fv value in
      let optimised_arr, arr =
        expr arr String.Set.(remove_from_env bindings (fv_index + fv_value))
      in
      let optimised_index, index =
        expr arr String.Set.(remove_from_env bindings (fv_arr + fv_value))
      in
      let optimised_value, value =
        expr value String.Set.(remove_from_env bindings (fv_arr + fv_index))
      in
      ( String.Set.(optimised_arr + optimised_index + optimised_value)
      , EWrite {arr; index; value} )
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
  | ELet {var; is_rec; value; body_in} ->
      let optimised_value, value' = expr value bindings in
      let bindings = String.Set.(remove_from_env bindings (Expr.fv value)) in
      let bindings = Env.add var value bindings in
      let optimised_body_in, body_in = expr body_in bindings in
      ( String.Set.(optimised_value + optimised_body_in)
      , if String.Set.mem var optimised_body_in then body_in
        else ELet {var; is_rec; value= value'; body_in} )
  | EMatch {arg; branches} ->
      let optimised, arg = expr arg bindings in
      let bindings = String.Set.(remove_from_env bindings (Expr.fv arg)) in
      let optimised, branches =
        List.fold_left_map
          (fun optimised (pat, e) ->
            let bindings =
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