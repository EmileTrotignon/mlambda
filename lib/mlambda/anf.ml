open Ast

let recompote (bds, e) =
  List.fold_left (fun e (var, e') -> Expr.let_ var ~equal:e' ~in_:e) e bds

let rec expr ?(bound = false) ~tail e =
  match e with
  | EVar _ | EPrim _ | EUnit | EPrimFunc (_, _) ->
      ([], e)
  | EFunc {args; body} ->
      ([], EFunc {args; body= recompote @@ expr ~tail body})
  | EApply {func; args} ->
      let bds_func, func = expr ~tail:false func in
      let bds_args, args = args |> List.map (expr ~tail:false) |> List.split in
      let bds_args = List.concat bds_args in
      let bds = bds_args @ bds_func in
      if bound || tail then (bds, EApply {func; args})
      else
        let var = Fresh_vars.fv () in
        ((Pattern.var var, EApply {func; args}) :: bds, Expr.var var)
  | ECons {cons; payload} ->
      let bds, payload = payload |> List.map (expr ~tail:false) |> List.split in
      let bds = List.concat bds in
      (bds, ECons {cons; payload})
  | EMatch {arg; branches= [(p, e)]} ->
      let bds_arg, arg = expr ~bound:true ~tail:false arg in
      let bds_e, e = expr ~tail:true e in
      let bds = bds_e @ [(p, arg)] @ bds_arg in
      (bds, e)
  | EMatch {arg; branches} ->
      (* if tail then ( *)
      let bds, arg = expr ~bound ~tail:false arg in
      let branches =
        List.map
          (function p, e -> (p, recompote (expr ~tail:true e)))
          branches
      in
      if bound || tail then (bds, EMatch {arg; branches})
      else
        let var = Fresh_vars.fv () in
        ((Pattern.var var, EMatch {arg; branches}) :: bds, Expr.var var)

let expr e = recompote (expr ~tail:true e)

let struct_item si =
  match si with
  | Binding {name; is_rec; body} ->
      Fresh_vars.fv_reset () ;
      Binding {name; is_rec; body= expr body}
  | MutualRecBindings bds ->
      MutualRecBindings
        (List.map
           (fun (p, e) ->
             Fresh_vars.fv_reset () ;
             (p, expr e) )
           bds )

let program = List.map struct_item