open Ast
open Option_monad
open Printf

let dst_name_of_int i = "dst" ^ string_of_int i

let index_name_of_int i = "i" ^ string_of_int i

type index = Int of int | Variable of string

let expr_of_index = Expr.(function Int i -> int i | Variable v -> var v)

(** [expr_dps_after_construct dst_name index rec_vars expr]
    - [dst_name] and [index] is the destination we are writing to.
    - [rec_vars] is the set of available variable to be obtained from the
      recursive call.
    - [expr] is the expression we are transforming. It must mention a single
      variable from [rec_vars]. It may have multiple branches, but each branch
      must mention the same variable from [rec_vars].
    The return value is [None] if the conditions on [expr] are not satisfied,
   and [Some (v, e)] otherwise, where [v] is the variable from [rec_vars] and
   [e] is an expression that writes [expr] to [dst_name.index] with a hole
   instead of [v], and returns (at runtime) the address of the hole. *)
let rec expr_dps_after_construct dst_name index rec_vars expr =
  let dst'_name = dst_name ^ "'" in
  let e_index = expr_of_index index in
  match expr with
  | EVar var when Env.mem var rec_vars ->
      Some (var, Expr.(tuple [var dst_name; e_index]))
  | ECons {cons; payload} ->
      let+ (var_used, expr), payload =
        List.find_and_replace_i
          (fun index expr ->
            let rec_vars =
              String.Set.(
                remove_from_env rec_vars
                  (unions
                     (List.mapi
                        (fun index' expr ->
                          if index' = index then empty else Expr.fv expr )
                        payload ) ))
            in
            let index = index + if Option.is_some cons then 1 else 0 in
            let+ pvars_used, expr' =
              expr_dps_after_construct dst_name (Int index) rec_vars expr
            in
            ((pvars_used, expr'), Expr.unit) )
          payload
      in
      ( var_used
      , let cons_ = cons in
        Expr.(
          let_var dst'_name
            ~equal:(ECons {cons= cons_; payload})
            ~in_:
              ( seqs
                  [write ~block:(var dst_name) ~i:e_index ~to_:(var dst'_name)]
              @@ let_var dst_name ~equal:(var dst'_name) ~in_:expr )) )
  | EMatch {arg; branches} ->
      let* var, branches =
        option_list_fold_map
          (fun var (pat, expr) ->
            let rec_vars =
              String.Set.remove_from_env rec_vars (Pattern.vars pat)
            in
            let* var', expr =
              expr_dps_after_construct dst_name index rec_vars expr
            in
            match var with
            | Some var ->
                if var = var' then Some (Some var, (pat, expr)) else None
            | None ->
                Some (Some var', (pat, expr)) )
          None branches
      in
      let* var in
      Some (var, EMatch {arg; branches})
  | _ ->
      None

(** [expr_dps_construct rec_vars_complete rec_vars rec_pattern expr]
    - [rec_vars_complete] contains all variables mapped to their index.
    - [rec_vars] contains variables available for "reading", those provided by
      the recursive call in the original function.
    - [rec_pattern] contains the pattern we are destructing the recursive
      call with.
    - [expr] contains the construct of the expression we are returning.
      [expr] and [rec_pattern] should have compatible shape, but [expr] may
      be more complex than [rec_pattern]
    It returns an option. This option is [None] if the transformation was
    unsuccessful.
    It is [Some r] if it was succesful. In that case, [r] is a list of pairs
    [(v, e)]. [v] is the recursive variable that is needed in the expression
    [e]. [e] is an expression that write a field of the returned constructor in
    its appropriate destination, with a hole instead of the recursive variable
    that should be there. Its returns (at runtime) a destination that will be
    passed to the recursive call for the hole to be filled.
*)
let rec expr_dps_construct rec_vars_complete rec_vars rec_pattern expr =
  print_string "EXPR DPS CONSTRUCT rec_vars =" ;
  Env.iter (fun var _ -> printf "%s " var) rec_vars ;
  print_string "; " ;
  Expr.print expr ;
  match (rec_pattern, expr) with
  | PVar var, expr ->
      let i =
        match Env.find_opt var rec_vars_complete with
        | None ->
            failwith (sprintf "could not find %s" var)
        | Some i ->
            i
      in
      let dst_name = dst_name_of_int i and index_name = index_name_of_int i in
      let+ r =
        expr_dps_after_construct dst_name (Variable index_name) rec_vars expr
      in
      [r]
  | ( PCons {cons= pcons; payload= ppayload}
    , ECons {cons= econs; payload= epayload} )
    when econs = pcons && List.length ppayload = List.length epayload ->
      let+ _, exprs =
        List.combine ppayload epayload
        |> option_list_fold_map
             (fun rec_vars (p, e) ->
               let+ exprs = expr_dps_construct rec_vars_complete rec_vars p e in
               let rec_vars =
                 List.fold_left
                   (fun rec_vars (var, _) ->
                     printf "removing %s\n" var ; Env.remove var rec_vars )
                   rec_vars exprs
               in
               (rec_vars, exprs) )
             rec_vars
      in
      List.concat exprs
  | _ ->
      None

let expr_dps_construct rec_vars = expr_dps_construct rec_vars rec_vars

let n_dsts n = List.init n (fun i -> (dst_name_of_int i, index_name_of_int i))

(** [expr_dps_after_destruct self args rec_pattern expr]
    - [self] is the name of the function we are transforming.
    - [args] are the arguments used in the recursive call to that function.
    - [rec_pattern] is the pattern that is used to destruct the recursive call.
    - [expr] is the expression we are transforming.
    Returns [Some e] if the transformation was succesful, [None] if it was not.
*)
let rec expr_dps_after_destruct self args rec_pattern expr =
  match (rec_pattern, expr) with
  | ( PCons {cons= pcons; payload= ppayload}
    , ECons {cons= econs; payload= epayload} )
    when econs = pcons && List.length ppayload = List.length epayload ->
      let rec_vars = Pattern.vars_numbered rec_pattern in
      let+ exprs = expr_dps_construct rec_vars rec_pattern expr in
      let bds =
        List.map
          (fun (var, expr') ->
            let i_arg = Env.find var rec_vars in
            ( Pattern.(
                tuple
                  [var (dst_name_of_int i_arg); var (index_name_of_int i_arg)])
            , expr' ) )
          exprs
      in
      let destinations =
        rec_vars |> Env.bindings
        |> List.sort (fun (_, i) (_, i') -> Int.compare i i')
        |> List.map (fun (_, i) -> (dst_name_of_int i, index_name_of_int i))
      in
      let dst_args =
        List.fold_right
          (fun (dst_name, index) acc -> Expr.(var dst_name :: var index :: acc))
          destinations []
      in
      Expr.(let_and bds ~in_:(apply (var (self ^ "_dps")) (dst_args @ args)))
  | _, EMatch {arg; branches} ->
      let success, branches =
        List.fold_left_map
          (fun success (pat, expr) ->
            match expr_dps_after_destruct self args rec_pattern expr with
            | Some expr ->
                (true, (pat, expr))
            | None ->
                (success, (pat, expr)) )
          false branches
      in
      if success then Some (EMatch {arg; branches}) else None
  | _ ->
      None

let expr_dps_after_destruct self args rec_pattern expr =
  let r = expr_dps_after_destruct self args rec_pattern expr in
  ( match r with
  | None ->
      print_endline "failed expr_dps_after_destruct on :" ;
      Expr.print expr
  | Some _ ->
      print_endline "success expr_dps_after_destruct" ) ;
  r

(** [expr_dps_notrec rec_pat expr]
    - [rec_pat] is the pattern that is used somewhere else in the function to
      destruct the result of a recursive call.
    - [expr] is an expression of the function that does not contain a
      recursive call.
    Returns an expression that writes the result of [expr] to the destinations
    corresponding to the pattern. *)
let expr_dps_notrec rec_pat expr =
  let rec_vars = Pattern.vars_numbered rec_pat in
  Expr.(
    let_ rec_pat ~equal:expr
      ~in_:
        ( rec_vars |> Env.bindings
        |> List.map (fun (varname, i) ->
               write
                 ~block:(var (dst_name_of_int i))
                 ~i:(var (index_name_of_int i))
                 ~to_:(var varname) )
        |> seq_of_list ))

(** [expr_dps_before_destruct self n_args expr]
    - [self] is the name of the recursive function we are transforming.
    - [n_args] is its arity.
    - [expr] is the expression we are currently transforming.
    Returns [None] if the transformation fails, and [Some (p, e)] if it
    succeed, where [p] is the pattern with which a recursive call was destroyed,
    and [e] is an expression that writes the result of [expr] in destinations
    according to the pattern. *)
let rec expr_dps_before_destruct self n_args expr =
  let r =
    match expr with
    | EMatch
        {arg= EApply {func= EVar funcname; args}; branches= [(pattern, expr)]}
      when funcname = self && List.length args = n_args ->
        let+ expr = expr_dps_after_destruct self args pattern expr in
        (pattern, expr)
    | EMatch {arg; branches} ->
        let rec_pat, branches =
          List.fold_left_map
            (fun rec_pat (pat, expr) ->
              match expr_dps_before_destruct self n_args expr with
              | Some (rec_pat', expr) -> (
                match rec_pat with
                | Some rec_pat ->
                    if Pattern.(rec_pat = rec_pat') then
                      (Some rec_pat, (true, pat, expr))
                    else
                      failwith
                        "Destructing with two different patterns, cannot \
                         determine numbers of arguments"
                | None ->
                    (Some rec_pat', (true, pat, expr)) )
              | None ->
                  (None, (false, pat, expr)) )
            None branches
        in
        let+ rec_pat in
        let branches =
          List.map
            (fun (transformed, pat, expr) ->
              if transformed then (pat, expr)
              else (pat, expr_dps_notrec rec_pat expr) )
            branches
        in
        (rec_pat, EMatch {arg; branches})
    | _ ->
        None
  in
  (* ( match r with
     | None ->
         print_endline "failed transform_expr_dps_before_destruct"
     | Some (_, expr') ->
         print_endline "success transform_expr_dps_before_destruct" ;
         printf "expr_dps_before_destruct : %s became %s\n" (Expr.to_string expr)
           (Expr.to_string expr') ) ; *)
  r

let expr_dps_before_destruct self n_args expr =
  let r = expr_dps_before_destruct self n_args expr in
  ( match r with
  | None ->
      print_endline "failed transform_expr_dps_before_destruct"
  | Some (_, expr') ->
      print_endline "success transform_expr_dps_before_destruct" ;
      printf "expr_dps_before_destruct : %s became %s\n" (Expr.to_string expr)
        (Expr.to_string expr') ) ;
  r

(*
let dsts_of_pat pat =
  dsts_of_pat pat |> List.map (fun (a, b) -> [a; b]) |> List.concat *)

let expr_cons self expr =
  match expr with
  | EFunc {args; body} ->
      let+ rec_pat, transformed =
        expr_dps_before_destruct self (List.length args) body
      in
      let rec_vars = Pattern.vars_numbered rec_pat in
      let arity = Pattern.n_vars rec_pat in
      let name_dps = self ^ "_dps" in
      let dsts = n_dsts arity in
      let dps =
        let body = transformed in
        let args =
          (dsts |> List.map (fun (a, b) -> [a; b]) |> List.concat) @ args
        in
        EFunc {args; body}
      in
      let rec aux_construct (pat : pattern) =
        match pat with
        | PAny | PPrim _ ->
            assert false
        | PVar var ->
            let i = Env.find var rec_vars in
            Expr.(proj (var "dst") (var (index_name_of_int i)))
        | PCons {cons; payload} ->
            ECons {cons; payload= List.map aux_construct payload}
      in
      let entry_point =
        Expr.(
          func ~args
            ~body:
              (let_var "dst"
                 ~equal:(alloc ~size:(int (Pattern.n_vars rec_pat)))
                 ~in_:
                   (List.fold_lefti
                      (fun i in_ (_dst_name, index_name) ->
                        let_var index_name ~equal:(int i) ~in_ )
                      (seqs
                         [ apply (var name_dps)
                             ( ( dsts
                               |> List.map (fun (_a, b) -> ["dst"; b])
                               |> List.concat |> List.map var )
                             @ (args |> List.map var) ) ]
                         (aux_construct rec_pat) )
                      dsts ) ))
      in
      (entry_point, dps)
  | _ ->
      None

let expr_cons self expr =
  match expr_cons self expr with
  | Some v ->
      Some v
  | None ->
      Printf.printf "Could not transform %s\n" self ;
      None

let si si =
  match si with
  | Binding {name; body; is_rec} -> (
      if not is_rec then si
      else
        match expr_cons name body with
        | Some (entry, dps) ->
            MutualRecBindings [(name, entry); (name ^ "_dps", dps)]
        | None ->
            si )
  | MutualRecBindings _ ->
      si

let program program = List.map si program
