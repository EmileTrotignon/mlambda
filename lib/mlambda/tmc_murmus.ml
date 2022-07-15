open Ast
open Option_monad
open Printf

type index = Int of int | Variable of string

let expr_of_index = Expr.(function Int i -> int i | Variable v -> var v)

let fresh_block =
  let n = ref 0 in
  fun () ->
    n := !n + 1 ;
    "block" ^ string_of_int !n

let dst_list_name_output i = "dst_out_" ^ string_of_int i

let dst_list_name_input i = "dst_in_" ^ string_of_int i

let write_all_dst = Expr.var "write_all_dst"

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
let rec expr_dps_after_construct dst rec_vars expr =
  print_string "EXPR DPS AFTER CONSTRUCT rec_vars =" ;
  Env.iter (fun var _ -> printf "%s " var) rec_vars ;
  print_string "; " ;
  Expr.print expr ;
  match expr with
  | EVar var when Env.mem var rec_vars ->
      let i_var = Env.find var rec_vars in
      Some
        Expr.(
          write
            ~block:(var (dst_list_name_output i_var))
            ~i:(int 0)
            ~to_:
              ( match dst with
              | `list list_name ->
                  apply (var "list_concat")
                    [ var list_name
                    ; proj (var (dst_list_name_output i_var)) (int 0) ]
              | `block (block_name, e_index) ->
                  cons "::"
                    ~payload:
                      [ tuple [var block_name; e_index]
                      ; proj (var (dst_list_name_output i_var)) (int 0) ] ))
  | ECons {cons; payload} ->
      let block_name = fresh_block () in
      let exprs, payload =
        List.fold_lefti
          (fun index (exprs, payload) expr ->
            let index = index + if Option.is_some cons then 1 else 0 in
            match
              expr_dps_after_construct
                (`block (block_name, Expr.int index))
                rec_vars expr
            with
            | Some expr' ->
                (expr' :: exprs, Expr.unit :: payload)
            | None ->
                (exprs, expr :: payload) )
          ([], []) payload
      in
      let exprs = List.rev exprs and payload = List.rev payload in
      let cons_ = cons in
      Some
        ( match dst with
        | `list li_name ->
            Expr.(
              let_var block_name
                ~equal:(ECons {cons= cons_; payload})
                ~in_:
                  (seq_of_list
                     ( [apply write_all_dst [var li_name; var block_name]]
                     @ exprs ) ))
        | `block (dst_name, e_index) ->
            Expr.(
              let_var block_name
                ~equal:(ECons {cons= cons_; payload})
                ~in_:
                  (seq_of_list
                     ( [ write ~block:(var dst_name) ~i:e_index
                           ~to_:(var block_name) ]
                     @ exprs ) )) )
  | EMatch {arg; branches} ->
      let* branches =
        option_list_map
          (fun (pat, expr) ->
            let* expr = expr_dps_after_construct dst rec_vars expr in
            Some (pat, expr) )
          branches
      in
      Some (EMatch {arg; branches})
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
      let dst_name = dst_list_name_input i in
      let+ r = expr_dps_after_construct (`list dst_name) rec_vars expr in
      r
  | ( PCons {cons= pcons; payload= ppayload}
    , ECons {cons= econs; payload= epayload} )
    when econs = pcons && List.length ppayload = List.length epayload ->
      let+ exprs =
        List.combine ppayload epayload
        |> option_list_map (fun (p, e) ->
               let+ exprs = expr_dps_construct rec_vars_complete rec_vars p e in
               exprs )
      in
      Expr.seq_of_list exprs
  | _ ->
      None

let expr_dps_construct rec_vars = expr_dps_construct rec_vars rec_vars

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
      let+ expr = expr_dps_construct rec_vars rec_pattern expr in
      let destinations =
        rec_vars |> Env.bindings
        |> List.sort (fun (_, i) (_, i') -> Int.compare i i')
        |> List.map (fun (_, i) ->
               Expr.(proj (var (dst_list_name_output i)) (int 0)) )
      in
      Expr.(seq expr (apply (var (self ^ "_dps")) (destinations @ args)))
  | PVar _, _ ->
      let rec_vars = Pattern.vars_numbered rec_pattern in
      let+ expr = expr_dps_construct rec_vars rec_pattern expr in
      let destinations =
        rec_vars |> Env.bindings
        |> List.sort (fun (_, i) (_, i') -> Int.compare i i')
        |> List.map (fun (_, i) ->
               Expr.(proj (var (dst_list_name_output i)) (int 0)) )
      in
      Expr.(seq expr (apply (var (self ^ "_dps")) (destinations @ args)))
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
               apply write_all_dst [var (dst_list_name_input i); var varname] )
        |> seq_of_list ))

(** [expr_dps_before_destruct self n_args expr]
    - [self] is the name of the recursive function we are transforming.
    - [n_args] is its arity.
    - [expr] is the expression we are currently transforming.
    Returns [None] if the transformation fails, and [Some (p, e)] if it
    succeed, where [p] is the pattern with which a recursive call was destroyed,
    and [e] is an expression that writes the result lelet r =let r =t r = r = [expr] in destinations
    according to the pattern. *)
let rec expr_dps_before_destruct self n_args expr =
  match expr with
  | EMatch {arg= EApply {func= EVar funcname; args}; branches= [(pattern, expr)]}
    when funcname = self && List.length args = n_args ->
      let+ expr = expr_dps_after_destruct self args pattern expr in
      let arity = Pattern.n_vars pattern in
      let expr =
        Expr.(
          let_and
            (List.init arity (fun i ->
                 (Pattern.var (dst_list_name_output i), tuple [cons "[]"]) ) )
            ~in_:expr)
      in
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

let index_name i = "i" ^ string_of_int i

let expr self e =
  match e with
  | EFunc {args; body} ->
      let+ rec_pat, transformed =
        expr_dps_before_destruct self (List.length args) body
      in
      let rec_vars = Pattern.vars_numbered rec_pat in
      let arity = Pattern.n_vars rec_pat in
      let name_dps = self ^ "_dps" in
      let dps =
        let body = transformed in
        let args = List.init arity dst_list_name_input @ args in
        EFunc {args; body}
      in
      let rec aux_construct (pat : pattern) =
        match pat with
        | PAny | PPrim _ ->
            assert false
        | PVar var ->
            let i = Env.find var rec_vars in
            Expr.(proj (var "dst") (int i))
        | PCons {cons; payload} ->
            ECons {cons; payload= List.map aux_construct payload}
      in
      let entry_point =
        Expr.(
          func ~args
            ~body:
              (let_var "dst"
                 ~equal:(alloc ~size:(int arity))
                 ~in_:
                   (seq
                      (apply (var name_dps)
                         ( List.init arity (fun i ->
                               list [tuple [var "dst"; int i]] )
                         @ (args |> List.map var) ) )
                      (aux_construct rec_pat) ) ))
      in
      (entry_point, dps)
  | _ ->
      None

let expr_cons self e =
  match expr self e with
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
