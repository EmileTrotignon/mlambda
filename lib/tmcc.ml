open Ast
open Option_monad
open Print
open Printf

let dst_name_of_int i = "dst" ^ string_of_int i

let index_name_of_int i = "index" ^ string_of_int i

type index = Int of int | Variable of string

let expr_of_index = Expr.(function Int i -> int i | Variable v -> var v)

let rec expr_dps_after_construct dst_name index rec_vars expr =
  (* print_endline "expr_dps_after_construct" ;
     print_expr stdout expr ; *)
  let dst'_name = dst_name ^ "'" in
  let e_index = expr_of_index index in
  match expr with
  | EVar var ->
      let* i = Env.find_opt var rec_vars in
      Some ((var, i), Expr.(tuple [var dst_name; e_index]))
  | ECons {cons; payload} ->
      let+ (var_used, (expr : expr)), payload =
        List.find_and_replace_i
          (fun index expr ->
            (* Here we use the current index, because if we manage to transform
               [expr] into [expr'], [expr'] should write in [dst.index]. This
               does not affect success *)
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
            (* We enter this code if transforming the payload was successful.
               Here, [expr'] is an expression that write the result of the
               evaluation of [expr] in the destination (by induction hypothesis).
               In that case, we replace the payload by [e_unit] and we return the
               transformed expression along with the set of optimised lets. *)
            ((pvars_used, expr'), Expr.unit) )
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
      ( var_used
      , let cons_ = cons in
        Expr.(
          let_ dst'_name
            ~equal:(ECons {cons= cons_; payload})
            ~in_:
              ( seqs
                  [write ~block:(var dst_name) ~i:e_index ~to_:(var dst'_name)]
              @@ let_ dst_name ~equal:(var dst'_name)
                   ~in_:(*let_ index_name ~equal:e_index ~in_:*) expr )) )
  | _ ->
      None

let expr_dps_after_construct dst_name index rec_vars expr =
  let r = expr_dps_after_construct dst_name index rec_vars expr in
  (*( match r with
    | None ->
        print_endline "failed expr_dps_after_construct on:" ;
        print_expr stdout expr
    | Some _ ->
        print_endline "success expr_dps_after_construct" ) ;*)
  r

let rec expr_dps_construct rec_vars destruct_pattern expr =
  let r =
    match (destruct_pattern, expr) with
    | PVar var, expr ->
        let i =
          match Env.find_opt var rec_vars with
          | None ->
              failwith (sprintf "could not find %s" var)
          | Some i ->
              i
        in
        let dst_name = dst_name_of_int i and index_name = index_name_of_int i in
        let+ r =
          expr_dps_after_construct dst_name (Variable index_name) rec_vars expr
        in
        ([r], [(dst_name, index_name)])
    | ( PCons {cons= pcons; payload= ppayload}
      , ECons {cons= econs; payload= epayload} )
      when econs = pcons && List.length ppayload = List.length epayload ->
        let+ exprs, dsts =
          List.combine ppayload epayload
          |> option_list_fold_map
               (fun rec_vars (p, e) ->
                 print_string "rec_vars = " ;
                 Env.iter
                   (fun var _ -> print_string var ; print_string " ")
                   rec_vars ;
                 print_newline () ;
                 let+ exprs, dsts = expr_dps_construct rec_vars p e in
                 (*let rec_vars =
                     List.fold_left
                       (fun rec_vars ((var, _), _) ->
                         printf "removing %s\n" var ; Env.remove var rec_vars )
                       rec_vars exprs
                   in*)
                 (rec_vars, (exprs, dsts)) )
               rec_vars
          |> Option.map snd |> Option.map List.split
        in
        (List.concat exprs, List.concat dsts)
    | _ ->
        None
  in
  ( match r with
  | Some _ ->
      print_endline "success expr_dps_construct on:"
  | None ->
      print_endline "failed expr_dps_construct on:" ) ;
  print_string "pattern : " ;
  print_pattern stdout destruct_pattern ;
  print_string "expr :    " ;
  print_expr stdout expr ;
  r

let expr_dps_construct rec_vars destruct_pattern expr =
  let r = expr_dps_construct rec_vars destruct_pattern expr in
  ( match r with
  | None ->
      print_endline "failed expr_dps_construct on :" ;
      print_expr stdout expr
  | Some _ ->
      print_endline "success expr_dps_construct" ) ;
  r

let n_dsts n = List.init n (fun i -> (dst_name_of_int i, index_name_of_int i))

let rec expr_dps_after_destruct self args destruct_pattern expr =
  print_expr stdout expr ;
  let r =
    match (destruct_pattern, expr) with
    | ( PCons {cons= pcons; payload= ppayload}
      , ECons {cons= econs; payload= epayload} )
      when econs = pcons && List.length ppayload = List.length epayload ->
        let rec_vars = Pattern.vars_numbered destruct_pattern in
        let+ exprs, destinations =
          expr_dps_construct rec_vars destruct_pattern expr
        in
        let bds =
          List.map
            (fun ((_var, i_arg), expr') ->
              ( Pattern.(
                  tuple
                    [var (dst_name_of_int i_arg); var (index_name_of_int i_arg)])
              , expr' ) )
            exprs
        in
        (*let (_rec_vars, bds), destinations =
            List.fold_left_mapi
              (fun i (rec_vars, bds) ele ->
                let dst_name = dst_name_of_int i
                and index_name = index_name_of_int i in
                match expr_dps_construct rec_vars ele with
                | None ->
                    ( ( rec_vars
                      , ( Pattern.any
                        , Expr.(
                            seq
                              (write ~block:(var dst_name) ~i:(var index_name)
                                 ~to_:ele )
                              unit) )
                        :: bds )
                    , (dst_name, Variable index_name) )
                | Some ((var_used, i_arg), expr') ->
                    ( ( Env.(remove var_used rec_vars)
                      , ( Pattern.(
                            tuple
                              [ var (dst_name_of_int i_arg)
                              ; var (index_name_of_int i_arg) ])
                        , expr' )
                        :: bds )
                    , (dst_name, Variable index_name) ) )
              (rec_vars, []) epayload
          in*)
        let dst_args =
          List.fold_right
            (fun (dst_name, index) acc ->
              Expr.(var dst_name :: var index :: acc) )
            destinations []
        in
        Expr.(let_and bds ~in_:(apply (var (self ^ "_dps")) (dst_args @ args)))
    | _, ELet {var; is_rec; value; body_in} ->
        let+ body_in =
          expr_dps_after_destruct self args destruct_pattern body_in
        in
        ELet {var; is_rec; value; body_in}
    | _, EMatch {arg; branches} ->
        let success, branches =
          List.fold_left_map
            (fun success (pat, expr) ->
              match expr_dps_after_destruct self args destruct_pattern expr with
              | Some expr ->
                  (true, (pat, expr))
              | None ->
                  (success, (pat, expr)) )
            false branches
        in
        if success then Some (EMatch {arg; branches}) else None
    | _ ->
        None
  in
  ( match r with
  | Some _ ->
      print_endline "success expr_dps_after_destruct on:"
  | None ->
      print_endline "failed expr_dps_after_destruct on:" ) ;
  print_expr stdout expr ; r

let expr_dps_after_destruct self args destruct_pattern expr =
  let r = expr_dps_after_destruct self args destruct_pattern expr in
  ( match r with
  | None ->
      print_endline "failed expr_dps_after_destruct on :" ;
      print_expr stdout expr
  | Some _ ->
      print_endline "success expr_dps_after_destruct" ) ;
  r

(* type cons_desc = {cons: string option; arity: int} *)
(*
let cons_desc_of_pat pat =
  match (pat : pattern) with
  | PAny ->
      None
  | PPrim _ ->
      None
  | PVar _ ->
      None
  | PCons {cons; _} ->
      Some {cons; arity= Pattern.n_vars pat} *)

let expr_dps_notrec rec_pat expr =
  let rec_vars = Pattern.vars_numbered rec_pat in
  Expr.(
    let_pat rec_pat ~equal:expr
      ~in_:
        ( rec_vars |> Env.bindings
        |> List.map (fun (varname, i) ->
               write
                 ~block:(var (dst_name_of_int i))
                 ~i:(var (index_name_of_int i))
                 ~to_:(var varname) )
        |> seq_of_list ))

let rec expr_dps_before_destruct self n_args expr =
  let r =
    match expr with
    | ELet {var; is_rec; value; body_in} ->
        print_endline "ELet {var; is_rec; value; body_in}" ;
        let+ p, body_in = expr_dps_before_destruct self n_args body_in in
        (p, ELet {var; is_rec; value; body_in})
    | EMatch
        {arg= EApply {func= EVar funcname; args}; branches= [(pattern, expr)]}
      when funcname = self && List.length args = n_args -> (
        print_endline
          "EMatch {arg= EApply {func= EVar funcname; args}; branches= \
           [(pattern, expr)]}" ;
        match expr_dps_after_destruct self args pattern expr with
        | Some expr ->
            print_endline "/!\\ => got some 123" ;
            print_expr stdout expr ;
            Some (pattern, expr)
        | None ->
            print_endline "got none 123" ;
            None )
    | EMatch {arg; branches} ->
        print_endline "EMatch {arg; branches} ->" ;
        let rec_pat, branches =
          List.fold_left_map
            (fun rec_pat (pat, expr) ->
              match expr_dps_before_destruct self n_args expr with
              | Some (rec_pat', expr) -> (
                match rec_pat with
                | Some rec_pat ->
                    (* TODO : fail if cons_desc are different *)
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
        print_endline "EMatch {arg; branches} -> 2" ;
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
         printf "expr_dps_before_destruct : %s became %s\n" (string_of_expr expr)
           (string_of_expr expr') ) ; *)
  r

let expr_dps_before_destruct self n_args expr =
  let r = expr_dps_before_destruct self n_args expr in
  ( match r with
  | None ->
      print_endline "failed transform_expr_dps_before_destruct"
  | Some (_, expr') ->
      print_endline "success transform_expr_dps_before_destruct" ;
      printf "expr_dps_before_destruct : %s became %s\n" (string_of_expr expr)
        (string_of_expr expr') ) ;
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
              (let_ "dst"
                 ~equal:(alloc ~size:(int (Pattern.n_vars rec_pat)))
                 ~in_:
                   (List.fold_lefti
                      (fun i in_ (_dst_name, index_name) ->
                        let_ index_name ~equal:(int i) ~in_ )
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
