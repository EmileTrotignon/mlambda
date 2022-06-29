open Ast
open Option_monad
open Print
open Printf

let dst_name_of_int i = "dst" ^ string_of_int i

let index_name_of_int i = "index" ^ string_of_int i

type index = Int of int | Variable of string

let expr_of_index = Expr.(function Int i -> int i | Variable v -> var v)

let rec transform_expr_dps_after_construct dst_name index rec_vars expr =
  print_endline "transform_expr_dps_after_construct" ;
  print_expr stdout expr ;
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
              transform_expr_dps_after_construct dst_name (Int index) rec_vars
                expr
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
  let r = transform_expr_dps_after_construct dst_name index rec_vars expr in
  ( match r with
  | None ->
      print_endline "failed expr_dps_after_construct"
  | Some _ ->
      print_endline "success expr_dps_after_construct" ) ;
  r

let n_dsts n = List.init n (fun i -> (dst_name_of_int i, index_name_of_int i))

let rec expr_dps_after_destruct self args destruct_pattern expr =
  print_endline "expr_dps_after_destruct" ;
  match (destruct_pattern, expr) with
  | ( PCons {cons= pcons; payload= ppayload}
    , ECons {cons= econs; payload= epayload} )
    when econs = pcons && List.length ppayload = List.length epayload ->
      let rec_vars = Pattern.vars_numbered destruct_pattern in
      let (_rec_vars, bds), destinations =
        List.fold_left_mapi
          (fun i (rec_vars, bds) ele ->
            let dst_name = dst_name_of_int i
            and index_name = index_name_of_int i in
            match
              expr_dps_after_construct dst_name (Variable index_name) rec_vars
                ele
            with
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
      in
      let dst_args =
        List.fold_right
          (fun (dst_name, index) acc ->
            Expr.var dst_name :: expr_of_index index :: acc )
          destinations []
      in
      Some
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

let expr_dps_after_destruct self args destruct_pattern expr =
  let r = expr_dps_after_destruct self args destruct_pattern expr in
  ( match r with
  | None ->
      print_endline "failed expr_dps_after_destruct"
  | Some _ ->
      print_endline "success expr_dps_after_destruct" ) ;
  r

type cons_desc = {cons: string option; arity: int}

let cons_desc_of_pat pat =
  match (pat : pattern) with
  | PAny ->
      None
  | PPrim _ ->
      None
  | PVar _ ->
      None
  | PCons {cons; payload} ->
      Some {cons; arity= List.length payload}

let rec expr_dps_before_destruct self n_args expr =
  let r =
    match expr with
    | ECons {cons; payload} ->
        Some
          ( {cons; arity= List.length payload}
          , Expr.(
              seqs
                (List.mapi
                   (fun i expr ->
                     write
                       ~block:(var (dst_name_of_int i))
                       ~i:(var (index_name_of_int i))
                       ~to_:expr )
                   payload )
                unit) )
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
        let* cons_desc = cons_desc_of_pat pattern in
        match expr_dps_after_destruct self args pattern expr with
        | Some expr ->
            print_endline "/!\\ => got some 123" ;
            print_expr stdout expr ;
            Some (cons_desc, expr)
        | None ->
            print_endline "got none 123" ;
            None )
    | EMatch {arg; branches} ->
        print_endline "EMatch {arg; branches} ->" ;
        let cons_desc, branches =
          List.fold_left_map
            (fun cons_desc (pat, expr) ->
              match expr_dps_before_destruct self n_args expr with
              | Some (cons_desc', expr) -> (
                match cons_desc with
                | Some cons_desc ->
                    (* TODO : fail if cons_desc are different *)
                    if cons_desc = cons_desc' then (Some cons_desc, (pat, expr))
                    else (Some cons_desc, (pat, expr))
                | None ->
                    (Some cons_desc', (pat, expr)) )
              | None ->
                  failwith "every branch needs to succeed" )
            None branches
        in
        print_endline "EMatch {arg; branches} -> 2" ;
        let+ cons_desc in
        (cons_desc, EMatch {arg; branches})
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
      let+ {arity; cons}, transformed =
        expr_dps_before_destruct self (List.length args) body
      in
      let name_dps = self ^ "_dps" in
      let dsts = n_dsts arity in
      let dps =
        let body = transformed in
        let args =
          (dsts |> List.map (fun (a, b) -> [a; b]) |> List.concat) @ args
        in
        EFunc {args; body}
      in
      let cons_ = cons in
      let entry_point =
        Expr.(
          func ~args
            ~body:
              (let_ "dst"
                 ~equal:
                   (alloc
                      ~size:
                        (int (arity + if Option.is_some cons_ then 1 else 0)) )
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
                         (ECons
                            { cons= cons_
                            ; payload=
                                List.map
                                  (fun (_dst, index) ->
                                    proj (var "dst") (var index) )
                                  dsts } ) )
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
