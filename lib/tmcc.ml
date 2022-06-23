open Ast
open Option_monad
open Print
open Printf

let dst_name_of_int i = "dst" ^ string_of_int i

let index_name_of_int i = "index" ^ string_of_int i

type index = Int of int | Variable of string

let rec transform_expr_dps_after_construct dst_name index pvars expr =
  print_endline "transform_expr_dps_after_construct" ;
  print_expr stdout expr ;
  let dst'_name = dst_name ^ "'" in
  let e_index =
    match index with Variable name -> Expr.var name | Int i -> Expr.int i
  in
  match expr with
  | EVar var when String.Set.mem var pvars ->
      Some (String.Set.singleton var, dst_name, index, Fun.id)
  | ECons {cons; payload} ->
      let+ (pvars_used, dst_name, index, (expr : expr -> expr)), payload =
        List.find_and_replace_i
          (fun index expr ->
            (* Here we use the current index, because if we manage to transform
               [expr] into [expr'], [expr'] should write in [dst.index]. This
               does not affect success *)
            let pvars =
              String.Set.(
                pvars
                - unions
                    (List.mapi
                       (fun index' expr ->
                         if index' = index then empty else Expr.fv expr )
                       payload ))
            in
            let index = index + if Option.is_some cons then 1 else 0 in
            let+ pvars_used, dst_name, index, (expr' : expr -> expr) =
              transform_expr_dps_after_construct dst_name (Int index) pvars expr
            in
            (* We enter this code if transforming the payload was successful.
               Here, [expr'] is an expression that write the result of the
               evaluation of [expr] in the destination (by induction hypothesis).
               In that case, we replace the payload by [e_unit] and we return the
               transformed expression along with the set of optimised lets. *)
            ((pvars_used, dst_name, index, expr'), Expr.unit) )
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
      ( pvars_used
      , dst_name
      , index
      , fun expr_arg ->
          let cons_ = cons in
          Expr.(
            let_ dst'_name
              ~equal:(ECons {cons= cons_; payload})
              ~in_:
                ( seqs
                    [write ~block:(var dst_name) ~i:e_index ~to_:(var dst'_name)]
                @@ let_ dst_name ~equal:(var dst'_name) ~in_:(expr expr_arg) ))
      )
  | _ ->
      None

let expr_dps_after_construct dst_name index pvars expr =
  let r = transform_expr_dps_after_construct dst_name index pvars expr in
  ( match r with
  | None ->
      print_endline "failed transform_expr_dps_after_construct"
  | Some _ ->
      print_endline "success transform_expr_dps_after_construct" ) ;
  r

let e_index = function Int i -> Expr.int i | Variable v -> Expr.var v

let n_dsts n = List.init n (fun i -> (dst_name_of_int i, index_name_of_int i))

let rec expr_dps_after_destruct self args destruct_pattern expr =
  print_endline "expr_dps_after_destruct" ;
  match (destruct_pattern, expr) with
  | ( PCons {cons= pcons; payload= ppayload}
    , ECons {cons= econs; payload= epayload} )
    when econs = pcons && List.length ppayload = List.length epayload ->
      let pvars = Pattern.vars destruct_pattern in
      let (_success, _pvars, fexpr), destinations =
        List.fold_left_mapi
          (fun i (_success, pvars, fexpr) ele ->
            let dst_name = dst_name_of_int i
            and index_name = index_name_of_int i in
            match
              expr_dps_after_construct dst_name (Variable index_name) pvars ele
            with
            | None ->
                ( ( true
                  , pvars
                  , fun (aexpr : expr) ->
                      fexpr
                        Expr.(
                          seq
                            (write ~block:(var dst_name) ~i:(var index_name)
                               ~to_:ele )
                            aexpr) )
                , (dst_name, Variable index_name) )
            | Some (pvars_used, dst_name, index, fexpr') ->
                ( ( true
                  , String.Set.(pvars - pvars_used)
                  , fun aexpr -> fexpr (fexpr' aexpr) )
                , (dst_name, index) ) )
          (false, pvars, Fun.id) epayload
      in
      let dst_args =
        List.fold_right
          (fun (dst_name, index) acc ->
            Expr.var dst_name :: e_index index :: acc )
          destinations []
      in
      Some (fexpr Expr.(apply (var (self ^ "_dps")) (dst_args @ args)))
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
      print_endline "failed transform_expr_dps_after_destruct"
  | Some _ ->
      print_endline "success transform_expr_dps_after_destruct" ) ;
  r

let prim_eq pr1 pr2 =
  match (pr1, pr2) with
  | PrString str, PrString str' ->
      String.equal str str'
  | PrInt i, PrInt i' ->
      Int.equal i i'
  | PrBool b, PrBool b' ->
      Bool.equal b b'
  | _, _ ->
      false

let rec pat_eq p1 p2 =
  match (p1, p2) with
  | PAny, PAny ->
      true
  | PPrim prim, PPrim prim' ->
      prim_eq prim prim'
  | PVar var, PVar var' ->
      String.equal var var'
  | PCons {cons; payload}, PCons {cons= cons'; payload= payload'} ->
      Option.equal String.equal cons cons' && List.equal pat_eq payload payload'
  | _, _ ->
      false

let _pat_eq p1 p2 =
  let r = pat_eq p1 p2 in
  printf "%s and %s are %s\n" (string_of_pattern p1) (string_of_pattern p2)
    (if r then "equivalent" else "not equivalent") ;
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
            Some (cons_desc, expr)
        | None ->
            print_endline "got none" ; None )
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
  ( match r with
  | None ->
      print_endline "failed transform_expr_dps_before_destruct"
  | Some (_, expr') ->
      print_endline "success transform_expr_dps_before_destruct" ;
      printf "expr_dps_before_destruct : %s became %s\n" (string_of_expr expr)
        (string_of_expr expr') ) ;
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
