open Ast
open Builder
open Option_monad
open Print
open Printf

let dst_name_of_int i = "dst" ^ string_of_int i

let index_name_of_int i = "index" ^ string_of_int i

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

  (* let to_env ~f set =
     fold (fun elt map -> Env.add elt (f elt) map) set Env.empty *)

  let ( + ) = union

  let ( - ) = diff

  let unions sets = List.fold_left union empty sets

  let remove_from_env env set = Env.filter (fun key _ -> not @@ mem key set) env
end

let%memo rec pat_vars (pat : pattern) =
  match pat with
  | PAny ->
      StringSet.empty
  | PPrim _ ->
      StringSet.empty
  | PVar var ->
      StringSet.singleton var
  | PCons {cons= _; payload} ->
      payload |> List.map pat_vars |> StringSet.unions

let%memo rec fv (expr : expr) =
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

let rec _pat_expr_compatible pat expr =
  match (pat, expr) with
  | PVar _, _ ->
      true
  | PPrim prim, EPrim prim' when prim = prim' ->
      true
  | PCons {cons; payload}, ECons {cons= cons'; payload= payload'}
    when cons = cons' ->
      List.for_all2 _pat_expr_compatible payload payload'
  | _, ELet _ ->
      (* false for now, but we may want to support this case latter *)
      false
  | _ ->
      false

let _fresh_var =
  let i = ref 0 in
  fun () ->
    i := !i + 1 ;
    "fv" ^ string_of_int !i

let _get_pvar pat =
  match pat with PVar v -> v | _ -> failwith "This pattern is complex"

type index = Int of int | Variable of string

let rec transform_expr_dps_after_construct dst_name index pvars expr =
  print_endline "transform_expr_dps_after_construct" ;
  print_expr stdout expr ;
  let dst'_name = dst_name ^ "'" in
  let e_index =
    match index with Variable name -> e_var name | Int i -> e_int i
  in
  match expr with
  | EVar var when StringSet.mem var pvars ->
      Some (StringSet.singleton var, dst_name, index, Fun.id)
  | ECons {cons; payload} ->
      let+ (pvars_used, dst_name, index, (expr : expr -> expr)), payload =
        List.find_and_replace_i
          (fun index expr ->
            (* Here we use the current index, because if we manage to transform
               [expr] into [expr'], [expr'] should write in [dst.index]. This
               does not affect success *)
            let pvars =
              StringSet.(
                pvars
                - unions
                    (List.mapi
                       (fun index' expr ->
                         if index' = index then empty else fv expr )
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
            ((pvars_used, dst_name, index, expr'), e_unit) )
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
          e_let dst'_name
            ~equal:(ECons {cons; payload})
            ~in_:
              ( e_seqs
                  [ e_write ~block:(e_var dst_name) ~i:e_index
                      ~to_:(e_var dst'_name) ]
              @@ e_let dst_name ~equal:(e_var dst'_name) ~in_:(expr expr_arg) )
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

let e_index = function Int i -> e_int i | Variable v -> e_var v

let n_dsts n = List.init n (fun i -> (dst_name_of_int i, index_name_of_int i))

let rec expr_dps_after_destruct self args destruct_pattern expr =
  print_endline "expr_dps_after_destruct" ;
  match (destruct_pattern, expr) with
  | ( PCons {cons= pcons; payload= ppayload}
    , ECons {cons= econs; payload= epayload} )
    when econs = pcons && List.length ppayload = List.length epayload ->
      let pvars = pat_vars destruct_pattern in
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
                        (e_seq
                           (e_write ~block:(e_var dst_name)
                              ~i:(e_var index_name) ~to_:ele )
                           aexpr ) )
                , (dst_name, Variable index_name) )
            | Some (pvars_used, dst_name, index, fexpr') ->
                ( ( true
                  , StringSet.(pvars - pvars_used)
                  , fun aexpr -> fexpr (fexpr' aexpr) )
                , (dst_name, index) ) )
          (false, pvars, Fun.id) epayload
      in
      let dst_args =
        List.fold_right
          (fun (dst_name, index) acc -> e_var dst_name :: e_index index :: acc)
          destinations []
      in
      Some (fexpr (e_apply (e_var (self ^ "_dps")) (dst_args @ args)))
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
          , e_seqs
              (List.mapi
                 (fun i expr ->
                   e_write
                     ~block:(e_var (dst_name_of_int i))
                     ~i:(e_var (index_name_of_int i))
                     ~to_:expr )
                 payload )
              e_unit )
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
      let entry_point =
        e_func ~args
          ~body:
            (e_let "dst"
               ~equal:
                 (e_alloc
                    ~size:(e_int (arity + if Option.is_some cons then 1 else 0)) )
               ~in_:
                 (List.fold_lefti
                    (fun i in_ (_dst_name, index_name) ->
                      e_let index_name ~equal:(e_int i) ~in_ )
                    (e_seqs
                       [ e_apply (e_var name_dps)
                           ( ( dsts
                             |> List.map (fun (_a, b) -> ["dst"; b])
                             |> List.concat |> List.map e_var )
                           @ (args |> List.map e_var) ) ]
                       (ECons
                          { cons
                          ; payload=
                              List.map
                                (fun (_dst, index) ->
                                  e_proj (e_var "dst") (e_var index) )
                                dsts } ) )
                    dsts ) )
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

(* [transform_expr_dps self n_args index expr] can be :
   - [None] if the transformation fails.
   - [Some expr'] with [expr'] an expression that write the result of the
     evaluation of [expr] in [e_var "dst"] and satisfies some other constraints :
   - [expr] exists in the context of a recursive function of name [self] that
     takes [n_args] arguments. [expr'] is such that recursive calls to [self] in
     tail modulo cons position are replaced to calls to [self ^ "dps"] in tail
     position. *)
let rec expr_dps self n_args index let_cands expr =
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
         ever used in that branch, then we replace it with its arguments.
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
        expr_dps self n_args index let_cands body_in
      in
      (* If this variable is part of the optmised lets set, then we optimise
         away the let-binding *)
      if StringSet.mem var optimised_lets then
        (StringSet.remove var optimised_lets, body_in)
      else (optimised_lets, ELet {var; value; body_in; is_rec= false})
  | ELet {var; is_rec; value; body_in} ->
      assert (not is_rec) ;
      let fv_value = fv value in
      (* If recursive-call bound variable is mentionned in the right-hand side
         of the let, we can not move the recursive call in tail position. *)
      let let_cands = StringSet.remove_from_env let_cands fv_value in
      (* We then transform the code after the binding. *)
      let+ optimised_lets, body_in =
        expr_dps self n_args index let_cands body_in
      in
      (* And we just restore the binding with the transformed [body_in] *)
      let expr = ELet {var; is_rec; value; body_in} in
      (optimised_lets, expr)
  | ECons {cons; payload} ->
      let fv_payload = List.map fv payload in
      let+ (optimised_lets, expr), payload =
        List.find_and_replace_i
          (fun index expr ->
            (* Here we use the current index, because if we manage to transform
               [expr] into [expr'], [expr'] should write in [dst.index]. This
               does not affect success *)
            (*  *)
            let fv_others =
              fv_payload
              |> List.filteri (fun i _ -> i <> index)
              |> StringSet.unions
            in
            let let_cands = StringSet.remove_from_env let_cands fv_others in
            let+ optimised_lets, expr' =
              expr_dps self n_args (Some index) let_cands expr
            in
            (* We enter this code if transforming the payload was successful.
               Here, [expr'] is an expression that write the result of the
               evaluation of [expr] in the destination (by induction hypothesis).
               In that case, we replace the payload by [e_unit] and we return the
               transformed expression along with the set of optimised lets. *)
            ((optimised_lets, expr'), e_unit) )
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
      ( optimised_lets
      , e_let "dst'"
          ~equal:(ECons {cons; payload})
          ~in_:
            ( e_seqs
                [e_write ~block:(e_var "dst") ~i:e_index ~to_:(e_var "dst'")]
            @@ e_let "dst" ~equal:(e_var "dst'") ~in_:expr ) )
  | EMatch {arg; branches} ->
      (* We need to transform the branches. Each branch can be either
         succesfully transformed, or plugged as-is in a write statement.
         We consider that the transformation is a success if at least one branch
         was succesfully transformed. *)
      let (had_a_succes, optimised_lets), branches =
        List.fold_left_map
          (fun (had_a_success, lets) (pat, expr) ->
            match expr_dps self n_args index let_cands expr with
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
      (* In all these cases, we fail. *)
      None

let expr_dps self n_args expr =
  expr |> expr_dps self n_args None Env.empty |> Option.map snd

(** [transform_expr self expr] is either [None] if no transformation was found,
    or [Some (entry, dps)] where a call to [entry] has the same semantics as
    call to [self] (in the context [let rec self = expr]). And [dps] is [expr]
    in destination passing style, that is [dps dst i arg] writes [self arg] in
    [dst.i]. *)
let transform_expr self expr =
  match expr with
  | EFunc {args; body} ->
      let+ transformed = expr_dps self (List.length args) body in
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

let si_cons si =
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

let program_cons program = List.map si_cons program
