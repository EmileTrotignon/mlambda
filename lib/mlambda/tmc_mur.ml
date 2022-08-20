open Ast
open Result_monad
open Printf

type step =
  | AfterConstruct
  | DuringConstruct
  | AfterDestruct
  | BeforeDestruct
  | Entry

type mur_error = {step: step; reason: string; expr: expr}

let error_to_string {step; expr= _; reason} =
  sprintf "Failure %s because %s"
    ( match step with
    | AfterConstruct ->
        "after construct"
    | DuringConstruct ->
        "during construct"
    | AfterDestruct ->
        "after destruct"
    | BeforeDestruct ->
        "before destruct"
    | Entry ->
        "during entry" )
    reason

let dst_name_of_int i = "dst" ^ string_of_int i

let index_name_of_int i = "i" ^ string_of_int i

let step = AfterConstruct

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
let rec expr_dps_after_construct ~block ~index rec_vars expr =
  match expr with
  | EVar var when String.Set.mem var rec_vars ->
      Ok (var, Expr.(tuple [block; index]))
  | ECons {cons; payload} -> (
      let block_name = Fresh_vars.block () in
      match
        List.find_and_replace_i
          (fun index expr ->
            if
              not
              @@ List.forall_i
                   (fun index' expr' ->
                     index = index'
                     || String.Set.(is_empty (inter rec_vars (Expr.fv expr')))
                     )
                   payload
            then None
            else
              match
                let index = index + if Option.is_some cons then 1 else 0 in
                expr_dps_after_construct
                  ~block:Expr.(var block_name)
                  ~index:Expr.(int index)
                  rec_vars expr
              with
              | Error _ ->
                  None
              | Ok (var_used, expr') ->
                  Some ((var_used, expr'), Expr.unit) )
          payload
      with
      | None ->
          Error
            {step; reason= "did not find any correct field in constructor"; expr}
      | Some ((var_used, expr), payload) ->
          Ok
            ( var_used
            , let cons_ = cons in
              Expr.(
                let_var block_name
                  ~equal:(ECons {cons= cons_; payload})
                  ~in_:(seq (write ~block ~i:index ~to_:(var block_name)) expr))
            ) )
  | EMatch {arg; branches} ->
      let fv_arg = Expr.fv arg in
      if String.Set.(is_empty (inter rec_vars fv_arg)) then
        let+ var, branches =
          result_list_fold_map
            (fun var (pat, expr) ->
              let rec_vars = String.Set.diff rec_vars (Pattern.vars pat) in
              let* var', expr =
                expr_dps_after_construct ~block ~index rec_vars expr
              in
              match var with
              | Some var ->
                  if var = var' then Ok (Some var, (pat, expr))
                  else
                    Error
                      { step
                      ; expr
                      ; reason=
                          "different branches mention different recursive \
                           variables" }
              | None ->
                  Ok (Some var', (pat, expr)) )
            None branches
        in
        match var with
        | None ->
            failwith "Should never happen"
        | Some var ->
            (var, EMatch {arg; branches})
      else
        Error
          { step
          ; expr
          ; reason= "the expression that is matched mentions recursive variable"
          }
  | _ ->
      Error {step; expr; reason= "Unhandeld case"}

let step = DuringConstruct

(** [expr_dps_construct rec_vars_complete rec_vars rec_pattern expr]
    - [rec_vars_complete] contains all variables mapped to their index.
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
let rec expr_dps_construct rec_vars rec_pattern expr =
  match (rec_pattern, expr) with
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
        expr_dps_after_construct
          ~block:Expr.(var dst_name)
          ~index:Expr.(var index_name)
          (String.Set.env_domain rec_vars)
          expr
      in
      [r]
  | ( PCons {cons= pcons; payload= ppayload}
    , ECons {cons= econs; payload= epayload} )
    when econs = pcons && List.length ppayload = List.length epayload ->
      let+ exprs =
        List.combine ppayload epayload
        |> result_list_map (fun (p, e) -> expr_dps_construct rec_vars p e)
      in
      List.concat exprs
  | _ ->
      Error {step; expr; reason= "Unhandled case"}

let expr_dps_construct rec_vars_complete rec_pattern expr =
  let* r = expr_dps_construct rec_vars_complete rec_pattern expr in
  let vars = r |> List.map fst |> String.Set.of_list in
  if List.length r = String.Set.cardinal vars then Ok r
  else
    Error
      { step
      ; expr
      ; reason=
          "Constructor fields do not have a unique mention of each recursive \
           variable" }

let n_dsts n = List.init n (fun i -> (dst_name_of_int i, index_name_of_int i))

let restore_reccall self args rec_vars exprs =
  let bds =
    List.map
      (fun (var, expr') ->
        let i_arg = Env.find var rec_vars in
        ( Pattern.(
            tuple [var (dst_name_of_int i_arg); var (index_name_of_int i_arg)])
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

let step = AfterDestruct

(** [expr_dps_after_destruct self args rec_pattern expr]
    - [self] is the name of the function we are transforming.
    - [args] are the arguments used in the recursive call to that function.
    - [rec_pattern] is the pattern that is used to destruct the recursive call.
    - [expr] is the expression we are transforming.
    Returns [Some e] if the transformation was succesful, [None] if it was not.
*)
let rec expr_dps_after_destruct self args rec_pattern expr =
  match (rec_pattern, expr) with
  | PVar _, _ ->
      let rec_vars = Pattern.vars_numbered rec_pattern in
      let+ exprs = expr_dps_construct rec_vars rec_pattern expr in
      restore_reccall self args rec_vars exprs
  | ( PCons {cons= pcons; payload= ppayload}
    , ECons {cons= econs; payload= epayload} )
    when econs = pcons && List.length ppayload = List.length epayload ->
      let rec_vars = Pattern.vars_numbered rec_pattern in
      let+ exprs = expr_dps_construct rec_vars rec_pattern expr in
      restore_reccall self args rec_vars exprs
  | _, EMatch {arg; branches} ->
      let success, branches =
        List.fold_left_map
          (fun success (pat, expr) ->
            match expr_dps_after_destruct self args rec_pattern expr with
            | Ok expr ->
                (true, (pat, expr))
            | Error _ ->
                (success, (pat, expr)) )
          false branches
      in
      if success then Ok (EMatch {arg; branches})
      else Error {step; expr; reason= "no transformable branch was found"}
  | _ ->
      Error {step; expr; reason= "unhandled case"}

(** [expr_dps_notrec rec_pat expr]
    - [rec_pat] is the pattern that is used somewhere else in the function to
      destruct the result of a recursive call.
    - [expr] is an expression of the function that does not contain a
      recursive call.
    Returns an expression that writes the result of [expr] to the destinations
    corresponding to the pattern. *)

let step = BeforeDestruct

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
  match expr with
  | EMatch {arg= EApply {func= EVar funcname; args}; branches= [(pattern, expr)]}
    when funcname = self && List.length args = n_args ->
      let+ expr =
        expr_dps_after_destruct self args pattern (Inline.expr expr)
      in
      (pattern, expr)
  | EMatch {arg; branches} -> (
      let rec_pat, branches =
        List.fold_left_map
          (fun rec_pat (pat, expr) ->
            match expr_dps_before_destruct self n_args expr with
            | Ok (rec_pat', expr) -> (
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
            | Error _ ->
                (None, (false, pat, expr)) )
          None branches
      in
      match rec_pat with
      | None ->
          Error
            { step
            ; expr
            ; reason= "no destructor was found in any of the branches" }
      | Some rec_pat ->
          let branches =
            List.map
              (fun (transformed, pat, expr) ->
                if transformed then (pat, expr)
                else (pat, expr_dps_notrec rec_pat expr) )
              branches
          in
          Ok (rec_pat, EMatch {arg; branches}) )
  | _ ->
      Error {step; expr; reason= "unhandled case"}

let step = Entry

let expr self e =
  match e with
  | EFunc {args; body} ->
      let+ rec_pat, transformed =
        Fresh_vars.block_reset () ;
        body |> Anf.expr |> expr_dps_before_destruct self (List.length args)
      in
      let rec_vars = Pattern.vars_numbered rec_pat in
      let arity = Pattern.n_vars rec_pat in
      let name_dps = self ^ "_dps" in
      let dsts = n_dsts arity in
      let dps =
        let args =
          (dsts |> List.map (fun (a, b) -> [a; b]) |> List.concat) @ args
        in
        EFunc {args; body= transformed}
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
      Error {step; expr= e; reason= "unhandled case"}

let si si =
  match si with
  | Binding {name; body; is_rec} -> (
      if not is_rec then si
      else
        match expr name body with
        | Ok (entry, dps) ->
            MutualRecBindings [(name, entry); (name ^ "_dps", dps)]
        | Error e ->
            printf "Error : %s\n" (error_to_string e) ;
            si )
  | MutualRecBindings _ ->
      si

let program program = List.map si program
