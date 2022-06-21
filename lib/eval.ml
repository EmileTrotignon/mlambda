open Ast
open Printf
open Print

let env_add ident value env =
  (* printf "Adding %s to env\n" ident ;
     print_string "with value : " ;
     print_value stdout value ; *)
  Env.add ident value env

let rec expr env e =
  (* print_endline "eval" ;
     print_expr stdout e ;
     print_endline "in env :" ;
     print_env stdout env ; *)
  match e with
  | EUnit ->
      VUnit
  | EVar ident -> (
    match Env.find_opt ident env with
    | Some v ->
        v
    | None ->
        failwith (sprintf "Variable %s not found" ident) )
  | ELet {var; is_rec; value; body_in} ->
      let value = if is_rec then rec_func env var value else expr env value in
      let env = env_add var value env in
      expr env body_in
  | EAlloc n ->
      let n = n |> expr env |> get_int in
      VArray (Array.init n (fun _ -> VUnit))
  | EProj {arr; index} ->
      print_endline "eval EProj" ;
      let arr = arr |> expr env |> get_array in
      let index = index |> expr env |> get_int in
      if index >= Array.length arr then
        failwith
          (sprintf "Index %i is out of bounds for array %s" index
             (string_of_value (VArray arr)) )
      else arr.(index)
  | EWrite {arr; index; value} ->
      let arr = arr |> expr env |> get_array in
      let index = index |> expr env |> get_int in
      let value = expr env value in
      if index >= Array.length arr then
        failwith
          (sprintf "Index %i is out of bounds for array %s" index
             (string_of_value (VArray arr)) )
      else arr.(index) <- value ;
      VUnit
  | EApply {func; args} ->
      apply env func args
  | EFunc {args; body} ->
      VFunc {env; args; body}
  | EPrimFunc (name, func) ->
      VPrFunc (name, func)
  | ECons {cons= Some cons; payload= []} ->
      VCons cons
  | ECons {cons= None; payload= []} ->
      VUnit
  | ECons {cons= Some cons; payload} ->
      VArray (VCons cons :: (payload |> List.map (expr env)) |> Array.of_list)
  | ECons {cons= None; payload} ->
      VArray (payload |> List.map (expr env) |> Array.of_list)
  | EPrim pr ->
      prim pr
  | EMatch {arg; branches} -> (
      let arg = expr env arg in
      match
        List.find_map
          (fun (pat, body) ->
            Option.map (fun env -> expr env body) @@ pattern env arg pat )
          branches
      with
      | None ->
          Print.print_value stdout arg ;
          Print.print_expr stdout e ;
          failwith "match failed"
      | Some v ->
          v )

and pattern env value pat =
  match (pat, value) with
  | PAny, _ ->
      Some env
  | PVar ident, value ->
      let env = env_add ident value env in
      Some env
  | PCons {cons= Some cons; payload= []}, VCons cons' when cons = cons' ->
      Some env
  | PCons {cons= None; payload= li}, VArray arr ->
      let li' = Array.to_list arr in
      let rec aux li li' env =
        match (li, li') with
        | [], [] ->
            Some env
        | p :: li, v :: li' -> (
          match pattern env v p with None -> None | Some env -> aux li li' env )
        | _ ->
            None
      in
      aux li li' env
  | PCons {cons= Some cons; payload}, VArray arr -> (
      let li' = Array.to_list arr in
      match li' with
      | [] ->
          None
      | v_cons :: li' -> (
        match v_cons with
        | VCons cons' when cons = cons' ->
            let rec aux li li' env =
              match (li, li') with
              | [], [] ->
                  Some env
              | p :: li, v :: li' -> (
                match pattern env v p with
                | None ->
                    None
                | Some env ->
                    aux li li' env )
              | _ ->
                  None
            in
            aux payload li' env
        | _ ->
            None ) )
  | PCons _, _ ->
      None
  | PPrim (PrInt i), VInt i' ->
      if i = i' then Some env else None
  | PPrim (PrString str), VString str' ->
      if str = str' then Some env else None
  | PPrim (PrBool b), VBool b' ->
      if b = b' then Some env else None
  | PPrim _, _ ->
      None

and prim prim =
  match prim with
  | PrBool b ->
      VBool b
  | PrInt i ->
      VInt i
  | PrString s ->
      VString s

and apply env func args =
  let func = func |> expr env |> get_func in
  match func with
  | Lang (env_func, pat_args, body) ->
      apply_lang pat_args body args env env_func
  | Prim (_name, func) ->
      let args = args |> List.map (expr env) in
      func args

and apply_lang pat_args body args env env_func =
  match (pat_args, args) with
  | [], [] ->
      expr env_func body
  | _ :: pat_args, [] ->
      expr env_func (EFunc {args= pat_args; body})
  | ident :: pat_args, arg :: args ->
      let arg = expr env arg in
      let env_func = env_add ident arg env_func in
      apply_lang pat_args body args env env_func
  | [], _ :: _ ->
      apply env_func body args

and rec_func env self e =
  let v = expr env e in
  match v with
  | VFunc func ->
      let env = env_add self v func.env in
      func.env <- env ;
      v
  | _ ->
      failwith "only functions can be recursive"

let mutual_rec env bds =
  let vs = List.map (fun (ident, body) -> (ident, expr env body)) bds in
  List.iter
    (fun (_, value1) ->
      match value1 with
      | VFunc func1 ->
          List.iter
            (fun (ident2, value2) ->
              let env = env_add ident2 value2 func1.env in
              func1.env <- env )
            vs
      | _ ->
          failwith "only functions can be mutually recursive" )
    vs ;
  vs

let si env = function
  | Binding {name; is_rec; body} ->
      let value = if is_rec then rec_func env name body else expr env body in
      env_add name value env
  | MutualRecBindings bds ->
      let vs = mutual_rec env bds in
      List.fold_left (fun env (ident, value) -> env_add ident value env) env vs

let program_ env program = List.fold_left si env program

let log = open_out "log_print"

let prim_env =
  program_ Env.empty
    Struct_item.
      [ prim_func_def_ar2 "add" (fun v1 v2 ->
            let v1 = get_int v1 in
            let v2 = get_int v2 in
            VInt (v1 + v2) )
      ; prim_func_def_ar2 "mod" (fun v1 v2 ->
            let v1 = get_int v1 in
            let v2 = get_int v2 in
            VInt (v1 mod v2) )
      ; prim_func_def_ar1 "is_pair" (fun v ->
            let v = get_int v in
            VBool (v mod 2 = 0) )
      ; prim_func_def_ar2 "sub" (fun v1 v2 ->
            let v1 = get_int v1 in
            let v2 = get_int v2 in
            VInt (v1 - v2) )
      ; prim_func_def_ar2 "mult" (fun v1 v2 ->
            let v1 = get_int v1 in
            let v2 = get_int v2 in
            VInt (v1 * v2) )
      ; prim_func_def_ar1 "double" (fun v ->
            let v = get_int v in
            VInt (2 * v) )
      ; prim_func_def_ar1 "print" (fun v -> print_value log v ; VUnit) ]

let program ?env p =
  match env with
  | None ->
      program_ prim_env p
  | Some env' ->
      let env = Env.union prim_env env' in
      program_ env p

let expr ?env e =
  match env with
  | None ->
      expr prim_env e
  | Some env ->
      expr (program_ prim_env env) e
