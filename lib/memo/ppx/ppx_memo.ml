open Ppxlib

let name = "memo"

let name_mult = "memo.mult"

let pexp_simple_fun pat body = Ast_pattern.(pexp_fun nolabel none pat body)

let ( let* ) x y = x y

let rec pexp_mult_fun () =
  Ast_pattern.(
    map'
      ~f:(fun loc k arg body ->
        let* args, body = parse (pexp_mult_fun ()) loc body in
        k (arg :: args, body) )
      (pexp_simple_fun __ __)
    ||| map ~f:(fun k body -> k ([], body)) __)

let pexp_mult_fun patli body =
  Ast_pattern.(
    map'
      ~f:(fun loc k (patli', body') ->
        let k' = parse patli loc patli' k in
        parse body loc body' k' )
      (pexp_mult_fun ()))

let pattern =
  Ast_pattern.(
    pstr
      ( pstr_value __
          ( value_binding
              ~pat:(as__ (ppat_var __))
              ~expr:
                ( pexp_fun __ __ __ __
                ||| pexp_constraint (pexp_fun __ __ __ __) drop )
          ^:: nil )
      ^:: nil ))

let expand ~loc ~path:_ isrec func_pat func_name (arg_label : arg_label)
    optionnal_arg arg body =
  let isrec = match isrec with Nonrecursive -> false | Recursive -> true in
  let func_exp =
    Ast_builder.Default.(pexp_ident ~loc (Loc.make ~loc (Lident func_name)))
  in
  if isrec then
    [%stri
      let [%p func_pat] =
        let [%p func_pat] =
         fun [%p func_pat] ->
          [%e
            Ast_builder.Default.pexp_fun ~loc arg_label optionnal_arg arg body]
        in
        Memolib.memo_rec `gc [%e func_exp]]
  else
    [%stri
      let [%p func_pat] =
        let [%p func_pat] = fun [%p arg] -> [%e body] in
        Memolib.memo `gc [%e func_exp]]

let ext = Extension.declare name Extension.Context.structure_item pattern expand

let () = Ppxlib.Driver.register_transformation name ~extensions:[ext]
