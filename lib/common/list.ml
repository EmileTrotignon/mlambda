include Stdlib.List

let rec find_mapi_aux i f li =
  match li with
  | [] ->
      None
  | x :: li -> (
    match f i x with Some e -> Some e | None -> find_mapi_aux (i + 1) f li )

let find_mapi f li = find_mapi_aux 0 f li

let fold_lefti f init li =
  fold_left (fun (i, acc) ele -> (i + 1, f i acc ele)) (0, init) li |> snd

let rec fold_left_mapi_aux i f acc li =
  match li with
  | [] ->
      (acc, [])
  | x :: li ->
      let acc, x = f i acc x in
      let r, li = fold_left_mapi_aux (i + 1) f acc li in
      (r, x :: li)

let fold_left_mapi f init li = fold_left_mapi_aux 0 f init li

let rec find_and_replace_exn pred li =
  match li with
  | [] ->
      raise Not_found
  | x :: li -> (
    match pred x with
    | Some (a, b) ->
        (a, b :: li)
    | None ->
        let a, li = find_and_replace_exn pred li in
        (a, x :: li) )

let rec find_and_replace_i_exn_aux i pred li =
  match li with
  | [] ->
      raise Not_found
  | x :: li -> (
    match pred i x with
    | Some (a, b) ->
        (a, b :: li)
    | None ->
        let a, li = find_and_replace_i_exn_aux (i + 1) pred li in
        (a, x :: li) )

let find_and_replace_i_exn pred li = find_and_replace_i_exn_aux 0 pred li

open Option_monad

let rec find_and_replace pred li =
  match li with
  | [] ->
      None
  | x :: li -> (
    match pred x with
    | Some (a, b) ->
        Some (a, b :: li)
    | None ->
        let+ a, li = find_and_replace pred li in
        (a, x :: li) )

let rec find_and_replace_i_aux i pred li =
  match li with
  | [] ->
      None
  | x :: li -> (
    match pred i x with
    | Some (a, b) ->
        Some (a, b :: li)
    | None ->
        let+ a, li = find_and_replace_i_aux (i + 1) pred li in
        (a, x :: li) )

let find_and_replace_i pred li = find_and_replace_i_aux 0 pred li

let rec exists_i i b p li =
  if b then true
  else
    match li with [] -> b | ele :: li -> exists_i (i + 1) (b || p i ele) p li

let exists_i p li = exists_i 0 false p li

let rec forall_i i b p li =
  if not b then false
  else
    match li with [] -> b | ele :: li -> forall_i (i + 1) (b && p i ele) p li

let forall_i p li = forall_i 0 true p li
