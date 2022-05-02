include Stdlib.List

let rec find_mapi_aux i f li =
  match li with
  | [] ->
      None
  | x :: li -> (
    match f i x with Some e -> Some e | None -> find_mapi_aux (i + 1) f li )

let find_mapi f li = find_mapi_aux 0 f li

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
