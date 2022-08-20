let ( let* ) = Result.bind

let ( let+ ) opt f = Result.map f opt

let result_list_map f li =
  Stdlib.List.fold_right
    (fun ele li ->
      let* li in
      let+ ele = f ele in
      ele :: li )
    li (Ok [])

let rec result_list_fold_map f acc li =
  match li with
  | [] ->
      Ok (acc, [])
  | ele :: li ->
      let* acc, ele = f acc ele in
      let+ acc, li = result_list_fold_map f acc li in
      (acc, ele :: li)
