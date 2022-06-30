let ( let* ) = Option.bind

let ( let+ ) opt f = Option.map f opt

let rec option_list_map f li =
  match li with
  | [] ->
      Some []
  | ele :: li ->
      let* ele = f ele in
      let+ li = option_list_map f li in
      ele :: li

let rec option_list_fold_map f acc li =
  match li with
  | [] ->
      Some (acc, [])
  | ele :: li ->
      let* acc, ele = f acc ele in
      let+ acc, li = option_list_fold_map f acc li in
      (acc, ele :: li)
