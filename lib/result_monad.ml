let ( let* ) = Result.bind

let ( let+ ) opt f = Result.map f opt

let result_list_map f li =
  Stdlib.List.fold_right
    (fun ele li ->
      let* li in
      let+ ele = f ele in
      ele :: li )
    li (Ok [])
