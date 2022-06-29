let ( let* ) = Option.bind

let ( let+ ) opt f = Option.map f opt

let option_list_map f li =
  Stdlib.List.fold_right
    (fun ele li ->
      let* li in
      let+ ele = f ele in
      ele :: li )
    li (Some [])