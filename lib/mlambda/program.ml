open Ast

type t = program

let find_def (program : t) name =
  List.find_map
    (function
      | Binding {name= name'; is_rec; body} ->
          if name = name' then Some (is_rec, body) else None
      | MutualRecBindings bds ->
          List.find_map
            (function
              | name', body when name' = name -> Some (true, body) | _ -> None
              )
            bds )
    program

include Print.Program