open Mlambda

let ( |.> ) v f = f v ; v

let dps_name name = name ^ "_dps"


let tailrecness env funcname =
  Alcotest.(check bool)
    "is tailrec" true
    (let is_rec, body =
       funcname |> dps_name |> Program.find_def env |> Option.get
     in
     assert is_rec ;
     Expr.is_tailrec funcname body )
