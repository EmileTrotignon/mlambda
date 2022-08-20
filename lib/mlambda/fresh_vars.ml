let fv, fv_reset =
  let i = ref 0 in
  ( (fun () ->
      i := !i + 1 ;
      "fv_" ^ string_of_int !i )
  , fun () -> i := 0 )

let block, block_reset =
  let i = ref 0 in
  ( (fun () ->
      i := !i + 1 ;
      "block_" ^ string_of_int !i )
  , fun () -> i := 0 )
