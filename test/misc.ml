open Mlambda

let test_write () =
  Alcotest.(check int)
    "same int" 1
    ( Eval.expr
        Expr.(
          let_var "arr"
            ~equal:(alloc ~size:(int 2))
            ~in_:
              (seqs
                 [write ~block:(var "arr") ~i:(int 0) ~to_:(int 1)]
                 (proj (var "arr") (int 0)) ))
    |> Value.int_exn )