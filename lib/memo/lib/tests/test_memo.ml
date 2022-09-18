open Memolib

let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

let fib = memo `gc fib

let fib_memo =
  let fib fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2) in
  memo_rec `perm fib

let fib_dyn n =
  if n < 2 then 1
  else
    let un1 = ref 1 and un2 = ref 1 in
    for _ = 2 to n do
      let fib_i = !un1 + !un2 in
      un2 := !un1 ;
      un1 := fib_i
    done ;
    !un1

let test_memo_fib () =
  Alcotest.(check int) "same ints" (fib_memo 100_000) (fib_dyn 100_000)

let () = print_endline "coucou" ; flush stdout

let () =
  let open Alcotest in
  run "Memoization"
    [("Fibonnaci", [test_case "Test fib_memo" `Quick test_memo_fib])]

let () = print_endline "coucou2" ; flush stdout
