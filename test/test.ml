let () =
  let open Alcotest in
  run "Tests"
    [ ("Misceallenous tests", Misc.[test_case "Mlambda write" `Quick test_write])
    ; ("simple", Simple.tests)
    ; ("mur", Mur.tests)
    ; ("murmus", Murmus.tests)
    ; ("tmc simple", Tmc.tests)
    ; ("anf", Anf.tests)
    ; ("inline", Inline.tests)
    ; ("tmc mur", Tmc_mur.tests)
    ; ("tmc murmus", Tmc_murmus.tests) ]
