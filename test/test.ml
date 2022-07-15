let () =
  let open Alcotest in
  run "Tests"
    [ ("Misceallenous tests", Misc.[test_case "Mlambda write" `Quick test_write])
    ; ( "Program tests"
      , Programs.
          [ test_case "Map" `Quick map
          ; test_case "Map double" `Quick map_double
          ; test_case "Map double cond" `Quick map_double_cond ] )
    ; ("tmc", Tmc.tests)
    ; ("tmc mur", Tmc_mur.tests)
    ; ("tmc murmus", Tmc_murmus.tests) ]
