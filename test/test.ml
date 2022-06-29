let () =
  let open Alcotest in
  run "Tests"
    [ ("Misceallenous tests", Misc.[test_case "Mlambda write" `Quick test_write])
    ; ( "Program tests"
      , Programs.
          [ test_case "Map" `Quick map
          ; test_case "Map double" `Quick map_double
          ; test_case "Map double cond" `Quick map_double_cond ] )
    ; ( "Maps tmc"
      , Tmc.
          [ test_case "Tmc tailrecness" `Quick tailrecness
          ; test_case "Map tmc" `Quick map
          ; test_case "Map with let tmc" `Quick map_let
          ; test_case "Map double tmc" `Quick map_double
          ; test_case "Map double cond tmc" `Quick map_double_cond
          ; test_case "Map double cond let tmc" `Quick map_double_cond_let ] )
    ; ( "Maps tmcc"
      , Tmcc.
          [ (*test_case "Tmc tailrecness" `Quick tailrecness*)
           test_case "Test partition_map" `Quick partition_map
          ; test_case "Test partition_map_twister" `Quick partition_map_twister
          ] ) ]
