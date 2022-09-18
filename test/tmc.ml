open Mlambda

include Simple.Tests (struct
  let log = "tmc.log"

  let transform env = env |> Inline.program |> Tmc.program

  let expects_tailrec = true
end)