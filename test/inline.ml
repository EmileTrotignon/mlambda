open Mlambda

module Simple = Simple.Tests (struct
  let log = "inline_simple.log"

  let transform env = env |> Anf.program |> Inline.program

  let expects_tailrec = false
end)

module Mur = Mur.Tests (struct
  let log = "inline_mur.log"

  let transform env = env |> Anf.program |> Inline.program

  let expects_tailrec = false
end)

module Murmus = Murmus.Tests (struct
  let log = "inline_murmus.log"

  let transform env = env |> Anf.program |> Inline.program

  let expects_tailrec = false
end)

let tests = Simple.tests @ Mur.tests @ Murmus.tests