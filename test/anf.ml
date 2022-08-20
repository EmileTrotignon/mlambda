open Mlambda

module Simple = Simple.Tests (struct
  let log = "anf_simple.log"

  let transform = Anf.program

  let expects_tailrec = false
end)

module Mur = Mur.Tests (struct
  let log = "anf_mur.log"

  let transform = Anf.program

  let expects_tailrec = false
end)

module Murmus = Murmus.Tests (struct
  let log = "anf_mur.log"

  let transform = Anf.program

  let expects_tailrec = false
end)

let tests = Simple.tests @ Mur.tests @ Murmus.tests