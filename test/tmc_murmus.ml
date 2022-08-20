open Mlambda

let transform li =  li |> Tmc_murmus.program |> Inline.program

module Regression_simple = Simple.Tests (struct
  let log = "tmc_murmus_regression_simple.log"

  let transform = transform

  let expects_tailrec = true
end)

module Regression_mur = Mur.Tests (struct
  let log = "tmc_murmus_regression_mur.log"

  let transform = transform

  let expects_tailrec = true
end)

module Current = Murmus.Tests (struct
  let log = "tmc_murmus.log"

  let transform = transform

  let expects_tailrec = true
end)

let tests = Regression_simple.tests @ Regression_mur.tests @ Current.tests