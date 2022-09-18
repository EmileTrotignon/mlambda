open Mlambda

module Regression = Simple.Tests (struct
  let log = "tmc_mur_regression.log"

  let transform = Tmc_mur.program

  let expects_tailrec = true
end)

module Current = Mur.Tests (struct
  let log = "tmc_mur.log"

  let transform = Tmc_mur.program

  let expects_tailrec = true
end)

let tests = Regression.tests @ Current.tests
