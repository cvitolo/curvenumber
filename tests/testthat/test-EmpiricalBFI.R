context("EmpiricalBFI")

test_that("EmpiricalBFI works", {

  data("SevernTS")
  bfi <- EmpiricalBFI(Qflow = SevernTS$Q)

  expect_equal(bfi, 0.349)

})
