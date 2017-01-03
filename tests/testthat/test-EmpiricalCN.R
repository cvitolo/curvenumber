context("EmpiricalCN")

test_that("EmpiricalCN works", {

  data("SevernTS")
  severnCN <- EmpiricalCN(tseries = SevernTS[1:1000,])

  expect_equal(severnCN, 76)

})
