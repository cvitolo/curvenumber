context("FindPevents")

test_that("FindPevents works", {

  data("SevernTS")
  tableP <- FindPevents(SevernTS$P[1:1000])

  expect_equal(dim(tableP), c(6, 13))

})
