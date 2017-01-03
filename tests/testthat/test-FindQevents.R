context("FindQevents")

test_that("FindQevents works", {

  data("SevernTS")
  tableP <- FindPevents(SevernTS$P[1:1000])
  tableQ <- FindQevents(SevernTS$Q[1:1000], tableP, hours2extend=6)

  expect_equal(dim(tableQ), c(6, 20))

})
