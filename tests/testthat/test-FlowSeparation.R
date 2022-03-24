context("FlowSeparation")

test_that("FlowSeparation works", {

  data("SevernTS")
  tableP <- FindPevents(SevernTS$P[1:1000])
  tableQ <- FindQevents(SevernTS$Q[1:1000], tableP, hours2extend=6)
  df <- FlowSeparation(SevernTS, tableQ, stepsBack=5, timeUnits = "hours",
                       plotOption = TRUE, event2plot = 3)

  expect_equal(dim(df), c(6, 22))

})
