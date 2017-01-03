context("PlotEvents")

test_that("PlotEvents works", {

  data("SevernTS")
  tableP <- FindPevents(SevernTS$P[1:100])
  tableQ <- FindQevents(Q = SevernTS$Q[1:100],
                        eventTableP = tableP,
                        hours2extend = 6)

  sampleplot <- PlotEvents(tseries = SevernTS[1:100,], eventTable = tableQ)

  expect_equal(sampleplot$mfrow, c(2, 1))

})
