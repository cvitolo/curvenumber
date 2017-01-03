context("DirectStormRunoff")

test_that("DirectStormRunoff generates correct values", {

  SQ <- DirectStormRunoff(P = 30, CN = 80, PQunits = "mm")

  expect_equal(SQ$S, 63.5)
  expect_equal(SQ$Q, 3.704)

})
