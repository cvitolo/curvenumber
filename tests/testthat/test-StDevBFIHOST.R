context("StDevBFIHOST")

test_that("StDevBFIHOST works", {

  data("PlynlimonSUBCATCHMENTS")
  data("PlynlimonSOIL")
  data("S1")

  stdevBFI <- StDevBFIHOST(soil = PlynlimonSOIL,
                           catchment = PlynlimonSUBCATCHMENTS[1,],
                           lookupTable = S1)

  expect_equal(stdevBFI, 0.018)

})
