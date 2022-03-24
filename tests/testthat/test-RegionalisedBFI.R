context("RegionalisedBFI")

test_that("RegionalisedBFI works", {

  data("PlynlimonSUBCATCHMENTS")
  data("PlynlimonSOIL")
  data("S1")
  bfi <- RegionalisedBFI(soil = PlynlimonSOIL,
                         catchment = PlynlimonSUBCATCHMENTS[1,],
                         lookupTable = S1)

  expect_equal(bfi, 0.347)

})
