context("RegionalisedBFIdominant")

test_that("RegionalisedBFIdominant works", {

  # Load Plynlimon sub-catchments spatial polygons data frame
  data("PlynlimonSUBCATCHMENTS")
  # Load soil map (HOST dominant classes)
  data("PlynlimonSOILdominant")
  # Load lookup table
  data("S1")

  bfi <- RegionalisedBFIdominant(soil = PlynlimonSOILdominant,
                         catchment = PlynlimonSUBCATCHMENTS[1,],
                         lookupTable = S1)

  expect_equal(bfi, 0.329)

})
