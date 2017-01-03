context("RegionalisedCNdominant")

test_that("RegionalisedCNdominant works", {

  # Load Plynlimon sub-catchments spatial polygons data frame
  data("PlynlimonSUBCATCHMENTS")
  # Load soil map (HOST dominant classes)
  data("PlynlimonSOILdominant")
  # Load land cover (VEGETATION 2013) MAP
  data("PlynlimonVEG")
  # Load lookup table
  dfLookup <- MakeLoopkupTable("Fair woods + Poor pasture")

  cn <- RegionalisedCNdominant(soil = PlynlimonSOILdominant,
                               catchment = PlynlimonSUBCATCHMENTS[1,],
                               lookupTable = dfLookup,
                               vegetation = PlynlimonVEG,
                               artificialDrainage = "none")

  expect_equal(cn, 77)

})
