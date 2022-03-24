context("RegionalisedCN")

test_that("RegionalisedCN works", {

  # Load Plynlimon sub-catchments spatial polygons data frame
  data("PlynlimonSUBCATCHMENTS")
  # Load soil map (HOST percentage distribution of classes)
  data("PlynlimonSOIL")
  # Load land cover (VEGETATION 2013) MAP
  data("PlynlimonVEG")
  # Load lookup table
  dfLookup <- MakeLoopkupTable("Fair woods + Poor pasture")

  cn <- RegionalisedCN(soil = PlynlimonSOIL,
                       catchment = PlynlimonSUBCATCHMENTS[1,],
                       lookupTable = dfLookup,
                       vegetation = PlynlimonVEG,
                       artificialDrainage = "none")

  expect_equal(cn, 78)

})
