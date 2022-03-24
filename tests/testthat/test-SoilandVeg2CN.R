context("SoilandVeg2CN")

test_that("SoilandVeg2CN works", {

  dfLookup <- MakeLoopkupTable("Fair woods + Poor pasture")
  cnLocal <- SoilandVeg2CN(soilCODE = 15,
                           vegetationCODE = 6,
                           lookupTable = dfLookup,
                           artificialDrainage = "none")

  expect_equal(cnLocal, 74)

})
