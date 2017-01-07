context("MakeLoopkupTable")

test_that("MakeLoopkupTable default option works", {

  dfLookup <- MakeLoopkupTable()

  expect_equal(as.character(dfLookup$Class[1]), "Coniferous Woodland")
  expect_equal(as.character(dfLookup$SoilA[1]), "36")
  expect_equal(as.character(dfLookup$SoilB[1]), "60")
  expect_equal(as.character(dfLookup$SoilC[1]), "73")
  expect_equal(as.character(dfLookup$SoilD[1]), "79")

})

test_that("MakeLoopkupTable Severn&Wye option works", {

  dfLookup <- MakeLoopkupTable("Severn&Wye")

  expect_equal(as.character(dfLookup$SoilA[1]), "36")
  expect_equal(as.character(dfLookup$SoilB[1]), "60")
  expect_equal(as.character(dfLookup$SoilC[1]), "73")
  expect_equal(as.character(dfLookup$SoilD[1]), "79")

})

test_that("MakeLoopkupTable Good woods + Good pasture option works", {

  dfLookup <- MakeLoopkupTable("Good woods + Good pasture")

  expect_equal(as.character(dfLookup$SoilA[1]), "30")
  expect_equal(as.character(dfLookup$SoilB[1]), "55")
  expect_equal(as.character(dfLookup$SoilC[1]), "70")
  expect_equal(as.character(dfLookup$SoilD[1]), "77")
  expect_equal(as.character(dfLookup$SoilA[4]), "39")
  expect_equal(as.character(dfLookup$SoilB[4]), "61")
  expect_equal(as.character(dfLookup$SoilC[4]), "74")
  expect_equal(as.character(dfLookup$SoilD[4]), "80")

})
