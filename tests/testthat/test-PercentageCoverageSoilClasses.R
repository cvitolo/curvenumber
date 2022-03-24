context("PercentageCoverageSoilClasses")

test_that("PercentageCoverageSoilClasses works", {

  data("PlynlimonSUBCATCHMENTS")
  data("PlynlimonSOIL")

  # Clip soil map over Severn catchment
  soilMap <- ClipMap(map = PlynlimonSOIL,
                     catchment = PlynlimonSUBCATCHMENTS[1,])

  soilcov <- PercentageCoverageSoilClasses(soilMap = soilMap,
                                           ignoreColumns = c(32, 33))

  expect_equal(soilcov[[1]], 0.54)
  expect_equal(soilcov[[2]], 0.07)
  expect_equal(soilcov[[3]], 0.01)
  expect_equal(soilcov[[4]], 0.14)
  expect_equal(soilcov[[5]], 0.25)

})
