context("ClipSoilMap")

test_that("ClipSoilMap generates correct SpatialPolygonsDataFrame", {

  data("PlynlimonSUBCATCHMENTS")
  data("PlynlimonSOIL")

  soilMap <- ClipSoilMap(soil = PlynlimonSOIL,
                         catchment = PlynlimonSUBCATCHMENTS[1,])

  expect_equal(dim(soilMap@data), c(19, 36))

})
