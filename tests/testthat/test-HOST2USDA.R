context("HOST2USDA")

test_that("HOST2USDA works", {

  usdaClass <- HOST2USDA(c("3","7"))

  expect_equal(usdaClass, c("A", "AB"))

})
