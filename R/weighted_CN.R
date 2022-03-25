#' Calculate the Weighted CN (Curve Number)
#'
#' This function computes the weighted CN (Curve Number) using the
#' user-supplied unit or the default unit of an acre.
#'
#'
#' @param CN numeric vector containing dimensionless Curve Number(s)
#' @param area numeric vector containing the surface land area
#' @param area_pct numeric vector containing the surface land area, as a
#'    percent (decimal or whole number)
#' @param area_units character vector containing the units for area
#'    (default = "acre"). The units should be consistent and not mixed.
#' @param CN_area_table data.frame/data.table/tibble, list, or matrix
#'    containing the CN in column 1 and the area in column 2
#' @param CN_area_pct_table data.frame/data.table/tibble, list, or matrix
#'    containing the CN in column 1 and the area_pct in column 2
#'
#' @return the Weighted Curve Number as a single numeric vector, in the range
#'    [0, 100]
#'
#'
#
#'
#'
#'
#'
#'
#' @examples
#'
#' # Note: the default area unit is an acre
#'
#' # Example 1
#'
#' library("curvenumber")
#'
#' area1 <- c(220, 150, 30)
#' CN1 <- c(75, 89, 80)
#' weighted_CN(CN = CN1, area = area1)
#'
#'
#' # Example 2
#'
#' library("curvenumber")
#'
#' area2 <- c(220, 150, 30)
#' area_pct2 <- area2 / sum(area2)
#' CN2 <- c(80, 95, 80)
#' CN_area_pct_table2 <- data.frame(CN2, area_pct2)
#' weighted_CN(CN_area_pct_table = CN_area_pct_table2)
#'
#'
#' # Example 3
#' 
#' install.load::load_package("curvenumber", "data.table")
#' 
#' CN_area_table3 <- data.table(CN = c(98, 100, 45), area = c(2.53, 453.00, 0.21))
#' weighted_CN(CN_area_table = CN_area_table3)
#'
#'
#' # Example 4
#'
#' library("curvenumber")
#'
#' CN4 <- c(98, 100, 45)
#' area_pct4 <- c(0.15, 0.23, 0.62)
#' weighted_CN(CN = CN4, area_pct = area_pct4)
#'
#'
#' # Example 5
#'
#' library("curvenumber")
#'
#' import::from(ramify, mat)
#'
#'
#' data_matrix5a <- matrix(c(98, 30, 40, 43, 57, 3.24, 1, 30, 50, 123), nrow = 5, ncol = 2, dimnames = list(rep("", 5), c("C", "Area")))
#' weighted_CN(CN_area_table = data_matrix5a)
#'
#'
#' # using ramify to create the matrix
#' data_matrix5b <- mat("98 30 40 43 57;3.24 1 30 50 123", rows = FALSE, sep = " ", dimnames = list(rep("", 5), c("CN", "Area")))
#' weighted_CN(CN_area_table = data_matrix5b)
#'
#'
#' # Example 6 - using area in square feet
#'
#' library("curvenumber")
#'
#' data_list6 <- list(CN = c(77, 29, 68), Area = c(43560, 56893, 345329.32))
#' weighted_CN(CN_area_table = data_list6, area_units = "square feet")
#'
#'
#' # Example 7 - using area in whole percents
#'
#' library("curvenumber")
#'
#' CN7 <- c(61, 74)
#' area_pct7 <- c(30, 70)
#' weighted_CN(CN = CN7, area_pct = area_pct7)
#'
#'
#' @importFrom data.table as.data.table
#' @importFrom units drop_units make_units set_units units
#' @importFrom fpCompare "%==%"
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom assertthat assert_that
#' @importFrom checkmate testDataTable qtest
#' @importFrom round round_r3
#'
#' @export
weighted_CN <- function (CN = NULL, CN_area_table = NULL, CN_area_pct_table = NULL, area = NULL, area_pct = NULL, area_units = c("acre", "square feet", "square mile", "hectare", "square kilometer")) {

if (missing(CN) & missing(area) & missing(area_pct) & missing(CN_area_pct_table)) {

# convert data.frame/data.table/tibble, list, matrix to data.table & then to numeric vector
# assume column 1 is CN and column 2 is area

CN_area_table <- as.data.table(CN_area_table)


assert_that(testDataTable(CN_area_table, types = "numeric", any.missing = FALSE, all.missing = FALSE, min.rows = 1, min.cols = 1, ncols = 2), msg = "Any row of CN_area_table contains a value that is NA, NaN, empty, or a string. Please try again.")
# only process enough known CN factor values and provide a stop warning if not enough

CN <- CN_area_table[, 1][[1]]

area <- CN_area_table[, 2][[1]]


} else if (missing(CN) & missing(area) & missing(area_pct) & missing(CN_area_table)) {

# convert data.frame/data.table/tibble, list, matrix to data.table & then to numeric vector
# assume column 1 is CN and column 2 is area_pct

CN_area_pct_table <- as.data.table(CN_area_pct_table)

assert_that(testDataTable(CN_area_pct_table, types = "numeric", any.missing = FALSE, all.missing = FALSE, min.rows = 1, min.cols = 1, ncols = 2), msg = "Any row of CN_area_pct_table contains a value that is NA, NaN, empty, or a string. Please try again.")
# only process enough known CN factor values and provide a stop warning if not enough


CN <- CN_area_pct_table[, 1][[1]]

area_pct <- CN_area_pct_table[, 2][[1]]

}


# Check for CN
assert_that(!any(qtest(CN, "N>=2(0,)") == FALSE), msg = "CN is 0, NA, NaN, Inf, -Inf, empty, or a string. Or, there are not at least 2 CN factor values. Please try again.")
# only process enough known CN factor values and provide a stop warning if not enough


area <- as.numeric(stri_replace_all_fixed(area, ",", ""))

ifelse(length(area_units) > 1, area_units <- "acre", area_units <- area_units)

ifelse(missing(area_units), area_units <- "acre", area_units <- area_units)



if (area_units == "acre") {

area <- area


} else if (area_units == "square feet") {

area <- set_units(area, ft^2) # ft^2

units(area) <- make_units(acre) # acres

area <- drop_units(area)


} else if (area_units == "square mile") {

area <- set_units(area,  mi^2) # mi^2

units(area) <- make_units(acre) # acres

area <- drop_units(area)


} else if (area_units == "hectare") {

area <- set_units(area, hectare) # hectare

units(area) <- make_units(acre) # acres

area <- drop_units(area)


} else if (area_units == "square kilometer") {

area <- set_units(area, km^2) # km^2

units(area) <- make_units(acre) # acres

area <- drop_units(area)

}


if (missing(area_pct)) {

weighted_CN <- round_r3(sum(CN * area) / sum(area), 2)

return(weighted_CN)


} else if (!missing(area_pct)) {

ifelse(area_pct < 1, area_pct_type <- "decimal", area_pct_type <- "whole")

if(area_pct_type == "decimal") {

ifelse(sum(area_pct) %==% 1, weighted_CN <- weighted_CN, stop("The area sum does not equal 100%."))

weighted_CN <- round_r3(sum(CN * area_pct) / sum(area_pct), 2)

return(weighted_CN)


} else if(area_pct_type == "whole") {

ifelse(sum(area_pct) %==% 100, weighted_CN <- weighted_CN, stop("The area sum does not equal 100%."))

weighted_CN <- round_r3(sum(CN * area_pct) / sum(area_pct), 2)

return(weighted_CN)
}
}
}
