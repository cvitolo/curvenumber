#' Calculate the Weighted Curve Number
#'
#' @param CN numeric vector containing dimensionless Curve Numbers
#' @param area numeric vector containing the surface land area
#' @param area_pct numeric vector containing the surface land area, as a
#'    percent (decimal or whole number)
#' @param area_units character vector containing the units for area
#' (default = "acres"). The units should be consistent and not mixed.
#' @param CN_area_table data.frame/data.table/tibble, list, or matrix
#' containing the CN in column 1 and the area in column 2
#' @param CN_area_pct_table data.frame/data.table/tibble, list, or matrix
#' containing the CN in column 1 and the area_pct in column 2
#'
#' @return Weighted Curve Number as a single numeric vector, in the range
#' [0, 100]
#'
#'
#'
#' @source
#' r - Better error message for stopifnot? - Stack Overflow answered by Andrie on Dec 1 2011. See \url{http://stackoverflow.com/questions/8343509/better-error-message-for-stopifnot}.
#'
#'
#'
#' @references
#' Engineering Hydrology Training Series Module 104 - Runoff Curve Number Computations Study Guide, United States Department of Agriculture Soil Conservation Service National Employee Development Staff, September 1989, page 21 \url{https://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/training/runoff-curve-numbers1.pdf}.
#'
#'
#'
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#' @examples
#' library("curvenumber")
#' library("ramify")
#'
#' Example 1 - Reference (page 21)
#' area <- c(220, 150, 30)
#' CN <- c(75, 89, 80)
#' weighted_CN(CN = CN, area = area)
#'
#'
#' Example 2 - Reference (page 21)
#' area <- c(220, 150, 30)
#' area_pct <- area / sum(area)
#' CN <- c(80, 95, 80)
#' CN_area_pct_table <- data.frame(CN, area_pct)
#' weighted_CN(CN_area_pct_table = CN_area_pct_table)
#'
#'
#' Example 3
#' CN_area_table <- data.table(CN = c(98, 100, 45), area = c(2.53, 453.00, 0.21))
#' weighted_CN(CN_area_table = CN_area_table)
#'
#'
#' Example 4
#' CN <- c(98, 100, 45)
#' area_pct <- c(0.15, 0.23, 0.62)
#' weighted_CN(CN = CN, area_pct = area_pct)
#'
#'
#' Example 5
#' data_matrix1 <- matrix(c(98, 30, 40, 43, 57, 3.24, 1, 30, 50, 123), nrow = 5, ncol = 2, dimnames = list(rep("", 5), c("CN", "Area")))
#' weighted_CN(CN_area_table = data_matrix1)
#'
#'
#' using ramify
#' data_matrix2 <- mat("98 30 40 43 57;3.24 1 30 50 123", rows = FALSE, sep = " ", dimnames = list(rep("", 5), c("CN", "Area")))
#' weighted_CN(CN_area_table = data_matrix2)
#'
#'
#' Example 6
#' data_list <- list(CN = c(77, 29, 68), Area = c(43560, 56893, 345329.32))
#' weighted_CN(CN_area_table = data_list, area_units = "square feet")
#'
#'
#'
#'
#' @import data.table units fpCompare stringi
#'
#' @export
weighted_CN <- function (CN = NULL, area = NULL, area_pct = NULL, area_units = c("acre", "square feet", "square mile", "hectare", "square kilometer"), CN_area_table = NULL, CN_area_pct_table = NULL) {


if (missing(CN) & missing(area) & missing(area_pct) & missing(CN_area_pct_table)) {

# convert data.frame/data.table/tibble, list, matrix to data.table & then to numeric vector
# assume column 1 is CN and column 2 is area

CN_area_table <- as.data.table(CN_area_table)

CN <- CN_area_table[, 1][[1]]

area <- CN_area_table[, 2][[1]]


} else if (missing(CN) & missing(area) & missing(area_pct) & missing(CN_area_table)) {

# convert data.frame/data.table/tibble, list, matrix to data.table & then to numeric vector
# assume column 1 is CN and column 2 is area_pct

CN_area_pct_table <- as.data.table(CN_area_pct_table)

CN <- CN_area_pct_table[, 1][[1]]

area_pct <- CN_area_pct_table[, 2][[1]]

}


if (length(CN) < 2) {

stop("There are not at least 2 Curve Number values. Try again with at least 2 Curve Number values.")
# Source 1 / only process enough known variables and provide a stop warning if not enough


} else {


area <- as.numeric(stri_replace_all_fixed(area, ",", ""))

ifelse(length(area_units) > 1, area_units <- "acre", area_units <- area_units)


# define missing units
acre <- make_unit("acre")

hectare <- make_unit("hectare")


if (area_units == "acre") {

area <- area


} else if (area_units == "square feet") {

area <- with(ud_units, area * ft^2) # ft^2

units(area) <- with(ud_units, acre) # acres

area <- as.numeric(area)


} else if (area_units == "square mile") {

area <- with(ud_units, area * mi^2) # mi^2

units(area) <- with(ud_units, acre) # acres

area <- as.numeric(area)


} else if (area_units == "hectare") {

area <- with(ud_units, area * hectare) # hectare

units(area) <- with(ud_units, acre) # acres

area <- as.numeric(area)


} else if (area_units == "square kilometer") {

area <- with(ud_units, area * km^2) # km^2

units(area) <- with(ud_units, acre) # acres

area <- as.numeric(area)

}


if (missing(area_pct)) {

weighted_CN <- round(sum(CN * area) / sum(area))

return(weighted_CN)


} else if (!missing(area_pct)) {

ifelse(area_pct < 1, area_pct_type <- "decimal", area_pct_type <- "whole")

if(area_pct_type == "decimal") {

ifelse(sum(area_pct) %==% 1, weighted_CN <- weighted_CN, stop("The area sum does not equal 100%."))
# Source 1 / only process enough known variables and provide a stop warning if not enough

weighted_CN <- round(sum(CN * area_pct) / sum(area_pct))

return(weighted_CN)


} else if(area_pct_type == "whole") {

ifelse(sum(area_pct) %==% 100, weighted_CN <- weighted_CN, stop("The area sum does not equal 100%."))
# Source 1 / only process enough known variables and provide a stop warning if not enough

weighted_CN <- round(sum(CN * area_pct) / sum(area_pct))

return(weighted_CN)
}
}
}
}
