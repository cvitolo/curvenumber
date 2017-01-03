#' Percentage coverage of soil classes.
#'
#' @details This function recognises the column containing soil classes (it assumes that the name of the column contains the string "code").
#'
#' @param soilMap SpatialPolygonsDataFrame containing percentage of HOST soil classes, i.e. see data("PlynlimonSOIL").
#' @param ignoreColumns (optional) vector containing the columns to ignore
#' @param removeZeros (optional) boolean. If TRUE (default), the function returns only non zero classes.
#'
#' @return A named numeric vector containing the percentage coverage of soil classes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   data("PlynlimonSUBCATCHMENTS")
#'   data("PlynlimonSOIL")
#'   # Clip soil map over Severn catchment
#'   soilMap <- ClipSoilMap(soil = PlynlimonSOIL,
#'                          catchment = PlynlimonSUBCATCHMENTS[1,])
#'   PercentageCoverageSoilClasses(soilMap=soilMap, ignoreColumns = c(32, 33))
#' }
#'

PercentageCoverageSoilClasses <- function(soilMap,
                                          ignoreColumns = NULL,
                                          removeZeros = TRUE){

  # Remove column to ignore, if needed
  if (!is.null(ignoreColumns)) {
    df <- soilMap@data[, -ignoreColumns]
  }else{
    df <- soilMap@data
  }

  # Find column containing soil classes, look for the string "code"
  columnNumber <- grep(pattern = "code", x = tolower(names(df)))
  df <- df[, columnNumber]

  # Calculate the percentage coverage of each bandValue
  df[] <- lapply(df, function(x){as.numeric(as.character(x))/100})

  percentageCoverage <- round(apply(df, 2, sum)/dim(df)[1],2)

  if (removeZeros == TRUE) {
    percentageCoverage <- percentageCoverage[percentageCoverage != 0]
  }

  return(percentageCoverage)

}
