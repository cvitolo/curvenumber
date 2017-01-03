#' Generate Base Flow Index (BFI) map from percentages of HOST classes.
#'
#' @param soil SpatialPolygonsDataFrame containing percentage of soil classes, this can be the HOST classification for the UK, see data("PlynlimonSOIL").
#' @param catchment SpatialPolygonsDataFrame containing a single catchment boundary.
#' @param lookupTable A dataframe containing as many rows as the number of soil classes and at least one column (named "BFIHOST").
#'
#' @return standard deviation of BFI.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   data("PlynlimonSUBCATCHMENTS")
#'   data("PlynlimonSOIL")
#'   data("S1")
#'
#'   stdevBFI <- StDevBFIHOST(soil = PlynlimonSOIL,
#'                            catchment = PlynlimonSUBCATCHMENTS[1,],
#'                            lookupTable = S1)
#' }
#'

StDevBFIHOST <- function(soil, catchment, lookupTable){

  # Clip soil map over catchment
  soilMap <- ClipSoilMap(soil, catchment)

  # Calculate the percentage coverage of each bandValue
  percentageCoverage <- PercentageCoverageSoilClasses(soilMap=soilMap,
                                                      removeZeros = FALSE)

  # Weighted standard deviation
  stDeviation <- NA
  idx <- c()
  for (i in 1:length(percentageCoverage)){
    soilClass <- as.numeric(gsub("[^0-9]", "", names(percentageCoverage)[i]))
    if (soilClass %in% lookupTable$HOSTclass){
      idx <- c(idx, i)
      stDeviation[i] <- lookupTable$sHOST[which(lookupTable$HOSTclass ==                                                         soilClass)]
    }
  }

  # weighted standard deviations
  finalStDeviation <- sqrt(sum(percentageCoverage[idx]^2*stDeviation^2))

  return(round(finalStDeviation, 3))

}
