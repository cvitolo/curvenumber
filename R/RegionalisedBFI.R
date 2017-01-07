#' Calculate regionalised Base Flow Index (BFI) from percentages of soil classes.
#'
#' @param soil SpatialPolygonsDataFrame containing percentage of soil classes, this can be the HOST classification for the UK, see data("PlynlimonSOIL").
#' @param catchment SpatialPolygonsDataFrame containing a single catchment boundary.
#' @param lookupTable A dataframe containing as many rows as the number of soil classes and at least one column (named "BFIHOST").
#'
#' @return A numeric value representing the regionalised BFI.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Load Plynlimon sub-catchments spatial polygons data frame
#'   data("PlynlimonSUBCATCHMENTS")
#'   # Load soil map (percentage distribution of classes)
#'   data("PlynlimonSOIL")
#'   # Load lookup table
#'   data("S1")
#'
#'   RegionalisedBFI(soil = PlynlimonSOIL,
#'                   catchment = PlynlimonSUBCATCHMENTS[1,],
#'                   lookupTable = S1)
#' }
#'

RegionalisedBFI <- function(soil, catchment, lookupTable){

  # Clip soil map over Severn catchment
  soilMap <- ClipMap(soil, catchment)

  # Calculate the percentage coverage of each bandValue
  percentageCoverage <- PercentageCoverageSoilClasses(soilMap=soilMap,
                                                      removeZeros = FALSE)

  soilClasses <- as.numeric(gsub("[^0-9]", "", names(percentageCoverage)))
  idxClasses <- which(soilClasses %in% intersect(soilClasses,
                                               lookupTable$HOSTclass))

  regionalBFI <- sum(percentageCoverage[idxClasses]*lookupTable$BFIHOST)

  return(round(regionalBFI, 3))

}
