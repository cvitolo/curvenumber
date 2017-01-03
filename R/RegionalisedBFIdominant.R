#' Calculate Base Flow Index (BFI) map from soil (HOST) map
#'
#' @param soil filename of a raster containing the dominant soil classes (HOST).
#' @param catchment SpatialPolygonsDataFrame containing a single catchment boundary to be used as mask to crop the raster.
#' @param lookupTable A dataframe containing as many rows as the number of soil classes and at least two columns (named "HOSTclass" and "BFIHOST").
#'
#' @return Regionalised BFI number for a given catchment, this is in the range [0,1] and is calculated only using dominant soil classes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Load Plynlimon sub-catchments spatial polygons data frame
#'   data("PlynlimonSUBCATCHMENTS")
#'   # Load soil map (HOST dominant classes)
#'   data("PlynlimonSOILdominant")
#'   # Load lookup table
#'   data("S1")
#'
#'   bfi <- RegionalisedBFIdominant(soil = PlynlimonSOILdominant,
#'                          catchment = PlynlimonSUBCATCHMENTS[1,],
#'                          lookupTable = S1)
#' }
#'

RegionalisedBFIdominant <- function(soil, catchment, lookupTable){

  freqT <- data.frame(table(raster::extract(soil, catchment)))

  # calculate the percentage coverage of each bandValue
  freqT$percentage <- freqT$Freq/sum(freqT$Freq)

  # Find correspondent BFI
  freqT$BFI <- NA
  # change values from HOST to BFI
  for (i in 1:dim(freqT)[1]){
    freqT$BFI[i] <- ifelse(is.na(freqT$Var1[i]), NA,
                           lookupTable$BFI[which(lookupTable$HOST==
                                                   freqT$Var1[i])])
  }

  # sum up the above to get the final CN value
  BFI <- round(sum(freqT$percentage*as.numeric(as.character(freqT$BFI))),3)

  return(BFI)

  }
