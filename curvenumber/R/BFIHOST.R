#' Generate Base Flow Index (BFI) map from soil (HOST) and vegetation (LCM) maps
#'
#' @param soilMap filename of a single band raster map (e.g. tif) that contains HOST soil classes (these are integers in the range [1,29]). By default this should be in the working directory.
#' @param shpFolder (optional) the path to the folder containing the GIS layers. By default this is the working directory.
#' @param mask (optional) filename of a shapefile containing the a single polygon to be used as mask to crop the raster. This is usually the catchment boundary.
#' @param myCRS proj4 for the desired Coordinate Reference System (default = British National Grid). See http://spatialreference.org/
#'
#' @return BFI map, a GeoTiff raster with same resolution of soil and vegetation maps and extent equal to the mask
#'
#' @examples
#' # BFI <- BFIHOST(soilMap=mySoilMap,shpFolder=myShpFolder,mask="mask_tmp")
#'

BFIHOST <- function(soilMap,shpFolder,mask,myCRS=NULL){

  # require(rgdal)
  # require(raster)

  if (is.null(myCRS)) {
    # use British National Grid by default
    myCRS <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000
    +datum=OSGB36 +units=m +no_defs +ellps=airy
    +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"
  }

  Soil <- raster(soilMap)
  proj4string(Soil) <- CRS(myCRS)

  rawMask <- readOGR(dsn=shpFolder,layer=mask)
  projectedMask <- spTransform(rawMask, crs(Soil))

  freqT <- data.frame(table(raster::extract(Soil,projectedMask)))

  # calculate the percentage coverage of each bandValue
  freqT$percentage <- freqT$Freq/sum(freqT$Freq)

  # Find correspondent BFI
  BFIHOST <- NULL
  load(system.file("BFIHOST.rda", package = 'curvenumber'))

  freqT$BFI <- NA
  # change values from HOST to BFI
  for (i in 1:dim(freqT)[1]){
    freqT$BFI[i] <- ifelse(is.na(freqT$Var1[i]), NA,
                           BFIHOST$BFI[which(BFIHOST$HOST==freqT$Var1[i])])
  }

  # sum up the above to get the final CN value
  BFI <- round(sum(freqT$percentage*as.numeric(as.character(freqT$BFI))),2)

  return(BFI)

}
