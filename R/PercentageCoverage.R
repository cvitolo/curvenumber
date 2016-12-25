#' Calculate the Curve Number from HOST soil map
#'
#' @param myRaster filename of a single band raster map (e.g. tif) that contains classes. By default this should be in the working directory.
#' @param shpFolder (optional) the path to the folder containing the GIS layers. By default this is the working directory.
#' @param myMask (optional) filename of a shapefile containing the a single polygon to be used as mask to crop the raster. This is usually the catchment boundary.
#' @param myCRS proj4 for the desired Coordinate Reference System (default = British National Grid). See http://spatialreference.org/
#' @param host if TRUE it uses HOST classes, otherwise it assumes that the classification is based on the Soil Hydrology Study conducted by JP Bell in 1968-1969: Bell, J.P. (1969). The Soil Hydrology of the Plynlimon Catchments. Institute of Hydrology Report No. 8, Institute of Hydrology, Wallingford, UK.
#' @param methodBy this is a string that identifies the methodology to be used. Possible values are: "Bulygina et al., 2011" (default), "Halcrow, 2011".
#' @param plotOption if TRUE it plots the soil map and mask
#'
#' @return table containing the percentage coverages
#'
#' @examples
#' # PercentageCoverage(myRaster=mySoilMap1000,shpFolder=myShpFolder,myMask="severn")
#'

PercentageCoverage <- function(myRaster, shpFolder=NULL, myMask=NULL, myCRS=NULL,
                               host=FALSE, methodBy="Bulygina et al., 2011",plotOption=FALSE){

  # require(rgdal)
  # require(raster)

  if (is.null(myCRS)) {
    # use British National Grid by default
    myCRS <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000
+datum=OSGB36 +units=m +no_defs +ellps=airy
+towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"
  }

  # if the shpFolder is not given in input, use the working directory
  if (is.null(shpFolder)) shpFolder <- getwd()

  # load the soil raster map, which shows the host classes
  #Soil <- raster(paste(shpFolder,myRaster,sep=""))
  Soil <- raster(myRaster)
  proj4string(Soil) <- CRS(myCRS)

  # if a mask is not given in input, calculate the class frequency (freqT) of
  # each cell of the entire raster. Otherwise crop the raster based on the mask
  # and calculate the class frequency based on that mask's area.
  if (is.null(myMask)){
    freqT <- data.frame(freq(Soil))
  }else{
    rawMask <- readOGR(dsn=shpFolder,layer=myMask)
    # Shapefile reprojection
    projectedMask <- spTransform(rawMask, crs(Soil))
    freqT <- data.frame(table(raster::extract(Soil,projectedMask)))
  }
  names(freqT) <- c("bandValue","frequency")

  # calculate the percentage coverage of each bandValue
  freqT$percentage <- round(100*freqT$frequency/sum(freqT$frequency),0)

  # if the raster shows zero values or there is no perfect overlap between soil
  # and mask, the areas outside the polygon are removed
  if (any(freqT$bandValue==0)) {
    message("Some cells contain zeros, they will be ignored.")
    freqT <- freqT[!(freqT$bandValue==0),]
    row.names(freqT) <- NULL
  }

  if (host==FALSE){
    # Mapping raster band values into HOST classes
    freqT$HOSTclass <- HOSTband2class(freqT$bandValue)
  }else{
    freqT$HOSTclass <- freqT$bandValue
  }

  # Mapping between HOST into USDA classes
  # (default method is Bulygina et al., 2011; othe method is Halcrow, 2011)
  freqT$USDAclass <- HOST2USDA(freqT$HOSTclass, methodBy)

  if (plotOption == TRUE){
    plot(extent(projectedMask))
    plot(Soil,add=TRUE)
    if (!is.null(mask)) plot(projectedMask,col=rgb(1,0,0,0.2),add=TRUE)
  }

  return(freqT)

}
