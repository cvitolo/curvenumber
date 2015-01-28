#' Calculate the Curve Number from HOST soil map
#'
#' @param soilMap filename of a single band raster map (e.g. tif) that contains HOST soil classes (these are integers in the range [1,29]). By default this should be in the working directory.
#' @param tabulatedCN list containing the CN tabulated values for the four hydrologic soil group (e.g. list("A"=30,"B"=55,"C"=70,"D"=77)). This is tabulated here: http://en.wikipedia.org/wiki/Runoff_curve_number.
#' @param shpFolder (optional) the path to the folder containing the GIS layers. By default this is the working directory.
#' @param mask (optional) filename of a shapefile containing the a single polygon to be used as mask to crop the raster. This is usually the catchment boundary.
#' @param plotOption if TRUE it plots the soil map and mask
#'
#' @return Curve Number, in the range [0,100]
#'
#' @examples
#' # CNfromMaps(soilMap="PlynlimonSoils.tif",
#' # tabulatedCN=list("A"=30,"B"=55,"C"=70,"D"=77),
#' # shpFolder="/home/claudia/Dropbox/Projects/PURE/PURE_shared/Data/",
#' # mask="severn", plotOption=FALSE)
#'

CNfromMaps <- function(soilMap, tabulatedCN,
                       shpFolder=NULL, mask=NULL,plotOption=FALSE){

  # require(raster)
  # require(rgdal)

  # if the shpFolder is not given in input, use the working directory
  if (is.null(shpFolder)) shpFolder <- getwd()

  # load the soil raster map, which shows the host classes
  #Soil <- raster(paste(shpFolder,soilMap,sep=""))
  Soil <- raster(soilMap)

  # if a mask is not given in input, calculate the class frequency (freqT) of
  # each cell of the entire raster. Otherwise crop the raster based on the mask
  # and calculate the class frequency based on that mask's area.
  if (is.null(mask)){
    freqT <- data.frame(freq(Soil))
  }else{
    rawMask <- readOGR(dsn=shpFolder,layer=mask)
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

  # Mapping raster band values into USDA classes
  freqT$HOSTclass <- HOSTband2class(freqT$bandValue)

  # Mapping between HOST into USDA classes (Bulygina et al., 2011)
  freqT$USDAclass <- HOST2USDA(freqT$HOSTclass)

  # aggregate unique classes
  USDAclass <- unique(freqT$USDAclass)
  freqClass <- c()
  for (i in 1:length(USDAclass)) {
    freqClass <- c(freqClass,
                   sum(freqT$frequency[which(freqT$USDAclass==USDAclass[i])]) )
  }
  aggregatedTable <- data.frame("USDAclass"=USDAclass, "frequency"=freqClass)
  totalNcells <- sum(aggregatedTable$frequency)

  # calculate weighted average of CN values suggested for each class
  aggregatedTable <- cbind(aggregatedTable,
                           "CN"=rep(NA, dim(aggregatedTable)[1]))
  for (r in 1:dim(aggregatedTable)[1]) {
    if ( nchar(as.character(aggregatedTable$USDAclass[r]))==1 ){
      cn <- which(names(tabulatedCN) %in% aggregatedTable$USDAclass[r])
      aggregatedTable$CN[r] <- round((aggregatedTable$frequency[r]/totalNcells)*tabulatedCN[[cn]],0)
    }else{
      combinedCN <- as.character(aggregatedTable$USDAclass[r])
      cn1 <- which(names(tabulatedCN) %in% substr(combinedCN,1,1))
      cn2 <- which(names(tabulatedCN) %in% substr(combinedCN,2,2))
      aggregatedTable$CN[r] <- round(
        0.5*(aggregatedTable$frequency[r]/totalNcells)*tabulatedCN[[cn1]] +
        0.5*(aggregatedTable$frequency[r]/totalNcells)*tabulatedCN[[cn2]], 0 )
    }
  }

  # sum up the above to get the final CN value
  CN <- round(sum(aggregatedTable$CN),0)

  if (plotOption == TRUE){
    plot(Soil)
    if (!is.null(mask)) plot(projectedMask,col=rgb(1,0,0,0.2),add=TRUE)
  }

  return(CN)

}
