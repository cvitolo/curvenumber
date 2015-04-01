#' Generate Base Flow Index (BFI) map from percentages of HOST classes.
#'
#' @param soilMap shapefile that contains percentage of HOST soil classes.
#'
#' @return BFIHOST a number in the range [0,1] that represents the theoretical BFI.
#'
#' @examples
#' # BFI <- BFIHOST(soilMap=mySoilMap)
#'

BFIHOST <- function(soilMap){

  # require(rgdal)
  # require(raster)
  # soilMap <- "/home/claudia/Dropbox/Projects/PURE/PURE_shared/Data/vectors/host_54022.shp"

  # Read in the shapefile
  soilSHP <- read.dbf(gsub(".shp", ".dbf",soilMap), header=TRUE)

  # Calculate the percentage coverage of each bandValue
  totArea <- sum(soilSHP$dbf$AREAkm2)
  columnNames <- c("HOSTCODE1","HOSTCODE2","HOSTCODE3","HOSTCODE4",
                   "HOSTCODE5","HOSTCODE6","HOSTCODE7","HOSTCODE8",
                   "HOSTCODE9","HOSTCODE10","HOSTCODE11","HOSTCODE12",
                   "HOSTCODE13","HOSTCODE14","HOSTCODE15","HOSTCODE16",
                   "HOSTCODE17","HOSTCODE18","HOSTCODE19","HOSTCODE20",
                   "HOSTCODE21","HOSTCODE22","HOSTCODE23","HOSTCODE24",
                   "HOSTCODE25","HOSTCODE26","HOSTCODE27","HOSTCODE28",
                   "HOSTCODE29")

  listClasses <- as.list(rep(NA,length(columnNames)))
  names(listClasses) <- columnNames
  for (cols in columnNames){
    for (rows in 1:dim(soilSHP$dbf)[1]){
      listClasses[[cols]] <- sum(soilSHP$dbf[,cols]/100*soilSHP$dbf$AREAkm2)
    }
  }

  # calculate the percentage coverage of each bandValue
  percentageCoverage <- round(unlist(listClasses)/totArea,2)
  percentageCoverage <- as.list(percentageCoverage[percentageCoverage!=0])

  # Find corresponding BFIHOST
  BFIHOST <- NULL
  load(system.file("BFIHOST.rda", package = 'curvenumber'))

  BFI <- NA
  # change values from HOST to BFI
  for (i in 1:length(percentageCoverage)){
    soilClass <- substr(names(percentageCoverage)[i],9,
                        nchar(names(percentageCoverage)[i]))
    BFI[i] <- BFIHOST$BFI[which(BFIHOST$HOST==soilClass)]
  }

  # sum up the above to get the final CN value
  BFI <- round(sum(as.numeric(as.character(percentageCoverage))*as.numeric(as.character(BFI))),3)

  return(BFI)

}
