#' Calculate Curve Number for soil (HOST) and vegetation (LCM) maps
#'
#' @param soilMap filename of a single band raster map (e.g. tif) that contains HOST soil classes (these are integers in the range [1,29]). By default this should be in the working directory.
#' @param vegetationMap (optional) filename of a shapefile containing the vegetation map (polygons). This should be in the shpFolder directory.
#' @param shpFolder (optional) the path to the folder containing the GIS layers. By default this is the working directory.
#' @param mask (optional) filename of a shapefile containing the a single polygon to be used as mask to crop the raster. This is usually the catchment boundary.
#' @param myCRS proj4 for the desired Coordinate Reference System (default = British National Grid). See http://spatialreference.org/
#' @param artificialDrainage (default = "none"), possible values are none, low, medium,high.
#' @param lookupTable this is a data.frame to link Soil, Vegetation and Hydrological conditions.
#'
#' @return Curve Number map, a GeoTiff raster with same resolution of soil and vegetation maps and extent equal to the mask
#'
#' @examples
#' # CNmap <- CNfromMaps(soilMap=mySoilMap,vegetationMap=myVegMap,
#' #                     shpFolder=myShpFolder,mask="severn",
#' #                     myCRS=NULL,artificialDrainage="none")
#'

CNfromMapsHOSTdominant <- function(soilMap,vegetationMap,shpFolder,mask,
                       myCRS=NULL,artificialDrainage="none",lookupTable){

  # require(rgdal)
  # require(raster)

  if (is.null(myCRS)) {
    # use British National Grid by default
    myCRS <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000
    +datum=OSGB36 +units=m +no_defs +ellps=airy
    +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"
  }

  Soil <- raster(soilMap)
  Vegetation <- raster(vegetationMap)
  proj4string(Soil) <- CRS(myCRS)
  proj4string(Vegetation) <- CRS(myCRS)

  rawMask <- readOGR(dsn=shpFolder,layer=mask)
  projectedMask <- spTransform(rawMask, crs(Vegetation))

  a <- extent(projectedMask)
  b <- extent(projectedMask)
  extent_list <- list(a, b)
  # make a matrix out of it, each column represents a raster, rows the values
  extent_list<-lapply(extent_list, as.matrix)
  matrix_extent<-matrix(unlist(extent_list), ncol=2)
  rownames(matrix_extent)<-c("xmin", "ymin", "xmax", "ymax")

  # create an extent with the extrem values of your extent
  best_extent<-extent(min(matrix_extent[1,]), max(matrix_extent[3,]),
                      min(matrix_extent[2,]), max(matrix_extent[4,]))

  # the range of your extent in degrees
  ranges<-apply(as.matrix(best_extent), 1, diff)
  # the resolution of your raster (pick one) or add a desired resolution
  reso<-res(Soil)
  # deviding the range by your desired resolution gives you the number of rows and columns
  nrow_ncol<-ranges/reso

  # create your raster with the following
  r1 <- raster(best_extent, nrows=nrow_ncol[2], ncols=nrow_ncol[1], crs=Soil@crs)

  newSoil <-resample(Soil, r1, method="ngb")
  newVegetation <-resample(Vegetation, r1, method="ngb")

  CNmap <- overlay(newSoil,newVegetation, fun=function(x,y){return(x+y*1000)} )

  # calculate the percentage coverage of each bandValue
  freqT <- data.frame(table(raster::extract(CNmap,projectedMask)))

  # If the CNmap shows values less than 1000 it means that the vegetation map
  # is smaller than the soil map. If the CNmap shows values less that can be
  # divided by 1000 with no decimals it means that the soil map is smaller than
  # the vegetation map. In both cases, when there is no perfect overlap, the
  # non-overlapping cells are removed.
  if (any(as.numeric(as.character(freqT$Var1))<1000)) {
    message("Some cells do not overlap, they will be ignored.")
    freqT <- freqT[as.numeric(as.character(freqT$Var1))>1000,]
    row.names(freqT) <- NULL
  }
  if (any(as.numeric(as.character(freqT$Var1))%%1000==0)) {
    message("Some cells do not overlap, they will be ignored.")
    freqT <- freqT[as.numeric(as.character(freqT$Var1))%%1000!=0,]
    row.names(freqT) <- NULL
  }

  freqT$percentage <- round(freqT$Freq/sum(freqT$Freq),2)
  temp <- as.character(freqT$Var1)
  freqT$soil <- as.numeric(substr(temp, nchar(temp)-3+1, nchar(temp)))
  freqT$veg <- as.numeric(as.character(freqT$Var1))%/%1000

  if (grepl("host",soilMap)){
    freqT$soil <- HOSTband2class(freqT$soil)
  }

  freqT$usda <- HOST2USDA(freqT$soil)
  freqT$CN <- NA
  freqT$usdaNumber <- NA

  A <- B <- C <- D <- AB <- BC <- CD <- c()

  for (i in 1:length(freqT$veg)){

    A <- c(A, as.numeric(as.character(lookupTable$SoilA[lookupTable$Code==freqT$veg[i]])))
    B <- c(B, as.numeric(as.character(lookupTable$SoilB[lookupTable$Code==freqT$veg[i]])))
    C <- c(C, as.numeric(as.character(lookupTable$SoilC[lookupTable$Code==freqT$veg[i]])))
    D <- c(D, as.numeric(as.character(lookupTable$SoilD[lookupTable$Code==freqT$veg[i]])))
    AB <- c(AB, round(sum(A[i],B[i])/2,0) )
    BC <- c(BC, round(sum(B[i],C[i])/2,0) )
    CD <- c(CD, round(sum(C[i],D[i])/2,0) )

    freqT$CN[i] <- eval(parse(text=paste(as.character(freqT$usda[i]),"[1]",sep="")))

    if (freqT$usda[i]=="A")  freqT$usdaNumber[i] <- 1
    if (freqT$usda[i]=="AB") freqT$usdaNumber[i] <- 2
    if (freqT$usda[i]=="B")  freqT$usdaNumber[i] <- 3
    if (freqT$usda[i]=="BC") freqT$usdaNumber[i] <- 4
    if (freqT$usda[i]=="C")  freqT$usdaNumber[i] <- 5
    if (freqT$usda[i]=="CD") freqT$usdaNumber[i] <- 6
    if (freqT$usda[i]=="D")  freqT$usdaNumber[i] <- 7

  }

  freqT$A <- A
  freqT$AB <- AB
  freqT$B <- B
  freqT$BC <- BC
  freqT$C <- C
  freqT$CD <- CD
  freqT$D <- D

  if (artificialDrainage != "none"){

    if (artificialDrainage == "low") {
      freqT$USDAnumberDrainage <- freqT$usdaNumber - 1
    }
    if (artificialDrainage == "medium") {
      freqT$USDAnumberDrainage <- freqT$usdaNumber - 2
    }
    if (artificialDrainage == "high") {
      freqT$USDAnumberDrainage <- freqT$usdaNumber - 3
    }

    # Correct CN for drained soils
    for (i in 1: dim(freqT)[1]){
      mycolumn <- max(1,as.numeric(as.character(freqT$USDAnumberDrainage[i])))
      freqT$CN[i] <- eval(parse(text=paste("freqT[i,c('A','AB','B','BC','C','CD','D')][[",mycolumn,"]]",sep="")))
    }

  }

  # sum up the above to get the final CN value
  CN <- round(sum(as.numeric(as.character(freqT$CN))*as.numeric(as.character(freqT$percentage))),0)

  return(CN)

  }
