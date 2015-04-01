#' Calculate Curve Number for soil (HOST) and vegetation (LCM) maps
#'
#' @param Soil filename of a shapefile containing the percentage coverage of HOST classes
#' @param Vegetation filename of a raster containing the vegetation map.
#' @param Mask filename of a shapefile containing a single polygon to be used as mask to crop the raster. This is usually the catchment boundary.
#' @param myCRS proj4 for the desired Coordinate Reference System (default = British National Grid). See http://spatialreference.org/
#' @param artificialDrainage (default = "none"), possible values are none, low, medium,high.
#' @param lookupTable this is a data.frame to link Soil, Vegetation and Hydrological conditions.
#'
#' @return Curve Number map, a GeoTiff raster with same resolution of soil and vegetation maps and extent equal to the mask
#'
#' @examples
#' # CNmap <- CNfromMaps(Soil,Vegetation,Mask,lookupTable,
#' #                     myCRS=NULL,artificialDrainage="none",dominant=FALSE)
#'

CNfromMaps <- function(Soil,Vegetation,Mask,lookupTable,
                       myCRS=NULL,artificialDrainage="none"){

  options(warn=-1)

  if (is.null(myCRS)) {
    # use British National Grid by default
    myCRS <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"
  }

  # Read in vegetation raster map and set a mask for the catchment boundary
  Vegetation <- raster(Vegetation)
  proj4string(Vegetation) <- CRS(myCRS)

  # Read in catchment mask
  Mask <- spTransform(readOGR(dsn = dirname(Mask),
                              layer = strsplit(basename(Mask),"\\.")[[1]][1],
                              verbose=FALSE),
                      crs(Vegetation))

  # Read in percentage coverage HOST vector map
  # (and crop a SpatialPolygon* object with another one)
  Soil <- crop(shapefile(Soil), Mask)
  Vegetation <- mask(Vegetation, Mask)

  # Set extent and resample
  # make a matrix out of the mask's extent,
  a <- extent(Mask)
  b <- extent(Vegetation)
  c <- extent(Soil)
  extent_list <- list(a, b, c)
  # make a matrix out of it, each column represents a raster, rows the values
  extent_list<-lapply(extent_list, as.matrix)
  matrix_extent<-matrix(unlist(extent_list), ncol=3)
  rownames(matrix_extent)<-c("xmin", "ymin", "xmax", "ymax")

  # create an extent with the extreme values of your extent
  best_extent<-extent(min(matrix_extent[1,]), max(matrix_extent[3,]),
                      min(matrix_extent[2,]), max(matrix_extent[4,]))

  # the range of your extent in degrees
  ranges<-apply(as.matrix(best_extent), 1, diff)
  # the resolution of your raster (pick one) or add a desired resolution
  reso<-res(Vegetation)
  # deviding the range by your desired resolution gives you the number of rows and columns
  nrow_ncol<-ranges/reso

  # create your raster with the following
  r1 <- raster(best_extent, nrows=nrow_ncol[2], ncols=nrow_ncol[1],
               crs=Vegetation@crs)
  newVegetation <- resample(Vegetation, r1, method="ngb")

  # Read non null columns from res's table of attributes.
  tableOfAttributes <- Soil@data
  nonNullHOST <- c()
  for (cNames in names(tableOfAttributes)){
    if (substr(cNames,1,2)=="HO" &
          sum(tableOfAttributes[,cNames])!=0 &
          substr(cNames,9,nchar(cNames))!=98 &
          substr(cNames,9,nchar(cNames)) != 99) {
      nonNullHOST <- c(nonNullHOST,cNames)
    }
  }
  tableOfAttributes <- tableOfAttributes[,nonNullHOST]
  rownames(tableOfAttributes) <- NULL
  HOSTnumber <- substr(names(tableOfAttributes),9,nchar(names(tableOfAttributes)))

  CNrasters <- list(); counter <- 0
  # Create raster stack
  for (HOSTclass in as.numeric(HOSTnumber)) {

    # print(HOSTclass)

    counter <- counter + 1

    SoilandVeg2CNHOST <- function(x){

      if (x %in% 1:10){
        return(SoilandVeg2CN(soilCODE=HOSTclass,
                             vegetationCODE=x,
                             lookupTable,
                             artificialDrainage))
      }else{
        return(NA)
      }

    }

    CNrasterstemp <- calc(newVegetation,fun=SoilandVeg2CNHOST)

    r <- raster(extent(Soil))
    SoilHOST <- rasterize(Soil,r,field=paste("HOSTCODE",HOSTclass,sep=""))
    proj4string(SoilHOST) <- CRS(myCRS)
    newSoil <- resample(SoilHOST, r1, method="ngb")

    CNrasters[[counter]] <- overlay(CNrasterstemp,newSoil,
                                    fun=function(x,y){return(x*y/100)})

  }

  # use Reduce and + to compute the sum from a list of rasters
  CNmap <- Reduce("+",CNrasters)

  CN <- round(cellStats(CNmap, 'mean'),0)

  return(CN)

}
