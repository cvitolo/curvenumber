#' Calculate Curve Number for soil (HOST) and vegetation (LCM) maps
#'
#' @param soil filename of a shapefile containing the percentage coverage of HOST classes
#' @param catchment SpatialPolygonsDataFrame containing a single catchment boundary to be used as mask to crop the raster.
#' @param lookupTable this is a data.frame to link soil, vegetation and Hydrological conditions.
#' @param vegetation filename of a raster containing the vegetation map.
#' @param artificialDrainage (default = "none"), possible values are none, low, medium, high.
#'
#' @details Please make sure that soil, catchment and vegetation are projected in the same Coordinate Reference System (for info see http://spatialreference.org/).
#'
#' @return Regionalised Curve Number for a given catchment, this is an integer in the range [0,100] with 0 = no runoff and 100 = all rainfall becomes runoff.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Load Plynlimon sub-catchments spatial polygons data frame
#'   data("PlynlimonSUBCATCHMENTS")
#'   # Load soil map (HOST percentage distribution of classes)
#'   data("PlynlimonSOIL")
#'   # Load land cover (VEGETATION 2013) MAP
#'   data("PlynlimonVEG")
#'   # Load lookup table
#'   dfLookup <- MakeLoopkupTable("Fair woods + Poor pasture")
#'
#'   cn <- RegionalisedCN(soil = PlynlimonSOIL,
#'                        catchment = PlynlimonSUBCATCHMENTS[1,],
#'                        lookupTable = dfLookup,
#'                        vegetation = PlynlimonVEG,
#'                        artificialDrainage = "none")
#' }
#'

RegionalisedCN <- function(soil, catchment, lookupTable, vegetation,
                           artificialDrainage = "none"){

  options(warn = -1)

  # Crop SpatialPolygon* object (percentage coverage HOST vector map) based on
  # catchment
  soilCropped <- raster::crop(soil, catchment)
  vegetationCropped <- raster::crop(vegetation, catchment)
  vegetationMasked <- raster::mask(vegetationCropped, catchment)

  # Set extent and resample, make a matrix out of the mask's extent
  catchmentEXT <- raster::extent(catchment)
  vegetationEXT <- raster::extent(vegetationMasked)
  soilEXT <- raster::extent(soilCropped)

  # make a matrix out of it, each column represents a raster, rows the values
  extent_df <- cbind(as.vector(catchmentEXT),
                     as.vector(vegetationEXT),
                     as.vector(soilEXT))
  rownames(extent_df) <- c("xmin", "xmax", "ymin", "ymax")

  best_extent <- raster::extent(max(extent_df[1,]), min(extent_df[2,]),
                                max(extent_df[3,]), min(extent_df[4,]))

  # the range of your extent
  rangesTMP <- as.data.frame(apply(extent_df, 1, range))
  ranges <- data.frame("x" = c(min(rangesTMP$xmin), max(rangesTMP$xmax)),
                       "y" = c(min(rangesTMP$ymin), max(rangesTMP$ymax)),
                       row.names = c("min", "xmax"))
  ranges <- apply(ranges, 2, diff)

  # the resolution of your raster (pick one, or add a desired resolution)
  reso <- raster::res(vegetationMasked)
  # divide the range by your desired resolution
  # gives you the number of rows and columns
  nrow_ncol <- ranges/reso

  # Resample vegetation raster
  r1 <- raster::raster(best_extent,
                       nrows = nrow_ncol[2],
                       ncols = nrow_ncol[1],
                       crs = vegetationMasked@crs)
  vegetationResampled <- raster::resample(vegetationMasked, r1, method = "ngb")

  # Read non null columns from res's table of attributes.
  tableOfAttributes <- soilCropped@data
  nonNullHOST <- c()
  for (cNames in names(tableOfAttributes)){
    if (substr(cNames, 1, 2)=="HO" &
          sum(tableOfAttributes[,cNames]) != 0 &
          substr(cNames, 9, nchar(cNames)) != 98 &
          substr(cNames, 9, nchar(cNames)) != 99) {
      nonNullHOST <- c(nonNullHOST, cNames)
    }
  }
  tableOfAttributes <- tableOfAttributes[, nonNullHOST]
  rownames(tableOfAttributes) <- NULL
  HOSTnumber <- substr(names(tableOfAttributes), 9,
                       nchar(names(tableOfAttributes)))

  # Utility function to calculate CN cell by cell from a raster stack.
  SoilandVeg2CNHOST <- function(x){

    if (x %in% 1:10){
      return(SoilandVeg2CN(soilCODE = HOSTclass,
                           vegetationCODE = x,
                           lookupTable,
                           artificialDrainage))
    }else{
      return(NA)
    }

  }

  CNrasters <- list(); counter <- 0
  # Create list of rasters
  for (HOSTclass in HOSTnumber) {

    print(paste0("Calculating contribution of HOSTCODE", HOSTclass))

    counter <- counter + 1

    CNrasterstemp <- raster::calc(vegetationResampled, fun = SoilandVeg2CNHOST)

    # Resample soil raster
    r <- raster::raster(raster::extent(soilCropped))
    SoilHOST <- raster::rasterize(soilCropped, r,
                                  field = paste0("HOSTCODE", HOSTclass))
    soilResampled <- raster::resample(SoilHOST, r1, method="ngb")

    CNrasters[[counter]] <- raster::overlay(CNrasterstemp, soilResampled,
                                            fun=function(x,y){return(x*y/100)})

  }

  # use Reduce and + to compute the sum from a list of rasters
  CNmap <- Reduce("+", CNrasters)

  CN <- round(raster::cellStats(CNmap, 'mean'), 0)

  return(CN)

}
