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
#'   dfLookup <- MakeLoopkupTable("Severn&Wye")
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

  # Clip soil map over catchment
  soilMap <- ClipMap(soil, catchment)

  # Clip vegetation map over catchment
  vegMap <- ClipMap(vegetation, catchment)

  s <- RasterForResampling(soilMap, vegMap, catchment)
  vegMap <- raster::resample(vegMap, s, method="ngb")

  # Read non null columns from res's table of attributes.
  tableOfAttributes <- soilMap@data[, 3:31]
  nonNullHOST <- apply(tableOfAttributes, 2, sum)
  nonNullHOST <- nonNullHOST[nonNullHOST != 0]

  # Utility function to calculate CN cell by cell from a raster stack.
  SoilandVeg2CNHOST <- function(x){

    if (x %in% 1:10){

      return(SoilandVeg2CN(vegetationCODE = x,
                           soilCODE = HOSTclass,
                           lookupTable,
                           artificialDrainage))

    }else{

      return(NA)

    }
  }

  CNraster <- raster::stack()
  # Create list of rasters
  for (i in 1:length(nonNullHOST)) {

    HOSTclass <- gsub("[^0-9]", "", names(nonNullHOST)[i])
    print(paste0("Calculating contribution of HOSTCODE", HOSTclass))

    tempRaster <- raster::calc(vegMap, fun = function(x){SoilandVeg2CNHOST(x)})

    # Resample soil raster
    SoilRaster <- raster::rasterize(x = soilMap, y = vegMap,
                                    field = paste0("HOSTCODE", HOSTclass))

    tempRasterSoil <- raster::overlay(tempRaster, SoilRaster,
                                      fun = function(x,y){return(x*y/100)})

    CNraster <- raster::stack(CNraster, tempRasterSoil)

  }

  # use Reduce and + to compute the sum from a list of rasters
  CNmap <- sum(CNraster)

  CN <- round(raster::cellStats(CNmap, 'mean'), 0)

  return(CN)

}
