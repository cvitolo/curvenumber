#' Clip soil map over a polygon and (re)calculate areas.
#'
#' @param soil SpatialPolygonsDataFrame containing percentage of HOST soil classes, i.e. see data("PlynlimonSOIL").
#' @param catchment SpatialPolygonsDataFrame containing a single catchment boundary.
#'
#' @return A SpatialPolygonsDataFrame clipped over a catchment with an additional column with updated areas.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   data("PlynlimonSUBCATCHMENTS")
#'   data("PlynlimonSOIL")
#'   soilMap <- ClipSoilMap(soil = PlynlimonSOIL,
#'                          catchment = PlynlimonSUBCATCHMENTS[1,])
#' }
#'

ClipSoilMap <- function(soil, catchment){

  soilMap <- raster::intersect(soil, catchment)
  soilMap$AreaKm2 <- raster::area(soilMap)/1000000

  return(soilMap)

}
