#' Clip map over a polygon and (re)calculate areas.
#'
#' @param map SpatialPolygonsDataFrame or RasterLayer to clip (e.g. soil map containing percentage of HOST soil classes, PlynlimonSOIL).
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
#'   clippedMap <- ClipMap(map = PlynlimonSOIL,
#'                         catchment = PlynlimonSUBCATCHMENTS[1,])
#' }
#'

ClipMap <- function(map, catchment){

  clippedMap <- raster::intersect(map, catchment)
  clippedMap$AreaKm2 <- raster::area(clippedMap)/1000000

  return(clippedMap)

}
