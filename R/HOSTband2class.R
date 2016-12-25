#' Map raster band values into HOST classes
#'
#' @param rasterBandValue this is a vector of raster band values (e.g. c("3","7")). This can contain a single value. Each value is an integer in the range [1,12].
#'
#' @return HOST class, this can be an integer (or a vector of inegers) in the range [1,29].
#'
#' @examples
#' HOSTband2class(c("3","7"))
#'

HOSTband2class <- function(rasterBandValue){

  HOSTclass <- c()

  for (bandValue in rasterBandValue) {

    if (bandValue == 1)  HOSTclass <- c(HOSTclass, 29)
    if (bandValue == 2)  HOSTclass <- c(HOSTclass, 26)
    if (bandValue == 3)  HOSTclass <- c(HOSTclass, 15)
    if (bandValue == 4)  HOSTclass <- c(HOSTclass, 17)
    if (bandValue == 5)  HOSTclass <- c(HOSTclass, 29)
    if (bandValue == 6)  HOSTclass <- c(HOSTclass, 17)
    if (bandValue == 7)  HOSTclass <- c(HOSTclass, 18)
    if (bandValue == 8)  HOSTclass <- c(HOSTclass,  6)
    if (bandValue == 9)  HOSTclass <- c(HOSTclass,  0)
    if (bandValue == 10) HOSTclass <- c(HOSTclass,  0)
    if (bandValue == 11) HOSTclass <- c(HOSTclass, 24)
    if (bandValue == 12) HOSTclass <- c(HOSTclass,  0)

  }

  return(HOSTclass)

}
