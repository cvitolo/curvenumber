#' Map HOST classes into USDA classes
#'
#' @param vectorHOSTclass this is a vector of HOST classes (e.g. c("3","7")). This can contain a single value.
#' @param methodBy this is a string that identifies the methodology to be used. Possible values are: "Bulygina et al., 2011" (default), "Halcrow, 2011".
#'
#' @return USDA class, this can be: A, B, C, D or a combination of them.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   HOST2USDA(c("3","7"))
#' }
#'

HOST2USDA <- function(vectorHOSTclass, methodBy="Bulygina et al., 2011"){

  USDAclass <- c()

  if (methodBy=="Bulygina et al., 2011"){
    for (HOSTclass in vectorHOSTclass) {

      if (any(c(1,2,3,5,11,13) == HOSTclass)) {
        USDAclass <- c(USDAclass, "A")
      }

      if (any(c(4,7) == HOSTclass)) {
        USDAclass <- c(USDAclass, "AB")
      }

      if (any(c(6,8,9,10,16) == HOSTclass)) {
        USDAclass <- c(USDAclass, "B")
      }

      if (17==HOSTclass) {
        USDAclass <- c(USDAclass, "BC")
      }

      if (any(c(18,19,20) == HOSTclass)) {
        USDAclass <- c(USDAclass, "C")
      }

      if (any(c(14,15,28) == HOSTclass)) {
        USDAclass <- c(USDAclass, "CD")
      }

      if (any(c(12,21,22,23,24,25,26,27,29) == HOSTclass)) {
        USDAclass <- c(USDAclass, "D")
      }

    }
  }

  if (methodBy=="Halcrow, 2011"){
    for (HOSTclass in vectorHOSTclass) {

      if (any(c(1, 2, 13) == HOSTclass)) {
        USDAclass <- c(USDAclass, "A")
      }

      if (any(c(3, 4, 5, 7, 11) == HOSTclass)) {
        USDAclass <- c(USDAclass, "B")
      }

      if (any(c(6, 8, 9, 16, 17, 18, 20, 28) == HOSTclass)) {
        USDAclass <- c(USDAclass, "C")
      }

      if (any(c(10, 12, 14, 15, 19, 21, 22,
                23, 24, 25, 26, 27, 29) == HOSTclass)) {
        USDAclass <- c(USDAclass, "D")
      }

    }
  }

  return(USDAclass)

}
