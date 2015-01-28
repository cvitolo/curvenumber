#' Map HOST classes into USDA classes
#'
#' @param vectorHOSTclass this is a vector of HOST classes (e.g. c("3","7")). This can contain a single value.
#'
#' @return USDA class, this can be: A, B, C, D or a combination of them.
#'
#' @examples
#' HOST2USDA(c("3","7"))
#'

HOST2USDA <- function(vectorHOSTclass){

  USDAclass <- c()

  for (HOSTclass in vectorHOSTclass) {

    if (any(c(1,2,3,5,11,13)==HOSTclass)) {
      USDAclass <- c(USDAclass, "A")
    }

    if (any(c(4,7)==HOSTclass)) {
      USDAclass <- c(USDAclass, "AB")
    }

    if (any(c(6,8,9,10,16)==HOSTclass)) {
      USDAclass <- c(USDAclass, "B")
    }

    if (17==HOSTclass) {
      USDAclass <- c(USDAclass, "BC")
    }

    if (any(c(18,19,20)==HOSTclass)) {
      USDAclass <- c(USDAclass, "C")
    }

    if (any(c(14,15,28)==HOSTclass)) {
      USDAclass <- c(USDAclass, "CD")
    }

    if (any(c(12,21,22,23,24,25,26,27,29)==HOSTclass)) {
      USDAclass <- c(USDAclass, "D")
    }

  }

  return(USDAclass)

}
