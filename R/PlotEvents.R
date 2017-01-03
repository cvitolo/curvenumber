#' Plot PQ events
#'
#' @param tseries time series object containing, at least, precipitation ("P") and discharge ("Q")
#' @param eventTable table containing summary of P&Q events
#'
#' @return Plot showing PQ events
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   data("SevernTS")
#'   tableP <- FindPevents(SevernTS$P[1:100])
#'   tableQ <- FindQevents(Q = SevernTS$Q[1:100],
#'                         eventTableP = tableP,
#'                         hours2extend = 6)
#'
#'   PlotEvents(tseries = SevernTS[1:100,],
#'              eventTable = tableQ)
#' }
#'

PlotEvents <- function(tseries, eventTable){

  # Set margins: Bottom, Left, Top, Right.
  op <- par(mfrow = c(2,1), mar=c(1,4,1,1), oma=c(1,2,2,1))

  plot(tseries$P, type = "n", cex = 0.1,
       main = "", xlab = "", ylab = "P", xaxt = "n")
  rect(eventTable$timeStart, par("usr")[3],
       eventTable$timeEndP, par("usr")[4], col = "gray")
  lines(tseries$P)
  abline(v = as.POSIXct(eventTable$timeCentroidP), col="red")
  legend("topleft", legend = "Pcentroid", col = "red", lty = 1)

  plot(tseries$Q, type = "n", main = "", xlab = "", ylab = "Q")
  rect(eventTable$timeStart, par("usr")[3],
       eventTable$timeEndQ, par("usr")[4], col = "gray")
  lines(tseries$Q)
  # abline(v = as.POSIXct(eventTable$timeCentroidP), col = "red")
  abline(v = as.POSIXct(eventTable$timeCentroidQ), col = "blue")
  legend("topleft", legend = "Qcentroid", col = "blue", lty = 1)

  par(mfrow=c(1,1))

}
