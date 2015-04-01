#' Plot PQ events
#'
#' @param dataX is a data.frame containing P, Q (and E) time series
#' @param infoP table containing summary of P events
#' @param infoQ table containing summary of Q events
#'
#' @return Plot showing PQ events
#'
#' @examples
#' # PlotEvents(dataX, infoP, infoQ)
#'

PlotEvents <- function(dataX, infoP, infoQ){

  eventTableP4Plot <- infoP

  # correct eventTableP4Plot
  for (event in 1:dim(eventTableP4Plot)[1]){

    eventTableP4Plot$timeStart[event] <- index(dataX$P)[eventTableP4Plot$indexStart[event]-1]

    eventTableP4Plot$Duration[event] <- eventTableP4Plot$Duration[event] + 1
    eventTableP4Plot$PreDuration[event] <- eventTableP4Plot$PreDuration[event] - 1

  }

  # Set margins: Bottom, Left, Top, Right.
  op <- par(mfrow = c(2,1), mar=c(1,4,1,1), oma=c(1,2,2,1))

  plot(dataX$P,type="n",cex=0.1,
       main="",xlab="",ylab="P [mm/d]",xaxt="n")
  rect(eventTableP4Plot$timeStart,
       par("usr")[3],
       eventTableP4Plot$timeEnd,
       par("usr")[4],
       col="gray")
  points(dataX$P,type="l")
  abline(v=eventTableP4Plot$timeCentroid,col="red")
  #legend("top",c("Pcentroid","Qcentroid"),col=c("red","blue"),lty = c(1,1))

  plot(dataX$Q,type="n",main="",xlab="",ylab="Q [mm/d]")
  rect(eventTableP4Plot$timeStart,
       par("usr")[3],
       infoQ$timeEnd,
       par("usr")[4],
       col="gray")
  points(dataX$Q,type="l")
  abline(v=infoP$timeCentroid,col="red")
  abline(v=infoQ$timeCentroid,col="blue")

  par(mfrow=c(1,1))

}
