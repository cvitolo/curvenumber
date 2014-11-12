#' Find discharge events
#'
#' @param dataQ is a Q time series
#' @param newInfoP table containing summary of P events
#' @param plotOption boolean, if TRUE (default) it prints a plot to show the division of the events
#'
#' @return table containing summary of events for the Q time series
#'
#' @examples
#' # tableQ <- findQevents(data$Q,tableP)
#'

findQevents <- function(dataQ, newInfoP, plotOption=TRUE){

  infoQ <- newInfoP[,c("Time","Month","Year","Value","Duration","PreDuration")]
  # Temporary set the end of the Q event after 6 hours from the P centroid
  infoQ$Duration <- newInfoP$Duration + 6
  infoQ$EndTime <- index(data)[which(index(data) %in% newInfoP$EndTime) + 6]

  newInfoQ <- centroid(dataQ, infoQ,
                       useMax = TRUE, altStart = newInfoP$indexCentroid)

  if (plotOption == TRUE) {
    plot(dataQ,type="n",main="Discharge",xlab="",ylab="mm")
    rect(newInfoQ$Time,
         par("usr")[3],
         infoQ$EndTime,
         par("usr")[4],
         col="gray")
    points(dataQ,type="l")
    abline(v=newInfoP$timeCentroid,col="red")
    abline(v=newInfoQ$timeCentroid,col="blue")
  }

  return(newInfoQ)

}
