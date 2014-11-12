#' Find precipitation events
#'
#' @param dataX is a time series
#' @param plotOption boolean, if TRUE (default) it prints a plot to show the division of the events
#'
#' @return table containing summary of events for the time series dataX
#'
#' @examples
#' # tableP <- findPevents(data$P)
#'

findPevents <- function(dataX, plotOption=TRUE){

  evp <- eventseq(x        = dataX,
                  thresh   = 0.1,
                  inthresh = 0,
                  mindur   = 1,
                  mingap   = 2)

  #   xyplot(data) +
  #     layer_(panel.xblocks(evp, col = c("grey90", "grey80"), border = "grey80"))

  infoP <- eventinfo(dataX, evp, FUN = max)

  # correct infoP
  for (row in 1:dim(infoP)[1]){

    infoP$Time[row] <- index(data)[which(index(data) %in% infoP$Time[row])-1]

    infoP$Duration[row] <- infoP$Duration[row] + 1

  }



  infoP$EndTime <- index(data)[which(index(data) %in% infoP$Time) + infoP$Duration]

  newInfoP <- centroid(dataX, infoP)

  if (plotOption == TRUE) {
    plot(dataX,type="n",main="Precipitation",xlab="",ylab="mm")
    rect(infoP$Time,
         par("usr")[3],
         infoP$EndTime,
         par("usr")[4],
         col="gray")
    points(dataX,type="l")
    abline(v=newInfoP$timeCentroid,col="red")
  }

  return(newInfoP)

}
