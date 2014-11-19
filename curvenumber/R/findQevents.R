#' Find discharge events
#'
#' @param dataX is the table containing time series (P,Q and E)
#' @param infoP table containing summary of P events
#' @param hours2extend number of hours to extend the rainfall event to account for the discharge peak to occur
#'
#' @return table containing summary of events for the Q time series
#'
#' @examples
#' # tableQ <- findQevents(data$Q,tableP,hours2extend=6)
#'

findQevents <- function(dataX, infoP, hours2extend){

  # Format standard information table
  infoQ <- data.frame(matrix(NA,ncol=11,nrow=dim(infoP)[1]))
  names(infoQ) <- c("Event","indexStart","timeStart","indexEnd","timeEnd",
                    "indexCentroid","timeCentroid",
                    "Peak","Volume","Duration","PreDuration")

  infoQ$Event <- 1:dim(infoQ)[1]

  infoQ$indexStart <- infoP$indexStart
  infoQ$timeStart <- infoP$timeStart

  infoQ$indexEnd <- infoP$indexEnd + hours2extend
  infoQ$timeEnd <- index(dataX$Q)[infoQ$indexEnd]

  for (event in 1:dim(infoQ)[1]){

    infoQ$Peak[event] <- max(dataX$Q[infoQ$indexStart[event]:infoQ$indexEnd[event]])
    infoQ$Volume[event] <- sum(dataX$Q[infoQ$indexStart[event]:infoQ$indexEnd[event]])
    infoQ$Duration[event] <- infoP$Duration[event] + hours2extend

    if (event == 1) {
      infoQ$PreDuration[event] <- infoQ$indexStart[event] - 1
    }else{
      infoQ$PreDuration[event] <- infoQ$indexStart[event] - infoQ$indexEnd[event-1]
    }

  }

  eventTableQ <- centroid(dataX = dataX$Q, infoX = infoQ,
                          useMax = FALSE, altStart = NULL)

  return(eventTableQ)

}
