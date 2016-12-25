#' Find discharge events
#'
#' @param Q is the discharge time series
#' @param eventTable table containing summary of P events
#' @param hours2extend number of hours to extend the rainfall event to account for the discharge peak to occur
#'
#' @return table containing summary of events for the Q time series
#'
#' @examples
#' # tableQ <- findQevents(Q,eventTable,hours2extend=6)
#'

findQevents <- function(Q, eventTable, hours2extend){

  # Add columns to event table
  eventTable$indexEndQ <- eventTable$indexEndP + hours2extend
  eventTable$timeEndQ <- index(Q)[eventTable$indexEndQ]

  # It is possible that the recorded time steps end before terminating the last
  # event. If so, this last event should be removed.
  if (is.na(tail(eventTable$timeEndQ, n = 1))) {
    eventTable <- eventTable[1:(dim(eventTable)[1]-1),]
  }

  # Add placeholders
  eventTable$PeakQ <- NA
  eventTable$VolumeQ <- NA
  eventTable$DurationQ <- NA
  eventTable$indexCentroidQ <- NA
  eventTable$timeCentroidQ <- NA

  for (event in 1:dim(eventTable)[1]){

    # print(paste("Event n.",event,"out of",dim(eventTable)[1]))

    eventTable$PeakQ[event] <- max(Q[eventTable$indexStart[event]:eventTable$indexEndQ[event]])
    eventTable$VolumeQ[event] <- sum(Q[eventTable$indexStart[event]:eventTable$indexEndQ[event]])
    eventTable$DurationQ[event] <- eventTable$DurationP[event] + hours2extend

    eventDATA <- window(Q,
                        start = eventTable$timeStart[event],
                        end = eventTable$timeEndQ[event])
    centroidRelIndex <- round(wtd.mean(seq(1:length(eventDATA)),weights=eventDATA),0)
    eventTable$indexCentroidQ[event] <- eventTable$indexStart[event] + centroidRelIndex
    tempTime <- as.character(index(eventDATA)[centroidRelIndex])
    if (nchar(tempTime)==10) {
      eventTable$timeCentroidQ[event] <- paste(tempTime,"00:00:00")
    }else{
      eventTable$timeCentroidQ[event] <- tempTime
    }

  }

  # If there are missing Q values, the related information cannot be calculated
  # and the records should be removed
  if (any(is.na(eventTable$PeakQ))) {
    rows2remove <- which(is.na(eventTable$PeakQ))
    eventTable <- eventTable[-rows2remove,]
  }

  return(eventTable)

}
