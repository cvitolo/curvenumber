#' Find discharge events
#'
#' @param Q is the discharge time series
#' @param eventTableP table containing summary of P events
#' @param hours2extend number of hours to extend the rainfall event to account for the discharge peak to occur
#' @param verbose (optional) boolean (FALSE by default). If TRUE prints the progress in terms of event number.
#'
#' @return table containing summary of events for the Q time series
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
#' }
#'

FindQevents <- function(Q, eventTableP, hours2extend, verbose = FALSE){

  # Add columns to event table
  eventTableP$indexEndQ <- eventTableP$indexEndP + hours2extend
  eventTableP$timeEndQ <- zoo::index(Q)[eventTableP$indexEndQ]

  # It is possible that the recorded time steps end before terminating the last
  # event. If so, this last event should be removed.
  if (is.na(tail(eventTableP$timeEndQ, n = 1))) {
    eventTableP <- eventTableP[1:(dim(eventTableP)[1]-1),]
  }

  # Add placeholders
  eventTableP$PeakQ <- NA
  eventTableP$VolumeQ <- NA
  eventTableP$DurationQ <- NA
  eventTableP$indexCentroidQ <- NA
  eventTableP$timeCentroidQ <- NA

  for (event in 1:dim(eventTableP)[1]){

    if (verbose == TRUE) {
      print(paste("Q-event n.",event,"out of",dim(eventTableP)[1]))
    }

    tmpQ <- Q[eventTableP$indexStart[event]:eventTableP$indexEndQ[event]]

    if (!all(is.na(tmpQ))){

      eventTableP$PeakQ[event] <- max(tmpQ)
      eventTableP$VolumeQ[event] <- sum(tmpQ)
      eventTableP$DurationQ[event] <- eventTableP$DurationP[event] + hours2extend

      eventDATA <- window(Q,
                          start = eventTableP$timeStart[event],
                          end = eventTableP$timeEndQ[event])
      centroidRelIndex <- round(Hmisc::wtd.mean(seq(1:length(eventDATA)),
                                                weights=eventDATA),0)
      eventTableP$indexCentroidQ[event] <- eventTableP$indexStart[event] +
        centroidRelIndex
      tempTime <- as.character(zoo::index(eventDATA)[centroidRelIndex])
      if (nchar(tempTime)==10) {
        eventTableP$timeCentroidQ[event] <- paste(tempTime,"00:00:00")
      }else{
        eventTableP$timeCentroidQ[event] <- tempTime
      }

    }

  }

  return(eventTableP)

}
