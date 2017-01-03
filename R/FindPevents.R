#' Find precipitation events
#'
#' @param P is the precipitation time series
#' @param verbose (optional) boolean (FALSE by default). If TRUE prints the progress in terms of event number.
#'
#' @return table containing summary of events for the time series DATA
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   data("SevernTS")
#'   tableP <- FindPevents(SevernTS$P)
#' }
#'

FindPevents <- function(P, verbose = FALSE){

  # thresh is the minimum value bigger than 0
  evp <- hydromad::eventseq(x = P,
                            thresh   = min(P[which(P > 0)]),
                            inthresh = 0,
                            mindur   = 2,
                            mingap   = 12) #6?

  eventinfoP <- hydromad::eventinfo(P, evp, FUN = max)

  # Rename information in the eventinfoP table
  names(eventinfoP)[which(names(eventinfoP)=="Time")] <- "timeStart"
  names(eventinfoP)[which(names(eventinfoP)=="Value")] <- "PeakP"
  names(eventinfoP)[which(names(eventinfoP)=="Duration")] <- "DurationP"

  # Add information to eventinfoP table
  eventinfoP$EventID <- 1:dim(eventinfoP)[1]
  eventinfoP$indexStart <- which(zoo::index(P) %in% eventinfoP$timeStart)
  eventinfoP$indexEndP <- eventinfoP$indexStart + eventinfoP$DurationP

  if (any(eventinfoP$indexEndP > length(P))){

    eventinfoP$indexEndP[eventinfoP$indexEndP > length(P)] <- length(P)

  }

  eventinfoP$timeEndP <- zoo::index(P)[eventinfoP$indexEndP]
  eventinfoP$VolumeP <- hydromad::eventinfo(P, evp, FUN = sum)$Value

  # Create placeholders for centroid info
  eventinfoP$indexCentroidP <- NA
  eventinfoP$timeCentroidP <- NA

  # Calculate centroid info
  for (event in 1:dim(eventinfoP)[1]){

    if (verbose == TRUE) {
      print(paste("P-event n.", event, "out of", dim(eventinfoP)[1]))
    }

    eventDATA <- window(P,
                        start = eventinfoP$timeStart[event],
                        end = eventinfoP$timeEndP[event])

    centroidRelIndex <- round(Hmisc::wtd.mean(seq(1:length(eventDATA)),
                                              weights=eventDATA),0)

    eventinfoP$indexCentroidP[event] <- eventinfoP$indexStart[event] +
      centroidRelIndex

    tempTime <- as.character(zoo::index(eventDATA)[centroidRelIndex])

    if (nchar(tempTime)==10) {

      eventinfoP$timeCentroidP[event] <- paste(tempTime,"00:00:00")

    }else{

      eventinfoP$timeCentroidP[event] <- tempTime

    }

  }

  return(eventinfoP)

}
