#' Find precipitation events
#'
#' @param P is the precipitation time series
#'
#' @return table containing summary of events for the time series DATA
#'
#' @examples
#' # tableP <- findPevents(P)
#'

findPevents <- function(P){

  # require(hydromad)

  # thresh is the minimum value bigger than 0
  evp <- eventseq(x        = P,
                  thresh   = min(P[which(P>0)]),
                  inthresh = 0,
                  mindur   = 2,
                  mingap   = 12) #6?

  eventinfoP <- eventinfo(P, evp, FUN = max)

  # Rename information in the eventinfoP table
  names(eventinfoP)[which(names(eventinfoP)=="Time")] <- "timeStart"
  names(eventinfoP)[which(names(eventinfoP)=="Value")] <- "PeakP"
  names(eventinfoP)[which(names(eventinfoP)=="Duration")] <- "DurationP"

  # Add information to eventinfoP table
  eventinfoP$EventID <- 1:dim(eventinfoP)[1]
  eventinfoP$indexStart <- which( index(P) %in% eventinfoP$timeStart )
  eventinfoP$indexEndP <- eventinfoP$indexStart + eventinfoP$DurationP
  eventinfoP$timeEndP <- index(P)[eventinfoP$indexEndP]
  eventinfoP$VolumeP <- eventinfo(P, evp, FUN = sum)$Value

  # Create placeholders for centroid info
  eventinfoP$indexCentroidP <- NA
  eventinfoP$timeCentroidP <- NA

  # Calculate centroid info
  for (event in 1:dim(eventinfoP)[1]){
    eventDATA <- window(P,
                        start = eventinfoP$timeStart[event],
                        end = eventinfoP$timeEndP[event])
    centroidRelIndex <- round(wtd.mean(seq(1:length(eventDATA)),weights=eventDATA),0)
    eventinfoP$indexCentroidP[event] <- eventinfoP$indexStart[event] + centroidRelIndex
    tempTime <- as.character(index(eventDATA)[centroidRelIndex])
    if (nchar(tempTime)==10) {
      eventinfoP$timeCentroidP[event] <- paste(tempTime,"00:00:00")
    }else{
      eventinfoP$timeCentroidP[event] <- tempTime
    }
  }

  return(eventinfoP)

}
