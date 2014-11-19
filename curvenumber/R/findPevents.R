#' Find precipitation events
#'
#' @param dataX is the table containing time series (P,Q and E)
#'
#' @return table containing summary of events for the time series dataX
#'
#' @examples
#' # tableP <- findPevents(dataX)
#'

findPevents <- function(dataX){

  # thresh is the minimum value bigger than 0
  evp <- eventseq(x        = dataX$P,
                  thresh   = min(dataX$P[which(dataX$P>0),]),
                  inthresh = 0,
                  mindur   = 1,
                  mingap   = 12)

#   xyplot(dataX$P) +
#     layer_(panel.xblocks(evp,
#                          col = c("grey90", "grey80"),
#                          border = "grey80"))

  peakP <- eventinfo(dataX$P, evp, FUN = max)

  # Format standard information table
  infoP <- data.frame(matrix(NA,ncol=11,nrow=dim(peakP)[1]))
  names(infoP) <- c("Event","indexStart","timeStart","indexEnd","timeEnd",
                    "indexCentroid","timeCentroid",
                    "Peak","Volume","Duration","PreDuration")

  infoP$Event <- 1:dim(peakP)[1]

  infoP$indexStart <- which( index(dataX$P) %in% peakP$Time )
  infoP$timeStart <- peakP$Time

  infoP$indexEnd <- infoP$indexStart + peakP$Duration
  infoP$timeEnd <- index(dataX$P)[infoP$indexEnd]

  infoP$Peak <- peakP$Value
  infoP$Volume <- eventinfo(dataX$P, evp, FUN = sum)$Value
  infoP$Duration <- peakP$Duration
  infoP$PreDuration <- peakP$PreDuration

  eventTableP <- centroid(dataX = dataX$P, infoX = infoP,
                          useMax = FALSE, altStart = NULL)

  return(eventTableP)

}
