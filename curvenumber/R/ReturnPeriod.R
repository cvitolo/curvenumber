#' Calculate return periods for PQ events
#'
#' @param infoP table containing summary of P events
#' @param infoQ table containing summary of Q events
#'
#' @return data.frame containing 3 columns: Tr (return period), P (max precipitation) and Q (max discharge)
#'
#' @examples
#' # ReturnPeriod(infoP,infoQ)
#'

ReturnPeriod <- function(infoP, infoQ){

  P <- infoP$Volume
  Q <- infoQ$surfaceVolume
  numberOfEvents <- dim(infoQ)[1]

  # Order events in descending order and calculate matching return periods
  eventsP <- sort(P, decreasing = TRUE)
  returnPeriodP <- (length(eventsP)-1)/(1:length(eventsP))
  dP <- data.frame(x=returnPeriodP, y=eventsP)
  # loglogplot(dP)

  eventsQ <- sort(Q, decreasing = TRUE)
  returnPeriodQ <- (length(eventsQ)-1)/(1:length(eventsQ))
  dQ <- data.frame(x=returnPeriodQ, y=eventsQ)
  # loglogplot(dQ)

  if ( any(dQ$y==0 | dP$y < dQ$y) ){
    rows2remove <- which(dQ$y==0 | dP$y < dQ$y)
    dP <- dP[-rows2remove,]
    dQ <- dQ[-rows2remove,]
    numberOfEvents <- numberOfEvents - length(rows2remove)
  }

  # Define return periods based on P events
  Tr <- (numberOfEvents-1)/(1:numberOfEvents)
  df <- data.frame("Tr"=Tr,"P"=dP$y,"Q"=dQ$y)

  return(df)

}
