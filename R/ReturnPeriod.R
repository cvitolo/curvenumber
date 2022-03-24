#' Calculate return periods for PQ events
#'
#' @param eventTable is a table containing summary of P and Q events
#'
#' @return data.frame containing 3 columns: Tr (return period), P (max precipitation) and Q (max discharge)
#'
#' @examples
#' # ReturnPeriod(eventTable)
#'

ReturnPeriod <- function(eventTable){

  P <- eventTable$VolumeP
  Q <- eventTable$surfaceVolume

  # Order events in descending order and calculate matching return periods
  df <- data.frame("P"=sort(P, decreasing = TRUE),
                   "Q"=sort(Q, decreasing = TRUE))

  if ( any(df$P==0 | df$Q==0 | df$P < df$Q) ){
    rows2remove <- which(df$P==0 | df$Q==0 | df$P < df$Q)
    df <- df[-rows2remove,]
  }
  numberOfEvents <- dim(df)[1]

  # Define return periods
  df$Tr <- (numberOfEvents-1)/(1:numberOfEvents)

  return(df)

}
