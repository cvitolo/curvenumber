#' Calculate lagtime between precipitation and discharge time series
#'
#' @param newInfoP table containing summary of P events
#' @param newInfoQ table containing summary of Q events
#' @param timeUnits string defining the time unit to express the lagtime (default = "hours")
#'
#' @return lagtime in the units specified by timeUnits.
#'
#' @examples
#' # LagPQ <- lagtime(tableP, tableQ, timeUnits = "hours")
#'

lagtime <- function(newInfoP, newInfoQ, timeUnits = "hours"){

  temp <- difftime(newInfoQ$timeCentroid,newInfoP$timeCentroid, units=timeUnits)

  LagPQ <- as.numeric(temp)

  # Sometimes the lagtime is zero, should I set a minimum of 1 hour?
  # LagPQ[which(LagPQ==0)] <- 1

  return(LagPQ)

}
