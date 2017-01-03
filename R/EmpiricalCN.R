#' Calculate the Curve Number from time series data (P and Q)
#'
#' @param tseries time series object containing, at least, precipitation ("P") and discharge ("Q")
#' @param stepsBack timesteps to use to generate the linear model for the volume under the rising limb (default = 5)
#' @param hours2extend number of hours to extend the rainfall event to account for the discharge peak to occur (default = 6)
#' @param timeUnits time step units (default = "hours")
#' @param verbose (optional) boolean (FALSE by default). If TRUE prints the progress in terms of event number.
#'
#' @return Empirical Curve Number for a given catchment, this is an integer in the range [0,100] with 0 = no runoff and 100 = all rainfall becomes runoff.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   data("SevernTS")
#'   severnCN <- EmpiricalCN(tseries = SevernTS)
#' }
#'

EmpiricalCN <- function(tseries, stepsBack = 5,
                       hours2extend = 6, timeUnits = "hours", verbose = FALSE){

  # if (verbose == TRUE) print("Identify precipitation events")
  tableP <- FindPevents(P = tseries$P, verbose)

  # if (verbose == TRUE) print("Identify streamflow discharge events")
  tableQ <- FindQevents(Q = tseries$Q,
                        eventTableP = tableP,
                        hours2extend = hours2extend, verbose)

  # If there are missing Q values, the related information cannot be calculated
  # and the records should be removed
  if (any(is.na(tableQ$PeakQ))) {

    rows2remove <- which(is.na(tableQ$PeakQ))
    tableP <- tableP[-rows2remove,]
    tableQ <- tableQ[-rows2remove,]

  }

  # PlotEvents(dataX = tseries, infoP = tableP, infoQ = tableQ)

  # Boorman (1995) suggests to adjust the end of Q events according to 4*LAG,
  # where LAG is the time difference between the rainfall & runoff centroids.
  # But the procedure is recursive, the LAG cannot be calculated without knowing
  # the end of the Q event. Also, what shall we do for LAG = 0?
  # LagPQ <- lagtime(tableP, tableQ, timeUnits = "hours")
  # abline(v=index(tseries)[tableQ$indexCentroid + 4*LagPQ],col="green")

  # The procedure above was dropped in favour of a pragmatic decision:
  # we extend the Q event for a certain number of hours and calculate the runoff
  # centroid of the streamflow up to that point.
  newTableQ <- FlowSeparation(tseries, tableQ, stepsBack, timeUnits)

  # Remove events with surfaceVolume = 0.
  rows2remove <- which(newTableQ$surfaceVolume == 0)
  if ( length(rows2remove) > 0 ) {

    eventTable <- newTableQ[-rows2remove, ]

  }else{

    eventTable <- newTableQ

  }

  # Calculate return periods for PQ events
  df <- ReturnPeriod(eventTable)

  # calculate CN from data
  CN <- try(CalculateCN(df), TRUE)
  if (class(CN) == "try-error") CN <- NA

  return(CN)

}
