#' Identify PQ events applying Frequency Matching approach
#'
#' @param dataX is a data.frame containing P, Q (and E) time series
#' @param hours2extend number of hours to extend the rainfall event to account for the discharge peak to occur
#' @param plotOption boolean, if TRUE (default) it prints a plot to show the division of the events
#' @param stepsBack timesteps to use to generate the linear model for the volume under the rising limb (default = 5)
#' @param timeUnits time step units (default = "hours")
#'
#' @return table containing Return period and matching P and Q max values
#'
#' @examples
#' # df <- EventIdentification(dataX, hours2extend = 6, plotOption = FALSE)
#'

EventIdentification <- function(dataX, hours2extend = 6, plotOption = FALSE,
                                stepsBack=5, timeUnits = "hours"){

  tableP <- findPevents(dataX)
  tableQ <- findQevents(dataX, infoP=tableP, hours2extend)

  if ( any(is.na(tableQ$Peak)) ){

    row2remove <- which(is.na(tableQ$Value))

    tableP <- tableP[-row2remove,]
    tableQ <- tableQ[-row2remove,]

  }

  # PlotEvents(dataX, tableP, tableQ)

  # Boorman (1995) suggests to adjust the end of Q events according to 4*LAG,
  # where LAG is the time difference between the rainfall & runoff centroids.
  # But the procedure is recursive, the LAG cannot be calculated without knowing
  # the end of the Q event. Also, what shall we do for LAG = 0?
  # LagPQ <- lagtime(tableP, tableQ, timeUnits = "hours")
  # abline(v=index(dataX)[tableQ$indexCentroid + 4*LagPQ],col="green")

  # The procedure above was dropped in favour of a pragmatic decision:
  # we extend the Q event for a certain number of hours and calculate the runoff
  # centroid of the streamflow up to that point.

  if ( any(is.na(tableQ$Peak)) ){

    row2remove <- which(is.na(tableQ$Value))

    tableP <- tableP[-row2remove,]
    tableQ <- tableQ[-row2remove,]

  }

  newTableQ <- FlowSeparation(dataX, tableQ,
                              stepsBack=5, timeUnits = "hours",
                              plotOption, event2plot = 1)

  # Remove events with surfaceVolume = 0.
  rows2remove <- which(newTableQ$surfaceVolume==0)
  if ( length(rows2remove) > 0 ) {
    tableQ <- newTableQ[-rows2remove,]
    tableP <- tableP[-rows2remove,]
  }

  # Calculate return periods for PQ events
  df <- ReturnPeriod(tableP, tableQ)

  return(df)

}
