#' Calculate the Curve Number from time series data (P and Q)
#'
#' @param DATA time series object containing, at least, precipitation ("P") and discharge ("Q")
#' @param startPeriod string to define starting date (e.g. "1979-10-01 00:00:00")
#' @param endPeriod string to define ending date (e.g. "1984-09-30 23:00:00")
#' @param stepsBack timesteps to use to generate the linear model for the volume under the rising limb (default = 5)
#' @param hours2extend number of hours to extend the rainfall event to account for the discharge peak to occur (default = 6).
#'
#' @return Curve Number, in the range [0,100]
#'
#' @examples
#' # CNfromDATA(DATA=ID54090)
#'

CNfromDATA <- function(DATA,
                       startPeriod = NULL, endPeriod = NULL,
                       stepsBack = 5,
                       hours2extend = 6){

  options(warn=-1)

  if (is.null(startPeriod) & is.null(endPeriod)){

    newInputTS <- DATA

  }else{

    myStartPeriod <- as.POSIXct(startPeriod)
    myEndPeriod <- as.POSIXct(endPeriod)

    # extract time-window
    newInputTS <- window(DATA,start=myStartPeriod,end=myEndPeriod)

  }

  # identify events
  DF  <- EventIdentification(DATA = newInputTS,
                             hours2extend, plotOption = FALSE,
                             stepsBack, timeUnits = "hours")

  # calculate CN from data
  CN <- try(CalculateCN(dfTPQ = DF, PQunits = "mm", plotOption = FALSE),TRUE)
  if (class(CN) == "try-error") CN <- NA

  return(CN)

}
