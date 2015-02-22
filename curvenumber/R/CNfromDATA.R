#' Calculate the Curve Number from time series data (P and Q)
#'
#' @param myData time series object containing, at least, precipitation ("P") and discharge ("Q")
#' @param startPeriod string to define starting date (e.g. "1979-10-01 00:00:00")
#' @param endPeriod string to define ending date (e.g. "1984-09-30 23:00:00")
#' @param conversionFactor multiplicative conversion factor to take into account that some time series are expressed in "mm/day" but the time step is 1 hour (or similar situations)
#' @param verbose if TRUE it plots the time series and summary statistics.#'
#' @param stepsBack timesteps to use to generate the linear model for the volume under the rising limb (default = 5)
#' @param hours2extend number of hours to extend the rainfall event to account for the discharge peak to occur (default = 6).
#'
#' @return Curve Number, in the range [0,100]
#'
#' @examples
#' # CNfromDATA(myData=ID54090,startPeriod="1979-10-01 00:00:00",
#' # endPeriod="1984-09-30 23:00:00",conversionFactor=1/24,verbose=TRUE)
#'

CNfromDATA <- function(myData,
                       startPeriod=NULL,endPeriod=NULL,
                       conversionFactor=1,
                       verbose=FALSE,
                       stepsBack = 5,
                       hours2extend=6){

  options(warn=-1)

  InputTS <- myData*conversionFactor

  if (is.null(startPeriod) & is.null(endPeriod)){
    newInputTS <- InputTS
  }else{
    myStartPeriod <- as.POSIXct(startPeriod)
    myEndPeriod <- as.POSIXct(endPeriod)

    # extract time-window
    newInputTS <- window(InputTS,start=myStartPeriod,end=myEndPeriod)
  }

  if (verbose==TRUE){
    plot(newInputTS)
    print(summary(newInputTS))
  }

  # identify events
  DF  <- EventIdentification(dataX = newInputTS,
                             hours2extend, plotOption = FALSE,
                             stepsBack, timeUnits = "hours")

  # calculate CN from data
  CN <- CalculateCN(dfTPQ = DF, PQunits = "mm", plotOption = TRUE)

  return(CN)

}
