#' Separate Baseflow and Surface runoff
#'
#' @param dataX data.frame containing P, Q (and E)
#' @param infoQ table containing summary of Q events
#' @param stepsBack timesteps to use to generate the linear model for the volume under the rising limb (default = 5)
#' @param timeUnits time step units (default = "hours")
#' @param plotOption boolean, if TRUE (default) it prints a plot to show the division of the events
#' @param event2plot even number for which the plot should be generated (default = 3). This is only used if plotOption = TRUE
#'
#' @return updated table containing summary of Q events with 4 more columns:
#' volumeQ, baseflowVolume, surfaceVolume and surfacePeak
#'
#' @details From Boorman 1995: The recession prior to the event is continued through the event, and this flow is subtracted from the total flow hydrograph. A straight line is then drawn from beneath the peak flow, or centroid of peaks, to the point already identified as marking the end of response runoff. The response runoff is the portion of flow above this separation.
#'
#' @examples
#' # FlowSeparation(dataX, tableQ, stepsBack=5, timeUnits = "hours", plotOption = TRUE, event2plot = 3)
#'

FlowSeparation <- function(dataX, infoQ,
                           stepsBack=5, timeUnits = "hours",
                           plotOption = FALSE, event2plot = 2){

  # require(udunits2)
  multiplier <- ud.convert(1,timeUnits,"seconds")

  infoQ$baseflowVolume <- NA
  infoQ$surfaceVolume <- NA

  for (event in 1:dim(infoQ)[1]){

    p <- FALSE

    if (plotOption == TRUE & event2plot == event) p <- TRUE

    # Extract Q for the event window + stepsBack
    eventQ <- window(dataX$Q,
                     start=infoQ$timeStart[event]-stepsBack*multiplier,
                     end=infoQ$timeEnd[event])
    # PLOT:
    if (p == TRUE) plot(coredata(eventQ), pch=19, type="o", ylim=c(0,max(eventQ)))

    # Define the starting point of the event
    point1 <- c(stepsBack+1,coredata(dataX$Q[infoQ$indexStart[event]]))
    if (p == TRUE) abline(v=point1[1], col='gray', lty=2)

    # Define the points before the event
    x = 1:(stepsBack + 1)
    y = coredata(window(dataX$Q,
               start=infoQ$timeStart[event]-(stepsBack)*multiplier,
               end=infoQ$timeStart[event])[1:(stepsBack + 1)])
    preeventQ <- structure(list(x = x, y = y), .Names = c("x", "y"))
    # PLOT:
    if (p == TRUE) points(preeventQ, pch=19, col = "red")

    # Set a linear model to interpolate the point before the event.
    # modelQ.lm <- lm(formula = y ~ x, data = preeventQ)
    # abline(modelQ.lm, col="red")
    # The model above is not ideal, the line should be forced to go through
    # point1 (first point at the beginning of the event).

    # The first line minimises the points in preeventQ and passes through point1
    modelledLine1 <- lm(I(y-point1[2])~I(x-point1[1]) + 0, preeventQ)
    # PLOT:
    if (p == TRUE) {
      abline(predict(modelledLine1, newdata=list(x=0)) + point1[2],
             coef(modelledLine1), col='gray', lty=2)
    }
    # Note the + 0 at the end which indicates to lm that no intercept term should be fit.

    # We extend modelledLine1 to intersect the vertical line passing through the
    # centroid (point2), but if the slope is positive we force it to be = 0
    x2 <- which(index(eventQ) %in% infoQ$timeCentroid[event])
    if (p == TRUE) abline(v=x2, col='gray', lty=2)

    if (coef(modelledLine1)>0) {
      y2 <- point1[2]
      if (p == TRUE) abline(h=point1[2], col='red', lty=2)
    }else{
      y2 <- predict(modelledLine1, newdata = list(x=x2)) + point1[2]
      if (p == TRUE) {
        abline(predict(modelledLine1, newdata=list(x=0)) + point1[2],
               coef(modelledLine1), col='red', lty=2)
      }
    }
    point2 <- c(x2,y2)

    # Define the ending point of the event
    point3 <- c(length(eventQ),coredata(eventQ[length(eventQ)]))
    if (p == TRUE) abline(v=point3[1], col='gray', lty=2)

    # The second line goes from the centroid to the end of the event
    x = c(point2[1],point3[1])
    y = c(point2[2],point3[2])
    line2 <- structure(list(x = x, y = y), .Names = c("x", "y"))
    modelledLine2 <- lm(y~x, data=line2)
    if (p == TRUE) abline(modelledLine2, col="red", lty=2)

    # Let's separate Baseflow and surfaceflow from the total flow
    baseflow1 <- baseflow2 <- c()
    surfflow1 <- surfflow2 <- c()
    counter1 <- counter2 <- 0

    for (tindex in point1[1]:point3[1]){

      if (p == TRUE) points(tindex,eventQ[tindex], pch=19, col = ifelse(tindex <= point2[1], "blue", "pink") )

      # if the time step is related to modelledLine1
      if (tindex <= point2[1]){
        counter1 <- counter1 + 1

        # if the slope of modelledLine1 > 0
        if (coef(modelledLine1)>0) {
          baseflow1[counter1] <- point1[2]
        }else{ # if the slope of modelledLine1 <= 0
          baseflow1[counter1] <- as.numeric(predict(modelledLine1, newdata = list(x=point1[1])) + eventQ[[tindex]])
        }

        if ( eventQ[[tindex]] - baseflow1[counter1] > 0 ){
          surfflow1[counter1] <- eventQ[[tindex]] - baseflow1[counter1]
        }else{ # if the surfaceflow at a certain time step is negative then force it to be zero
          surfflow1[counter1] <- 0
        }

      }else{ # otherwise, if the time step is related to modelledLine2
        counter2 <- counter2 + 1
        baseflow2[counter2] <- predict(modelledLine2,newdata=list(x=tindex))

        if ( eventQ[[tindex]] - baseflow2[counter2] > 0 ){
          surfflow2[counter2] <- eventQ[[tindex]] - baseflow2[counter2]
        }else{
          surfflow2[counter2] <- 0
        }

      }

    }

    infoQ$surfaceVolume[event] <- sum(surfflow1,surfflow2)
    infoQ$baseflowVolume[event] <- infoQ$Volume[event] - infoQ$surfaceVolume[event]

    if (p == TRUE){
      points(c(point1[1],
               point2[1],
               point3[1]),
             c(point1[2],
               point2[2],
               point3[2]),
             pch=19, col = "green")

      text(c(point1[1],point2[1],point3[1]),
           c(point1[2],point2[2],point3[2]),
           labels=c("point1","point2","point3"),
           cex= 0.7, pos=3)
    }

  }

  return(infoQ)

}
