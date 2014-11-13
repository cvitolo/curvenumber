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

  multiplier <- ud.convert(1,timeUnits,"seconds")

  infoQ$baseflowVolume <- NA
  infoQ$surfaceVolume <- NA

  for (event in 1:dim(infoQ)[1]){

    # Extract Q for the event window + stepsBack
    eventQ <- window(dataX$Q,
                     start=infoQ$timeStart[event]-stepsBack*multiplier,
                     end=infoQ$timeEnd[event])

    # Define the starting point of the event
    point1 <- c(stepsBack+1,coredata(dataX$Q[infoQ$indexStart[event]]))

    # Define the point under the centroid of the event
    x = 1:(stepsBack + 1)
    y = window(dataX$Q,
               start=infoQ$timeStart[event]-(stepsBack)*multiplier,
               end=infoQ$timeStart[event])[1:(stepsBack + 1)]
    preeventQ <- structure(list(x = x, y = y), .Names = c("x", "y"))
    # Set a linear model to interpolate the point before the event.
    # The line should be force to go through the first point at the beginning
    # of the event. Note the + 0 at the end which indicates to lm that no
    # intercept term should be fit. For a general version use:
    # modelQ.lm <- lm(formula = y ~ x, data = preeventQ)
    modQ1 <- lm(I(y-point1[2])~I(x-point1[1]) + 0, preeventQ)
    lineQ1 <- predict(modQ1, newdata = list(x=0)) + coredata(y[length(y)])

    x2 <- which(index(eventQ) %in% infoQ$timeCentroid[event])

    if ( as.numeric(coef(modQ1))>0 ){
      y2 <- point1[2]
    }else{
      y2 <- predict(modQ1, newdata = list(x=x2)) + point1[2]
    }

    point2 <- c(x2, as.numeric(y2))

    # Define the ending point of the event
    point3 <- c(length(eventQ),coredata(eventQ[length(eventQ)]))

    volume1 <- (point2[2]+point1[2])*(point2[1]-point1[1])/2
    volume2 <- (point3[2]+point2[2])*(point3[1]-point2[1])/2

    infoQ$baseflowVolume[event] <- volume1 + volume2
    infoQ$surfaceVolume[event] <- infoQ$Volume[event] - (volume1 + volume2)

    if (plotOption == TRUE & event2plot == event){

      # set up a plot
      plot(coredata(eventQ), pch=19, xlim=c(1,point3[1]), type="o",
           ylim=c(min(point2[2],min(eventQ)),max(point2[2],max(eventQ))),
           panel.first=abline(h=point1[2],
                              v=c(point1[1],
                                  point2[1],
                                  point3[1]),
                              lty=3,col="gray"))

      points(c(point1[1],
               point2[1],
               point3[1]),
             c(point1[2],
               point2[2],
               point3[2]),
             pch=19, col = "red")
      text(c(point1[1],point2[1],point3[1]),
           c(point1[2],point2[2],point3[2]),
           labels=c("point1","point2","point3"),
           cex= 0.7, pos=3)

      if ( as.numeric(coef(modQ1))>0 ){
        lines(c(point1[1],
                point2[1]),
              c(point1[2],
                point2[2]), col='red')
      }else{
        abline(lineQ1, coef(modQ1), col='red')
      }

      lines(c(point2[1],
              point3[1]),
            c(point2[2],
              point3[2]), col='red')


    }

  }

  return(infoQ)

}
