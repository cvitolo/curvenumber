#' Separate Baseflow and Surface runoff
#'
#' @param data data.frame containing P, Q (and E)
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
#' # FlowSeparation(data, tableQ, stepsBack=5, timeUnits = "hours", plotOption = TRUE, event2plot = 3)
#'

FlowSeparation <- function(data, infoQ, stepsBack=5, timeUnits = "hours", plotOption = TRUE, event2plot = 3){

  infoQ$volumeQ <- infoQ$baseflowVolume <- infoQ$surfaceVolume <- infoQ$surfacePeak <- NA

  for (r in 1:dim(infoQ)[1]){

    eventLength <- as.numeric(difftime(infoQ$EndTime[r],
                                       infoQ$Time[r],
                                       units = timeUnits))

    P <- window(data$P, start=infoQ$Time[r], end=infoQ$EndTime[r])

    if ( !is.null(data$E) ){
      E <- window(data$E, start=infoQ$Time[r], end=infoQ$EndTime[r])
    }else{
      E <- rep(NA,eventLength)
    }

    Q <- window(data$Q, start=infoQ$Time[r], end=infoQ$EndTime[r])

    multiplier <- ud.convert(1,timeUnits,"seconds")

    dQ <- window(data$Q, start=infoQ$Time[r]-stepsBack*multiplier,
                 end=infoQ$Time[r])

    dQLength <- length(dQ)-1
    test <- structure(list(x = 1:dQLength,
                           y = dQ[1:dQLength]),
                      .Names = c("x", "y"))

    # Set a linear model to interpolate the point before the event.
    # The line should be force to go through the first point at the beginning
    # of the event. Note the + 0 at the end which indicates to lm that no
    # intercept term should be fit. For a general version use:
    # modelQ.lm <- lm(formula = y ~ x, data = dQ)

    modQ1 <- lm(I(y-coredata(dQ[length(dQ)]))~I(x-length(dQ)) +0, test)
    lineQ1 <- predict(modQ1, newdata = list(x=0)) + coredata(dQ[length(dQ)])

    lineQ2 <- function(x0,x1,y0,y1,x){

      y <- y0 + (x-x0)*(y1-y0)/(x1-x0)

      return(y)

    }

    eventQ <- coredata(window(data$Q, start=infoQ$Time[r]-stepsBack*multiplier,
                              end=infoQ$EndTime[r]))

    point1 <- c(length(dQ),coredata(dQ[length(dQ)]))
    point2 <- c(which(index(Q)==infoQ$timeCentroid[r])+dQLength,
                as.numeric(predict(modQ1, newdata = list(x=which(index(Q)==infoQ$timeCentroid[r])+dQLength)) + point1[2]))
    point3 <- c(length(eventQ),coredata(Q[length(Q)]))

    infoQ$volumeQ[r] <- sum(coredata(eventQ))

    volume1 <- (point2[2]+point1[2])*(point2[1]-point1[1])/2
    volume2 <- (point3[2]+point2[2])*(point3[1]-point2[1])/2

    infoQ$baseflowVolume[r] <- volume1 + volume2
    infoQ$surfaceVolume[r] <- infoQ$volumeQ[r] - (volume1 + volume2)
    infoQ$surfacePeak[r] <- infoQ$Value[r] - point2[2]

    if (plotOption == TRUE & event2plot == r){

      # set up a plot
      plot(eventQ,pch=19,xlim=c(1,point3[1]),ylim=c(0,max(eventQ)),
           panel.first=abline(h=point1[2],
                              v=c(point1[1],
                                  point2[1],
                                  point3[1]),
                              lty=3,col="gray"))
      abline(lineQ1, coef(modQ1), col='red')
      lines(c(point2[1],
              point3[1]),
            c(as.numeric(predict(modQ1, newdata = list(x=point2[1])) + point1[2]),
              coredata(Q[length(Q)])), col='red')
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

    }

  }

  return(infoQ)

}
