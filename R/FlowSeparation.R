#' Separate Baseflow and Surface runoff
#'
#' @param DATA data.frame containing P, Q (and E)
#' @param eventTable table containing summary of Q events
#' @param stepsBack timesteps to use to generate the linear model for the volume under the rising limb (default = 5)
#' @param timeUnits time step units (default = "hours")
#' @param plotOption boolean, if TRUE (default) it prints a plot to show the division of the events
#' @param event2plot even number for which the plot should be generated (default = 3). This is only used if plotOption = TRUE
#' @param verbose (optional) boolean (FALSE by default). If TRUE prints the progress in terms of event number.
#'
#' @return updated table containing summary of Q events with 4 more columns:
#' volumeQ, baseflowVolume, surfaceVolume and surfacePeak
#'
#' @details From Boorman 1995: The recession prior to the event is continued through the event, and this flow is subtracted from the total flow hydrograph. A straight line is then drawn from beneath the peak flow, or centroid of peaks, to the point already identified as marking the end of response runoff. The response runoff is the portion of flow above this separation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   data("SevernTS")
#'   tableP <- FindPevents(SevernTS$P[1:1000])
#'   tableQ <- FindQevents(SevernTS$Q[1:1000], tableP, hours2extend=6)
#'   df <- FlowSeparation(SevernTS, tableQ, stepsBack=5, timeUnits = "hours",
#'                        plotOption = TRUE, event2plot = 3)
#' }
#'

FlowSeparation <- function(DATA, eventTable,
                           stepsBack=5, timeUnits = "hours", plotOption = FALSE,
                           event2plot = 1, verbose = FALSE){

  # require(udunits2)
  # multiplier <- ud.convert(1,timeUnits,"seconds")
  if (timeUnits == "days")    multiplier <- 24*60*60 # seconds
  if (timeUnits == "hours")   multiplier <- 60*60 # seconds
  if (timeUnits == "minutes") multiplier <- 60 # seconds

  # Add placeholders
  eventTable$baseflowVolume <- NA
  eventTable$surfaceVolume <- NA

  for (event in 1:dim(eventTable)[1]){

    if (verbose == TRUE) {
      print(paste("FlowSeparation-event n.", event,
                  "out of", dim(eventTable)[1]))
    }

    # Define the starting point of the event
    point1 <- c(stepsBack+1, zoo::coredata(DATA$Q[eventTable$indexStart[event]]))

    # Extract Q for the event window + stepsBack
    eventDATA <- window(DATA$Q,
                        start=eventTable$timeStart[event]-stepsBack*multiplier,
                        end=eventTable$timeEndQ[event])

    # Define the points before the event
    x <- 1:(stepsBack + 1)
    y <- coredata(eventDATA)[1:(stepsBack + 1)]
    preEventQ <- structure(list(x = x, y = y), .Names = c("x", "y"))

    # Set a linear model to interpolate the points before the event.
    # modelQ.lm <- lm(formula = y ~ x, data = preEventQ)
    # abline(modelQ.lm, col="red")
    # The model above is not ideal, the line should be forced to go through
    # point1 (first point at the beginning of the event).
    # Therefore, the first line minimises the points in preEventQ and passes through point1
    modelledLine1 <- lm(I(y-point1[2])~I(x-point1[1]) + 0, preEventQ)

    # We extend modelledLine1 to intersect the vertical line passing through the
    # centroid (point2), but if the slope is positive we force it to be = 0
    x2 <- which(substr(index(eventDATA),1,19) %in%
                  eventTable$timeCentroidQ[event])
    if (is.na(coef(modelledLine1)) | coef(modelledLine1)>0) {
      x <- 1:(stepsBack + 1)
      y <- rep(point1[2],length(x))
      preEventQ2 <- structure(list(x = x, y = y), .Names = c("x", "y"))
      modelledLine1 <- lm(I(y-point1[2])~I(x-point1[1]) + 0, preEventQ2)
    }
    y2 <- predict(modelledLine1, newdata = list(x=x2))[[1]] + point1[2]
    point2 <- c(x2,y2)

    # Define the ending point of the event
    point3 <- c(length(eventDATA),coredata(eventDATA[length(eventDATA)]))

    # The second line goes from the centroid to the end of the event
    x = c(point2[1],point3[1])
    y = c(point2[2],point3[2])
    line2 <- structure(list(x = x, y = y), .Names = c("x", "y"))
    modelledLine2 <- lm(y~x, data=line2)

    # Let's separate Baseflow and surfaceflow from the total flow
    baseflow <- c()
    surfflow <- c()
    counter <- 0

    for (tindex in point1[1]:point3[1]){

      # if (p == TRUE) points(tindex,eventDATA[tindex], pch=19, col = ifelse(tindex <= point2[1], "blue", "pink") )

      counter <- counter + 1
      totFlow <- coredata(eventDATA)[tindex]

      if (tindex <= point2[1]){

        # if the time step is related to modelledLine1, get value of the line
        lineValue <- predict(modelledLine1, newdata=list(x=tindex))[[1]] + point1[2]
          #as.numeric(predict(modelledLine1, newdata=list(x=tindex))[[1]])

      }else{

        # if the time step is related to modelledLine2, get value of the line
        lineValue <- predict(modelledLine2,newdata=list(x=tindex))[[1]]

      }

      if (lineValue <= 0) lineValue <- 0

      if ( totFlow - lineValue > 0 ){
        surfflow[counter] <- totFlow - lineValue
      }else{
        # if the surfaceflow at a certain time step is negative then force it
        # to be zero
        surfflow[counter] <- 0
      }
      baseflow[counter] <- totFlow - surfflow[counter]

    }

    eventTable$surfaceVolume[event] <- sum(surfflow)
    eventTable$baseflowVolume[event] <- eventTable$VolumeQ[event] -
      eventTable$surfaceVolume[event]

    if (plotOption == TRUE & event2plot == event) {

      # suppress warnings when plotting
      options(warn=-1)

      # Set margins
      opar <- par()      # make a copy of current settings
      # margin size c(bottom, left, top, right) in lines
      par(mar=c(6,6,6,6))

      # Plot the total flow
      plot(coredata(eventDATA), type="l", ylim=c(0,1.5*max(eventDATA)),
           main = paste("Severn @ Plynlimon flume:\n event occurred between",
                        eventTable$timeStart[event],
                        "and",eventTable$timeEndQ[event]),
           xlab = "Time steps",
           #xlab = "",
           #ylab = "Q [mm/d]",
           ylab = "",
           cex.axis=1, las=1 )

      # Plot Surface/Baseflow
      l <- length(coredata(eventDATA))
      xx <- c((stepsBack+1):l, l:(stepsBack+1))
      # Plot polygon for surfaceflow
      ySurfaceflow <- c(coredata(eventDATA)[(stepsBack+1):l], rev(baseflow))
      polygon(xx, ySurfaceflow, col = "darkgray")
      # Plot polygon for baseflow
      yBaseflow <- c(baseflow, rev(rep(0,length(baseflow))))
      polygon(xx, yBaseflow, col = "lightgray")

      # Plot starting point of the event
      abline(v=point1[1], col='gray', lty=2)

      # Plot the points before the event
      points(preEventQ, pch=1, col = "black")

      # The first line minimises the points in preEventQ and passes through
      # point1. Note the + 0 at the end which indicates to lm that no intercept
      # term should be fit.
      abline(predict(modelledLine1, newdata=list(x=0)) + point1[2],
             coef(modelledLine1), col='red', lty=2)

      # Extend modelledLine1 to intersect the vertical line passing through the
      # centroid (point2), but if the slope is positive we force it to be = 0
      abline(v=x2, col='gray', lty=2)

      # Plot ending point of the event
      abline(v=point3[1], col='gray', lty=2)

      # The second line goes from the centroid to the end of the event
      abline(modelledLine2, col="red", lty=2)

      # Plot Points 1, 2 and 3
      points(c(point1[1],
               point2[1],
               point3[1]),
             c(point1[2],
               point2[2],
               point3[2]),
             pch=19, col = "red")

      # Add text
      text(c(point1[1],point2[1],point3[1]),
           c(point1[2],point2[2],point3[2]),
           labels=c("P1","P2","P3"),
           cex= 0.7, pos=3)

      legend("bottom",
             c("Surface Flow [mm/h]","Base Flow [mm/h]","Precipitation [mm/h]"),
             xpd = TRUE, horiz = TRUE, inset = c(0, 1),
             bty = "n", pch = c(15, 15, 15),
             col = c("darkgray", "lightgray", "darkblue"), cex = 1)
      # xpd = TRUE tells R that it is OK to plot outside the region
      # horiz = TRUE tells R that I want a horizontal legend
      # inset = c(x,y) moves the legend relative to the 'bottom' location
      # bty = 'n' means 'no' box will be drawn around it
      # pch and col are the types and colors of points
      # cex = 1 makes the legend as large as other fonts

      # Add precipitation to the top
      par(bty="n", new=T)
      P <- window(DATA$P,
                  start = eventTable$timeStart[event]-stepsBack*multiplier,
                  end = eventTable$timeEndQ[event])
      plot(P, type="h",
           ylim=rev(range(P)*5), # downward bars
           yaxt="n", xaxt="n", ann=F, # do not plot x and y axis
           # without xlim the two overlayed plots will not fit
           xlim=c(start(P),end(P)),
           lwd=1, col="darkblue" ) # suggested cosmetics

      # add right axis (4) to describe P
      axis(4, pretty(range(P)), col.axis="darkblue", col="darkblue", las=1, cex.axis=0.7 )
      #axis(4, pretty(range(P)), las=1, cex.axis=1 )
      #mtext("P [mm/d]", side = 4, line = 3, cex = par("cex.lab"))
      # reset border and overlay
      par(bty="o", new=F)
      par(opar)          # restore original settings
    }

  }

  return(eventTable)

}
