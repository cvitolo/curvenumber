#' Calculate centroid of time series events
#'
#' @param dataX is a time series
#' @param infoX table containing summary of events for the time series dataX
#' @param useMax boolean, if FALSE (default) the centroid of the event is calculated. If TRUE, the centroid is assumed to correspond to the following maximum. This is generally used with Q time series.
#' @param altStart vector containing the timestamp of alternative starting dates for the events. This is generally used with Q time series and set to the P centroid.
#'
#' @return Updated infoX table containing the time and index of centroid.
#'
#' @examples
#' # newInfoP <- centroid(dataP, infoP)
#' # newInfoQ <- centroid(data$Q, infoQ, useMax = TRUE, altStart = newInfoP$indexCentroid)
#'

centroid <-
function(dataX, infoX, useMax = FALSE, altStart = NULL){

  if (is.null(altStart)){

    startTimes <- which(index(data) %in% infoX$Time)

  }else{

    startTimes <- altStart

  }

  endTimes <- startTimes + infoX$Duration

  if (useMax == FALSE) {

    indexCentroid <- c()

    for (event in 1:dim(infoX)[1]){

      cumX <- cumXT <- 0

      for (timestep in 1:infoX$Duration[event]){

        X <- as.numeric(dataX[startTimes[event] + timestep])
        cumX <- cumX + X

        cumXT <- cumXT + X * timestep

      }

      indexCentroid[event] <- startTimes[event] + round(cumXT/cumX,0)

    }

    infoX$indexCentroid <- indexCentroid
    infoX$timeCentroid <- index(dataX)[indexCentroid]

  }else{

    maxX <- indexCentroid <- c()

    for (event in 1:dim(infoX)[1]){

      if ( endTimes[event] <= length(dataX) ){
        maxX[event] <- max(dataX[startTimes[event]:endTimes[event]])
        indexCentroid[event] <- startTimes[event] - 1 + which(dataX[startTimes[event]:endTimes[event]]==maxX[event])[1]
      }else{
        maxX[event] <- NA
        indexCentroid[event] <- NA
      }

    }

    infoX$Value <- maxX
    infoX$indexCentroid <- indexCentroid
    infoX$timeCentroid <- index(dataX)[indexCentroid]

  }

  return(infoX)

}
