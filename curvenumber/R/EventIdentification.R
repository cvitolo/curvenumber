#' Identify PQ events
#'
#' @param data is a data.frame containing P, Q (and E) time series
#' @param PQindependent boolean. If TRUE the events P and Q are considered independent and the matching return periods are calculated using the frequency matching approach. If FALSE (default) the events P and Q are dependent and they have the same return period
#' @param plotOption boolean, if TRUE (default) it prints a plot to show the division of the events
#' @param variable2plot is a variable to plot
#'
#' @return table containing Return period and matching P and Q max values
#'
#' @examples
#' # df <- EventIdentification(data)
#'

EventIdentification <- function(data, PQindependent=FALSE,
                                plotOption = TRUE, variable2plot="Q"){

  if (PQindependent == TRUE){
    # Apply Frequency Matching approach

    ### STEP 1 ###
    # Separate baseflow from runoff
    bfs <- zoo(BaseflowSeparation(coredata(data$Q))[,"bt"],
               order.by=index(data))

    ## You can check out how this looks with the hydrograph function:
    # hydrograph(input=data.frame("datetime"=index(data),
    #                             "P"=coredata(data$P),
    #                             "Q"=coredata(data$Q),
    #                             "Qbase"=coredata(bfs[,1])))

    # Use quick flow only (runoff - baseflow)
    qfl <- zoo(BaseflowSeparation(coredata(data$Q))[,"qft"],
               order.by=index(data))

    # set up a warmup period
    percentageWarmUp <- 10
    warmup <- round(dim(data)[1]/percentageWarmUp,0)
    pperiod <- (warmup+1):dim(data)[1]

    bfs <- bfs[pperiod]
    qfl <- qfl[pperiod]
    dataNew <- window(data,
                      start=index(data)[warmup+1],
                      end=index(data)[dim(data)[1]])
    dataNew$Q <- qfl
    data <- data.frame("datetime"=index(data)[pperiod],
                       "P"=data[pperiod,"P"],
                       "Q"=qfl)

    ## You can check out how this looks with the hydrograph function:
    # hydrograph(input=data.frame("datetime"=index(dataNew),
    #                             "P"=coredata(dataNew$P),
    #                             "Q"=coredata(dataNew$Q)))

    ### STEP 2 ###
    # Identify discrete RR events from time series using hydromad::eventseq.
    # This function returns a zoo object, with core data consisting of an
    # ordered factor, representing the identified events, and the same time
    # index as x. Periods between events are left as NA, unless all = TRUE in
    # which case they are treated as separate events. The returned object stores
    # thresh as an attribute. Typical input parameters are:
    # x        = P and Q(quick) time series
    # thresh   = threshold value
    # inthresh = second threshold to define when events stop
    # mindur   = the minimum number of time steps in each event window
    # indur    = P must remain below inthresh for indur time steps in order to terminate an event
    # mingap   = the the minimum number of time steps that can separate events
    evp <- eventseq(dataNew$P, thresh = 0.1, inthresh = 0, indur = 2, mingap = 6)
    evq <- eventseq(dataNew$Q, thresh = median(qfl), inthresh = median(qfl),
                    indur = 2, mingap = 6)

    ## You can check out the structure of the evp object
    # str(evp)
    #
    ## Plot
    # xyplot(dataNew) +
    #   layer_(panel.xblocks(evp, col = c("grey90", "grey80"), border = "grey80")) +
    #   layer(panel.xblocks(evq, block.y = 0, vjust = 1, col = 1))

    newTableP <- eventinfo(dataNew$P, evp, FUN = max)
    newTableQ <- eventinfo(dataNew$Q, evq, FUN = max)

    # Calculate return periods for PQ events
    df <- ReturnPeriod(infoP=newTableP, infoQ=newTableQ,
                       plotOption=TRUE, variable2plot = "Q")

  }

  if (PQindependent == FALSE){
    # Apply Nataliya's approach

    tableP <- findPevents(data$P, plotOption = FALSE)
    tableQ <- findQevents(data$Q,tableP, plotOption = FALSE)

    # adjust end of Q events according to 4*LAG?
    # if so, what shall we do for LAG = 0?
    # LagPQ <- lagtime(tableP, tableQ, timeUnits = "hours")
    # abline(v=index(data)[tableQ$indexCentroid + 4*LagPQ],col="green")

    if ( any(is.na(tableQ$Value)) ){

      row2remove <- which(is.na(tableQ$Value))

      tableP <- tableP[-row2remove,]
      tableQ <- tableQ[-row2remove,]

    }

    temp <- FlowSeparation(data, tableQ,
                           stepsBack=5, timeUnits = "hours",
                           plotOption = TRUE, event2plot = 3)

    newTableP <- tableP[-which(temp$surfacePeak<=0 | is.na(temp$surfacePeak) ),]
    newTableQ <- temp[-which(temp$surfacePeak<=0 | is.na(temp$surfacePeak) ),]

    # Calculate return periods for PQ events
    df <- ReturnPeriod(newTableP, newTableQ, plotOption, variable2plot)

  }

  return(df)

}
