#' Calculate return periods for PQ events
#'
#' @param infoP table containing summary of P events
#' @param infoQ table containing summary of Q events
#' @param plotOption boolean, if TRUE (default) it prints a plot to show the trend of the return period
#' @param variable2plot it can be P or Q (default = Q)
#'
#' @return data.frame containing 3 columns: Tr (return period), P (max precipitation) and Q (max discharge)
#'
#' @examples
#' # ReturnPeriod(infoP,infoQ)
#'

ReturnPeriod <- function(infoP, infoQ, plotOption=TRUE, variable2plot = "Q"){

  if ( dim(infoP)[1] == dim(infoQ)[1] ){

    P <- infoP$Value
    Q <- infoQ$surfacePeak

    # Order events in descending order and calculate matching return periods
    eventsP <- sort(P, decreasing = TRUE)
    returnPeriodP <- (length(eventsP)-1)/(1:length(eventsP))
    dP <- data.frame(x=returnPeriodP, y=eventsP)
    # loglogplot(dP)

    eventsQ <- sort(Q, decreasing = TRUE)
    returnPeriodQ <- (length(eventsQ)-1)/(1:length(eventsQ))
    dQ <- data.frame(x=returnPeriodQ, y=eventsQ)
    # loglogplot(dQ)

    # Define some return periods
    Tr <- returnPeriodP
    df <- data.frame("Tr"=Tr,"P"=dP$y,"Q"=dQ$y)

    if (plotOption == TRUE & variable2plot == "P") plot(df$Tr, df$P, type="o")
    if (plotOption == TRUE & variable2plot == "Q") plot(df$Tr, df$Q, type="o")

  }else{

    P <- infoP$Value
    Q <- infoQ$Value

    # Order events in descending order and calculate matching return periods
    eventsP <- sort(P, decreasing = TRUE)
    returnPeriodP <- (length(eventsP)-1)/(1:length(eventsP))
    dP <- data.frame(x=returnPeriodP, y=eventsP)
    # loglogplot(dP)

    eventsQ <- sort(Q, decreasing = TRUE)
    returnPeriodQ <- (length(eventsQ)-1)/(1:length(eventsQ))
    dQ <- data.frame(x=returnPeriodQ, y=eventsQ)
    # loglogplot(dQ)

    Tr <- 2:round(max(returnPeriodP,returnPeriodQ),0)

    # If the events were identified independently,
    # then we would need to calculate the regression function to model dP and dQ
    # Calculate the regression function that describes dP
    # option1: modelP.lm <- lm(formula = y ~ x + I(x^2), data = dP)
    # option2:
    modelP.lm <- lm(formula = y ~ log(x), data = dP)
    # Calculate the regression function that describes dQ
    # option1: modelQ.lm <- lm(formula = y ~ x + I(x^2), data = dQ)
    # option2:
    modelQ.lm <- lm(formula = y ~ log(x), data = dQ)

    # Use predict to estimate the values for the return period.
    # Note that predict expects a data.frame and the col names need to match
    newP <- predict(modelP.lm, newdata = data.frame(x = Tr))
    newQ <- predict(modelQ.lm, newdata = data.frame(x = Tr))

    df <- data.frame("Tr"=Tr,"P"=newP,"Q"=newQ)
    df <- df[order(df$Tr, decreasing=TRUE), ]
    row.names(df) <- NULL

    if (plotOption==TRUE & variable2plot == "P"){

      plot(dP$x, dP$y, type="o", ylim=c(min(dP$y,newP),max(dP$y,newP)))
      points(Tr, newP, col = "red")

    }

    if (plotOption==TRUE & variable2plot == "Q"){

      plot(dQ$x, dQ$y, type="o", ylim=c(min(dQ$y,newQ),max(dQ$y,newQ)))
      points(Tr, newQ, col = "red")

    }

  }

  return(df)

}
