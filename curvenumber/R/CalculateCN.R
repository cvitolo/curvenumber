#' Calculate the Curve Number from P and Q
#'
#' @param dfTPQ data.frame containing 3 columns: Tr (return period), P (max precipitation) and Q (max discharge)
#' @param PQunits units in which P and Q are expressed (default="mm")
#' @param plotOption boolean, if TRUE (default) it prints a plot to show the division of the events
#'
#' @return Curve Number, in the range [0,100]
#'

CalculateCN <- function(dfTPQ, PQunits = "mm", plotOption = FALSE){

  # where P & Q are in inches and area is in acre
  # Q <- dfTPQ$Q/25.4
  # P <- dfTPQ$P/25.5
  # area <- 4.8*247.105 # area <- DataList$Area*247.105

  P <- dfTPQ$P
  Q <- dfTPQ$Q

  # S <- 5*P + 10*Q - sqrt( (5*P+10*Q)^2 -25*(P^2-P*Q) ) # Nataliya
  S <- 5 * ( P + 2*Q - sqrt(4*Q^2 + 5*P*Q) )          # Hawkins

  if ( all(P>=0.2*S) ){
    message("OK, P is always >= 0.2 S")
  }else{
    message("Caution, P is not always >= 0.2 S (the corresponding Q should be 0 according to Hawkins (1993)")
    rows2remove <- which(P<0.2*S)
    dfTPQ <- dfTPQ[-rows2remove,]
    numberOfEvents <- dim(dfTPQ)[1]
    dfTPQ <- dfTPQ[with(dfTPQ, order(Q)), ]
    P <- dfTPQ$P
    Q <- dfTPQ$Q
    Tr <- (numberOfEvents-1)/(1:numberOfEvents)
    dfTPQ <- data.frame("Tr"=Tr,"P"=P,"Q"=Q)
    S <- 5 * ( P + 2*Q - sqrt(4*Q^2 + 5*P*Q) )
  }

  # Curve Number, in the range [0,100].
  # 0 = no runoff, 100 = all rainfall becomes runoff

  if (PQunits=="mm"){
    CN <- 25400/(254 + S)
  }

  if (PQunits=="inches"){
    CN <- 1000/(S + 10)
  }

  P <- dfTPQ$P

  # Plot CN-P behaviour to define the type of asymptote
  if (plotOption == TRUE) {
    plot(CN~P, xlab="Rainfall", ylab="Runoff CN", ylim=c(min(CN),100))
  }

  # There are three possible types of behaviour:
  # "standard" (increasing asymptotically),
  # "complacent" (decreasing indefinitely) and
  # "violent" (increasing asymptotically)
  # The only behaviour implemented here is the "standard".

  # Determine parameters first guess
  CN0 <- median( sort(CN, decreasing = FALSE)[1:5] )
  k=1 # k=0.003

  # Define non linear function
  f <- function(P,CN0,k) {CN0 + (100 - CN0) * exp(-k*P)}

  # compute reasonable starting values
  st <- coef(nls(log(CN) ~ log(f(P,CN0,k)), start = c(CN0 = CN0, k = 1)))

  # nonlinear least squares curve fiiting
  # fit <- nls(CN ~ CN0 + (100 - CN0) * exp(-k*P), start=list(CN0=CN0,k=k))
  fit <- nls(CN ~ f(P,CN0,k), start = st)
  # The variable CN is independent from P and it's the value that describes the
  # data set for larger rainfall events.

  # summary(fit)

  # Draw the fit on the plot by getting the prediction from the fit at 200 x-coordinates across the range of P
  if (plotOption == TRUE) {
    new = data.frame(P = seq(min(P),max(P),len=200))
    lines(new$P,predict(fit,newdata=new), col="red")
  }

  # Getting the sum of squared residuals:
  # sum(resid(fit)^2)

  # Finally, lets get the parameter confidence intervals.
  # confint(fit)

  message(paste("Curve Number:",round(coefficients(fit)[[1]],0)))

  return(round(coefficients(fit)[[1]],0))
}
