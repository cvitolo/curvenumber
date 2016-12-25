#' Calculates Base Flow Index
#'
#' @param Qflow time series of streamflow discharge
#' @param timestep time step for the measured data (equally spaced)
#'
#' @details Calculates Base Flow Index for a given discharge time series as described in Report # 108, Low flow estimation in the United Kingdom, by A. Gustard, A. Bullock, and J.M. Dickson, 1992
#'
#' @return Curve Number, in the range [0,100]
#' @author Nataliya Bulygina (July, 2008)
#'
#' @examples
#' # BaseFlowIndex(Q)

BaseFlowIndex <- function(Qflow, timestep=1/24){

  # require(zoo)
  # require(pracma)
  # require(xts)

  # aggregate the time series to daily data
  Qdaily <- apply.daily(na.approx(Qflow),sum)
  Qdaily <- as.numeric(Qdaily)

  # http://www.ceh.ac.uk/products/publications/documents/ih108_low_flow_estimation.pdf

  # The program calculates the minima of five-day non-overlapping consecutive
  # periods and subsequently searches for turning points in this sequence of
  # minima. The turning points are then connected to obtain the base flow
  # hydrograph which is constrained to equal the observed hydrograph ordinate on
  # any day when the separated hydrograph exceeds the observed. The procedure
  # for calculating the index is as follows:

  # 1. Divide the mean daily flow data into non-overlapping blocks of five days
  # and calculate the minima for each of these blocks, and let them be called
  # Q1, Q2, Q3, ..., Qn.

  # Calculate 5-day non-overlapping period minima
  lenQ5 <- floor(length(Qdaily)/5)
  Q5min <- rep(NA,lenQ5)
  for (t in 1:lenQ5){
    Q5min[t] <- min(Qdaily[(5*(t-1)+1):(5*t)])
  }

  # 2. Consider in turn (Q,, Q2, Q3,), (Q2, Q3, Q'), ... (Q 1, Q;, Q+,) etc.
  # In each case, if 0.9 x central value < outer values, then the central value
  # is an ordinate for the baseflow line. Continue this procedure until all the
  # data have been analysed to provide a derived set of baseflow ordinates
  # QB1, QB2, QB3,...QBn. which will have different time periods between them.

  # Find baseflow times
  l <- 0
  Baseraw <- Timeraw <- c()
  for (t in 1:(length(Q5min)-2)){

    if (0.9*Q5min[t+1] < min(Q5min[t],Q5min[t+2])){
      l <- l + 1
      Baseraw[l] <- Q5min[t+1]
      Timeraw[l] <- t*5 + 2.5 # corresponding time
    }

  }

  # 3. By linear interpolation between each QB value, estimate each daily value
  # of QB1, ... QBn,

  # Interpolate baseflow to daily steps
  Basedaily <- interp1(Timeraw, Baseraw,
                       seq(min(Timeraw), max(Timeraw),
                           len = length(5:(length(Qdaily)-5))),
                       method = "linear")

  # 4. If QBi > Q; then set QBi = Q2
  # Correct baseflow interpolated values: they do not exceed measured values
  Basedaily2 <- pmin(Basedaily, Qdaily[5:(length(Qdaily)-5)])

  # 5. Calculate VB the volume beneath the baseflow line between the first and
  # last baseflow turning points QB,... QB,
  #calculate baseflow volume, Vb
  Vb <- sum(Basedaily)

  # 6. Calculate total volume Vt the volume beneath the recorded mean daily
  # flows Q for the period QB,... QB.
  Vt <- sum(Qdaily[5:(length(Qdaily)-5)])

  # 7. The Base Flow Index is then Vb/Vt.
  BFI <- round(Vb/Vt,2)

  return(BFI)

}
