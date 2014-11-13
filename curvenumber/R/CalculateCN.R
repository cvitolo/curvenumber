#' Calculate the Curve Number from P and Q
#'
#' @param dfTPQ data.frame containing 3 columns: Tr (return period), P (max precipitation) and Q (max discharge)
#' @param PQunits units in which P and Q are expressed (default="mm")
#'
#' @return Curve Number, in the range [0,100]
#'

CalculateCN <- function(dfTPQ, PQunits = "mm"){

  # where P & Q are in inches and area is in acre
  # Q <- dfTPQ$Q/25.4
  # P <- dfTPQ$P/25.5
  # area <- 4.8*247.105 # area <- DataList$Area*247.105

  P <- dfTPQ$P
  Q <- dfTPQ$Q

  if (length(P)!=length(Q)) stop

  S <- 5*P + 10*Q - sqrt( (5*P+10*Q)^2 -25*(P^2-P*Q) ) # Nataliya
  #S <- 5 * ( P + 2*Q - sqrt(4*Q^2 + 5*P*Q) )             # Hawkins

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

  dfTPQ$CN <- CN

  return(dfTPQ)

}
