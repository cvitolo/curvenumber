#' Calculate the Curve Number from P and Q
#'
#' @param descendingDF data.frame containing 3 columns: Tr (return period), P (max precipitation) and Q (max discharge)
#' @param PQunits units in which P and Q are expressed (default="mm")
#'
#' @return Curve Number, in the range [0,100]
#'

CalculateCN <- function(descendingDF, PQunits = "mm"){

  # where P & Q are in inches and area is in acre
  # Q <- descendingDF$Q/25.4
  # P <- descendingDF$P/25.5
  # area <- 4.8*247.105 # area <- DataList$Area*247.105

  P <- descendingDF$P
  Q <- descendingDF$Q

  if (length(P)!=length(Q)) stop

  S <- 5*P + 10*Q - sqrt( (5*P+10*Q)^2 -25*(P^2-P*Q) )

  if ( all(P>0.2*S) ){
    message("OK, P is always >= 0.2 S")
  }else{
    message("Caution, P is not always >= 0.2 S (the corresponding Q should be 0 according to Hawkins (1993)")
  }

  # Curve Number, in the range [0,100].
  # 0 = no runoff, 100 = all rainfall becomes runoff

  if (PQunits=="mm"){
    CN <- 25400/(254 + S)
  }

  if (PQunits=="inches"){
    CN <- 1000/(S + 10)
  }

  descendingDF$CN <- CN

  return(descendingDF)

}
