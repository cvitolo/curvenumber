#' Calculate the direct storm runoff (Q) from P and CN
#'
#' @param P precipitation
#' @param CN curve number, in the range [0,100]
#' @param PQunits units in which P and Q are expressed (default="mm")
#'
#' @return Q, direct storm runoff.
#'

CalculateQ <- function(P, CN, PQunits = "mm"){

  if (PQunits=="mm"){
    S <- 25400/CN - 254
  }

  if (PQunits=="inches"){
    S <- 1000/CN - 10
  }

  if (P > 0.2*S) {
    Q <- ((P - 0.2*S)^2) / (P + 0.8*S)
  }else{
    Q <- 0
  }

  return(list("S"=round(S,3), "Q"=round(Q,3)))
}
