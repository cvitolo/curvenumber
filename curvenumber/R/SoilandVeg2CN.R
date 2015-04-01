#' Soil and Vegetation code to CN, and corrects it based on drainage level, if needed.
#'
#' @param soilCODE HOST soil class (integer in the range [1,29])
#' @param vegetationCODE integer in the range [1,10]
#' @param lookupTable this is a data.frame to link Soil, Vegetation and Hydrological conditions.
#' @param artificialDrainage (default = "none"), possible values are none, low, medium,high.
#'
#' @return CN, integer in the range [1,100]
#'
#' @examples
#' # SoilandVeg2CN(15,6,lookupTable,artificialDrainage="none")
#'

SoilandVeg2CN <- function(soilCODE,vegetationCODE,lookupTable,artificialDrainage){

  usda <- HOST2USDA(soilCODE)

  A <- lookupTable$SoilA[lookupTable$Code==vegetationCODE]
  B <- lookupTable$SoilB[lookupTable$Code==vegetationCODE]
  C <- lookupTable$SoilC[lookupTable$Code==vegetationCODE]
  D <- lookupTable$SoilD[lookupTable$Code==vegetationCODE]
  AB <- round(sum(A,B)/2,0)
  BC <- round(sum(B,C)/2,0)
  CD <- round(sum(C,D)/2,0)

  df <- data.frame("usda"=c("A","AB","B","BC","C","CD","D"),
                   "usdaNumber"=1:7)

  usdaNumber <- which(df$usda==usda)

  if (artificialDrainage != "none"){

    if (artificialDrainage == "low") {
      USDAnumberDrainage <- usdaNumber - 1
    }
    if (artificialDrainage == "medium") {
      USDAnumberDrainage <- usdaNumber - 2
    }
    if (artificialDrainage == "high") {
      USDAnumberDrainage <- usdaNumber - 3
    }

    # Correct CN for drained soils
    USDAnumberDrainage <- max(USDAnumberDrainage,1)
    usda <- as.character(df$usda[df$usdaNumber == USDAnumberDrainage])

  }

  CN <- eval(parse(text=as.character(usda)))

  return(CN)

}
