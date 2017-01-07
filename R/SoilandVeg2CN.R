#' Soil and Vegetation code to CN, and corrects it based on drainage level, if needed.
#'
#' @param vegetationCODE vegetation class code (e.g. for the UK vegetation map 2013 this is an integer in the range [1,10]).
#' @param soilCODE soil class code (e.g. for the HOST system this is an integer in the range [1,29]).
#' @param lookupTable this is a data.frame to link Soil, Vegetation and Hydrological conditions.
#' @param artificialDrainage (default = "none"), possible values are none, low, medium,high.
#'
#' @return Curve Number for a single raster cell, this is an integer in the range [0,100] with 0 = no runoff and 100 = all rainfall becomes runoff.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load lookup table
#' dfLookup <- MakeLoopkupTable("Severn&Wye")
#' cnLocal <- SoilandVeg2CN(vegetationCODE = 6,
#'                          soilCODE = 15,
#'                          lookupTable = dfLookup,
#'                          artificialDrainage = "none")
#' }
#'

SoilandVeg2CN <- function(vegetationCODE, soilCODE,
                          lookupTable, artificialDrainage="none"){

  if (vegetationCODE %in% 1:10){

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

  }else{

    CN <- NA

  }

  return(CN)

}
