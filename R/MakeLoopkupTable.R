#' Generate a lookup table for soil, vegetation and hydrological properties
#'
#' @param case string of characters. It can be one of the following options: "default", "Severn&Wye", "Good woods + Good pasture", "Good woods + Fair pasture", "Fair woods + Poor pasture".
#'
#' @return data.frame containing 10 rows (vegetation classes) and the following columns: Class (vegetation), Subclass (vegetation), Code, SoilA, SoilB, SoilC, SoilD.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   dfLookup <- MakeLoopkupTable()
#'   dfLookupSW <- MakeLoopkupTable("Severn&Wye")
#'   dfLookupGWGP <- MakeLoopkupTable("Good woods + Good pasture")
#'   dfLookupGWFP <- MakeLoopkupTable("Good woods + Fair pasture")
#'   dfLookupFWPP <- MakeLoopkupTable("Fair woods + Poor pasture")
#' }
#'

MakeLoopkupTable <- function(case = "default"){

  PlynlimonVegSoilTable <- NULL
  # This is not pre-imported as LazyData = FALSE in DESCRIPTION file
  load(system.file(file.path("data", "PlynlimonVegSoilTable.rda"),
                   package = "curvenumber"))

  dfLookup <- PlynlimonVegSoilTable[,c("Class","SubClass","VEG2013SubClass",
                                       "SoilA","SoilB","SoilC","SoilD")]
  names(dfLookup) <- c("Class","SubClass","Code",
                       "SoilA","SoilB","SoilC","SoilD")
  dfLookup <- dfLookup[!duplicated(dfLookup[,3:7]), ]
  dfLookup <- dfLookup[!is.na(dfLookup$Code), ]
  dfLookup <- dfLookup[order(dfLookup$Code), ]
  row.names(dfLookup) <- NULL

  # Modify the lookup table above, if needed.
  # Adjustments for the Severn&Wye catchments
  if (case == "Severn&Wye"){
    dfLookup[1,4:7] <- c(36,60,73,79)   # Coniferous Woodland/Conifer = Fair, woods
    dfLookup[2,4:7] <- c(36,60,73,79)   # Coniferous Woodland/Recent (<10yrs) = Fair, woods
    dfLookup[3,4:7] <- c(45,66,77,83)   # Coniferous Woodland/Felled = Fair, woods
    dfLookup[4,4:7] <- c(68,79,86,89)   # Acid Grassland/Acid = Poor, pasture
    dfLookup[5,4:7] <- c(68,79,86,89)   # Acid Grassland/Bracken = Poor, pasture
    dfLookup[6,4:7] <- c(45,66,77,83)   # Heather Burnt/heather = Poor, wood
    dfLookup[7,4:7] <- c(68,79,86,89)   # Improved Grassland/Improved grassland = Poor, pasture
    dfLookup[8,4:7] <- c(77,86,91,94)   # Inland Rock/Inland rock = Fallow, bare soil
    dfLookup[9,4:7] <- c(36,60,73,79)   # Broadleaved woodland/Deciduous = Fair, woods
    dfLookup[10,4:7] <- c(100,100,100,100)  # Freshwater Water/flooded = Open water

  }

  # Modify the lookup table above, if needed.
  # Adjustments for the Severn&Wye catchments
  if (case == "Severn&Wye_old"){
    dfLookup[1,4:7] <- c(30,55,70,77)   # Coniferous Woodland/Conifer = Good, woods
    dfLookup[2,4:7] <- c(30,55,70,77)   # Coniferous Woodland/Recent (<10yrs) = Good, woods
    dfLookup[3,4:7] <- c(30,55,70,77)   # Coniferous Woodland/Felled = Good, woods
    dfLookup[4,4:7] <- c(49,69,79,84)   # Acid Grassland/Acid = Fair, pasture
    dfLookup[5,4:7] <- c(35,56,70,77)   # Acid Grassland/Bracken = Fair, brush
    dfLookup[6,4:7] <- c(35,56,70,77)   # Heather Burnt/heather = Fair, brush
    dfLookup[7,4:7] <- c(49,69,79,84)   # Improved Grassland/Improved grassland = Fair, pasture
    dfLookup[8,4:7] <- c(77,86,91,94)   # Inland Rock/Inland rock = Fallow, bare soil
    dfLookup[9,4:7] <- c(30,55,70,77)   # Broadleaved woodland/Deciduous = Good, woods
    dfLookup[10,4:7] <- c(100,100,100,100)  # Freshwater Water/flooded = Open water

  }

  # ALTERNATIVELY, according to Bulygina et al. 2011:
  # Severn should have Forest + Pasture in good condition
  # Wye should have Pasture in fair condition
  # The rest is the same.

  ### SEVERN (Good woods + Good pasture)
  if (case == "Good woods + Good pasture"){
    dfLookup[1,4:7] <- c(30,55,70,77)       # Good, woods
    dfLookup[2,4:7] <- c(30,55,70,77)       # Good, woods
    dfLookup[3,4:7] <- c(30,55,70,77)       # Good, woods
    dfLookup[4,4:7] <- c(39,61,74,80)       # Good, pasture
    dfLookup[5,4:7] <- c(35,56,70,77)       # Fair, brush
    dfLookup[6,4:7] <- c(35,56,70,77)       # Fair, brush
    dfLookup[7,4:7] <- c(39,61,74,80)       # Good, pasture
    dfLookup[8,4:7] <- c(77,86,91,94)       # Fallow, bare soil
    dfLookup[9,4:7] <- c(30,55,70,77)       # Good, woods
    dfLookup[10,4:7] <- c(100,100,100,100)  # Open water
  }

  ### WYE (Good woods + Fair pasture)
  if (case == "Good woods + Fair pasture"){
    dfLookup[1,4:7] <- c(30,55,70,77)       # Good, woods
    dfLookup[2,4:7] <- c(30,55,70,77)       # Good, woods
    dfLookup[3,4:7] <- c(30,55,70,77)       # Good, woods
    dfLookup[4,4:7] <- c(49,69,79,84)       # Fair, pasture
    dfLookup[5,4:7] <- c(35,56,70,77)       # Fair, brush
    dfLookup[6,4:7] <- c(35,56,70,77)       # Fair, brush
    dfLookup[7,4:7] <- c(49,69,79,84)       # Fair, pasture
    dfLookup[8,4:7] <- c(77,86,91,94)       # Fallow, bare soil
    dfLookup[9,4:7] <- c(30,55,70,77)       # Good, woods
    dfLookup[10,4:7] <- c(100,100,100,100)  # Open water
  }

  ### SEVERN/WYE (Fair woods + Poor pasture)
  if (case == "Fair woods + Poor pasture"){
    dfLookup[1,4:7] <- c(36,60,73,79)       # Fair woods
    dfLookup[2,4:7] <- c(36,60,73,79)       # Fair woods
    dfLookup[3,4:7] <- c(36,60,73,79)       # Fair woods
    dfLookup[4,4:7] <- c(68,79,86,89)       # Poor, pasture
    dfLookup[5,4:7] <- c(35,56,70,77)       # Fair, brush
    dfLookup[6,4:7] <- c(35,56,70,77)       # Fair, brush
    dfLookup[7,4:7] <- c(68,79,86,89)       # Poor, pasture
    dfLookup[8,4:7] <- c(77,86,91,94)       # Fallow, bare soil
    dfLookup[9,4:7] <- c(36,60,73,79)       # Fair woods
    dfLookup[10,4:7] <- c(100,100,100,100)  # Open water
  }

  return(dfLookup)

}
