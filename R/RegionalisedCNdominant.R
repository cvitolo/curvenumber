#' Calculate Curve Number for soil dominant classes (HOST) and vegetation (LCM) maps
#'
#' @param soil filename of a raster containing the dominant soil classes (HOST).
#' @param catchment SpatialPolygonsDataFrame containing a single catchment boundary to be used as mask to crop the raster.
#' @param lookupTable this is a data.frame to link soil, vegetation and Hydrological conditions.
#' @param vegetation filename of a raster containing the vegetation map.
#' @param artificialDrainage (default = "none"), possible values are none, low, medium, high.
#'
#' @return Curve Number map, a GeoTiff raster with same resolution of soil and vegetation maps and extent equal to the mask
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Load Plynlimon sub-catchments spatial polygons data frame
#'   data("PlynlimonSUBCATCHMENTS")
#'   # Load soil map (HOST dominant classes)
#'   data("PlynlimonSOILdominant")
#'   # Load land cover (VEGETATION 2013) MAP
#'   data("PlynlimonVEG")
#'   # Load lookup table
#'   dfLookup <- MakeLoopkupTable("Fair woods + Poor pasture")
#'
#'   cn <- RegionalisedCNdominant(soil = PlynlimonSOILdominant,
#'                                catchment = PlynlimonSUBCATCHMENTS[1,],
#'                                lookupTable = dfLookup,
#'                                vegetation = PlynlimonVEG,
#'                                artificialDrainage = "none")
#' }
#'

RegionalisedCNdominant <- function(soil, catchment, lookupTable, vegetation,
                                   artificialDrainage = "none"){

  # Crop SpatialPolygon* object (percentage coverage HOST vector map) based on
  # catchment
  soilCropped <- raster::crop(soil, catchment)
  vegetationCropped <- raster::crop(vegetation, catchment)
  vegetationMasked <- raster::mask(vegetationCropped, catchment)

  # Set extent and resample, make a matrix out of the mask's extent
  catchmentEXT <- raster::extent(catchment)
  vegetationEXT <- raster::extent(vegetationMasked)
  soilEXT <- raster::extent(soilCropped)

  # make a matrix out of it, each column represents a raster, rows the values
  extent_df <- cbind(as.vector(catchmentEXT),
                     as.vector(vegetationEXT),
                     as.vector(soilEXT))
  rownames(extent_df) <- c("xmin", "xmax", "ymin", "ymax")

  best_extent <- raster::extent(max(extent_df[1,]), min(extent_df[2,]),
                                max(extent_df[3,]), min(extent_df[4,]))

  # the range of your extent
  rangesTMP <- as.data.frame(apply(extent_df, 1, range))
  ranges <- data.frame("x" = c(min(rangesTMP$xmin), max(rangesTMP$xmax)),
                       "y" = c(min(rangesTMP$ymin), max(rangesTMP$ymax)),
                       row.names = c("min", "xmax"))
  ranges <- apply(ranges, 2, diff)

  # the resolution of your raster (pick one, or add a desired resolution)
  reso <- raster::res(vegetationMasked)
  # divide the range by your desired resolution
  # gives you the number of rows and columns
  nrow_ncol <- ranges/reso

  # Resample Veg raster
  r1 <- raster::raster(best_extent,
                       nrows = nrow_ncol[2],
                       ncols = nrow_ncol[1],
                       crs = vegetationMasked@crs)
  vegetationResampled <- raster::resample(vegetationMasked, r1, method = "ngb")

  # Resample Soil raster
  soilResampled <- raster::resample(soilCropped, r1, method="ngb")

  CNmap <- raster::overlay(soilResampled, vegetationResampled,
                           fun=function(x,y){return(x+y*1000)})

  # calculate the percentage coverage of each bandValue
  freqT <- data.frame(table(raster::extract(CNmap, catchment)))

  # If the CNmap shows values less than 1000 it means that the vegetation map
  # is smaller than the soil map. If the CNmap shows values less that can be
  # divided by 1000 with no decimals it means that the soil map is smaller than
  # the vegetation map. In both cases, when there is no perfect overlap, the
  # non-overlapping cells are removed.
  if (any(as.numeric(as.character(freqT$Var1))<1000)) {
    message("Some cells do not overlap, they will be ignored.")
    freqT <- freqT[as.numeric(as.character(freqT$Var1))>1000,]
    row.names(freqT) <- NULL
  }
  if (any(as.numeric(as.character(freqT$Var1))%%1000==0)) {
    message("Some cells do not overlap, they will be ignored.")
    freqT <- freqT[as.numeric(as.character(freqT$Var1))%%1000!=0,]
    row.names(freqT) <- NULL
  }

  freqT$percentage <- round(freqT$Freq/sum(freqT$Freq),2)
  temp <- as.character(freqT$Var1)
  freqT$soil <- as.numeric(substr(temp, nchar(temp)-3+1, nchar(temp)))
  freqT$veg <- as.numeric(as.character(freqT$Var1))%/%1000

  # if (grepl("host", names(soilResampled)){
  #  freqT$soil <- HOSTband2class(freqT$soil)
  # }

  freqT$usda <- HOST2USDA(freqT$soil)
  freqT$CN <- NA
  freqT$usdaNumber <- NA

  A <- B <- C <- D <- AB <- BC <- CD <- c()

  for (i in 1:length(freqT$veg)){

    A <- c(A, as.numeric(as.character(lookupTable$SoilA[lookupTable$Code==
                                                          freqT$veg[i]])))
    B <- c(B, as.numeric(as.character(lookupTable$SoilB[lookupTable$Code==
                                                          freqT$veg[i]])))
    C <- c(C, as.numeric(as.character(lookupTable$SoilC[lookupTable$Code==
                                                          freqT$veg[i]])))
    D <- c(D, as.numeric(as.character(lookupTable$SoilD[lookupTable$Code==
                                                          freqT$veg[i]])))
    AB <- c(AB, round(sum(A[i],B[i])/2,0) )
    BC <- c(BC, round(sum(B[i],C[i])/2,0) )
    CD <- c(CD, round(sum(C[i],D[i])/2,0) )

    freqT$CN[i] <- eval(parse(text=paste0(as.character(freqT$usda[i]),"[1]")))

    if (freqT$usda[i]=="A")  freqT$usdaNumber[i] <- 1
    if (freqT$usda[i]=="AB") freqT$usdaNumber[i] <- 2
    if (freqT$usda[i]=="B")  freqT$usdaNumber[i] <- 3
    if (freqT$usda[i]=="BC") freqT$usdaNumber[i] <- 4
    if (freqT$usda[i]=="C")  freqT$usdaNumber[i] <- 5
    if (freqT$usda[i]=="CD") freqT$usdaNumber[i] <- 6
    if (freqT$usda[i]=="D")  freqT$usdaNumber[i] <- 7

  }

  freqT$A <- A
  freqT$AB <- AB
  freqT$B <- B
  freqT$BC <- BC
  freqT$C <- C
  freqT$CD <- CD
  freqT$D <- D

  if (artificialDrainage != "none"){

    if (artificialDrainage == "low") {
      freqT$USDAnumberDrainage <- freqT$usdaNumber - 1
    }
    if (artificialDrainage == "medium") {
      freqT$USDAnumberDrainage <- freqT$usdaNumber - 2
    }
    if (artificialDrainage == "high") {
      freqT$USDAnumberDrainage <- freqT$usdaNumber - 3
    }

    # Correct CN for drained soils
    for (i in 1: dim(freqT)[1]){
      mycolumn <- max(1,as.numeric(as.character(freqT$USDAnumberDrainage[i])))
      freqT$CN[i] <- eval(parse(text=paste("freqT[i,c('A','AB','B','BC','C',
                                           'CD','D')][[",mycolumn,"]]",sep="")))
    }

  }

  # sum up the above to get the final CN value
  CN <- round(sum(as.numeric(as.character(freqT$CN))*
                    as.numeric(as.character(freqT$percentage))),0)

  return(CN)

  }
