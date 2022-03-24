#' curvenumber: an implementation of the USDA Soil Conservation Service Curve Number method
#'
#' @author Claudia Vitolo and Nataliya Le Vine
#'
#' @name curvenumber
#' @docType package
#'
#' @description The SCS Curve Number (CN) is a well established method for the estimation of direct runoff from storm rainfall, developed by the USDA Soil Conservation Service and used in hydrologic engineering and environmental impact analyses. The curvenumber is an R package which allows to calculate: a) the direct storm runoff (Q), given rainfall (R) and CN number; b) the CN number, given rainfall (R) and runoff (Q) data; c) HOST-based soil classes mapping onto the CN soil classes.
#' The package contains example data as well as a number of examples to test the main functionalities. The function EventIdentification(), for instance, is used to identify rainfall-runoff events and calculating the matching return period, according to Hjelmfelt (1980). The function CalculateCN() is used to identify Curve Number and k coefficient as well as to plot the CN-P asymptotic behaviour (see figure below), according to Hawkins (1993). The direct storm runoff can be calulated using the function CalculateQ() and CNfromMaps() implements the methodology illustrated in Bulygina et al. (2011), allowing to calculate the CN given at least the soil map of the area.
#'
#' @references
#' Bulygina, N., N. McIntyre, and H. Wheater (2011), Bayesian conditioning of a rainfall-runoff model for predicting flows in ungauged catchments and under land use changes, Water Resour. Res., 47, W02503, doi:10.1029/2010WR009240.
#'
#' Hawkins, Richard H. 1993. Asymptotic Determination of Runoff Curve Numbers from Data. Journal of Irrigation and Drainage Engineering 119 (2). American Society of Civil Engineers: 334–45.
#'
#' Hjelmfelt, Allen T. 1980. Empirical Investigation of Curve Number Technique. Journal of the Hydraulics Division 106 (9). ASCE: 1471–6.
#'
#' @import rgdal
#' @import sp
#' @importFrom raster intersect area
#' @importFrom zoo na.approx index coredata
#' @importFrom xts apply.daily
#' @importFrom pracma interp1
#' @importFrom hydromad eventseq eventinfo
#' @importFrom Hmisc wtd.mean
#' @importFrom graphics abline axis legend par points polygon text lines rect
#' @importFrom stats coef end lm predict start window
#' @importFrom utils tail
#' @importFrom minpack.lm nlsLM
#'
NULL

#' Data set: CNlookupTable
#'
#' @description CNlookupTable is a look-up table for Curve Number.
#'
#' @usage data("CNlookupTable")
#'
#' @format A data frame with 81 rows and the following 8 columns:
#' \describe{
#'   \item{\code{Type}}{Type of vegetation}
#'   \item{\code{CoverDescription}}{Description (character)}
#'   \item{\code{Detail}}{Additional details (character)}
#'   \item{\code{Condition}}{Soil condition (character)}
#'   \item{\code{A}}{Soil group A (numeric)}
#'   \item{\code{B}}{Soil group B (numeric)}
#'   \item{\code{C}}{Soil group C (numeric)}
#'   \item{\code{D}}{Soil group D (numeric)}
#' }
#'
#' @details
#' Grassland (Poor: <50% ground cover or heavily grazed with no mulch; Fair: 50-75% ground cover and not heavily grazed; Good: >75% ground cover and light or only occasionally grazed.)
#' Brush (Poor: <50% ground cover; Fair: 50-75% ground cover; Good: >75% ground cover.)
#' Woods-grass combination (CN were computed for areas with 50% woods and 50% grass (pasture) cover. Other combinations of conditions may be computed from the CN's for woods and pasture.)
#' Wood (Poor: Forest litter, small trees, and brush are destroyed by heavy grazing or regular burning; Fair: Woods are grazed but not burned, and some forest litter covers the soil; Good: Woods are protected from grazing, and litter and brush adequately cover the soil.)
#'
#' @keywords datasets
#'
#' @source \url{https://en.wikipedia.org/wiki/Runoff_curve_number}
#'
"CNlookupTable"

#' Data set: PlynlimonTable
#'
#' @description PlynlimonTable is a look-up table for Plynlimon subcatchment names and id codes.
#'
#' @usage data("PlynlimonTable")
#'
#' @format A data frame with 9 rows and the following 3 columns:
#' \describe{
#'   \item{\code{ID}}{Subcatchment identification code (character)}
#'   \item{\code{Name}}{Full name of subcatchment (character)}
#'   \item{\code{ShortName}}{Short name of subcatchment (character)}
#' }
#'
#' @keywords datasets
#'
#' @references
#' Vitolo, C. (2015), Exploring data mining for hydrological modelling (Ph.D. thesis), Imperial College London, uri: http://hdl.handle.net/10044/1/30773.
#'
#' @source \url{http://hdl.handle.net/10044/1/30773}
#'
"PlynlimonTable"

#' Data set: PlynlimonSUBCATCHMENTS
#'
#' @description PlynlimonSUBCATCHMENTS is a spatial polygons data frame for Plynlimon subcatchments.
#'
#' @usage data("PlynlimonSUBCATCHMENTS")
#'
#' @format Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots:
#' \describe{
#'   \item{\code{data}}{A data.frame containing 9 rows and 2 columns: CATCHMENT and SUBCATCH (this corresponds to the PlynlimonTable$ShortName).}
#'   \item{\code{polygons}}{List of 11 polygons}
#'   \item{\code{plotOrder}}{Numeric value indicating the plot order}
#'   \item{\code{bbox}}{Matrix containing bounding box in British National Grid coordinates}
#'   \item{\code{proj4string}}{Proj4 string describing the British National Grid reference system}
#' }
#'
#' @keywords datasets
#'
#' @references
#' Vitolo, C. (2015), Exploring data mining for hydrological modelling (Ph.D. thesis), Imperial College London, uri: http://hdl.handle.net/10044/1/30773.
#'
#' @source \url{http://hdl.handle.net/10044/1/30773}
#'
"PlynlimonSUBCATCHMENTS"

#' Data set: SevernTS
#'
#' @description SevernTS contains time series information for one of the gauged sub-catchment in the Plynlimon area: Severn at Plynlimon flume. The ID code of this subcatchment is 54022, see the look-up table \code{PlynlimonTable}.
#'
#' @usage data("SevernTS")
#'
#' @format The time series object (e.g. ID54022) is of class zoo. The index is a POSIXct in the format "1975-04-27 03:00:00". The data is measured in mm/hour and contain the following variables:
#' \describe{
#'   \item{\code{P}}{Precipitation.}
#'   \item{\code{Q}}{Streamflow discharge}
#' }
#'
#' @keywords datasets
#'
#' @references
#' Vitolo, C. (2015), Exploring data mining for hydrological modelling (Ph.D. thesis), Imperial College London, uri: http://hdl.handle.net/10044/1/30773.
#'
#' @source \url{http://hdl.handle.net/10044/1/30773}
#'
"SevernTS"

#' Data set: PlynlimonSOILdominant
#'
#' @description PlynlimonSOILdominant is a raster layer containing the dominant soil classes for Plynlimon catchments (based on the HOST classification).
#'
#' @usage data("PlynlimonSOILdominant")
#'
#' @format Formal class 'RasterLayer' [package "raster"] made of a single layer. Values range between 0 and 29. For correspondence to USDA classes see S1 lookup table.
#'
#' @keywords datasets
#'
#' @references
#' Vitolo, C. (2015), Exploring data mining for hydrological modelling (Ph.D. thesis), Imperial College London, uri: http://hdl.handle.net/10044/1/30773.
#'
#' @source \url{http://hdl.handle.net/10044/1/30773}
#'
"PlynlimonSOILdominant"

#' Data set: PlynlimonSOIL
#'
#' @description PlynlimonSOIL is a spatial polygons data frame for Plynlimon soil (HOST percentage distribution classes).
#'
#' @usage data("PlynlimonSOIL")
#'
#' @format Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots:
#' \describe{
#'   \item{\code{data}}{A data.frame containing 35 rows and 34 columns: ID, ID0, HOSTCODE1-HOSTCODE29, HOSTCODE98-99 and AreaKm2.}
#'   \item{\code{polygons}}{List of 35 polygons}
#'   \item{\code{plotOrder}}{Numeric value indicating the plot order}
#'   \item{\code{bbox}}{Matrix containing bounding box in British National Grid coordinates}
#'   \item{\code{proj4string}}{Proj4 string describing the British National Grid reference system}
#' }
#'
#' @keywords datasets
#'
#' @references
#' Vitolo, C. (2015), Exploring data mining for hydrological modelling (Ph.D. thesis), Imperial College London, uri: http://hdl.handle.net/10044/1/30773.
#'
#' @source \url{http://hdl.handle.net/10044/1/30773}
#'
"PlynlimonSOIL"

#' Data set: PlynlimonVEG
#'
#' @description PlynlimonVEG is a vegetation map for Plynlimon are. It is extracted from the Vegetation map 2013 available from CEH data portal.
#'
#' @usage data("PlynlimonVEG")
#'
#' @format Formal class 'RasterLayer' [package "raster"] made of a single layer. Values range between 0 and 10. For correspondence to vegetation classes see \code{PlynlimonVegSoilTable} look up table.
#'
#' @keywords datasets
#'
#' @references
#' Vitolo, C. (2015), Exploring data mining for hydrological modelling (Ph.D. thesis), Imperial College London, uri: http://hdl.handle.net/10044/1/30773.
#'
#' @source \url{http://hdl.handle.net/10044/1/30773}
#'
"PlynlimonVEG"

#' Data set: PlynlimonVegSoilTable
#'
#' @description PlynlimonVegSoilTable is a look-up table for vegetation and soil classes.
#'
#' @usage data("PlynlimonVegSoilTable")
#'
#' @format A data frame with 58 rows and the following 12 columns:
#' \describe{
#'   \item{\code{Class}}{Description of vegetation class}
#'   \item{\code{SubClass}}{Description of vegetation subclass}
#'   \item{\code{LCM2007Class}}{Corresponding class in Land Cover Map 2007}
#'   \item{\code{LCM2007SubClass}}{Corresponding subclass in Land Cover Map 2007}
#'   \item{\code{LCM2000Class}}{Corresponding class in Land Cover Map 2000}
#'   \item{\code{LCM2000SubClass}}{Corresponding subclass in Land Cover Map 2000}
#'   \item{\code{LCM1990SubClass}}{Corresponding subclass in Land Cover Map 1990}
#'   \item{\code{VEG2013SubClass}}{Corresponding subclass in Vegetation Map 2013}
#'   \item{\code{SoilA}}{CN value for soil type A}
#'   \item{\code{SoilB}}{CN value for soil type B}
#'   \item{\code{SoilC}}{CN value for soil type C}
#'   \item{\code{SoilD}}{CN value for soil type D}
#' }
#'
#' @keywords datasets
#'
#' @references
#' Vitolo, C. (2015), Exploring data mining for hydrological modelling (Ph.D. thesis), Imperial College London, uri: http://hdl.handle.net/10044/1/30773.
#'
#' @source \url{http://hdl.handle.net/10044/1/30773}
#'
"PlynlimonVegSoilTable"

#' Data set: Table S1
#'
#' @description Table S1 is part of the supporting information of Bulygina et al. (2011). It contains the expected values of BFI*HOST and standard deviations sHOST for each HOST class, and proposed corresponding USDA class. The second column (BFIHOST) contains the theoretical BFI, as calculated by Boorman et al. (1995, page 34) from multiple regression analysis.
#'
#' @references
#' Bulygina, N., N. McIntyre, and H. Wheater (2011), Bayesian conditioning of a rainfall-runoff model for predicting flows in ungauged catchments and under land use changes, Water Resour. Res., 47, W02503, doi:10.1029/2010WR009240.
#'
#' @source \url{http://dx.doi.org/10.1029/2010WR009240}
#'
#' @usage data("S1")
#'
#' @format A data frame with 29 rows and the following 4 columns:
#' \describe{
#'   \item{\code{HOSTclass}}{An integer in the range [1, 29]}
#'   \item{\code{BFIHOST}}{An index in the range [0, 1]}
#'   \item{\code{sHOST}}{HOST standard deviation}
#'   \item{\code{USDAclass}}{USDA class corresponding to the given HOST class.}
#' }
#'
#' @keywords datasets
#'
"S1"
