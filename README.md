SCS Curve Number method (R-package)
===================================

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.14280.svg)](http://dx.doi.org/10.5281/zenodo.14280)

The SCS Curve Number (CN) is a well established method for the estimation of direct runoff from storm rainfall, developed by the USDA Soil Conservation Service (Rallison, 1980) and used in hydrologic engineering and environmental impact analyses. 

This package allows to calculate:  

* the direct storm runoff (Q), given rainfall (R) and CN number  
* the CN number, given rainfall (R) and runoff (Q) data
* HOST-based soil classes mapping onto the CN soil classes

**To cite this software:**  
C. Vitolo, N. Le Vine, CurveNumber (R package), (2015), GitHub repository, https://github.com/cvitolo/r_CurveNumber, doi: http://dx.doi.org/10.5281/zenodo.14280

### Background: the CN method in a nutshell
The CN number is a dimensionless parameter varying in the range [0,100]. This is tabulated based on land use type and hydrologic soil group. When the CN is known, the potential maximum retention (S) can be calculated using the following formula:

_S = 1000/CN - 10_

The storm runoff is then calculated as follows:

_Q = ((P-0.2S)^2)/(P+0.8S)_

with _P>0.2S_, where P is the rainfall depth (in inches).

When both P and Q are known, the CN can be calculated from data as suggested by Hawkins (1993). 

**References:**  
Richard H. Hawkins. Asymptotic Determination of Runoff Curve Numbers
from Data. Journal of Irrigation and Drainage Engineering, 119(2):334–345,
March 1993. ISSN 0733-9437. doi: 10.1061/(ASCE)0733-9437(1993)119:
2(334). [URL](http://ascelibrary.org/doi/abs/10.1061/%28ASCE%290733-9437%281993%29119%3A2%28334%29)

Allen T. Hjelmfelt. Empirical Investigation of Curve Number Technique. Journal of the Hydraulics Division, Vol. 106, No. 9, September 1980, pp. 1471-1476. [URL](http://cedb.asce.org/cgi/WWWdisplay.cgi?9734)

R. E. Rallison. Origin and evolution of the SCS runoff equation. In ASCE
lrrig. and Drain. Div. Symp. on Watershed Mgmt., Vol. 11, pages 912–
924, New York, New York, USA, 1980. [URL](http://cedb.asce.org/cgi/WWWdisplay.cgi?31601)

### Basics
Install and load packages
```R
# Install dependent packages from CRAN:
x <- c("zoo", "EcoHydRology", "udunits2","devtools")
install.packages(x)
lapply(x, require, character.only=T); rm(x)

# Install dependent packages from github:
install_github("josephguillaume/hydromad")
install_github("cvitolo/r_CurveNumber", subdir = "curvenumber")
```

### Load the library and some test data
Library and test dataset can be loaded as follows:
```R
library(curvenumber)
data(DATA) 
```

DATA is in mm/d but the time step is 1 hour, below is an adjustment:
```R
InputTS <- DATA/24; rm(DATA)
```

If the adjustment is not made, volumes should be re-calculated as the sum of the streamflow values multiplied by [the time interval length / your main time units]. E.g. if you use mm/day for hourly observations, the multiplier is [hour/day] = 1/24. If your main time units are the same as the time interval (mm/hr for hourly data), the multiplier is 1.

### Identify Rainfall-Runoff events
According to (Hawkins, 1993), in order to calculate the curve Number, the rainfall and runoff events can be identified separately. Return periods are then matched using the Frequency Matching approach (Hjelmfelt, 1980). 

```R
df  <- EventIdentification(dataX = InputTS,
                           hours2extend = 6, plotOption = FALSE,
                           stepsBack = 5, timeUnits = "hours")
```

### Calculate the Curve Number
Determine the Curve Number and k coefficient and also plot CN-P behaviour to 
define the type of asymptote
```R
CalculateCN(dfTPQ = df, PQunits = "mm", plotOption = TRUE)
```

The resulting Curve Number is: 82.

Please note that there are three types of behaviour: 
* "standard" (increasing asymptotically), 
* "complacent" (decreasing indefinitely) and 
* "violent" (increasing asymptotically).

Here, only the standard behaviour is implemented. In this case, CN (infinity) is the value of CN that corresponds to the largest rainfall events and can be 
calculated by a nonlinear least squares curve fitting (red line).

### Calculate the direct storm runoff
Given the maximum rainfall and the CN number, the potential maximum reterntion (S) and the direct storm runoff (Q) are calculated as follows:
```R
CalculateQ(P=30, CN=82, PQunits = "mm")

$S
[1] 55.756

$Q
[1] 4.762
```

### HOST-based soil classes mapping onto the CN soil classes
Hydrology of Soil Types (HOST) is the UK soils classification system. This is divided in 29 classes based on hydrological properties. The dominant soil class has been mapped for the entire Great Britain on a 1km resolution. 

Bulygina et al. (2011) defined the mapping between the HOST classes and USDA classes so that the CN can be calculated from soil and land use maps as well as from time series data of precipitation and streamflow discharge.

The function CalculateCNfromMaps() implements the methodology illustrated in Bulygina et al. (2011), allowing to calculate the CN given at least the soil map of the area:

```R
soilMap <- "/home/claudia/Dropbox/Projects/PURE/PURE_shared/Data/rasters/PontbrenSoilraster.tif"
shpFolder <- "/home/claudia/Dropbox/Projects/PURE/PURE_shared/Data/vectors/"

CNfromMaps(soilMap,
           tabulatedCN = list("A"=49,"B"=69,"C"=79,"D"=84),
           shpFolder,
           mask="pontbren09", plotOption=FALSE)

$CN
[1] 81
```

The result is 81, very close to the one obtained using Hawkins' method (82).


### Warnings
This package and functions herein are provided as is, without any guarantee.

### Please leave your feedback
This package was developed by [Claudia Vitolo](http://www.imperial.ac.uk/people/c.vitolo) and [Nataliya Le Vine](http://www.imperial.ac.uk/people/n.le-vine). We would greatly appreciate if you could leave your feedbacks via email (cvitolodev@gmail.com).
