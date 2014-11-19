CURVENUMBER (R-package)
===========================

SCS Curve Number method

This package implements the SCS Curve Number method according to [Hawkins (1993)](http://dx.doi.org/10.1061/(ASCE)0733-9437(1993)119:2(334)).

#### Basics
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

#### Load the library and some test data

```R
library(curvenumber)
data(DATA) 

# DATA is in mm/d but the time step is 1 hour, below is an adjustment:
InputTS <- DATA/24; rm(DATA)
```

### Identify Rainfall-Runoff events
According to [Hawkins (1993)](http://dx.doi.org/10.1061/(ASCE)0733-9437(1993)119:2(334)), in order to calculate the curve Number, the rainfall and runoff events can be identified separately. Return periods are then matched using the Frequency Matching approach [Hjelmfelt (1980)](http://cedb.asce.org/cgi/WWWdisplay.cgi?9734). 

```R
df  <- EventIdentification(dataX = InputTS,
                           hours2extend = 6, plotOption = FALSE,
                           stepsBack = 5, timeUnits = "hours")
```

### Calculate the Curve Number
Determine the Curve Number and k coefficient and also plot CN-P behaviour to 
define the type of asymptote
```R
coef <- CalculateCN(dfTPQ = df, PQunits = "mm", plotOption = TRUE)
```

Please note that there are three types of behaviour: 
* "standard" (increasing asymptotically), 
* "complacent" (decreasing indefinitely) and 
* "violent" (increasing asymptotically).
Here, only the standard behaviour is implemented. In this case, CN (infinity) is
the value of CN that corresponds to the largest rainfall events and can be 
calculated by a nonlinear least squares curve fitting (red line).

#### Warnings
This package and functions herein are provided as is, without any guarantee.

#### Please leave your feedback
I would greatly appreciate if you could leave your feedbacks via email (cvitolodev@gmail.com).
