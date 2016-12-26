
<!-- Edit the README.Rmd only!!! The README.md is generated automatically from README.Rmd. -->
[![Travis-CI Build Status](https://travis-ci.org/cvitolo/r_CurveNumber.svg?branch=master)](https://travis-ci.org/cvitolo/r_CurveNumber) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/cvitolo/r_CurveNumber?branch=master&svg=true)](https://ci.appveyor.com/project/cvitolo/r_CurveNumber) [![Coverage Status](https://img.shields.io/codecov/c/github/cvitolo/r_CurveNumber/master.svg)](https://codecov.io/github/cvitolo/r_CurveNumber?branch=master)

The SCS Curve Number (CN) is a well established method for the estimation of direct runoff from storm rainfall, developed by the USDA Soil Conservation Service and used in hydrologic engineering and environmental impact analyses. The `curvenumber` is an R package which allows to calculate: a) the direct storm runoff (Q), given rainfall (R) and CN number; b) the CN number, given rainfall (R) and runoff (Q) data; c) HOST-based soil classes mapping onto the CN soil classes.

The package contains example data as well as a number of examples to test the main functionalities. The function `EventIdentification()`, for instance, is used to identify rainfall-runoff events and calculating the matching return period, according to Hjelmfelt (1980). The function `CalculateCN()` is used to identify Curve Number and k coefficient as well as to plot the CN-P asymptotic behaviour (see figure below), according to Hawkins (1993). The direct storm runoff can be calulated using the function `CalculateQ()` and `CNfromMaps()` implements the methodology illustrated in Bulygina et al. (2011), allowing to calculate the CN given at least the soil map of the area.

### Dependencies

The curvenumber package, as well as the examples in the vignette, depend on a number of CRAN packages. Check for missing dependencies and install them:

``` r
packs <- c("dplyr", "zoo", "tgp", "stats", "utils", "BH", "Rcpp", "testthat",
           "qualV", "devtools")
new.packages <- packs[!(packs %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
```

### Installation

You can install this package from Github with [devtools](https://github.com/hadley/devtools):

``` r
devtools::install_github("cvitolo/curvenumber")
```

Load the package:

``` r
library("curvenumber")
```

### Usage

For details and examples usage, please refer to the [vignette](vignettes/curvenumber_vignette.Rmd).

### Meta

-   Code contributions are welcome! Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
-   Please [report any issues or bugs](https://github.com/cvitolo/fuse/issues).
-   License: [GPL-3](https://opensource.org/licenses/GPL-3.0)
-   Get citation information for the `fuse` package in R doing `citation(package = 'curvenumber')`
