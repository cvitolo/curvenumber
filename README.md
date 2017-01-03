
curvenumber: an R package to calculate Base Flow Index and Curve Number for gauged and ungauged river catchments
================================================================================================================

<!-- Edit the README.Rmd only!!! The README.md is generated automatically from README.Rmd. -->
[![Travis-CI Build Status](https://travis-ci.org/cvitolo/curvenumber.svg?branch=master)](https://travis-ci.org/cvitolo/curvenumber) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/cvitolo/curvenumber?branch=master&svg=true)](https://ci.appveyor.com/project/cvitolo/curvenumber) [![Coverage Status](https://img.shields.io/codecov/c/github/cvitolo/curvenumber/master.svg)](https://codecov.io/github/cvitolo/curvenumber?branch=master)

The curvenumber (Vitolo and Le Vine, 2016) is an R package (R Core Team, 2016) which allows to calculate two indices for the classification of hydrological responses: the Base Flow Index (BFI) and the Curve Number (CN). These indices are indicators of low and high flow responses, respectively. Techniques have been developed to calculate the indices for gauged and ungauged catchments in the United States (Hawkins, 1993; Hjelmfelt, 1980; Rallison, 1980).

This work presents the first stable release of the curvenumber R package, which allows to calculate the CN and BFI empirically as well as from spatial data layers for catchments in the United Kingdom. The proposed method is based on previous investigations made by Bulygina et al. (2011) but sets the scene for a more general approach that can be applied globally.

The package contains sample datasets as well as a number of examples to test the main functionalities. The functions `FindQevents()` and `FindQevents()`, for instance, are used to identify rainfall-runoff events while `ReturnPeriod()` is used to calculate the matching return period, according to Hjelmfelt (1980). The function `EmpiricalCN()` is used to identify the Curve Number from time series data as well as to plot the CN-P asymptotic behaviour, according to Hawkins (1993). The direct storm runoff can be calulated using the function `DirectStormRunoff()` and `RegionalisedCN()` allows to calculate the CN given soil and vegetation maps of the area. The package also allows to calculate the BFI given a soil map `RegionalisedBFI()` and/or a time series of river discharges `EmpiricalBFI()`.

Work is currently ongoing to develop the curvenumber package further and use it with a probabilistic hydrological multi-model framework (Vitolo et al., 2012, 2016) to predict the effects of land use changes on catchment flows.

Dependencies and installation
-----------------------------

The curvenumber package, as well as the examples in the vignette, depend on a number of CRAN packages. Check for missing dependencies and install them:

``` r
packs <- c("dplyr", "zoo", "tgp", "stats", "utils", "BH", "Rcpp", "testthat",
           "qualV", "devtools")
new.packages <- packs[!(packs %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
```

You can install this package from Github with [devtools](https://github.com/hadley/devtools):

``` r
devtools::install_github("cvitolo/curvenumber")
```

Load the package:

``` r
library("curvenumber")
```

Usage
-----

For details and examples usage, please refer to the [vignette](vignettes/curvenumber_vignette.Rmd).

Meta
----

-   Code contributions are welcome! Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
-   Please [report any issues or bugs](https://github.com/cvitolo/fuse/issues).
-   License: [GPL-3](https://opensource.org/licenses/GPL-3.0)
-   Get citation information for the `fuse` package in R doing `citation(package = 'curvenumber')`

References
----------

Bulygina, N., McIntyre, N. and Wheater, H.: Bayesian conditioning of a rainfall-runoff model for predicting flows in ungauged catchments and under land use changes, Water Resources Research, 47(2), 2011.

Hawkins, R. H.: Asymptotic determination of runoff curve numbers from data, Journal of Irrigation and Drainage Engineering, 119(2), 334–345, 1993.

Hjelmfelt, A. T.: Empirical investigation of curve number technique, Journal of the Hydraulics Division, 106(9), 1471–1476, 1980.

R Core Team: R: A language and environment for statistical computing, R Foundation for Statistical Computing, Vienna, Austria. \[online\] Available from: <https://www.R-project.org/>, 2016.

Rallison, R. E.: Origin and evolution of the scs runoff equation, in Symposium on watershed management 1980, pp. 912–924, ASCE., 1980.

Vitolo, C. and Le Vine, N.: Curvenumber: An implementation of the us soil conservation service curve number method., 2016.

Vitolo, C., Wells, P., Dobias, M. and Buytaert, W.: Fuse: Framework for understanding structural errors., 2012.

Vitolo, C., Wells, P., Dobias, M. and Buytaert, W.: Fuse: An r package for ensemble hydrological modelling, The Journal of Open Source Software, 1(8), doi:[10.21105/joss.00052](https://doi.org/10.21105/joss.00052), 2016.
