
curvenumber: an R package to calculate Base Flow Index and Curve Number for gauged and ungauged river catchments
================================================================================================================

<!-- Edit the README.Rmd only!!! The README.md is generated automatically from README.Rmd. -->
[![Travis-CI Build Status](https://travis-ci.org/cvitolo/curvenumber.svg?branch=master)](https://travis-ci.org/cvitolo/curvenumber) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/cvitolo/curvenumber?branch=master&svg=true)](https://ci.appveyor.com/project/cvitolo/curvenumber) [![Coverage Status](https://img.shields.io/codecov/c/github/cvitolo/curvenumber/master.svg)](https://codecov.io/github/cvitolo/curvenumber?branch=master)

The curvenumber (Vitolo and Le Vine 2016) is an R package (R Core Team 2016) which allows to calculate two indices for the classification of hydrological responses: the Base Flow Index and the Curve Number. These indices are indicators of low and high flow responses, respectively. Techniques have been developed to calculate the indices for gauged and ungauged catchments in the United States (Rallison 1980; Hjelmfelt 1980; Hawkins 1993).

This work presents the first stable release of the curvenumber R package, which allows to calculate the Curve Number and Base Flow Index empirically as well as from spatial data layers for catchments in the United Kingdom. The proposed method is based on previous investigations made by Bulygina, McIntyre, and Wheater (2011) but sets the scene for a more general approach that can be applied globally.

The package contains sample datasets as well as a number of examples to test the main functionalities. The functions `FindQevents()` and `FindQevents()`, for instance, are used to identify rainfall-runoff events while `ReturnPeriod()` is used to calculate the matching return period, according to Hjelmfelt (1980). The function `EmpiricalCN()` is used to identify the Curve Number from time series data as well as to plot the CN-P asymptotic behaviour, according to Hawkins (1993). The direct storm runoff can be calulated using the function `DirectStormRunoff()` and `RegionalisedCN()` allows to calculate the CN given soil and vegetation maps of the area. The package also allows to calculate the BFI given a soil map `RegionalisedBFI()` and/or a time series of river discharges `EmpiricalBFI()`.

Work is currently ongoing to develop the curvenumber package further and use it with a probabilistic hydrological multi-model framework (Vitolo et al. 2012; Vitolo et al. 2016) to predict the effects of land use changes on catchment flows.

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

Bulygina, Nataliya, Neil McIntyre, and Howard Wheater. 2011. “Bayesian Conditioning of a Rainfall-Runoff Model for Predicting Flows in Ungauged Catchments and Under Land Use Changes.” *Water Resources Research* 47 (2). Wiley Online Library.

Hawkins, Richard H. 1993. “Asymptotic Determination of Runoff Curve Numbers from Data.” *Journal of Irrigation and Drainage Engineering* 119 (2). American Society of Civil Engineers: 334–45.

Hjelmfelt, Allen T. 1980. “Empirical Investigation of Curve Number Technique.” *Journal of the Hydraulics Division* 106 (9). ASCE: 1471–6.

R Core Team. 2016. *R: A Language and Environment for Statistical Computing*. Vienna, Austria: R Foundation for Statistical Computing. <https://www.R-project.org/>.

Rallison, Robert E. 1980. “Origin and Evolution of the Scs Runoff Equation.” In *Symposium on Watershed Management 1980*, 912–24. ASCE.

Vitolo, Claudia, and Nataliya Le Vine. 2016. *Curvenumber: An Implementation of the Us Soil Conservation Service Curve Number Method*. doi:[http://dx.doi.org/10.5281/zenodo.14280](https://doi.org/http://dx.doi.org/10.5281/zenodo.14280).

Vitolo, Claudia, Peter Wells, Martin Dobias, and Wouter Buytaert. 2012. *Fuse: Framework for Understanding Structural Errors*. doi:[http://dx.doi.org/10.5281/zenodo.14005](https://doi.org/http://dx.doi.org/10.5281/zenodo.14005).

Vitolo, Claudia, Peter Wells, Martin Dobias, and Wouter Buytaert. 2016. “Fuse: An R Package for Ensemble Hydrological Modelling.” *The Journal of Open Source Software* 1 (8). The Open Journal. doi:[10.21105/joss.00052](https://doi.org/10.21105/joss.00052).
