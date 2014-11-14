CURVENUMBER (R-package)
===========================

SCS Curve Number method

This package implements the SCS Curve Number method according to [Hawkins (1993)](http://dx.doi.org/10.1061/(ASCE)0733-9437(1993)119:2(334)).

#### Basics
Install and load packages
```R
# Install dependent packages from CRAN:
x <- c("pure", "hydromad", "zoo", "EcoHydRology", "udunits2")
install.packages(x)
lapply(x, require, character.only=T); rm(x)

# Install dependent gists and packages from github:
library(devtools)
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
df  <- EventIdentification(dataX = InputTS, PQindependent = FALSE,
                           plotOption = FALSE, variable2plot = "Q")
```

### Calculate the Curve Number
Determine the CN for each event
```R
newDF <- CalculateCN(dfTPQ = df, PQunits = "mm")
```

Plot CN-P behaviour to define the type of asymptote
```R
plot(newDF$CN~newDF$P, xlab="Rainfall", ylab="Runoff CN", ylim=c(min(newDF$CN),100))
```

There are three types of behaviour: "standard" (increasing asymptotically), "complacent" (decreasing indefinitely) and "violent" (increasing asymptotically).
I the behaviour can be considered standard, then CNinfinity can be calculated by a nonlinear least squares curve fiiting.

The variable CNinf is independent from P.
It is the CN describing the data set for larger rainfall events.

```R
CN <- newDF$CN
P <- newDF$P

# Determine parameters first guess
CN0 <- median( sort(CN, decreasing = FALSE)[1:5] )
k=1 # k=0.003

# nonlinear least squares curve fiiting
fit <- nls(CN ~ CN0 + (100 - CN0) * exp(-k*P), start=list(CN0=CN0,k=k))

summary(fit)

coefficients(fit)
  
# Sum of squared residuals:
sum(resid(fit)^2) 

# Finally, lets get the parameter confidence intervals.
confint(fit) 
```

Draw the fit on the plot by getting the prediction from the fit at 200 x-coordinates across the range of P
```R
new = data.frame(P = seq(min(P),max(P),len=200))
lines(new$P,predict(fit,newdata=new), col="red")
```

#### Warnings
This package and functions herein are provided as is, without any guarantee.

#### Please leave your feedback
I would greatly appreciate if you could leave your feedbacks via email (cvitolodev@gmail.com).