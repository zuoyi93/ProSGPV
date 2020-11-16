ProSGPV
========
Penalized Regression with Second Generation P Values

## Installation

``` r
library(devtools)
devtools::install_github("zuoyi93/ProSGPV")
```

## Example


``` r
# load the package
library(ProSGPV)

# prepare the data
x = t.housing[,-ncol(t.housing)]
y = t.housing$V9

# run one-stage algorithm
out.sgpv.1 <- pro.sgpv(x = x, y = y, stage = 1)

# show the variable selection results
out.sgpv.1
```

    ## [[1]]
    ## [1]  7  9 10 12 14 22 23
    ## 
    ## [[2]]
    ## NULL

## Reference


