ProSGPV
========
Penalized Regression with Second Generation P Values

## Installation

``` r
library(devtools)
devtools::install_github("zuoyi93/ProSGPV")
```

## Example

Here, we use the Tehran housing data as an illustrative example. This data set contains 26 explanatory variables and one outcome. More details can be found at . 

At the end of this section, we also provide an illustration with fewer variables that is discussed in the paper in the [reference](#ref).

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

    ## Selected variables are V8 V12 V13 V15 V17 V25 V26

``` r
# get indices of variables selected
out.sgpv.1$var.index

```

    ## [1]  7  9 10 12 14 22 23

``` r
# extract OLS estimates
coef(out.sgpv.1)

```

    ##  (Intercept)            V8           V12           V13           V15           V17 
	## 77.448446890   1.209306136 -25.521519547  18.986926936   0.002033393  -3.288626450 
	##          V25           V26 
	## 11.352512518 -14.357918688 

``` r
# get prediction from the model
predict(out.sgpv.1)
```

S3 method `plot` is also available for the two-stage algorithm.

``` r
# two-stage algorithm
out.sgpv.2 <- pro.sgpv(x = x, y = y, stage = 2)

# plot the fully relaxed lasso solution path and final solution
plot(out.sgpv.2,lpv=3)
```
First, we plot the full solution path with point estimates and 95% confidence intervals. Note that the null region is in sky blue. The selected variables are colored blue on the y-axis.

![](fig/fig.1.png)

We can also zoom in to have a closer look.  

``` r
plot(out.sgpv.2,lpv=3,lambda.max=0.01)
```

![](fig/fig.2.png)

Alternatively, we can plot the confidence bound that is closer to the null.

``` r
plot(out.sgpv.2,lpv=1,lambda.max=0.01)
```

![](fig/fig.3.png)

Below is the figure with fewer variables that is shown in the paper down below. Only V3 is the true signal that generates the response. Fully relaxed lasso would have selected both V3 and V4, while our SGPV approach would only select V3.

![](fig/fig.4.png)

## Reference <a name="ref"></a>


