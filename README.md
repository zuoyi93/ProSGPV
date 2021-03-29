ProSGPV
========
Penalized Regression with Second-Generation P-Values

- [Introduction](#introduction)
- [Installation](#hinstallation)
- [Example](#example)
  * [Simulation-data](#simulation-data)
  * [Real-world-data](#real-world-data)
    + [One-stage-algorithm](#one-stage-algorithm)
    + [Two-stage-algorithm](#two-stage-algorithm)
  * [More-examples](#more-examples)
- [References](#references)

<!-- toc -->

# Introduction

We know that p-values can't be used for variable selection. However, you can do so with second-generation p-values. Here is how.

# Installation

To install it on CRAN, you can do

``` r
install.packages("ProSGPV")
```

For a development version of ProSGPV, you can install it by running the following command.  

``` r
library(devtools)
devtools::install_github("zuoyi93/ProSGPV")
```

# Example

## Simulation data 

Below is an illustration of how ProSGPV successfully selects the true support, while lasso and fully relaxed lasso fails. Five variables are simulated and only V3 is associated with the response. Plot (1) presents the lasso solution path. The vertical dotted line is <img src="https://latex.codecogs.com/png.latex?\color{blue}{\lambda_{\text{1se}}}" /> . (2) shows the fully relaxed lasso path. (3) shows the fully relaxed lasso paths with their 95% confidence intervals (in lighter color). (4) illustrates the two-stage ProSGPV algorithm selection path. The shaded area is the null region; the colored lines are each 95% confidence bound that is closer to the null region. Lasso and fully relaxed lasso would select both V2 and V3, while ProSGPV successfully screens out V2.  

![](man/figures/fig.4.png)

## Real-world data

Here, we use the Tehran housing data as an illustrative real-world data example of how ProSGPV works with linear regression. 

The Tehran housing data contain 26 explanatory variables and one outcome. Details about data collection can be found in this [paper](https://ascelibrary.org/doi/abs/10.1061/%28ASCE%29CO.1943-7862.0001047), and variable description can be found [here](man/t.housing.Rd). 


### One-stage algorithm

First, let's see the variable selection results using the fast one-stage ProSGPV algorithm.

``` r
# load the package
library(ProSGPV)

# prepare the data
x = t.housing[,-ncol(t.housing)]
y = t.housing$V9

# run one-stage algorithm
out.sgpv.1 <- pro.sgpv(x = x, y = y, stage = 1)
```

The selected variables are  

``` r
out.sgpv.1
```

    ## Selected variables are V8 V12 V13 V15 V17 V25 V26

We can also extract indices of variables selected

``` r
out.sgpv.1$var.index
```

    ## [1]  7  9 10 12 14 22 23


Now let's view the summary of the OLS model on those selected variables. 

``` r
summary(out.sgpv.1)
```

	## Call:
	## lm(formula = Response ~ ., data = lm.d)
	##
	## Residuals:
	##     Min       1Q   Median       3Q      Max 
	## -1226.05   -69.43    -7.74    53.65  1431.54 
	##
	## Coefficients:
	##               Estimate Std. Error t value Pr(>|t|)    
	## (Intercept)  7.745e+01  5.775e+01   1.341 0.180694    
	## V8           1.209e+00  1.323e-02  91.416  < 2e-16 ***
	## V12         -2.552e+01  2.624e+00  -9.725  < 2e-16 ***
	## V13          1.899e+01  2.532e+00   7.497 5.01e-13 ***
	## V15          2.033e-03  1.478e-04  13.759  < 2e-16 ***
	## V17         -3.289e+00  8.799e-01  -3.738 0.000216 ***
	## V25          1.135e+01  5.626e+00   2.018 0.044327 *  
	## V26         -1.436e+01  5.112e+00  -2.809 0.005240 ** 
	## ---
	## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
	## 
	## Residual standard error: 194 on 364 degrees of freedom
	## Multiple R-squared:  0.9746,	Adjusted R-squared:  0.9741 
	## F-statistic:  1997 on 7 and 364 DF,  p-value: < 2.2e-16


We can extract the point estimates by  

``` r
coef(out.sgpv.1)
```

We can get the predicted values by

``` r
predict(out.sgpv.1)
```

### Two-stage algorithm 

By default, the two-stage ProSGPV algorithm is applied to gain better parameter estimation and it can also deal with high dimensional data where  <img src="https://latex.codecogs.com/png.latex?\color{blue}{p>n}" />.  

``` r
out.sgpv.2 <- pro.sgpv(x = x, y = y)
```
The two-stage algorithm selects the following variables.

``` r
out.sgpv.2
```

    ## Selected variables are V8 V12 V13 V15 V17 V26

S3 method `plot` is available for the two-stage algorithm.

``` r
plot(out.sgpv.2)
```
First, we plot the full solution path with point estimates and 95% confidence intervals. Note that the null region is in sky blue. The selected variables are colored blue on the y-axis.

![](man/figures/fig.1.png)

We can also zoom in to have a closer look.  

``` r
plot(out.sgpv.2,lambda.max=0.01)
```

![](man/figures/fig.2.png)

Alternatively, we can plot the confidence bound that is closer to the null.

``` r
plot(out.sgpv.2,lpv=1,lambda.max=0.01)
```

![](man/figures/fig.3.png)

## More examples

For GLM examples, please refer to the [vignette](vignettes) folder, particularly this [file](vignettes/glm-vignette.Rmd). 


# References

The paper that proposed ProSGPV algorithm in linear regression:  

	Zuo Y, Stewart TG, Blume JD. Variable Selection with Second-Generation P-Values. arXiv preprint arXiv:2012.07941. 2020 Dec 15.

The paper that proposed ProSGPV algorithm in GLM:

	Coming soon

The papers regarding the second-generation p-values:  

	Blume JD, Greevy RA, Welty VF, Smith JR, Dupont WD. An introduction to second-generation p-values. The American Statistician. 2019 Mar 29;73(sup1):157-67.

	Blume JD, D’Agostino McGowan L, Dupont WD, Greevy Jr RA. Second-generation p-values: Improved rigor, reproducibility, & transparency in statistical analyses. PLoS One. 2018 Mar 22;13(3):e0188299.

