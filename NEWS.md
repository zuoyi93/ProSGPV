# ProSGPV

Penalized Regression with Second-Generation P-Values


## ProSGPV 1.0.0

8/6/2021: ProSGPV version 1.0.0 is here!

* Added the reference that proposed the ProSGPV algorithm

## ProSGPV 0.3.0

5/4/2021: ProSGPV is faster now!

* Changed the first-stage screening method from cross-validated lasso to one using generalized information criterion

* Removed `which.sgpv` as there is no variability in selection results given a data set

## ProSGPV 0.2.0

3/31/2021: ProSGPV can handle GLM now!

* Added functionalities of variable selection in Logistic, Poisson, and Cox models

* Added the `which.sgpv` function to find the most frequent model when the two-stage algorithm is used

* Added the S3 `plot` method for the one-stage algorithm

* Added vignettes to better illustrate how ProSGPV works

## ProSGPV 0.1.0

1/6/2021: The first version of ProSGPV is released now!

* Added a NEWS.md file to track changes to the package

