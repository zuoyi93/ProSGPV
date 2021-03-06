% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pro.sgpv.R
\name{predict.sgpv}
\alias{predict.sgpv}
\title{\code{predict.sgpv}: Prediction using the fitted model}
\usage{
\method{predict}{sgpv}(object, newdata, type, ...)
}
\arguments{
\item{object}{An \code{sgpv} objectect}

\item{newdata}{Prediction data set}

\item{type}{The type of prediction required. Can take the value of \code{link},
\code{response}, and \code{terms}. Default is \code{response}.}

\item{...}{Other \code{predict} arguments}
}
\value{
Predicted values
}
\description{
S3 method \code{predict} for an object of class \code{sgpv}
}
\examples{


# prepare the data
x <- t.housing[, -ncol(t.housing)]
y <- t.housing$V9

# run one-stage algorithm
out.sgpv <- pro.sgpv(x = x, y = y)

predict(out.sgpv)
}
