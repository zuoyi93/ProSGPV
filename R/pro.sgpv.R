#' pro.sgpv function
#'
#' This function outputs the variable selection results
#' from either one-stage algorithm or two-stage algorithm.
#'
#' @importFrom stats complete.cases coef lm predict
#' @importFrom glmnet cv.glmnet
#' @param x Independent variables, can be a \code{matrix} or a \code{data.frame}
#' @param y Dependent variable, can be a \code{vector} or a column from a \code{data.frame}
#' @param stage Algorithm indicator. 1 denotes the one-stage algorithm and 2 denotes the two-stage algorithm
#'
#' @return A list of following components:
#' \describe{
#' \item{var.index}{A vector of indices of selected variables}
#' \item{var.label}{A vector of labels of selected variables}
#' \item{lambda}{Cross-validated lambda in the two-stage algorithm. \code{NULL} for the one-stage algorithm}
#' \item{x}{Input data \code{x}}
#' \item{y}{Input data \code{y}}
#' }
#' @export
#'
#' @examples
#' # more examples at https://github.com/zuoyi93/ProSGPV

pro.sgpv <- function(x, y, stage=c(1,2)){

  if(!(stage%in% 1:2)) stop("Stage only takes value of 1 or 2.")

  if(nrow(x)!= length(y)) stop("Input x and y have different number of observations")

  if(!is.numeric(as.matrix(x)) | !is.numeric(y)) stop("The input data have non-numeric values.")

  if(any(complete.cases(x)==F) | any(complete.cases(y)==F)){
    warning("Only complete records will be used.")
    comp.index <- complete.cases(data.frame(x,y))
    x <- x[comp.index,]
    y <- y[comp.index]
  }

  if(is.null(colnames(x))) colnames(x) <- paste("V",1:ncol(x),sep="")

  xs <- scale(x)
  ys <- scale(y)

  if(stage == 2){
    lasso.cv <- cv.glmnet(xs,ys)
    lambda = lasso.cv$lambda.1se
    candidate.index <- which(coef(lasso.cv,s=lambda)[-1] != 0)
  }else{
    candidate.index <- 1:ncol(xs)
    lambda = NULL
  }

  out.sgpv <- get.var(candidate.index, xs, ys)

  out <- list(var.index=out.sgpv,
       var.label=colnames(x)[out.sgpv],
       lambda=lambda,
       x=x,
       y=y)

  class(out) <- "sgpv"
  return(out)

}

