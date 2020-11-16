#' pro.sgpv function
#'
#' This function outputs indices of selected variables
#' from either one-stage algorithm or two-stage algorithm and lambda.1se
#' from the cross-validated lasso
#'
#' @importFrom stats complete.cases coef
#' @importFrom glmnet cv.glmnet
#' @param x Independent variables, can be a \code{matrix} or a \code{data.frame}
#' @param y Dependent variable, can be a \code{vector} or a column from a \code{data.frame}
#' @param stage Algorithm indicator. 1 denotes the one-stage algorithm and 2 denotes the two-stage algorithm
#'
#' @return A list of following components:
#' \describe{
#' \item{out.sgpv}{A vector of indices of selected variables}
#' \item{lambda}{Cross-validated lambda in the two-stage algorithm. \code{NULL} for the one-stage algorithm}
#' }
#' @export
#'
#' @examples
#' library(ProSGPV)
#'
#' x = t.housing[,-ncol(t.housing)]
#' y = t.housing$V9
#' out.sgpv.1 <- pro.sgpv(x = x, y = y, stage = 1)
#'


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

  return(list(out.sgpv=out.sgpv,lambda=lambda))

}

