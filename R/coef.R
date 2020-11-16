#' coef S3 method
#'
#' Extract coefficients from the model fit
#'
#' @importFrom stats coef lm
#' @param object An \code{sgpv} object
#' @param ... Other \code{coef} arguments
#'
#' @return Coefficients in the OLS model
#' @export

coef.sgpv <- function(object,...){

  if(length(object$var.index)>0){
    lm.d <- data.frame(yy=object$y,xx = object$x[,object$var.index])
    colnames(lm.d)[-1] <- object$var.label
    coef(lm(yy~.,data=lm.d))
  }else stop("None of variables are selected.")

}
