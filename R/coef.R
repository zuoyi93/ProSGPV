#' coef S3 method
#'
#' Extract coefficients from the model fit
#'
#' @importFrom stats coef lm
#' @param object An \code{sgpv} object
#' @param ... Other arguments
#'
#' @return Coefficients in the OLS model
#' @export

coef.sgpv <- function(object,...){

  lm.d <- data.frame(yy=object$y,xx = object$x[,object$var.index])
  colnames(lm.d)[-1] <- object$var.label
  coef(lm(yy~.,data=lm.d))

}
