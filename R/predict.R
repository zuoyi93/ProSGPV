#' predict S3 method
#'
#' Prediction using the fitted model
#'
#' @importFrom stats lm predict
#' @param object An \code{sgpv} objectect
#' @param newx Prediction data set
#' @param ... Other arguments
#'
#' @return Predicted values
#' @export


predict.sgpv <- function(object,newx,...){

  lm.d <- data.frame(yy=object$y,xx = object$x[,object$var.index])
  colnames(lm.d)[-1] <- object$var.label
  lm.m <- lm(yy~.,data=lm.d)

  if(missing(newx)) newx <- object$x

  predict(lm.m,newx)

}
