#' predict S3 method
#'
#' Prediction using the fitted model
#'
#' @importFrom stats lm predict
#' @param object An \code{sgpv} objectect
#' @param newx Prediction data set
#' @param ... Other \code{predict} arguments
#'
#' @return Predicted values
#' @export
#' @examples
#' # more examples at https://github.com/zuoyi93/ProSGPV


predict.sgpv <- function(object,newx,...){

  if(length(object$var.index)>0){
    lm.d <- data.frame(yy=object$y,xx = object$x[,object$var.index])
    colnames(lm.d)[-1] <- object$var.label
    lm.m <- lm(yy~.,data=lm.d)

    if(missing(newx)) newx <- object$x

    predict(lm.m,newx)
  }else stop("None of variables are selected and prediction is not available.")


}
