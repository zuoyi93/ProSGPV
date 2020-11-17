#' summary S3 method
#'
#' Provide summary of the OLS model on the selected variables
#'
#' @param object An \code{sgpv} object
#' @param ... Other arguments
#'
#' @return Summary of an OLS model
#' @export
#'
#' @examples
#' # more examples at https://github.com/zuoyi93/ProSGPV
#'
summary.sgpv <- function(object,...){

  if(length(object$var.index)>0){
    lm.d <- data.frame(yy=object$y,xx = object$x[,object$var.index])
    colnames(lm.d)[1] <- "Response"
    colnames(lm.d)[-1] <- object$var.label
    summary(lm(Response~.,data=lm.d))
  }else stop("None of variables are selected.")

}
