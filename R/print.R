#' print S3 method
#'
#' @param x An \code{sgpv} object
#' @param ... Other \code{print} arguments
#'
#' @return Variable selection results
#' @export
#' @examples
#' # more examples at https://github.com/zuoyi93/ProSGPV

print.sgpv <- function(x,...){
  if(length(x$var.index)>0){
    cat("Selected variables are", x$var.label)
  }else cat("None of variables are selected.")

}
