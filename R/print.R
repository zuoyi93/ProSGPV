#' print S3 method
#'
#' @param x An \code{sgpv} object
#' @param ... Other arguments
#'
#' @return Variable selection results
#' @export

print.sgpv <- function(x,...){
  cat("Selected variables are", x$var.label)
}
