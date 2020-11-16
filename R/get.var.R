#' Get indices
#'
#' Get the indices of the variables selected by the algorithm
#'
#' @importFrom stats lm
#' @param candidate.index Indices of the candidate set
#' @param xs Standardized independent variables
#' @param ys Standardized dependent variable
#'
#' @return Indices of variables selected
#' @export



get.var <- function(candidate.index,xs,ys){

  if(length(candidate.index)==0){
    out.sgpv <- integer(0)
  }else{

    # run fully relaxed LASSO
    f.l <- lm(ys~xs[,candidate.index])

    # get confidence bands
    pe <- summary(f.l)$coef[-1,1]
    se <- summary(f.l)$coef[-1,2]
    null.bound.p <- mean(se)

    # screen variables
    out.sgpv <- candidate.index[which(abs(pe)> 1.96*se+null.bound.p )]
  }

  return( out.sgpv)
}
