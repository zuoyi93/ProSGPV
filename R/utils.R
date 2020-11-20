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

get.var <- function(candidate.index, xs, ys) {
  if (length(candidate.index) == 0) {
    out.sgpv <- integer(0)
  } else {

    # run fully relaxed LASSO
    f.l <- lm(ys ~ xs[, candidate.index])

    # get confidence bands
    pe <- summary(f.l)$coef[-1, 1]
    se <- summary(f.l)$coef[-1, 2]
    null.bound.p <- mean(se)

    # screen variables
    out.sgpv <- candidate.index[which(abs(pe) > 1.96 * se + null.bound.p)]
  }

  return(out.sgpv)
}


#' Get coefficients at each \code{lambda}
#'
#' Get the coefficients and confidence intervals from regression at each \code{lambda}
#' as well as the null bound in SGPVs
#' @importFrom glmnet glmnet
#' @param xs Standardized design matrix
#' @param ys Standardized outcome
#' @param lambda \code{lambda} in the lasso
#' @param lasso An \code{glmnet} object
#'
#' @return A vector that contains the point estimates, confidence intervals and the null bound

get.coef <- function(xs, ys, lambda, lasso) {
  p <- ncol(xs)

  # evaluate lasso at lambda
  index <- which(coef(lasso, s = lambda)[-1] != 0)

  # define the output
  out.coef <- numeric(p)
  out.lb <- numeric(p)
  out.ub <- numeric(p)

  if (lambda == 0) {

    # full ols model
    full.ols <- lm(ys ~ xs)

    pe <- summary(full.ols)$coef[-1, 1]
    se <- summary(full.ols)$coef[-1, 2]
    lb <- pe - 1.96 * se
    ub <- pe + 1.96 * se

    null.bound.lasso <- mean(summary(full.ols)$coef[-1, 2])

    out.coef <- as.numeric(pe)
    out.lb <- as.numeric(lb)
    out.ub <- as.numeric(ub)
  } else if (length(index) != 0) {

    # run fully relaxed LASSO
    f.l <- lm(ys ~ xs[, index])

    # get confidence bands
    pe <- summary(f.l)$coef[-1, 1]
    se <- summary(f.l)$coef[-1, 2]
    lb <- pe - 1.96 * se
    ub <- pe + 1.96 * se

    null.bound.lasso <- mean(summary(f.l)$coef[-1, 2])

    out.coef[index] <- as.numeric(pe)
    out.lb[index] <- as.numeric(lb)
    out.ub[index] <- as.numeric(ub)
  } else if (length(index) == 0) {

    # intercept only model
    null.bound.lasso <- 0
  }

  return(c(out.coef, out.lb, out.ub, null.bound.lasso))
}
