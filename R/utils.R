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


#' Generate simulation data
#'
#' This function can be used to generate autoregressive simulation data
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats rnorm
#'
#' @param n The number of observations
#' @param p The number of explanatory variables
#' @param s The number of true signals
#' @param beta.min The smallest effect size in absolute value
#' @param beta.max The largest effect size in absolute value
#' @param rho The autocorrelation level
#' @param nu The signal to noise ratio
#' @return A list of following components:
#' \describe{
#' \item{X}{The generated explanatory variable matrix}
#' \item{Y}{A vector of outcome}
#' \item{index}{The indices of true signals }
#' }
#' @export
#'
#' @examples
#' # generate data
#' data <- gen.data()
#'
#' # extract x
#' x <- data[[1]]
#'
#' # extract y
#' y <- data[[2]]
#'
#' # extract the indices of true signals
#' index <- data[[3]]
gen.data <- function(n = 100, p = 50, s = 10, beta.min = 1,
                     beta.max = 5, rho = 0, nu = 2) {
  if (s %% 2 == 1) stop("s can only be an even number.")
  if (beta.min < 0 | beta.max < 0) stop("The absolute value of the true effect size can not be negative.")
  if (s > p) stop("The number of signals can not exceed the number of total variables.")
  if (rho < 0 | rho > 1) stop("rho should be between 0 and 1.")
  if (nu < 0) stop("SNR can not be negative.")

  # beta coefficients
  beta <- sample(c(
    seq(beta.min, beta.max, length.out = s) * c(rep(1, s / 2), rep(-1, s / 2)),
    rep(0, p - s)
  ))

  cov.structure <- matrix(0, p, p)
  for (i in 1:p) {
    for (j in 1:p) {
      cov.structure[i, j] <- rho^(abs(i - j))
    }
  }

  # mvrnorm for X
  X <- mvrnorm(n = n, rep(0, p), Sigma = cov.structure)

  # find index for signals
  index <- which(beta != 0)

  # random error
  sigma <- sqrt(c(t(beta) %*% cov.structure %*% beta / nu))

  # generate Y
  Y <- rnorm(X %*% beta, X %*% beta, sd = sigma)

  return(list(X, Y, index))
}
