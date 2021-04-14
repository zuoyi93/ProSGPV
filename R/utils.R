#' \code{get.var}: Get indices
#'
#' Get the indices of the variables selected by the algorithm
#'
#' @importFrom stats lm glm
#' @importFrom survival Surv coxph
#' @importFrom brglm2 brglmFit
#' @param candidate.index Indices of the candidate set
#' @param xs Standardized independent variables
#' @param ys Standardized dependent variable
#' @param family A description of the error distribution and link function to be
#'  used in the model. It can take the value of `\code{gaussian}`, `\code{binomial}`,
#'  `\code{poisson}`, and `\code{cox}`.
#'
#' @return A list of following components:
#' \describe{
#' \item{out.sgpv}{A vector of indices of selected variables}
#' \item{null.bound.p}{Null bound in the SGPV screening}
#' \item{pe}{Point estimates in the candidate set}
#' \item{lb}{Lower bounds of effect estimates in the candidate set}
#' \item{ub}{Upper bounds of effect estimates in the candidate set}
#' }

get.var <- function(candidate.index, xs, ys, family) {
  if (length(candidate.index) == 0) {
    out.sgpv <- integer(0)
    null.bound.p <- NULL
    pe <- NULL
    se <- NULL
    lb <- NULL
    ub <- NULL
  } else {
    if (family == "gaussian") {
      f.l <- lm(ys ~ xs[, candidate.index])
      pe <- summary(f.l)$coef[-1, 1]
      se <- summary(f.l)$coef[-1, 2]
    } else if (family == "binomial") {
      glm.m <- glm(ys ~ xs[, candidate.index],
        family = family, method = "brglmFit", type = "MPL_Jeffreys"
      )
      pe <- coef(glm.m)[-1]
      se <- summary(glm.m)$coef[-1, 2]
    } else if (family == "poisson") {
      glm.m <- glm(ys ~ xs[, candidate.index], family = family)
      pe <- coef(glm.m)[-1]
      se <- summary(glm.m)$coef[-1, 2]
    } else {
      cox.m <- coxph(Surv(ys[, 1], ys[, 2]) ~ xs[, candidate.index])
      pe <- coef(cox.m)
      se <- summary(cox.m)$coef[, 3]
    }

    if(length(pe) == length(se)){
      null.bound.p <- mean(se)

      # screen variables
      out.sgpv <- candidate.index[which(abs(pe) > 1.96 * se + null.bound.p)]
    }else{

      name.drop <- setdiff(names(pe), names(se))
      pe <- pe[!names(pe) %in% name.drop]
      se <- se[!names(se) %in% name.drop]
      candidate.index <- candidate.index[!names(candidate.index) %in% name.drop]

      null.bound.p <- mean(se)

      # screen variables
      out.sgpv <- candidate.index[which(abs(pe) > 1.96 * se + null.bound.p)]

    }

  }

  return(list(
    out.sgpv,
    null.bound.p,
    pe,
    pe - 1.96 * se,
    pe + 1.96 * se
  ))
}


#' \code{get.coef}: Get coefficients at each \code{lambda}
#'
#' Get the coefficients and confidence intervals from regression at each \code{lambda}
#' as well as the null bound in SGPVs
#' @importFrom glmnet glmnet
#' @importFrom survival coxph Surv
#' @param xs Standardized design matrix
#' @param ys Standardized outcome
#' @param lambda \code{lambda} in the lasso
#' @param lasso An \code{glmnet} object
#' @param family A description of the error distribution and link function to be
#'  used in the model. It can take the value of `\code{gaussian}`, `\code{binomial}`,
#'  `\code{poisson}`, and `\code{cox}`.
#'
#' @return A vector that contains the point estimates, confidence intervals and the null bound

get.coef <- function(xs, ys, lambda, lasso, family) {
  p <- ncol(xs)

  # evaluate lasso at lambda
  if (family != "cox") {
    index <- which(coef(lasso, s = lambda)[-1] != 0)
  } else {
    index <- which(coef(lasso, s = lambda) != 0)
  }

  # define the output
  out.coef <- numeric(p)
  out.lb <- numeric(p)
  out.ub <- numeric(p)

  if (lambda == 0) {
    if (family == "gaussian") {
      full.m <- lm(ys ~ xs)
    } else if (family == "binomial") {
      full.m <- glm(ys ~ xs,
        family = "binomial",
        method = "brglmFit", type = "MPL_Jeffreys"
      )
    } else if (family == "poisson") {
      full.m <- glm(ys ~ xs, family = "poisson")
    } else {
      full.m <- coxph(Surv(ys[, 1], ys[, 2]) ~ xs)
    }

    if (family != "cox") {
      pe <- coef(full.m)[-1]
      se <- summary(full.m)$coef[-1, 2]
    } else {
      pe <- coef(full.m)
      se <- summary(full.m)$coef[, 3]
    }

    if (any(is.na(pe))) {
      index.na <- as.numeric(which(is.na(pe)))
      pe[is.na(pe)] <- 0

      for (i in seq_along(index.na)) {
        se <- append(se, F, after = index.na[i] + i - 1)
      }
    }

    lb <- pe - 1.96 * se
    ub <- pe + 1.96 * se

    null.bound.lasso <- mean(se)
    out.coef <- as.numeric(pe)
    out.lb <- as.numeric(lb)
    out.ub <- as.numeric(ub)
  } else if (length(index) != 0) {
    if (family == "gaussian") {
      full.m <- lm(ys ~ xs[, index])
    } else if (family == "binomial") {
      full.m <- glm(ys ~ xs[, index],
        family = "binomial",
        method = "brglmFit", type = "MPL_Jeffreys"
      )
    } else if (family == "poisson") {
      full.m <- glm(ys ~ xs[, index], family = "poisson")
    } else {
      full.m <- coxph(Surv(ys[, 1], ys[, 2]) ~ xs[, index])
    }

    if (family != "cox") {
      pe <- coef(full.m)[-1]
      se <- summary(full.m)$coef[-1, 2]
    } else {
      pe <- coef(full.m)
      se <- summary(full.m)$coef[, 3]
    }

    lb <- pe - 1.96 * se
    ub <- pe + 1.96 * se

    null.bound.lasso <- mean(se)
    out.coef[index] <- as.numeric(pe)
    out.lb[index] <- as.numeric(lb)
    out.ub[index] <- as.numeric(ub)
  } else if (length(index) == 0) {

    # intercept only model
    null.bound.lasso <- 0
  }

  return(c(out.coef, out.lb, out.ub, null.bound.lasso))
}


#' \code{gen.sim.data}: Generate simulation data
#'
#' This function can be used to generate autoregressive simulation data
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats rnorm rbinom rexp rpois runif
#' @param n Number of observations. Default is 100.
#' @param p Number of explanatory variables. Default is 50.
#' @param s Number of true signals. It can only be an even number. Default is 10.
#' @param family A description of the error distribution and link function to be
#'  used in the model. It can take the value of `\code{gaussian}`, `\code{binomial}`,
#'  `\code{poisson}`, and `\code{cox}`. Default is `\code{gaussian}`
#' @param beta.min The smallest effect size in absolute value. Default is 1.
#' @param beta.max The largest effect size in absolute value. Default is 5.
#' @param rho Autocorrelation level. A numerical value between -1 and 1. Default is 0.
#' @param nu Signal to noise ratio in linear regression. Default is 2.
#' @param sig Standard deviation in the design matrix. Default is 1.
#' @param intercept Intercept of the linear predictor in the GLM. Default is 0.
#' @param scale Scale parameter in the Weibull distribution. Default is 2.
#' @param shape Shape parameter in the Weibull distribution. Default is 1.
#' @param rateC Rate of censoring in the survival data. Default is 0.2.
#' @return A list of following components:
#' \describe{
#' \item{X}{The generated explanatory variable matrix}
#' \item{Y}{A vector of outcome. If \code{family} is `\code{cox}`, a two-column
#' object is returned where the first column is the time and the second column is
#' status (0 is censoring and 1 is event)}
#' \item{index}{The indices of true signals }
#' \item{beta}{The true coefficient vector of length \code{p}}
#' }
#' @export
#'
#' @examples
#' # generate data for linear regression
#' data.linear <- gen.sim.data(n = 20, p = 10, s = 4)
#'
#' # extract x
#' x <- data.linear[[1]]
#'
#' # extract y
#' y <- data.linear[[2]]
#'
#' # extract the indices of true signals
#' index <- data.linear[[3]]
#'
#' # extract the true coefficient vector
#' true.beta <- data.linear[[4]]
#'
#' # generate data for logistic regression
#' data.logistic <- gen.sim.data(n = 20, p = 10, s = 4, family = "binomial")
#'
#' # extract x
#' x <- data.logistic[[1]]
#'
#' # extract y
#' y <- data.logistic[[2]]
#'
#' # extract the indices of true signals
#' index <- data.logistic[[3]]
#'
#' # extract the true coefficient vector
#' true.beta <- data.logistic[[4]]
gen.sim.data <- function(n = 100, p = 50, s = 10,
                         family = c("gaussian", "binomial", "poisson", "cox"),
                         beta.min = 1, beta.max = 5, rho = 0, nu = 2,
                         sig = 1, intercept = 0,
                         scale = 2, shape = 1, rateC = 0.2) {
  if (s %% 2 == 1) stop("s can only be an even number.")
  if (beta.min < 0 | beta.max < 0) stop("The absolute value of the true effect size can not be negative.")
  if (rho < -1 | rho > 1) stop("rho should be between -1 and 1.")
  if (nu < 0) stop("SNR can not be negative.")

  if (missing(family)) family <- "gaussian"
  family <- match.arg(family)

  # beta coefficients
  beta <- sample(c(
    seq(beta.min, beta.max, length.out = s) * c(rep(1, s / 2), rep(-1, s / 2)),
    rep(0, p - s)
  ))

  cov.structure <- matrix(0, p, p)
  for (i in 1:p) {
    for (j in 1:p) {
      if (rho != 0) {
        cov.structure[i, j] <- sign(rho) * abs(rho)^(abs(i - j))
      } else {
        cov.structure[i, j] <- abs(rho)^(abs(i - j))
      }
    }
  }

  if (rho < 0) diag(cov.structure) <- 1

  # mvrnorm for X
  X <- mvrnorm(n = n, rep(0, p), Sigma = cov.structure * sig^2)

  # find index for signals
  index <- which(beta != 0)

  if (family == "gaussian") {

    # random error
    sigma <- sqrt(c(t(beta) %*% cov.structure %*% beta / nu))

    # generate Y
    Y <- rnorm(X %*% beta, X %*% beta, sd = sigma)
  } else if (family == "binomial") {

    # generate z
    z <- X %*% beta + intercept
    pr <- 1 / (1 + exp(-z))

    # generate Y
    Y <- rbinom(n, 1, pr)
  } else if (family == "poisson") {

    # generate z
    z <- X %*% beta + intercept

    # generate Y
    Y <- rpois(n, exp(z))
  } else if (family == "cox") {

    # generate z
    z <- X %*% beta + intercept

    # Weibull latent event times
    v <- runif(n)
    Time <- (-log(v) / (scale * exp(z)))^(1 / shape)

    # censoring times
    C <- rexp(n, rate = rateC)

    # follow-up times and event indicators
    time <- pmin(Time, C)
    status <- as.numeric(Time <= C)

    Y <- cbind(time, status)
  }

  return(list(X, Y, index, beta))
}

#' \code{one.time.size}: utility function for \code{which.sgpv} function
#'
#' Return the selected model as well as the model size in each iteration
#'
#' @param x Input X
#' @param y Input Y
#' @param family A description of the error distribution and link function to be
#'  used in the model. It can take the value of `\code{gaussian}`, `\code{binomial}`,
#'  `\code{poisson}`, and `\code{cox}`. Default is `\code{gaussian}`
#'
#' @return A list of following components:
#' \describe{
#' \item{model}{The indices of selected variables}
#' \item{size}{The size of the model}
#' }

one.time.size <- function(x, y, family) {
  out <- pro.sgpv(x, y, family = family)

  return(list(out$var.index, length(out$var.index)))
}

#' which.sgpv: Find the most frequent model selected by the two-stage ProSGPV
#'
#' Return the selected variables in the most frequent model selected by ProSGPV,
#' the random seed to produce the result, and a density plot of model size. Examples
#' can be found at https://github.com/zuoyi93/ProSGPV/tree/master/vignettes.
#'
#' @importFrom stats density
#'
#' @param object An \code{sgpv} object
#' @param num.sim Number of repetitions. Default is 1000
#'
#' @return A list of following components:
#' \describe{
#' \item{var.index}{The indices of selected variables in the most frequent model}
#' \item{var.label}{Labels of selected variables in the most frequent model}
#' \item{random.seed}{A random seed to reproduce the selection result}
#' \item{models}{A list of unqiue models sorted by frequency}
#' }
#' @export

which.sgpv <- function(object, num.sim = 100) {
  if (class(object) != "sgpv") stop("`object` has to be of class `sgpv`.")

  if (object$stage == 1) {
    if (length(object$var.label) > 0) {
      cat("The one-stage algorithm always selects", object$var.label, "\n")
    } else {
      cat("The one-stage algorithm always selects none of the variables.\n")
    }

    return(list(
      var.index = object$var.index,
      var.label = object$var.label,
      random.seed = 1,
      models = object$var.index
    ))
  } else { # two-stage algorithm

    temp <- replicate(num.sim, one.time.size(object$x, object$y, object$family))

    # the most frequent model
    out.1 <- names(sort(table(paste(temp[1, ])), decreasing = T)[1])
    if (!grepl(":", out.1, fixed = T)) {
      out.1 <- unlist(regmatches(out.1, gregexpr("\\(?[0-9,.]+", out.1)))
      out.1 <- as.numeric(gsub("\\(", "", gsub(",", "", out.1)))
    } else {
      out.1 <- as.numeric(unlist(regmatches(out.1, gregexpr("\\(?[0-9,.]+", out.1))))
      out.1 <- seq(out.1[1], out.1[2], 1)
    }

    # find the random seed to reproduce the result
    i <- 1
    while (T) {
      set.seed(i)
      if( 0 %in% out.1 ){
        if( (length(pro.sgpv(object$x, object$y,
                    family = object$family)$var.index) == 0) & (out.1 == 0) ) break
      }else{
        if (setequal(
          pro.sgpv(object$x, object$y, family = object$family)$var.index,
          out.1
        )) {
          break
        }
      }

      i <- i + 1
    }

    # make a histogram

    plot(density(unlist(temp[2, ])),
      main = "Density of model size",
      xlab = "Model size"
    )
    polygon(density(unlist(temp[2, ])), col = "red", border = "blue")

    return(list(
      var.index = out.1,
      var.label = colnames(object$x)[out.1],
      random.seed = i,
      models = sort(table(paste(temp[1, ])), decreasing = T)
    ))
  }
}
