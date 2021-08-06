#' \code{get.candidate}: Get candidate set
#'
#' Get the indices of the candidate set in the first stage
#'
#' @importFrom stats lm glm
#' @importFrom glmnet glmnet
#' @importFrom survival Surv coxph
#' @importFrom stats binomial deviance poisson
#' @param xs Standardized independent variables
#' @param ys Standardized dependent variable
#' @param family A description of the error distribution and link function to be
#'  used in the model. It can take the value of `\code{gaussian}`, `\code{binomial}`,
#'  `\code{poisson}`, and `\code{cox}`.
#'
#' @return A list of following components:
#' \describe{
#' \item{candidate.index}{A vector of indices of selected variables in the candidate set}
#' \item{lambda}{The \code{lambda} selected by generalized information criterion}
#' }

get.candidate <- function(xs, ys, family){

  n <- nrow(xs)

  if(family == "gaussian"){

    # lasso
    lasso <- glmnet(xs,ys)
    yhat.m <- predict(lasso,newx=xs)
    p.k <- lasso$df + 2

    lasso.lambda.index <- which.min(sapply(1:ncol(yhat.m),function(z)
      n*log(sum((ys-yhat.m[,z] )^2)) + p.k[z] * log(log(n))*log(p.k[z]) ) )
    lambda <- lasso$lambda[lasso.lambda.index]
    candidate.index <- which(coef(lasso, s = lambda)[-1] != 0)


  }else if (family == "binomial"){

    lasso.log <- glmnet(xs,ys,family = "binomial")

    yhat.m <- predict(lasso.log,newx=xs,type="response")
    p.k <- lasso.log$df + 1

    lasso.lambda.index <- which.min(sapply(1:ncol(yhat.m),function(z)
      binomial()$aic(ys, rep(1,n), yhat.m[,z], rep(1,n)) +
        p.k[z] * log(log(n))*log(p.k[z]) ) )
    lambda <- lasso.log$lambda[lasso.lambda.index]
    candidate.index <- which(coef(lasso.log,s = lambda)[-1] != 0)

  }else if (family == "poisson"){

    lasso.poisson <- glmnet(xs,ys,family = "poisson")

    yhat.m <- predict(lasso.poisson,newx=xs,type="response")
    p.k <- lasso.poisson$df + 1

    lasso.lambda.index <- which.min(sapply(1:ncol(yhat.m),function(z)
      poisson()$aic(ys, rep(1,n), yhat.m[,z], rep(1,n)) +
        p.k[z] * log(log(n))*log(p.k[z]) ) )
    lambda <- lasso.poisson$lambda[lasso.lambda.index]
    candidate.index <- which(
      coef(lasso.poisson, s = lambda)[-1] != 0)

  }else if (family == "cox"){

    lasso.cox <- glmnet(xs,Surv(ys[,1], ys[,2]),family = "cox")
    p.k <- lasso.cox$df

    lasso.lambda.index <- which.min(sapply(1:length(p.k),function(z)
      -(lasso.cox$nulldev - deviance(lasso.cox))[z] +
        p.k[z] * log(log(n))*log(p.k[z]) ) )
    lambda <- lasso.cox$lambda[lasso.lambda.index]
    candidate.index <- which(as.numeric(coef(lasso.cox, s = lambda)) != 0)

  }

  return(list(candidate.index, lambda))
}


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
#' @param gvif A logical operator indicating whether a generalized variance inflation factor-adjusted
#' null bound is used. Default is FALSE.
#'
#' @return A list of following components:
#' \describe{
#' \item{out.sgpv}{A vector of indices of selected variables}
#' \item{null.bound.p}{Null bound in the SGPV screening}
#' \item{pe}{Point estimates in the candidate set}
#' \item{lb}{Lower bounds of effect estimates in the candidate set}
#' \item{ub}{Upper bounds of effect estimates in the candidate set}
#' }

get.var <- function(candidate.index, xs, ys, family, gvif) {

  pp <- length(candidate.index)

  if ( pp == 0) {
    out.sgpv <- integer(0)
    null.bound.p <- NULL
    pe <- NULL
    se <- NULL
    lb <- NULL
    ub <- NULL
  } else {
    if (family == "gaussian") {

      mod <- lm(ys ~ xs[, candidate.index])
      pe <- summary(mod)$coef[-1, 1]
      se <- summary(mod)$coef[-1, 2]

    } else if (family == "binomial") {

      mod <- glm(ys ~ xs[, candidate.index],
                   family = family, method = "brglmFit", type = "MPL_Jeffreys"
      )
      pe <- coef(mod)[-1]
      se <- summary(mod)$coef[-1, 2]

    } else if (family == "poisson") {

      mod <- glm(ys ~ xs[, candidate.index], family = family)
      pe <- coef(mod)[-1]
      se <- summary(mod)$coef[-1, 2]

    } else {

      mod <- coxph(Surv(ys[, 1], ys[, 2]) ~ xs[, candidate.index])
      pe <- coef(mod)
      se <- summary(mod)$coef[, 3]

    }

    if(length(pe) != length(se)){
      name.drop <- setdiff(names(pe), names(se))
      pe <- pe[!names(pe) %in% name.drop]
      se <- se[!names(se) %in% name.drop]
      candidate.index <- candidate.index[!is.na(pe)]
    }

    if(gvif){

      if(pp > 1){
        gvif.m <- gvif(mod, family = family)
      }else{
        gvif.m <- 1
      }

      null.bound.p <- as.numeric(se %*% (1/gvif.m) /pp)
    }else{
      null.bound.p <- mean(se)
    }

    # screen variables
    out.sgpv <- candidate.index[which(abs(pe) > 1.96 * se + null.bound.p)]

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

#' \code{gvif}: Get GVIF for each variable
#'
#' Get generalized variance inflation factor (GVIF) for each variable.
#' See Fox (1992) \doi{10.1080/01621459.1992.10475190} for more details on how to calculate GVIF.
#'
#' @importFrom stats cov2cor vcov
#' @param mod A model object with at least two explanatory variables
#' @param family A description of the error distribution and link function to be
#'  used in the model. It can take the value of `\code{gaussian}`, `\code{binomial}`,
#'  `\code{poisson}`, and `\code{cox}`.
#'
#' @return A vector of GVIF for each variable in the model

gvif <- function(mod, family){

  if(family != "cox"){
    v <- vcov(mod)[-1,-1]
    R <- cov2cor(v)
  }else{
    v <- vcov(mod)
    R <- cov2cor(v)
  }

  na.ind <- anyNA(R)

  if(na.ind){
    na.index <- as.numeric(which(rowSums(is.na(R)) > 1))
    R[is.na(R)] <- 0
  }

  detR <- det(R)
  n.terms <- length(coef(mod)) - 1
  result <- numeric(n.terms)

  for (term in 1:n.terms) {
    result[term] <- det(as.matrix(R[term, term])) *
      det(as.matrix(R[-term, -term])) / detR
  }



  if(na.ind){
    result[-na.index]
  }else{
    result
  }

}

