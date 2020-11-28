#' pro.sgpv function
#'
#' This function outputs the variable selection results
#' from either one-stage algorithm or two-stage algorithm.
#'
#' @importFrom stats complete.cases coef lm predict
#' @importFrom glmnet cv.glmnet
#' @param x Independent variables, can be a \code{matrix} or a \code{data.frame}
#' @param y Dependent variable, can be a \code{vector} or a column from a \code{data.frame}
#' @param stage Algorithm indicator. 1 denotes the one-stage algorithm and
#' 2 denotes the two-stage algorithm. Default is 1.
#'
#' @return A list of following components:
#' \describe{
#' \item{var.index}{A vector of indices of selected variables}
#' \item{var.label}{A vector of labels of selected variables}
#' \item{lambda}{Cross-validated lambda in the two-stage algorithm. \code{NULL} for the one-stage algorithm}
#' \item{x}{Input data \code{x}}
#' \item{y}{Input data \code{y}}
#' }
#' @export
#' @seealso
#' * [print.sgpv()] prints the variable selection results
#' * [coef.sgpv()] extracts coefficient estimates
#' * [summary.sgpv()] summarizes the OLS outputs
#' * [predict.sgpv()] predicts the outcome
#' * [plot.sgpv()] plots variable selection results
#' @examples
#'
#' # load the package
#' library(ProSGPV)
#'
#' # prepare the data
#' x <- t.housing[, -ncol(t.housing)]
#' y <- t.housing$V9
#'
#' # run one-stage algorithm
#' out.sgpv.1 <- pro.sgpv(x = x, y = y, stage = 1)
#'
#' # More examples at https://github.com/zuoyi93/ProSGPV
pro.sgpv <- function(x, y, stage = c(1, 2)) {
  if (!(stage %in% 1:2)) stop("Stage only takes value of 1 or 2.")

  if (nrow(x) != length(y)) stop("Input x and y have different number of observations")

  if (!is.numeric(as.matrix(x)) | !is.numeric(y)) stop("The input data have non-numeric values.")

  if (any(complete.cases(x) == F) | any(complete.cases(y) == F)) {
    warning("Only complete records will be used.")
    comp.index <- complete.cases(data.frame(x, y))
    x <- x[comp.index, ]
    y <- y[comp.index]
  }

  if (is.null(colnames(x))) colnames(x) <- paste("V", 1:ncol(x), sep = "")

  xs <- scale(x)
  ys <- scale(y)

  if (stage == 2) {
    lasso.cv <- cv.glmnet(xs, ys)
    lambda <- lasso.cv$lambda.1se
    candidate.index <- which(coef(lasso.cv, s = lambda)[-1] != 0)
  } else {
    candidate.index <- 1:ncol(xs)
    lambda <- NULL
  }

  out.sgpv <- get.var(candidate.index, xs, ys)

  out <- list(
    var.index = out.sgpv,
    var.label = colnames(x)[out.sgpv],
    lambda = lambda,
    x = x,
    y = y
  )

  class(out) <- "sgpv"
  return(out)
}

#' Print variable selection results
#'
#' S3 method \code{print} for an S3 object of class \code{sgpv}
#'
#' @param x An \code{sgpv} object
#' @param ... Other \code{print} arguments
#'
#' @return Variable selection results
#' @export
#' @examples
#'
#' # load the package
#' library(ProSGPV)
#'
#' # prepare the data
#' x <- t.housing[, -ncol(t.housing)]
#' y <- t.housing$V9
#'
#' # run one-stage algorithm
#' out.sgpv.1 <- pro.sgpv(x = x, y = y, stage = 1)
#'
#' out.sgpv.1
print.sgpv <- function(x, ...) {
  if (length(x$var.index) > 0) {
    cat("Selected variables are", x$var.label)
  } else {
    cat("None of variables are selected.")
  }
}

#' Extract coefficients from the model fit
#'
#' S3 method \code{coef} for an S3 object of class \code{sgpv}
#'
#' @importFrom stats coef lm
#' @param object An \code{sgpv} object
#' @param ... Other \code{coef} arguments
#'
#' @return Coefficients in the OLS model
#' @export
#' @examples
#'
#' # load the package
#' library(ProSGPV)
#'
#' # prepare the data
#' x <- t.housing[, -ncol(t.housing)]
#' y <- t.housing$V9
#'
#' # run one-stage algorithm
#' out.sgpv.1 <- pro.sgpv(x = x, y = y, stage = 1)
#'
#' # get coefficients
#' coef(out.sgpv.1)
coef.sgpv <- function(object, ...) {
  if (length(object$var.index) > 0) {
    lm.d <- data.frame(yy = object$y, xx = object$x[, object$var.index])
    colnames(lm.d)[-1] <- object$var.label
    coef(lm(yy ~ ., data = lm.d))
  } else {
    message("None of variables are selected. Therefore, all coefficient estimates are 0.")
  }
}

#' Summary of the OLS model on selected variables
#'
#' S3 method \code{summary} for an S3 object of class \code{sgpv}
#'
#' @param object An \code{sgpv} object
#' @param ... Other arguments
#'
#' @return Summary of an OLS model
#' @export
#' @examples
#'
#' # load the package
#' library(ProSGPV)
#'
#' # prepare the data
#' x <- t.housing[, -ncol(t.housing)]
#' y <- t.housing$V9
#'
#' # run one-stage algorithm
#' out.sgpv.1 <- pro.sgpv(x = x, y = y, stage = 1)
#'
#' # get regression summary
#' summary(out.sgpv.1)
summary.sgpv <- function(object, ...) {
  if (length(object$var.index) > 0) {
    lm.d <- data.frame(yy = object$y, xx = object$x[, object$var.index])
    colnames(lm.d)[1] <- "Response"
    colnames(lm.d)[-1] <- object$var.label
    summary(lm(Response ~ ., data = lm.d))
  } else {
    message("None of variables are selected.")
    message("Therefore, the summary is shown for the model with intercept only")
    lm.d <- data.frame(yy = object$y)
    colnames(lm.d)[1] <- "Response"
    summary(lm(Response ~ 1, data = lm.d))
  }
}

#' Prediction using the fitted model
#'
#' S3 method \code{predict} for an object of class \code{sgpv}
#'
#' @importFrom stats lm predict
#' @param object An \code{sgpv} objectect
#' @param newx Prediction data set
#' @param ... Other \code{predict} arguments
#'
#' @return Predicted values
#' @export
#' @examples
#'
#' # load the package
#' library(ProSGPV)
#'
#' # prepare the data
#' x <- t.housing[, -ncol(t.housing)]
#' y <- t.housing$V9
#'
#' # run one-stage algorithm
#' out.sgpv.1 <- pro.sgpv(x = x, y = y, stage = 1)
#'
#' predict(out.sgpv.1)
predict.sgpv <- function(object, newx, ...) {
  if (length(object$var.index) > 0) {
    lm.d <- data.frame(yy = object$y, xx = object$x[, object$var.index])
    colnames(lm.d)[-1] <- object$var.label
    lm.m <- lm(yy ~ ., data = lm.d)

    if (missing(newx)) newx <- object$x

    predict(lm.m, newx)
  } else {
    message("None of variables are selected.")
    message("Therefore, the prediction is based on the intercept only model.")

    lm.d <- data.frame(yy = object$y)
    lm.m <- lm(yy ~ 1, data = lm.d)
    predict(lm.m)
  }
}

#' Plot variable selection results
#'
#' S3 method \code{plot} for an object of class \code{sgpv}. This function plots
#' the fully relaxed lasso solution path on
#' the standardized scale and the final variable selection results
#'
#' @importFrom graphics abline axis lines mtext polygon
#' @param x An \code{sgpv} object
#' @param lpv Lines per variable. It can take the value of 1 meaning that only the
#' bound that is closest to the null will be plotted, or the value of 3 meaning that
#' point estimates as well as 95% confidence interval will be plotted. Default is 3.
#' @param lambda.max The maximum lambda on the plot. Default is \code{NULL}.
#' @param ... Other \code{plot} arguments
#'
#' @return NULL
#' @export
#' @examples
#'
#' # load the package
#' library(ProSGPV)
#'
#' # prepare the data
#' x <- t.housing[, -ncol(t.housing)]
#' y <- t.housing$V9
#'
#' # two-stage algorithm
#' out.sgpv.2 <- pro.sgpv(x = x, y = y, stage = 2)
#'
#' # plot the fully relaxed lasso solution path and final solution
#' plot(out.sgpv.2)
#'
#' # zoom in a little bit
#' plot(out.sgpv.2, lambda.max = 0.01)
#'
#' # only plot one confidence bound
#' plot(out.sgpv.2, lpv = 1, lambda.max = 0.01)
plot.sgpv <- function(x, lpv = 3, lambda.max = NULL, ...) {
  if (is.null(x$lambda)) stop("One-stage algorithm doesn't have the plot function.")
  if (!lpv %in% c(1, 3)) stop("lpv argument only takes values of 1 and 3.")

  if (length(x$var.index) == 0) message("None of variables are selected")

  # get information from data
  p <- ncol(x$x)
  x.names <- colnames(x$x)

  # standardize data
  xs <- scale(x$x)
  ys <- scale(x$y)

  # maximum lambda
  if (is.null(lambda.max)) lambda.max <- max(abs(sapply(1:ncol(xs), function(z) xs[, z] %*% ys)) / nrow(xs)) * 1.1

  step <- lambda.max / 100

  # get a sequence of lambda
  lambda.seq <- seq(0, lambda.max, step)

  # fit lasso once
  lasso <- glmnet(xs, ys)

  # get coefficient estimates at each lambda
  results <- sapply(lambda.seq, function(z) get.coef(xs = xs, ys = ys, lambda = z, lasso = lasso))

  # prepare data to plot
  to.plot <- data.frame(
    lambda = rep(lambda.seq, each = p),
    v = rep(x.names, length(lambda.seq)),
    pe = c(results[1:p, ]),
    lb = c(results[(p + 1):(2 * p), ]),
    ub = c(results[(2 * p + 1):(3 * p), ])
  )

  if (lpv == 1) {
    to.plot$bound <- ifelse(abs(to.plot$lb) < abs(to.plot$ub), to.plot$lb, to.plot$ub)
    plot.d <- to.plot[, c("lambda", "v", "bound")]

    # find the location of x.names
    location.beta <- to.plot$bound[to.plot$lambda == 0]
  } else if (lpv == 3) {
    plot.d <- to.plot

    # find the location of x.names
    location.beta <- to.plot$pe[to.plot$lambda == 0]
  }

  # get the indices of selected variables
  selected.index <- x$var.index
  black.index <- setdiff(1:p, selected.index)

  # change the color of the variables
  color.use <- c(
    rep("black", length(black.index)),
    rep("blue", length(selected.index))
  )

  location.beta <- location.beta[c(black.index, selected.index)]
  var.axis <- x.names[c(black.index, selected.index)]

  # find the limit of the canvas
  if (lpv == 3) {
    ylim <- c(
      min(c(location.beta, to.plot$lb, to.plot$ub)) * 1.01,
      max(c(location.beta, to.plot$lb, to.plot$ub)) * 1.01
    )

    ylim <- c(-max(abs(ylim)), max(abs(ylim)))
    ytick <- c(
      -round(max(abs(ylim)), 2), -round(max(abs(ylim)), 2) / 2, 0,
      round(max(abs(ylim)), 2) / 2, round(max(abs(ylim)), 2)
    )
  } else if (lpv == 1) {
    ylim <- c(
      min(plot.d$bound) * 1.01,
      max(plot.d$bound) * 1.01
    )

    ylim <- c(-max(abs(ylim)), max(abs(ylim)))
    ytick <- c(
      -round(max(abs(ylim)), 2), -round(max(abs(ylim)), 2) / 2, 0,
      round(max(abs(ylim)), 2) / 2, round(max(abs(ylim)), 2)
    )
  }

  # create a data set for the null bound
  n.bound <- results[(3 * p + 1), ]

  # find the lambda.1se
  vlambda <- x$lambda

  # plot the results
  if (lpv == 1) {
    xvals <- split(plot.d$lambda, plot.d$v)
    yvals <- split(plot.d$bound, plot.d$v)

    plot(lambda.seq,
      type = "n", ylim = ylim, xlim = c(0, lambda.max),
      xlab = expression(lambda), ylab = "Bound that is closer to the null",
      main = "Solution to the two-stage algorithm with one line per variable",
      axes = F, frame.plot = T
    )
    axis(1, at = round(seq(0, lambda.max, length.out = 5), 3))
    axis(2, at = ytick, labels = c(ytick[1:2], 0, ytick[4:5]))
    mtext(var.axis, side = 2, at = location.beta, col = color.use)
    mtext(bquote(lambda["1se"]), side = 1, at = vlambda)
    polygon(c(lambda.seq, rev(lambda.seq)), c(-n.bound, rev(n.bound)), col = "grey", border = "grey")
    invisible(mapply(lines, xvals, yvals, col = 1:p))
    abline(h = 0)
    abline(v = vlambda, lty = 2)
  } else if (lpv == 3) {
    plot(lambda.seq,
      type = "n", ylim = ylim, xlim = c(0, lambda.max),
      xlab = expression(lambda), ylab = "Point estimates and confidence intervals",
      main = "Solution to the two-stage algorithm with three lines per variable",
      axes = F, frame.plot = T
    )
    axis(1, at = round(seq(0, lambda.max, length.out = 5), 3))
    axis(2, at = ytick, labels = c(ytick[1:2], 0, ytick[4:5]))

    # null region
    polygon(c(lambda.seq, rev(lambda.seq)), c(-n.bound, rev(n.bound)), col = "grey", border = "grey")

    # text on axis
    mtext(var.axis, side = 2, at = location.beta, col = color.use)
    mtext(bquote(lambda["1se"]), side = 1, at = vlambda)

    # point estimates
    xvals <- split(plot.d$lambda, plot.d$v)
    yvals <- split(plot.d$pe, plot.d$v)
    invisible(mapply(lines, xvals, yvals, col = 1:p))

    # upper bound
    xvals <- split(plot.d$lambda, plot.d$v)
    yvals <- split(plot.d$ub, plot.d$v)
    invisible(mapply(lines, xvals, yvals, col = 1:p, lty = 2))

    # lower bound
    xvals <- split(plot.d$lambda, plot.d$v)
    yvals <- split(plot.d$lb, plot.d$v)
    invisible(mapply(lines, xvals, yvals, col = 1:p, lty = 2))

    abline(h = 0)
    abline(v = vlambda, lty = 2)
  }
}
