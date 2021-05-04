#' \code{pro.sgpv} function
#'
#' This function outputs the variable selection results
#' from either one-stage algorithm or two-stage algorithm.
#'
#' @importFrom stats complete.cases coef lm predict
#' @importFrom glmnet glmnet
#' @param x Independent variables, can be a \code{matrix} or a \code{data.frame}
#' @param y Dependent variable, can be a \code{vector} or a column from a \code{data.frame}
#' @param stage Algorithm indicator. 1 denotes the one-stage algorithm and
#' 2 denotes the two-stage algorithm. Default is 2. When \code{n} is less than \code{p},
#' only the two-stage algorithm is available.
#' @param family A description of the error distribution and link function to be
#'  used in the model. It can take the value of `\code{gaussian}`, `\code{binomial}`,
#'  `\code{poisson}`, and `\code{cox}`. Default is `\code{gaussian}`
#'
#' @return A list of following components:
#' \describe{
#' \item{var.index}{A vector of indices of selected variables}
#' \item{var.label}{A vector of labels of selected variables}
#' \item{lambda}{\code{lambda} selected by generalized information criterion in the two-stage algorithm. \code{NULL} for the one-stage algorithm}
#' \item{x}{Input data \code{x}}
#' \item{y}{Input data \code{y}}
#' \item{family}{\code{family} from the input}
#' \item{stage}{\code{stage} from the input}
#' \item{null.bound}{Null bound in the SGPV screening}
#' \item{pe.can}{Point estimates in the candidate set}
#' \item{lb.can}{Lower bounds of CI in the candidate set}
#' \item{ub.can}{Upper bounds of CI in the candidate set}
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
#' # prepare the data
#' x <- t.housing[, -ncol(t.housing)]
#' y <- t.housing$V9
#'
#' # run ProSGPV in linear regression
#' out.sgpv <- pro.sgpv(x = x, y = y)
#'
#' # More examples at https://github.com/zuoyi93/ProSGPV/tree/master/vignettes
pro.sgpv <- function(x, y, stage = c(1, 2),
                     family = c("gaussian", "binomial", "poisson", "cox")) {
  if (!is.numeric(as.matrix(x)) | !is.numeric(y)) stop("The input data have non-numeric values.")

  if (any(complete.cases(x) == F) | any(complete.cases(y) == F)) {
    warning("Only complete records will be used.\n")
    comp.index <- complete.cases(data.frame(x, y))
    if (family != "cox") {
      x <- x[comp.index, ]
      y <- y[comp.index]
    } else {
      x <- x[comp.index, ]
      y <- y[comp.index, ]
    }
  }

  if (missing(stage)) stage <- 2
  if (!stage %in% c(1, 2)) stop("`stage` only takes 1 or 2.")

  if (missing(family)) family <- "gaussian"
  family <- match.arg(family)

  # when p > n, only two-stage is available
  if (stage == 1 & nrow(x) < ncol(x)) stage <- 2

  if (is.null(colnames(x))) colnames(x) <- paste("V", 1:ncol(x), sep = "")

  # standardize inputs in linear regression
  if (family == "gaussian") {
    xs <- scale(x)
    ys <- scale(y)
  } else {
    xs <- as.matrix(x)
    ys <- y
  }

  # get candidate set
  if (stage == 2) {
    temp0 <- get.candidate(xs, ys, family)
    candidate.index <- temp0[[1]]
    lambda <- temp0[[2]]
  } else {
    candidate.index <- 1:ncol(xs)
    lambda <- NULL
  }

  temp <- get.var(candidate.index, xs, ys, family)
  out.sgpv <- temp[[1]]
  null.bound <- temp[[2]]
  pe.can <- temp[[3]]
  lb.can <- temp[[4]]
  ub.can <- temp[[5]]

  out <- list(
    var.index = out.sgpv,
    var.label = colnames(x)[out.sgpv],
    lambda = lambda,
    x = data.frame(x),
    y = y,
    family = family,
    stage = stage,
    null.bound = null.bound,
    pe.can = pe.can,
    lb.can = lb.can,
    ub.can = ub.can
  )

  class(out) <- "sgpv"
  return(out)
}

#' \code{print.sgpv}: Print variable selection results
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
    cat("Selected variables are", x$var.label, "\n")
  } else {
    cat("None of variables are selected.\n")
  }
}

#' \code{coef.sgpv}: Extract coefficients from the model fit
#'
#' S3 method \code{coef} for an S3 object of class \code{sgpv}
#'
#' @importFrom stats coef lm
#' @importFrom survival coxph Surv
#' @importFrom brglm2 brglmFit
#' @param object An \code{sgpv} object
#' @param ... Other \code{coef} arguments
#'
#' @return Coefficients in the OLS model
#' @export
#' @examples
#'
#' # prepare the data
#' x <- t.housing[, -ncol(t.housing)]
#' y <- t.housing$V9
#'
#' # run one-stage algorithm
#' out.sgpv <- pro.sgpv(x = x, y = y)
#'
#' # get coefficients
#' coef(out.sgpv)
coef.sgpv <- function(object, ...) {
  out.coef <- numeric(ncol(object$x))

  if (length(object$var.index) > 0) {
    data.d <- data.frame(yy = object$y, xx = object$x[, object$var.index])

    if (object$family == "gaussian") {
      out.sgpv.coef <- coef(lm(yy ~ ., data = data.d))[-1]
    } else if (object$family == "binomial") {
      out.sgpv.coef <- coef(glm(yy ~ .,
        family = "binomial", data = data.d,
        method = "brglmFit", type = "MPL_Jeffreys"
      ))[-1]
    } else if (object$family == "poisson") {
      out.sgpv.coef <- coef(glm(yy ~ ., family = "poisson", data = data.d))[-1]
    } else {
      cox.m <- coxph(Surv(object$y[, 1], object$y[, 2]) ~
      as.matrix(object$x[, object$var.index]))
      out.sgpv.coef <- coef(cox.m)
    }

    for (i in 1:length(object$var.index)) {
      out.coef[object$var.index[i]] <- out.sgpv.coef[i]
    }
  }
  out.coef
}

#' \code{summary.sgpv}: Summary of the final model
#'
#' S3 method \code{summary} for an S3 object of class \code{sgpv}
#'
#' @importFrom brglm2 brglmFit
#' @param object An \code{sgpv} object
#' @param ... Other arguments
#'
#' @return Summary of a model
#' @export
#' @examples
#'
#' # prepare the data
#' x <- t.housing[, -ncol(t.housing)]
#' y <- t.housing$V9
#'
#' # run one-stage algorithm
#' out.sgpv <- pro.sgpv(x = x, y = y)
#'
#' # get regression summary
#' summary(out.sgpv)
summary.sgpv <- function(object, ...) {
  if (length(object$var.index) > 0) {
    if (object$family != "cox") {
      data.d <- data.frame(yy = object$y, xx = object$x[, object$var.index])
      colnames(data.d)[1] <- "Response"
      colnames(data.d)[-1] <- object$var.label

      if (object$family == "gaussian") {
        summary(lm(Response ~ ., data = data.d))
      } else if (object$family == "binomial") {
        summary(glm(Response ~ .,
          family = "binomial", data = data.d,
          method = "brglmFit", type = "MPL_Jeffreys"
        ))
      } else {
        summary(glm(Response ~ ., family = "poisson", data = data.d))
      }
    } else {
      data.d <- data.frame(object$x[, object$var.index])
      colnames(data.d) <- object$var.label
      summary(coxph(Surv(object$y[, 1], object$y[, 2]) ~ ., data = data.d))
    }
  } else {
    message("None of variables are selected.")
    message("Therefore, the summary is shown for the model with intercept only\n")

    if (object$family != "cox") {
      data.d <- data.frame(yy = object$y)
      colnames(data.d)[1] <- "Response"
    }

    if (object$family == "gaussian") {
      summary(lm(Response ~ 1, data = data.d))
    } else if (object$family == "binomial") {
      summary(glm(Response ~ 1,
        family = "binomial", data = data.d,
        method = "brglmFit", type = "MPL_Jeffreys"
      ))
    } else if (object$family == "poisson") {
      summary(glm(Response ~ 1, family = "poisson", data = data.d))
    } else {
      summary(coxph(Surv(object$y[, 1], object$y[, 2]) ~ 1))
    }
  }
}

#' \code{predict.sgpv}: Prediction using the fitted model
#'
#' S3 method \code{predict} for an object of class \code{sgpv}
#'
#' @importFrom stats lm predict
#' @importFrom brglm2 brglmFit
#'
#' @param object An \code{sgpv} objectect
#' @param newdata Prediction data set
#' @param type The type of prediction required. Can take the value of `link`,
#' `response`, and `terms`. Default is `response`.
#' @param ... Other \code{predict} arguments
#'
#' @return Predicted values
#' @export
#' @examples
#'
#'
#' # prepare the data
#' x <- t.housing[, -ncol(t.housing)]
#' y <- t.housing$V9
#'
#' # run one-stage algorithm
#' out.sgpv <- pro.sgpv(x = x, y = y)
#'
#' predict(out.sgpv)
predict.sgpv <- function(object, newdata, type, ...) {
  if (object$family == "cox") stop("Sorry, prediction method isn't available for Cox models yet.\n")

  if (length(object$var.index) > 0) {
    data.d <- data.frame(Response = object$y, xx = object$x[, object$var.index])
    colnames(data.d)[-1] <- object$var.label

    if (missing(newdata)) newdata <- object$x
    if (is.null(colnames(newdata))) colnames(newdata) <- paste("V", 1:ncol(newdata), sep = "")
    if (missing(type)) type <- "response"

    if (object$family == "gaussian") {
      lm.m <- lm(Response ~ ., data = data.d)
      predict(lm.m, data.frame(newdata))
    } else if (object$family == "poisson") {
      glm.m <- glm(Response ~ ., family = "poisson", data = data.d)
      predict(glm.m, data.frame(newdata), type = type)
    } else {
      glm.m <- glm(Response ~ .,
        family = "binomial", data = data.d,
        method = "brglmFit", type = "MPL_Jeffreys"
      )
      predict(glm.m, data.frame(newdata), type = type)
    }
  } else {
    message("None of variables are selected.")
    message("Therefore, the prediction is based on the intercept only model.\n")

    if (missing(newdata)) newdata <- object$x

    if (object$family == "gaussian") {
      data.d <- data.frame(yy = object$y)
      lm.m <- lm(yy ~ 1, data = data.d)
      predict(lm.m, data.frame(newdata))
    } else {
      glm.m <- glm(yy ~ 1,
        data = data.frame(
          yy = object$y,
          xx = object$x
        ),
        family = object$family
      )

      predict(glm.m, newdata = data.frame(newdata), type = "response")
    }
  }
}

#' \code{plot.sgpv}: Plot variable selection results
#'
#' S3 method \code{plot} for an object of class \code{sgpv}. When the two-stage
#' algorithm is used, this function plots the fully relaxed lasso solution path on
#' the standardized scale and the final variable selection results. When the
#' one-stage algorithm is used, a histogram of all coefficients with selected effects
#' is shown.
#'
#' @importFrom graphics abline axis lines mtext polygon
#' @importFrom graphics legend points
#' @param x An \code{sgpv} object
#' @param lpv Lines per variable. It can take the value of 1 meaning that only the
#' bound that is closest to the null will be plotted, or the value of 3 meaning that
#' point estimates as well as 95% confidence interval will be plotted. Default is 3.
#' @param lambda.max The maximum lambda on the plot. Default is \code{NULL}.
#' @param short.label An indicator if a short label is used for each variable for
#' better visualization. Default is \code{TRUE}
#' @param ... Other \code{plot} arguments
#'
#' @return NULL
#' @export
#' @examples
#'
#' # prepare the data
#' x <- t.housing[, -ncol(t.housing)]
#' y <- t.housing$V9
#'
#' # one-stage algorithm
#' out.sgpv.1 <- pro.sgpv(x = x, y = y, stage = 1)
#'
#' # plot the selection result
#'
#' plot(out.sgpv.1)
#'
#' # two-stage algorithm
#' out.sgpv.2 <- pro.sgpv(x = x, y = y)
#'
#' # plot the fully relaxed lasso solution path and final solution
#' plot(out.sgpv.2)
#'
#' # zoom in a little bit
#' plot(out.sgpv.2, lambda.max = 0.01)
#'
#' # only plot one confidence bound
#' plot(out.sgpv.2, lpv = 1, lambda.max = 0.01)
plot.sgpv <- function(x, lpv = 3, lambda.max = NULL, short.label = T, ...) {
  if (!lpv %in% c(1, 3)) stop("lpv argument only takes values of 1 and 3.")

  if (length(x$var.index) == 0) {
    message("None of variables are selected.\n")
    if (x$stage == 2) stop("No visualization is available.\n")
  }

  # get information from data
  p <- ncol(x$x)
  if (short.label == T) {
    x.names <- paste("V", 1:p, sep = "")
  } else {
    x.names <- colnames(x$x)
  }


  # standardize inputs in linear regression
  if (x$family == "gaussian") {
    xs <- scale(x$x)
    ys <- scale(x$y)
  } else {
    xs <- as.matrix(x$x)
    ys <- x$y
  }

  if (x$stage == 2) {

    # maximum lambda in gaussian
    if (is.null(lambda.max) & x$family == "gaussian") {
      lambda.max <- max(abs(sapply(1:ncol(xs), function(z) xs[, z] %*% ys)) / nrow(xs)) * 1.1
    } else if (is.null(lambda.max)) {
      lambda.max <- 0.5
    }


    step <- lambda.max / 100

    # get a sequence of lambda
    lambda.seq <- seq(0, lambda.max, step)

    # fit lasso once
    if (x$family != "cox") {
      lasso <- glmnet(xs, ys, family = x$family)
    } else {
      lasso <- glmnet(xs, Surv(ys[, 1], ys[, 2]), family = x$family)
    }

    # get coefficient estimates at each lambda
    if (p < length(x$y)) {
      results <- sapply(lambda.seq, function(z) {
        get.coef(
          xs = xs, ys = ys,
          lambda = z,
          lasso = lasso,
          family = x$family
        )
      })

      # prepare data to plot
      to.plot <- data.frame(
        lambda = rep(lambda.seq, each = p),
        v = rep(x.names, length(lambda.seq)),
        pe = c(results[1:p, ]),
        lb = c(results[(p + 1):(2 * p), ]),
        ub = c(results[(2 * p + 1):(3 * p), ])
      )
    } else {
      results <- sapply(lambda.seq[-1], function(z) {
        get.coef(
          xs = xs, ys = ys,
          lambda = z,
          lasso = lasso,
          family = x$family
        )
      })

      # prepare data to plot
      to.plot <- data.frame(
        lambda = rep(lambda.seq[-1], each = p),
        v = rep(x.names, length(lambda.seq) - 1),
        pe = c(results[1:p, ]),
        lb = c(results[(p + 1):(2 * p), ]),
        ub = c(results[(2 * p + 1):(3 * p), ])
      )
    }


    if (lpv == 1) {
      to.plot$bound <- ifelse(abs(to.plot$lb) < abs(to.plot$ub), to.plot$lb, to.plot$ub)
      plot.d <- to.plot[, c("lambda", "v", "bound")]

      # find the location of x.names
      location.beta <- to.plot$bound[to.plot$lambda == to.plot$lambda[1]]
    } else if (lpv == 3) {
      plot.d <- to.plot

      # find the location of x.names
      location.beta <- to.plot$pe[to.plot$lambda == to.plot$lambda[1]]
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

    # find the lambda.gic
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
      mtext(bquote(lambda["gic"]), side = 1, at = vlambda)
      # null region
      if (p < length(x$y)) {
        polygon(c(lambda.seq, rev(lambda.seq)), c(-n.bound, rev(n.bound)), col = "grey", border = "grey")
      } else {
        polygon(c(lambda.seq[-1], rev(lambda.seq[-1])), c(-n.bound, rev(n.bound)), col = "grey", border = "grey")
      }
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
      if (p < length(x$y)) {
        polygon(c(lambda.seq, rev(lambda.seq)), c(-n.bound, rev(n.bound)), col = "grey", border = "grey")
      } else {
        polygon(c(lambda.seq[-1], rev(lambda.seq[-1])), c(-n.bound, rev(n.bound)), col = "grey", border = "grey")
      }

      # text on axis
      mtext(var.axis, side = 2, at = location.beta, col = color.use)
      mtext(bquote(lambda["gic"]), side = 1, at = vlambda)

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
  } else if (length(x$var.index) > 0) { # one-stage algorithm

    var.names <- colnames(x$x)
    pe <- x$pe.can
    lb <- x$lb.can
    ub <- x$ub.can
    p <- ncol(x$x)
    null.bound <- x$null.bound

    col.names <- rep("black", p)
    col.names[x$var.index] <- "blue"

    # set the x-axis
    x.lower <- min(lb) * ifelse(min(lb) < 0, 1.1, 0.9)
    x.upper <- max(ub) * ifelse(max(ub) > 0, 1.1, 0.9)

    plot(0,
      type = "n", xlab = "Effect estimates", ylab = "Variables", yaxt = "n",
      xlim = c(x.lower, x.upper),
      ylim = c(0, p + 1),
      main = "Selection results in the one-stage algorithm"
    )
    mtext(paste("ProSGPV selects", length(x$var.index), "variables."), side = 3)
    lines(x = rep(null.bound, 2), y = c(0, p + 1), col = "green4")
    lines(x = -rep(null.bound, 2), y = c(0, p + 1), col = "green4")
    for (i in 1:p) {
      lines(x = c(lb[i], ub[i]), y = c(i, i), col = col.names[i])
      mtext(var.names[i], side = 2, at = i, col = col.names[i])
      points(x = pe[i], y = i, pch = 20, col = col.names[i])
    }
    legend("topleft",
      box.col = "white", box.lwd = 0,
      legend = c("ProSGPV selection", "Null bound"), col = c("blue", "green4"),
      lty = c(1, 1), cex = 0.8
    )
  } else {
    message("All effects in the full model are no different than or overlap with the null.\n")
  }
}
