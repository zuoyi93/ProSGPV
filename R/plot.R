#' plot S3 method
#'
#' Plot the fully relaxed lasso solution path on
#' the standardized scale and the final variable selection results
#'
#' @importFrom graphics abline axis lines mtext polygon
#' @param x An \code{sgpv} object
#' @param lpv Lines per variable. It can take the value of 1 meaning that only the
#' bound that is closest to the null will be plotted, or the value of 3 meaning that
#' point estimates as well as 95% confidence interval will be plotted
#' @param lambda.max The maximum lambda on the plot. Default is \code{NULL}.
#' @param ... Other \code{plot} arguments
#'
#' @return NULL
#' @export
#'

plot.sgpv <- function(x,lpv=c(1,3),lambda.max=NULL,...){

  if(is.null(x$lambda)) stop("One-stage algorithm doesn't have the plot function.")
  if(! lpv %in% c(1,3)) stop("lpv argument only takes values of 1 and 3.")

  # get information from data
  p <- ncol(x$x)
  x.names <- colnames(x$x)

  # standardize data
  xs <- scale(x$x)
  ys <- scale(x$y)

  # maximum lambda
  if(is.null(lambda.max)) lambda.max <- max(abs(sapply(1:ncol(xs),function(z) xs[,z] %*% ys ))/nrow(xs))*1.1

  step = lambda.max/100

  # get a sequence of lambda
  lambda.seq <- seq(0,lambda.max,step)

  # get coefficient estimates at each lambda
  results <- sapply(lambda.seq,function(z) get.coef(xs=xs,ys=ys,lambda=z))

  # prepare data to plot
  to.plot <- data.frame(lambda=rep(lambda.seq,each=p),
                        v=rep(x.names,length(lambda.seq)),
                        pe=c(results[1:p,]),
                        lb=c(results[(p+1):(2*p),]),
                        ub=c(results[(2*p+1):(3*p),])
                        )

  if(lpv==1){
    to.plot$bound <- ifelse(abs(to.plot$lb)<abs(to.plot$ub), to.plot$lb, to.plot$ub )
    plot.d <- to.plot[,c("lambda","v","bound")]

    # find the location of x.names
    location.beta <- to.plot$bound[to.plot$lambda==0]

  }else if(lpv==3){
    plot.d <- to.plot

    # find the location of x.names
    location.beta <- to.plot$pe[to.plot$lambda==0]
  }

  # get the indices of selected variables
  selected.index <- x$var.index

  # change the color of the variables
  color.use <- rep("black",p)
  color.use[selected.index] <- "blue"

  # find the limit of the canvas
  if(lpv==3){
    ylim <- c(min(c(location.beta,to.plot$lb,to.plot$ub) )*1.01,
              max(c(location.beta,to.plot$lb,to.plot$ub))*1.01)

    ylim <- c(-max(abs(ylim)),max(abs(ylim)))
    ytick <- c(-round(max(abs(ylim)),2),-round(max(abs(ylim)),2)/2,0,
               round(max(abs(ylim)),2)/2,round(max(abs(ylim)),2) )
  }else if (lpv==1){
    ylim <- c(min(plot.d$bound) * 1.01,
              max(plot.d$bound) * 1.01)

    ylim <- c(-max(abs(ylim)),max(abs(ylim)))
    ytick <- c(-round(max(abs(ylim)),2),-round(max(abs(ylim)),2)/2,0,
               round(max(abs(ylim)),2)/2,round(max(abs(ylim)),2) )

  }

  # create a data set for the null bound
  n.bound <- results[(3*p+1),]

  # find the lambda.1se
  vlambda <- x$lambda

  # plot the results
  if(lpv==1){

    xvals <- split(plot.d$lambda,plot.d$v)
    yvals <- split(plot.d$bound,plot.d$v)

    plot(lambda.seq,type="n",ylim=ylim,xlim=c(0,lambda.max),
         xlab=expression(lambda),ylab="Bound that is closer to the null",
         main="Solution to the two-stage algorithm with one line per variable",
         axes=F,frame.plot=T)
    axis(1,at=round(seq(0,lambda.max,length.out=5),3) )
    axis(2,at=ytick,labels=c(ytick[1:2],0,ytick[4:5]))
    abline(h=0)
    polygon(c(lambda.seq,rev(lambda.seq)),c(-n.bound,rev(n.bound)),col="grey",border="grey")
    abline(v=vlambda,lty=2)
    mtext(x.names,side=2,at=location.beta,col=color.use)
    mtext(bquote(lambda["1se"]),side=1,at=vlambda)
    invisible(mapply(lines,xvals,yvals,col=1:p))

  }else if (lpv==3){

    plot(lambda.seq,type="n",ylim=ylim,xlim=c(0,lambda.max),
         xlab=expression(lambda),ylab="Point estimates and confidence intervals",
         main="Solution to the two-stage algorithm with three lines per variable",
         axes=F,frame.plot=T)
    axis(1,at=round(seq(0,lambda.max,length.out=5),3) )
    axis(2,at=ytick,labels=c(ytick[1:2],0,ytick[4:5]))
    abline(h=0)
    polygon(c(lambda.seq,rev(lambda.seq)),c(-n.bound,rev(n.bound)),col="grey",border="grey")
    abline(v=vlambda,lty=2)
    mtext(x.names,side=2,at=location.beta,col=color.use)
    mtext(bquote(lambda["1se"]),side=1,at=vlambda)

    # point estimates
    xvals <- split(plot.d$lambda,plot.d$v)
    yvals <- split(plot.d$pe,plot.d$v)
    invisible(mapply(lines,xvals,yvals,col=1:p))

    # upper bound
    xvals <- split(plot.d$lambda,plot.d$v)
    yvals <- split(plot.d$ub,plot.d$v)
    invisible(mapply(lines,xvals,yvals,col=1:p,lty=2))

    # lower bound
    xvals <- split(plot.d$lambda,plot.d$v)
    yvals <- split(plot.d$lb,plot.d$v)
    invisible(mapply(lines,xvals,yvals,col=1:p,lty=2))

  }


}
