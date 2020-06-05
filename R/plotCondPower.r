#' make a conditional power plot
#' @param condPowerOut  Output of conditional power function
#'
plotCondPower <- function(condPowerOut=condPow) {
  # make a conditional power plot based on the output
  # of condPower

  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))

  par(mar=c(4,4, 2, 4))
  prob.signif <- condPowerOut$prob.signif
  tau=condPowerOut$tau
  B.tau=condPowerOut$B.tau
  theta=condPowerOut$theta
  B.predict=condPowerOut$B.predict

  xx <- c(0,1)
  yy <- c(0, max(theta, B.predict))
  plot(yy ~ xx, xlab="tau", ylab="Conditional power", type="n", axes=F)
  segments(x0=0, y0=0, x1=1, y1=theta, lty=1)
  segments(x0=0, y0=0, x1=tau, y1=B.tau, col="blue")
  segments(x0=tau, y0=B.tau, x1=1, y1=B.predict, lty=2, col="blue")

  points(x=1, y=theta, pch=16)
  points(x=tau, y=B.tau, pch=16, col="blue")
  points(x=1, y=B.predict, pch=16, col="blue")
  axis(1)
  axis(2)
  axis(4, las=1)

}

