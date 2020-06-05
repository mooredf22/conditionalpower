# generate clinical trial data set

interleave <- function(v1,v2)
{
  # interleave v1 and v2
  ord1 <- 2*(1:length(v1))-1
  ord2 <- 2*(1:length(v2))
  c(v1,v2)[order(c(ord1,ord2))]
}

#interleave(1:5, 6:10)

covidGen <- function(nCont=60, nExp=60, p0=0.571, p1=0.801) {
  # nCont:  number of control patients
  # nExp:   number of experimental patients

  controlOutcome <- rbinom(n=nCont, size=1, prob=p0)
  experimentalOutcome <- rbinom(n=nExp, size=1, prob=p1)
  Outcome <- c(controlOutcome, experimentalOutcome)
  Arm <- c(rep(0, nCont), rep(1, nExp))
  TrialOutTemp <- data.frame(Outcome, Arm)
  # alternate interventions
  interleave.index <- interleave(1:nCont, (nCont+1):(nCont+nExp))
  TrialOut <- TrialOutTemp[interleave.index,]

  return(TrialOut)
}

#' calculate conditional power
#'
#' @param tau information time of calculation (0 to 1)
#' @param zz.interim value of Z statistic at interim
#' @return prob.signif  probability of a significant result
#' @return B.predict predicted B based on current trend
condPower <- function(tau=0.5, zz.interim, alpha=0.025, power=0.8) {
  # zz.interim <- 2.243
  theta <- 1.96 + 0.8416    # E(Z) = z_alpha + z_beta
  theta <- qnorm(1 - alpha) + qnorm(power)
  B.tau <- zz.interim*sqrt(tau)  # brownian motion

  # now consider the conditional power plot, with the original trend
  #  The y-intercept is theta, as is the slope of the original trend
  # A single point, at the interim analyisis is x1=0.5, y1=B.tau
  #  The general equation is y -y1 = m(x-x1)
  #  Here m=theta, so we have
  #  y = theta*(x - x1) + y1  # evaluate at x=1 (future analysis time)
   B.predict <- theta*(1 - tau) + B.tau  # this is the predicted B using original trend

   prob.signif <- pnorm((qnorm(1 - alpha) - B.predict)/sqrt(.5), lower.tail = F)

  return(list(prob.signif=prob.signif, tau=tau, B.tau=B.tau, theta=theta,
              B.predict=B.predict))

}

trialProperties <- function(nCont=60, nExp=60, p0=0.571, p1=0.801,
                            tau=0.5, nBoot=1000) {
pval.vec <- rep(NA, nBoot)
condPow.vec <- rep(NA, nBoot)
for (i in 1:nBoot) {
  #i=1
   TrialOut <- covidGen(nCont=nCont, nExp=nExp, p0=p0, p1=p1)
  result <- glm(Outcome ~ Arm, family=binomial, data=TrialOut)
  result.summ <- summary(result)
  coef <- result.summ$coefficients
  pval.i <- coef[2,4]
  pval.vec[i] <- pval.i

  n.subjects <- nrow(TrialOut)
  n.interim <- round(n.subjects*tau)  # number of patients at interim analysis

  interim.result <- glm(Outcome ~ Arm, family=binomial, data=TrialOut[1:n.interim,])
  interim.summ <- summary(interim.result)
  coef.interim <- interim.summ$coefficients
  zz.interim <- coef.interim[2,3]
  condPow.i <- condPower(tau=0.5, zz.interim=zz.interim )$prob.signif
  condPow.vec[i] <- condPow.i
  if ((i %% 1000) == 0) cat(paste(i, "\n"))  # print i every multiple of 1000
  }
  power.standard <- sum(pval.vec <= 0.05)/nBoot
  power.futility <- sum(condPow.vec > 0.15 & pval.vec <= 0.05)/nBoot

  power.loss = power.standard - power.futility

  result <- list(pval.vec=pval.vec, condPow.vec=condPow.vec,
                 power.standard=power.standard, power.futility=power.futility,
                 power.loss=power.loss)
  result
}

