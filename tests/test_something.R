# What was this??
data.table::setDTthreads(1)
library(efdm)
statespace <- expand.grid(a=1:2, b=1:2, vol=1:5)
pairdata <- data.frame(a=c(1,1,2,2), b=c(1,2,1,2), vol0=c(1,1,1,1), vol1=c(2,3,4,5))
state0 <- statespace
actprob <- statespace
actprob$test <- 1
state0$area <- 0
state0$area[1] <- 1

prior <- statespace
prior$vol0 <- prior$vol1 <- prior$vol
prior$vol <- NULL
prior$nobs <- 3
if(FALSE) {
  act4 <- define_activity("test", c("vol"))
  transprobs(act4) <- estimatetransprobs("vol", pairdata, statespace, factors=c("a", "b"), prior=prior)
  transprobs(act4)
  runEFDM(state0, actprob, list(act4), 1)
  act1 <- define_activity("test", c("vol"))
  transprobs(act1) <- estimatetransprobs("vol", pairdata, statespace, by=c("a", "b"), prior=prior)
  transprobs(act1)
  runEFDM(state0, actprob, list(act1), 1)
}
