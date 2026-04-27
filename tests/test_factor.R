library(efdm)
library(testthat)

statespace <- expand.grid(sp=factor(c("spruce", "pine")), vol=1:4)
actprob <- statespace
actprob$test <- actprob$sp == "spruce"
actprob$test2 <- 1-actprob$test

act1 <- define_activity("test", "vol")
transprobs(act1) <- data.frame(sp=factor("spruce"), vol0=1:4, vol1=4, prob=1)

act2 <- define_activity("test2", "vol")
transprobs(act2) <- data.frame(sp=factor("pine"), vol0=1:4, vol1=1, prob=1)

# Factors are matched on their character representation.
# Try to test that anyway
state0 <- data.frame(sp=factor(c("spruce", "pine")), vol=2, area=1)
state1 <- runEFDM(state0, actprob, list(act1, act2), 2)
state0 <- data.frame(sp=factor(c("pine", "spruce")), vol=2, area=1)
state2 <- runEFDM(state0, actprob, list(act1, act2), 2)
expect_equal(state1, state2)
