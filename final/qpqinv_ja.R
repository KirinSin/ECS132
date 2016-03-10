qpqinv <-function(lmout) {
  error <- coef(summary(lmout))[, "Std. Error"]
  cov <- vcov(lmout)
  result = cov/error**2
}
