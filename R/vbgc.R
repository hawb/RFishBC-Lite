
vbFuns <- function(t, Linf, K, t0) {
  Linf * (1 - exp(-K * (t - t0)))
}

vbStarts <- function(age, len) {
  meanL <- tapply(len, age, mean)
  ages <- as.numeric(names(meanL))
  
  cfs <- coef(lm(meanL[-1] ~ meanL[-length(meanL)]))
  Linf <- cfs[1] / (1 - cfs[2])
  K <- -log(cfs[2])
  youngest <- min(ages)
  t0 <- youngest + (1 / K) * log((Linf - meanL[1]) / Linf)
  
  list(Linf = Linf, K = K, t0 = t0)
}
