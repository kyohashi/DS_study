library(mcmcse)
library(MASS)
library(R2WinBUGS)
library(ggplot2)
library(ggExtra)

# generate data
set.seed(1234)
y = rnorm(1000, 3, 5)

# initial vector setting and parameter values
T = 10000; B = T/10; B1 = B + 1
mu = sig = numeric(T)

# initial parameter values
mu[1] = 0
sig[1] = 1
u.mu = u.sig = runif(T)

# rejection counter
REJmu = 0; REJsig = 0

# log posterior density (up to a constant)
logpost = function(mu, sig){
  loglike = sum(dnorm(y, mu, sig, log=TRUE))
  return(loglike - log(sig))
}

# sampling loop
for (t in 2:T){
  print(t)
  mut = mu[t-1]
  sigt = sig[t-1]
  
  # uniform proposal with kappa = 0.5
  mucand = mut + runif(1, -0.5, 0.5)
  sigcand = abs(sigt + runif(1, -0.5, 0.5))
  
  alph.mu = logpost(mucand, sigt) - logpost(mut, sigt)
  if (log(u.mu[t]) <= alph.mu) 
    mu[t] = mucand
  else{
    mu[t] = mu[t-1]
    REJmu = REJmu + 1
  }
  
  alph.sig = logpost(mu[t], sigcand) - logpost(mu[t], sigt)
  if (log(u.sig[t]) <= alph.sig)
    sig[t] = sigcand
  else{
    sig[t] = sig[t-1]
    REJsig = REJsig + 1
  }
}

# sequence of sampled values and ACF plots
plot(mu)
plot(sig)
acf(mu)
acf(sig)

# posterior summary
summary(mu[B1:T])
summary(sig[B1:T])

# Monte Carlo standard errors
D = data.frame(mu[B1:T], sig[B1:T])

# acceptance rates
ACCmu = 1 - REJmu/T
ACCsig = 1 - REJsig/T
cat("Acceptance Rate mu =", ACCmu)
cat("Acceptance Rate sig =", ACCsig)

# kerner density plots
f1 = kde2d(mu[B1:T], sig[B1:T], n=50, lims = c(2.5, 3.4, 4.7, 5.3))
filled.contour(f1, xlab="mu", ylab="sig",
               color.palette = colorRampPalette(c('white', 'lightgray', 'gray', 'darkgray', 'black')))

# estimates of effective sample sizes
effectiveSize(mu[B1:T])
effectiveSize(sig[B1:T])
ess(D)
multiESS(D)

# posterior probability on hypothesis mu < 3
sum(mu[B1:T] < 3) / (T-B)
