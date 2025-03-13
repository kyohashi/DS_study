rm(list = ls())

library(coda)
# Data
y=c(28,8,-3,7,-1,1,18,12)
sigma=c(15,10,16,11,9,11,10,18)
sigma2 = sigma^2
J = 8
# Total MCMC Iterations
T = 20000
# Ten unknowns (eight effects, plus their mean and variance)
samps = matrix(, T, 10)
colnames(samps) <- c("mu","tau","Sch1","Sch2","Sch3","Sch4",
                     "Sch5","Sch6","Sch7","Sch8")
# Starting values
mu=mean(y)
tau2=median(sigma2)
# Sampling loop
for (t in 1:T) {th.mean=(y/sigma2+mu/tau2)/(1/sigma2+1/tau2)
th.sd=sqrt(1/(1/sigma2+1/tau2))
theta=rnorm(J,th.mean,th.sd)
mu=rnorm(1,mean(theta),sqrt(tau2/J))
# precision
invtau2=rgamma(1,J/2+0.1,sum((theta-mu)^2)/2+0.1)
tau2 = 1/invtau2
tau = sqrt(tau2)
# accumulate samples
samps[t,3:10] = theta
samps[t,1] =mu
samps[t,2] =tau}
# posterior summary
summary(as.mcmc(samps))
post.mn = apply(samps,2,mean)
post.sd = apply(samps,2,sd)
post.median = apply(samps,2,median)
post.95=apply(samps, 2, quantile, probs = c(0.95)) 
post.05=apply(samps, 2, quantile, probs = c(0.05))
# Trace and density plots
plot(as.mcmc(samps))

rel = data.frame(
  sch_eff = post.median[3:10],
  y = y
)


plot(rel$sch_eff, rel$y)
abline(lm(y ~ sch_eff, rel), col="red", lwd=2)

