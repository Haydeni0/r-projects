library(tidyverse)
library(bayesplot)
library(truncnorm)
library(rstan)
library(reshape2)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# Load, tidy and plot data
load("censSurvival.RData")
PlotData1 <- melt(censSurvival, id.vars = "days")
ggplot(PlotData1) +
  geom_point(aes(x = value, y = days, colour = variable)) +
  geom_smooth(aes(x = value, y = days, colour = variable), method = lm) +
  facet_wrap( ~ variable, scales = "free_x")



## Removing censored data ----

# dataset with censored data removed (those that survived past 365)
cens.removed <- censSurvival[censSurvival$days < 365, ]

stan_list <- list(
  N = dim(cens.removed)[1],
  p = dim(cens.removed)[2] - 1,
  y = cens.removed$days,
  X = as.matrix(cens.removed[, -1]),
  alpha_mean = 200,
  alpha_sd = 800,
  beta_sd = 10,
  sigma_mean = 80,
  sigma_sd = 200
)
fit <-
  stan(
    file = "censSurvival.stan",
    data = stan_list,
    chains = 8,
    iter = 10000
  )

fit_extract <- extract(fit)

# Better traceplot
ggplot((melt(fit_extract))) + geom_point(aes(x = iterations, y = value), alpha = 0.3) +
  facet_wrap( ~ L1, scales = "free_y") + theme_bw()


summary(fit)$summary
mcmc_hist(fit)


## Bayesian regression with censored data
cens.only <- filter(censSurvival, days >= 365)


stan_list.cens <- list(
  N = dim(cens.removed)[1],
  Ncens = dim(censSurvival)[1] - dim(cens.removed)[1],
  p = dim(cens.removed)[2] - 1,
  y = cens.removed$days,
  X = as.matrix(cens.removed[, -1]),
  Xcens = as.matrix(cens.only[, -1]),
  alpha_mean = 200,
  alpha_sd = 800,
  beta_sd = 10,
  sigma_mean = 1,
  sigma_sd = 100
)

fit.cens <-
  stan(
    file = "censSurvival_censored.stan",
    data = stan_list.cens,
    chains = 4,
    iter = 4000
  )
fit_extract.cens <- extract(fit.cens)

# Better traceplot
ggplot((melt(fit_extract.cens[1:3]))) + geom_point(aes(x = iterations, y = value), alpha = 0.3) +
  facet_wrap( ~ L1, scales = "free_y") + theme_bw()


summary(fit.cens)$summary[1:4,]
mcmc_hist(fit.cens, pars = c("alpha", "beta[1]", "beta[2]", "sigma"))

## plot predictions------------

bayesparams <- extract(fit.cens,pars=c("alpha","beta","sigma"),include=TRUE)
Samples <- matrix(NA,nrow=2000,ncol=nrow(censSurvival))
tmpSam <- c()
for(k in 1:2000){
  for(j in 1:nrow(censSurvival)){
    tmpSam <- rtruncnorm(n=1, a=0, mean=bayesparams$alpha[k] +
                           as.numeric(censSurvival[j,-1])%*%bayesparams$beta[k,], sd=bayesparams$sigma[k])
    Samples[k,j] <- ifelse(tmpSam>365,365,tmpSam)
  }
}
postPredreg <- apply(Samples, 2, quantile, c(0.05,0.5,0.95))
PlottingData <- data.frame(lower=postPredreg[1,], upper=postPredreg[3,],
                           median=postPredreg[2,])
PlottingData <- cbind(PlottingData,censSurvival)
PlottingData2 <- melt(PlottingData, id.vars=c('median','lower','upper','days'))
ggplot(PlottingData2) +
  geom_point(aes(x=value, y=median,colour=variable)) +
  geom_errorbar(aes(x=value, ymin=lower, ymax=upper, colour=variable)) +
  geom_point(aes(x=value, y=days), colour="blue") +
  facet_wrap(~variable, scales="free_x") +
  ylab("Time to Death")

