library(tidyverse)
library(bayesplot)
library(truncnorm)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

load("TomCr.RData")
TomCrF <- as_tibble(TomCr) %>% filter( !is.na(TC.Co.Star.Age.Difference))
TomCrF$TC.Co.Star.Age.Difference <- as.numeric(as.character(TomCrF$TC.Co.Star.Age.Difference))
TomCrF <- TomCrF[,-c(1,3,4,5)]

ggplot(TomCrF, aes(x = TC.age,y=TC.Co.Star.Age.Difference)) +
  geom_point()

# Bayesian regression to the data
bsig <- 5
ssig <- 100
mb <- 0
CruiseData <- list(N=36, x=TomCrF$TC.age, y=TomCrF$TC.Co.Star.Age.Difference,
                   N_new=36, x_new=TomCrF$TC.age, Sigma_b = bsig, Sigma_s = ssig, b=mb)
CruiseStan <- stan("TomCr.stan", data=CruiseData, chains = 6, iter = 5000)

# Check summary
summary(CruiseStan, pars = c("alpha", "beta", "sigma"))$summary
# Check traceplot
# traceplot(CruiseStan, pars=c("alpha","beta","sigma"))
# mcmc_trace(CruiseStan)

# predict data
olspredicted <- extract(CruiseStan, pars=c("alpha","beta","sigma","lp__"), include=FALSE)
postPred <- apply(olspredicted$y_new, 2, quantile, c(0.05,0.5,0.95))
PlottingData <- data.frame(lower=postPred[1,], upper=postPred[3,],
                           median=postPred[2,], TC_age=TomCrF$TC.age)
ggplot(PlottingData) +
  geom_line(aes(x=TC_age,y=median),colour="red") +
  geom_line(aes(x=TC_age,y=lower),colour="red",lty="dashed") +
  geom_line(aes(x=TC_age,y=upper),colour="red",lty="dashed") +
  geom_point(data=TomCrF,aes(x = TC.age,y=TC.Co.Star.Age.Difference)) +
  ylab("TC Co-Star Age Difference")

#
color_scheme_set("brightblue")
mcmc_hist(as.matrix(CruiseStan),pars=c("alpha","beta","sigma"))

