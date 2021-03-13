library(tidyverse)
library(bayesplot)
library(truncnorm)
library(rstan)
library(reshape2)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

load("salaries.RData")
salaries <- as_tibble(salaries)
names(salaries) <-
  c("Sex", "Rank", "Rank.years", "Degree", "Degree.years", "Salary")

# Plot data
ggplot(melt(salaries, id.vars = 'Salary')) +
  geom_point(aes(x = value, y = Salary, colour = variable)) +
  facet_wrap(~ variable, scales = "free_x")

# Since there are more than 2 levels to the factor 'Rank', be careful.
# Make two new indicator variables for associate and professor that will
# be modelled as additions to the salary of an assistant.
salaries <- salaries %>% mutate(isAssociate = Rank == "associate",
                                isFullProf = Rank == "full")

## GLM first -----
model.glm <-
  glm(
    Salary ~ Sex + Rank + Rank.years + Degree + Degree.years,
    data = salaries,
    family = gaussian(link = "identity")
  )
summary(model.glm)
model.glm$deviance/model.glm$df.residual 
## Bayesian multiple regression ----

# Take out variable 2 as this is the rank, which we have split up previously
# Also take out variable 6 , as that is Salary.
StanData <- list(
  N = 52,
  X = salaries[,-c(2, 6)],
  # remove
  y = salaries$Salary,
  N_new = 52,
  X_new = salaries[,-c(2, 6)],
  p = 6,
  #values for hyperparameters
  a = mean(salaries$Salary),
  Sigma_a = sd(salaries$Salary),
  beta0 = rep(0, 6),
  Sigma_b = rep(1000,6),
  Sigma_s = 1000
) #Pass data and hyperparameters to Stan

bayesreg <-
  stan("salaries.stan",
       data = StanData,
       chains = 6,
       iter = 20000)

# Assess convergence and model
summary(bayesreg)$summary[1:8, ]
traceplot(bayesreg,linetype = , pars = c("alpha", "sigma", "beta"))
regpredicted <-
  extract(bayesreg,
          pars = c("alpha", "beta", "sigma", "lp__"),
          include = FALSE)
postPredreg <-
  apply(regpredicted$y_new, 2, quantile, c(0.05, 0.5, 0.95))
PlottingData <-
  data.frame(lower = postPredreg[1, ],
             upper = postPredreg[3, ],
             median = postPredreg[2, ])
PlottingData <- cbind(PlottingData, salaries)
PlottingData2 <-
  melt(PlottingData, id.vars = c('median', 'lower', 'upper', 'Salary'))
ggplot(PlottingData2) +
  geom_point(aes(x = value, y = median, colour = variable)) +
  geom_errorbar(aes(
    x = value,
    ymin = lower,
    ymax = upper,
    colour = variable
  )) +
  geom_point(aes(x = value, y = Salary), colour = "blue") +
  facet_wrap( ~ variable, scales = "free_x") +
  ylab("Salary")

mcmc_hist(as.matrix(bayesreg), pars=c("alpha", "beta[1]", "beta[2]",
                                      "beta[3]", "beta[4]", "beta[5]", "sigma"))


