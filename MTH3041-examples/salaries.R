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
  facet_wrap( ~ variable, scales = "free_x")

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

## Bayesian multiple regression ----
ta <- 0 #values for hyperparameters
tsiga <- 100
tbeta0 <- rep(0, 6)
tsigbeta0 <- rep(100, 6)
tSigs <- 100
StanData <- list(
  N = 52,
  X = salaries[, -c(2, 6)],
  y = salaries$Salary,
  N_new = 52,
  X_new = salaries[, -c(2, 6)],
  p = 6,
  a = ta,
  Sigma_a = tsiga,
  beta0 = tbeta0,
  Sigma_b = tsigbeta0,
  Sigma_s = tSigs
) #Pass data and hyperparameters to Stan
#take out variable 2 as this is the rank, which we have broken up
bayesreg <-
  stan("multipleReg.stan",
       data = StanData,
       chains = 2,
       iter = 2000)
