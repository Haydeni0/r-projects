library(tidyverse)
library(bayesplot)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(caret)


pulsars <- as_tibble(read.csv("pulsar_stars.csv"))
N = nrow(pulsars)
pulsars$target_class <- as.integer(as.numeric(pulsars$target_class))


# Split data into a training and test dataset
set.seed(1)
training_idx = sample(N, ceiling(0.75 * N))
pulsars.training <- pulsars[training_idx, ]
pulsars.test <- pulsars[-training_idx, ]

names(pulsars)
ggplot(data = pulsars) + geom_point(
  aes(x = Mean.of.the.integrated.profile,
      y = Standard.deviation.of.the.integrated.profile,
      color = target_class)
) +
  theme_bw()

# GLM ----------
#---------------
glmodel <-
  glm(target_class ~ .,
      data = pulsars.training,
      family = binomial(link = "logit"))
glmodel2 <-
  glm(
    target_class ~ . - Excess.kurtosis.of.the.DM.SNR.curve,
    data = pulsars.training,
    family = binomial(link = "logit")
  )
summary(glmodel)
summary(glmodel2)
1 - pchisq(glmodel$deviance, glmodel$df.residual)
1 - pchisq(glmodel2$deviance, glmodel2$df.residual)

anova(glmodel, glmodel2, test = "Chisq")

pred.glmodel.training <-
  predict(glmodel, newdata = pulsars.training[, 1:8], type = "response")
pred.glmodel.training <- 1 * (pred.glmodel.training > 0.5)
pred.glmodel.test <-
  predict(glmodel, newdata = pulsars.test[, 1:8], type = "response")
pred.glmodel.test <- 1 * (pred.glmodel.test > 0.5)

# 98% accuracy for in and out of sample predictions
confusionMatrix(as.factor(as.numeric(pred.glmodel.training)),
                as.factor(pulsars.training$target_class))
confusionMatrix(as.factor(as.numeric(pred.glmodel.test)),
                as.factor(pulsars.test$target_class))

auc.pred.glm <- prediction(as.numeric(pred.glmodel.test),
                           as.factor(pulsars.test$target_class))
auc.perf.glm <- performance(auc.pred.glm, measure = "auc")
auc.perf.glm@y.values


# stan -------------
# ------------------

stan_list = list(
  N = nrow(pulsars.training),
  p = 8,
  y = pulsars.training$target_class,
  X = pulsars.training[, 1:8],
  
  alpha_mean = -10,
  alpha_sd = 100,
  beta_mean = c(0, 0, 0, 0, 0, 0, 0, 0),
  beta_sd = c(30, 30, 30, 30, 30, 30, 30, 30),
  
  N_test = nrow(pulsars.test),
  X_test = pulsars.test[, 1:8]
)

bmodel <- stan(
  "logisticRegression.stan",
  data = stan_list,
  chains = 4,
  iter = 2000
)
summary(bmodel)$summary[1:10,]
plot(
  bmodel,
  pars = c(
    # "alpha",
    "beta[1]",
    "beta[2]",
    # "beta[3]",
    "beta[4]",
    "beta[5]",
    "beta[6]",
    "beta[7]",
    "beta[8]"
  )
)
mcmc_trace(
  bmodel,
  pars = c(
    "alpha",
    "beta[1]",
    "beta[2]",
    "beta[3]",
    "beta[4]",
    "beta[5]",
    "beta[6]",
    "beta[7]",
    "beta[8]"
  )
)

pred.bmodel <-
  extract(bmodel,
          pars = c("alpha", "beta", "theta", "lp__"),
          include = F)


pred.training <- apply(pred.bmodel$theta_training, 2, quantile, 0.5)
pred.test <- apply(pred.bmodel$theta_test, 2, quantile, 0.5)

pred.training <-  exp(pred.training) / (1 - exp(pred.training))
pred.test <-  exp(pred.test) / (1 - exp(pred.test))

pred.training <- 1 * (pred.training > 0.5)
pred.test <- 1 * (pred.test > 0.5)

confusionMatrix(as.factor(as.numeric(pred.training)),
                as.factor(pulsars.training$target_class))
confusionMatrix(as.factor(as.numeric(pred.test)),
                as.factor(pulsars.test$target_class))

# Random forest --------------
#-----------------------------
library(randomForest)
library(ROCR)

rfmodel <- randomForest(as.factor(target_class) ~ .,
                        data = pulsars.training)

pred.rf <- predict(rfmodel, newdata = pulsars.test[, 1:8])

# 98% accuracy for in and out of sample predictions
confusionMatrix(pred.rf,
                as.factor(pulsars.test$target_class))

auc.pred.rf <-
  prediction(as.numeric(pred.rf), as.factor(pulsars.test$target_class))
auc.perf.rf <- performance(auc.pred.rf, measure = "auc")
auc.perf.rf@y.values


# xgboost ------
#---------------
library(xgboost)

D.train <-
  xgb.DMatrix(data = as.matrix(pulsars.training[, 1:8]),
              label = pulsars.training$target_class)

xgbmodel <- xgboost(
  data = D.train,
  nthread = 4,
  nrounds = 1000,
  verbose = 0
)

pred.xgb.training <- predict(xgbmodel, newdata = D.train)
pred.xgb.training <- as.numeric(pred.xgb.training > 0.5)

D.test <- xgb.DMatrix(data = as.matrix(pulsars.test[, 1:8]))
pred.xgb.test <- predict(xgbmodel, newdata = D.test)
pred.xgb.test <- as.numeric(pred.xgb.test > 0.5)

# confusionMatrix(as.factor(pred.xgb.training), as.factor(pulsars.training$target_class))
# confusionMatrix(as.factor(pred.xgb.test), as.factor(pulsars.test$target_class))

auc.pred.xgb <-
  prediction(as.numeric(pred.xgb.test),
             as.factor(pulsars.test$target_class))
auc.perf.xgb <- performance(auc.pred.xgb, measure = "auc")
auc.perf.xgb@y.values
