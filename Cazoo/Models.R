library(MASS)
library(mice)

# How should we deal with NaN values?
# - It looks like mainly older observations are missing, so it may be fine to discard NaN values as it is only 10% of the data.
# - Could also use multiple imputation
# - Or some mechanism to impute values based on cars with the same Make and Model

# Should we remove zero values when using a gamma glm?
# - Could use a zero-inflated gamma model
# - After looking at the data, these are probably incorrect entries and can be discarded

# How should we deal with extreme values? eg. limousine, electric
# - Maybe just discard them somehow

# What type of model do we want to use?
# - Is the response:
#   - Continuous or discrete?
#   - strictly positive?

# Training and testing datasets?

## Make a training and testing dataset ----
num_training <- 8000
num_testing <- nrow(offers_made) - num_training

set.seed(0)
training_idx <- sample(nrow(offers_made), num_training)
training_data <- (offers_made[training_idx,])
testing_data <- (offers_made[-training_idx,])
# Sort the dataset based on TradeValuation1
sort_perm <- sort(testing_data$TradeValuation1, index.return = TRUE)

## A gaussian multiple regression model

model1 <- glm(TradeValuation1 ~ . , 
              family = gaussian(link = "identity"), data = subset(training_data, select = -c(CompetitorB, CompetitorC, VRM, Model)))
# model1 <- glm(TradeValuation1 ~ . , 
#               family = gaussian(link = "identity"), data = subset(training_data, select = -c(VRM, Model)))

model2 <- glm(TradeValuation1 ~ . , 
              family = Gamma(link = "log"), data = filter(subset(training_data, select = -c(CompetitorB, CompetitorC, VRM, Model)), TradeValuation1 != 0))
model2.1 <- glm(TradeValuation1 ~ . , 
                family = Gamma(link = "log"), data = filter(subset(training_data, select = -c(CompetitorB, CompetitorC, VRM, Model, OfferDate)), TradeValuation1 != 0))

model3 <- glm(TradeValuation1 ~ Mileage, 
              family = gaussian(link = "log"), data = filter(subset(training_data, select = -c(CompetitorB, CompetitorC, VRM, Model)), TradeValuation1 != 0))


# Choose the model we want to use to plot
pmodel <- model2

summary(pmodel)
par(mfrow = c(2,2))
plot(pmodel)
par(mfrow = c(1,1))

pred.link <- predict(pmodel,newdata=testing_data, type = "link",se.fit = T)
pred.response <- predict(pmodel,newdata=testing_data, type = "response",se.fit = T)

ggplot() + geom_line(data = testing_data, aes(1:num_testing, TradeValuation1[sort_perm$ix]), color = my_colours[1], size = 1) + 
  geom_point(aes(1:num_testing, pred.response$fit[sort_perm$ix]), alpha = 0.3) +
  ggtitle("Predicted values for the testing dataset")+
  xlab("((sorted observations))") + ylab("TradeValuation1")+
  theme_bw()

ggplot() +  
  geom_abline(slope = 0, intercept = 0, size = 1) +
  geom_point(data = testing_data, aes(1:num_testing, TradeValuation1[sort_perm$ix] - pred.response$fit[sort_perm$ix]), color = "slategrey", size = 1) +
  ggtitle("Residual plot")+
  xlab("((sorted observations))") + ylab("TradeValuation1 - predicted")+
  theme_bw()

ggplot() +  
  geom_abline(slope = 0, intercept = 0, size = 1) +
  geom_point(data = testing_data, aes(1:num_testing, TradeValuation1 - pred.response$fit), color = "slategrey", size = 1) +
  ggtitle("Residual plot")+
  xlab("((unsorted observations))") + ylab("TradeValuation1 - predicted")+
  theme_bw()


rmse <- sqrt(mean((testing_data$TradeValuation1 - pred.response$fit)^2, na.rm = TRUE))


ggplot(data = testing_data) +
  geom_point(aes(Mileage, TradeValuation1)) + 
  geom_point(aes(Mileage, pred.response$fit), colour = "violetred") + 
  # geom_line(aes(Mileage, exp(pred.link$fit + 30*pred.link$se.fit)), color = "slategrey", linetype = "dashed", size = 0.8) +
  # geom_line(aes(Mileage, exp(pred.link$fit - 30*pred.link$se.fit)), color = "slategrey", linetype = "dashed", size = 0.8) +
  theme_bw()



