
library(caTools)
set.seed(0)
training_idx <-sample.split(sales_merged$VRM, 2/3)
training_data <- (sales_merged[training_idx,])
testing_data <- (sales_merged[-training_idx,])
# Sort the dataset based on SalePrice
sort_perm <- sort(testing_data$SalePrice, index.return = TRUE)
num_testing <- nrow(testing_data)

# Modelling SalePrice against all explanatory variables (except trade values)
model1 <- glm(SalePrice ~ Condition + Age + Mileage.x + PreviousOwners + ServiceHistory + Make + 
                TransmissionType + FuelType + Colour + BodyType, 
              family = gaussian(link = "log"), data = training_data)
# Modelling SalePrice against all explanatory variables (except trade values and NA columns)
model2 <- glm(SalePrice ~ Condition + Age + Mileage.x + PreviousOwners + ServiceHistory + Make + Colour, 
              family = gaussian(link = "identity"), data = training_data)

# Modelling SalePrice against trade value
model3 <- glm(SalePrice ~ TradeValuation1, 
              family = gaussian(link = "identity"), data = training_data)

# Modelling SalePrice against all explanatory variables
model4 <- glm(SalePrice ~ log(TradeValuation1) + Condition + Age + Mileage.x + PreviousOwners + ServiceHistory + Make + 
                TransmissionType + FuelType + Colour + BodyType, 
              family = gaussian(link = "log"), data = training_data)

# Model 4 is obviously the best, but doesnt predict from data with missing values, and also includes the trade valuation which 
# might be the thing we want to predict?
# Model 1 is the second best, but doesnt predict from data with missing values.
# Model 2 is far less accurate, but can predict from data with missing TransmissionType, FuelType and BodyType
# I feel like model3 is just cheating, as we just use tradevaluation to explain the sale price..

pmodel <- model2
rm(pred.response)

summary(pmodel)
par(mfrow = c(2,2))
plot(pmodel)
par(mfrow = c(1,1))



num_testing <- nrow((testing_data))
sort_perm <- sort(testing_data$SalePrice, index.return = TRUE)

pred.response <- predict(pmodel,newdata=testing_data, type = "response",se.fit = T)

ggplot() + geom_line(data = testing_data, aes(1:num_testing, SalePrice[sort_perm$ix]), color = my_colours[1], size = 1) + 
  geom_point(aes(1:num_testing, pred.response$fit[sort_perm$ix]), alpha = 0.3) +
  ggtitle("Predicted values for the testing dataset")+
  xlab("((sorted observations))") + ylab("TradeValuation1")+
  theme_bw()


# Dont include Na values
num_testing <- nrow(na.omit(testing_data))
sort_perm <- sort(na.omit(testing_data)$SalePrice, index.return = TRUE)

pred.response <- predict(pmodel,newdata=na.omit(testing_data), type = "response",se.fit = T)

ggplot() + geom_line(data = na.omit(testing_data), aes(1:num_testing, SalePrice[sort_perm$ix]), color = my_colours[1], size = 1) + 
  geom_point(aes(1:num_testing, pred.response$fit[sort_perm$ix]), alpha = 0.3) +
  ggtitle("Predicted values for the testing dataset")+
  xlab("((sorted observations))") + ylab("TradeValuation1")+
  theme_bw()

## TEST ----

ggplot(data = training_data) + geom_point(aes(TradeValuation1, SalePrice))




