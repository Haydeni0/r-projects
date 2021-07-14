library(tidyverse)

offers_made <-
  read.csv("OffersMadeR.csv",
           header = T,
           na.strings = c("", "NA"))
sale_data <-
  read.csv("SaleDataR.csv",
           header = T,
           na.strings = c("", "NA"))


#Rescale Sale/TradeValuation2
sale_data$`Sale_TradeValuation1` = sale_data$`Sale_TradeValuation1` / 100

################################
# Merge Offers and Sales
sales_merged <- merge(offers_made, mutate(sale_data, saleID = 1:nrow(sale_data)), by = "VRM")
# Remove duplicate sales, choose the one with the later offer date, as it is likely the one that is bought by cazoo
sales_merged <- sales_merged[order(sales_merged$OfferDate, decreasing=T),]
is_dup <- duplicated(sales_merged$saleID)
sales_merged <- sales_merged[!is_dup,]
sales_merged <- sales_merged[order(sales_merged$OfferDate, decreasing=F),]

################################
# Models
# Modelling SalePrice against all explanatory variables
model1 <- glm(SalePrice ~ Condition + Age + Mileage.x + PreviousOwners + ServiceHistory + Make + 
                TransmissionType + FuelType + Colour + BodyType, 
              family = gaussian(), data = sales_merged)
# Modelling SalePrice against all explanatory variables, using a log link
model2 <- glm(SalePrice ~ Condition + Age + Mileage.x + PreviousOwners + ServiceHistory + Make + 
                TransmissionType + FuelType + Colour + BodyType, 
              family = gaussian(link = "log"), data = sales_merged)












pmodel <- model2

# Dont include Na values
num_testing <- nrow(na.omit(sales_merged))
sort_perm <- sort(na.omit(sales_merged)$SalePrice, index.return = TRUE)

pred.response <- predict(pmodel,newdata=na.omit(sales_merged), type = "response",se.fit = T)

ggplot() + geom_line(data = na.omit(sales_merged), aes(1:num_testing, SalePrice[sort_perm$ix]), color = "red", size = 1) + 
  geom_point(aes(1:num_testing, pred.response$fit[sort_perm$ix]), alpha = 0.3) +
  ggtitle("Predicted values for the testing dataset")+
  xlab("((sorted observations))") + ylab("SalePrice")+
  theme_bw()

