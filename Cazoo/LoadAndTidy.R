library(tidyverse)
library(hablar)
library(RColorBrewer)
library(units)

# Load and tidy data ----

offers_made <-
  read.csv("OffersMade.csv",
           header = T,
           na.strings = c("", "NA"))
sale_data <-
  read.csv("SaleData.csv",
           header = T,
           na.strings = c("", "NA"))

names(offers_made) <- offers_made[1, ]
names(sale_data) <- sale_data[1, ]
offers_made <- offers_made[2:nrow(offers_made), ]
offers_made <- offers_made[, -15]
sale_data <- sale_data[2:nrow(sale_data), ]

# Replace pound signs
# offers_made <-

offers_made <- lapply(offers_made, function(x) {
  gsub("£", "", x)
})
sale_data <- lapply(sale_data, function(x) {
  gsub("£", "", x)
})
offers_made <- lapply(offers_made, function(x) {
  gsub("?", "", x)
})
sale_data <- lapply(sale_data, function(x) {
  gsub("?", "", x)
})
offers_made <- lapply(offers_made, function(x) {
  gsub(",", "", x)
})
sale_data <- lapply(sale_data, function(x) {
  gsub(",", "", x)
})
sale_data <- lapply(sale_data, function(x) {
  gsub("%", "", x)
})
offers_made <- lapply(offers_made, function(x) {
  gsub(" ", "", x)
})
sale_data <- lapply(sale_data, function(x) {
  gsub(" ", "", x)
})

# Set units
# offers_made <- offers_made %>% mutate(
#   Age = set_units(Age, Years),
#   Mileage = set_units(Mileage, Miles),
#   TradeValuation1 = set_units(TradeValuation1, "£"),
#   CompetitorB = set_units(CompetitorB, "£"),
#   CompetitorC = set_units(CompetitorC, "£")
#   )

# Convert to tibble
offers_made <- as_tibble(offers_made)
sale_data <- as_tibble(sale_data)


# Convert all to factors
offers_made <-
  offers_made %>% convert(fct(1:ncol(offers_made)))
sale_data <-
  sale_data %>% convert(fct(1:ncol(sale_data)))
# Convert double
offers_made <-
  as_tibble(offers_made) %>% convert(dbl(c(4, 5, 6, 14, 15, 16)))
sale_data <-
  as_tibble(sale_data) %>% convert(dbl(c(4, 6:8)))
# Convert date
offers_made <-
  as_tibble(offers_made) %>% convert(dte(2))
sale_data <-
  as_tibble(sale_data) %>% convert(dte(2:3))

#Rescale Sale/TradeValuation1
sale_data$`Sale_TradeValuation2` = sale_data$`Sale_TradeValuation2` / 100

# Relevel factors
levels(offers_made$Condition) <- c("excellent", "good", "average", "poor" )
offers_made$ServiceHistory <- relevel(offers_made$ServiceHistory, "full")
offers_made$FuelType <- relevel(offers_made$FuelType, "Petrol")
offers_made$BodyType <- relevel(offers_made$BodyType, "Hatchback")
offers_made$TransmissionType <- relevel(offers_made$TransmissionType, "Manual")

levels(sale_data$GradeatAuction) <- c("5", "4", "3", "2", "1")

## Merge Offers and Sales ----
sales_merged <- merge(offers_made, mutate(sale_data, saleID = 1:nrow(sale_data)), by = "VRM")
# Remove duplicate sales, choose the one with the later offer date, as it is likely the one that is bought by cazoo
sales_merged <- sales_merged[order(sales_merged$OfferDate, decreasing=T),]
is_dup <- duplicated(sales_merged$saleID)
sales_merged <- sales_merged[!is_dup,]
sales_merged <- sales_merged[order(sales_merged$OfferDate, decreasing=F),]
