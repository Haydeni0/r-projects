library(gridExtra)

my_colours = brewer.pal(11, "Set1")

## Continuous variables ----

# Mileage
ggplot(data = offers_made) +
  geom_point(aes(Mileage, TradeValuation1), color = my_colours[1]) + theme_bw()
# Age
ggplot(data = offers_made) +
  geom_point(aes(Age, TradeValuation1), color = my_colours[2]) + theme_bw()
# PreviousOwners
ggplot(data = offers_made) +
  geom_jitter(aes(factor(PreviousOwners), TradeValuation1), color = my_colours[2], width = 0.3, alpha = 0.1) + 
  geom_violin(aes(factor(PreviousOwners), TradeValuation1), color = my_colours[1], fill = NaN, size = 0.5) +
  xlab("PreviousOwners") +
  theme_bw()
# Date
ggplot(data = offers_made) +
  geom_point(aes(OfferDate, TradeValuation1), color = my_colours[2]) + theme_bw()

## Discrete Variables --------------

col_list <-
  c("Condition",
    "ServiceHistory",
    "FuelType",
    "TransmissionType",
    "Colour",
    "BodyType",
    "Make")

getMeanPlot <- function(col_name) {
  gb <- offers_made %>% group_by_(col_name[[1]])
  data_summary <-
    summarise(gb, MeanTV = mean(TradeValuation1))
  # data_summary <- data_summary[order(data_summary$MeanTV),]
  # browser()
  p <- ggplot(data = data_summary) +
    geom_bar(aes_(
      x = as.name(col_name),
      y = as.name("MeanTV")
    ), stat = "identity")
  return(p)
}

plot_list <- lapply(col_list, getMeanPlot)
do.call("grid.arrange", c(plot_list, ncol = 1, top = "asd"))

# Be careful of making conclusions if there is limited data
count(offers_made, FuelType)
count(offers_made, TransmissionType)
count(offers_made, Colour)
count(offers_made, Condition)
count(offers_made, ServiceHistory)
count(offers_made, BodyType)
count(offers_made, Make)

