library("rstan") # observe startup messages
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(tidyverse)

# Import and tidy data
rats.tbl <- as_tibble(read.delim("rats_data.txt", sep = " ")) %>%
  mutate(ID = 1:30) %>% pivot_longer(1:5)

names(rats.tbl) <- c("ID", "day", "weight")

rats.tbl$day <-
  gsub("day", rats.tbl$day, replacement = "") %>% as.integer()


ggplot(data = rats.tbl) + geom_point(aes(x = day, y = growth, color = ID)) + theme_bw()


## Try a GLM----
# Standard linear regression
model.glm <-
  glm(growth ~ day,
      data = rats.tbl,
      family = gaussian(link = "identity"))
summary(model.glm)


pred.data <- tibble(day = seq(
  from = 1,
  to = 36,
  length.out = 100
))
pred <-
  predict(model.glm, newdata = pred.data)

# Plot predictions
ggplot() + geom_point(data = rats.tbl, aes(x = day, y = growth, color = ID)) +
  theme_bw() + geom_line(aes(x = pred.data$day, y = pred))




## Use hierarchical model ----

y <- as.matrix(read.table('rats_data.txt', header = TRUE))
x <- c(8, 15, 22, 29, 36)
xbar <- mean(x)
N <- nrow(y)
T <- ncol(y)
rats.data = list(y = y, x=x, xbar=xbar, N=N, T=T)
rats_fit <- stan("rats.stan", data = rats.data)

summary(rats_fit)$summary

