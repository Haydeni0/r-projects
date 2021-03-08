library(tidyverse)
library(gridExtra)
library(nnet)
library(aod)

# Load and set up data
filename <- "iris.data"
iris.tbl <- tibble(read.csv(filename))
names(iris.tbl) <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "class")
iris.tbl$class <- as.factor(iris.tbl$class)


# Exploratory plots
plot.sepal <- ggplot(data = iris.tbl) + geom_point(aes(x = sepal_length, y = sepal_width, color = class)) +
  theme_bw()
plot.petal <- ggplot(data = iris.tbl) + geom_point(aes(x = petal_length, y = petal_width, color = class)) +
  theme_bw()
grid.arrange(plot.sepal, plot.petal,nrow = 1)



model1 <- multinom(class ~ sepal_length + sepal_width + petal_length + petal_width,data = iris.tbl)
