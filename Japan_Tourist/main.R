library(tidyverse)
library(xts)
library(hablar)
library(tseries)
library(psd)

df <- read_csv("Japan_Tourist.csv")

# Convert to a time series vector in order
df <- pivot_longer(df, cols=2:13, names_to = "month", values_to = "visitors")

df$month <- match(df$month, month.name)
df$date <- as.yearmon(paste(df$Year, df$month), "%Y %m")
df <- transmute(df, date = date, visitors = visitors)
df <- df[order(df$date),]

ggplot(data = df) + geom_line(aes(as.Date(date), visitors)) + theme_bw() + 
  scale_y_log10() + scale_x_date(
    date_breaks = "2 years")

ts <- xts(df$visitors, order.by = df$date)

ts_res <- diff(log(ts))
ts_res <- ts_res[2:nrow(ts_res)]
ts_sres <- diff(ts_res, 2)
ts_sres <- ts_sres[5:nrow(ts_sres)]


plot(ts)
plot(ts_res)
plot(ts_sres)

spectrum(ts_res)
ps <- pspectrum(ts_res)
plot(ps$freq, ps$spec)

acf(ts_res)
acf(ts_sres)

