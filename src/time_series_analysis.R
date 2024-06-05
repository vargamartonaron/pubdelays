setwd("~/pubdelays")

library(readr)
library(dplyr)
library(lubridate)
library(forecast)
library(tseries)
library(nlme)
library(lmtest)
install.packages("zoo", repos = "https://cloud.r-project.org/")
library(zoo)
install.packages("astsa", repos = "https://cloud.r-project.org/")
library(astsa)


data = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")

data = data |> 
  mutate(article_date = floor_date(as_date(article_date), "month")) |> 
  group_by(article_date) |> 
  mutate(acceptance_delay = mean(acceptance_delay, na.rm = TRUE))


data <- data[data$article_date >= as.Date("2016-01-01"), ]

data |> 
  distinct(article_date, keep.all = T) |>
  arrange(article_date) |>
  print(n = 200)

#stop("temporary")

#Create time series data

ts_data_filtered <- data |> 
  select(article_date, acceptance_delay) |> 
  filter(!is.na(article_date) & !is.na(acceptance_delay)) |> 
  filter(acceptance_delay <= 700 & acceptance_delay >= 10) |> 
  distinct() |> 
  mutate(acceptance_delay = round(acceptance_delay, 0))


#############################################################################
# Example
#############################################################################

# Convert data to time series object
data.ts <- ts(data[,2], start=c(2016,1), end = c(2023, 2), frequency = 12)

# View data
data.ts

# Plot data to visualise time series
options(scipen=5)
plot(data.ts, ylim=c(0,40000), type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2014, col="gray", lty="dashed", lwd=2)

# View ACF/PACF plots of undifferenced data
acf2(data.ts, max.lag=24)

# View ACF/PACF plots of differenced/seasonally differenced data
acf2(diff(diff(data.ts,12)), max.lag=24)

# Create variable representing step change and view
step <- as.numeric(as.yearmon(time(data.ts))>='Dec 2019')
step

# Create variable representing ramp (change in slope) and view
ramp <- append(rep(0,36), seq(1,12,1))
ramp  

# Use automated algorithm to identify p/q parameters
# Specify first difference = 1 and seasonal difference = 1
model1 <- auto.arima(data.ts, seasonal=TRUE, xreg=cbind(step,ramp), max.d=1, max.D=1, stepwise=FALSE, trace=TRUE)

# Check residuals
checkresiduals(model1)
Box.test(model1$residuals, lag = 24, type = "Ljung-Box")

# Estimate parameters and confidence intervals
summary(model1)
confint(model1)

# To forecast the counterfactual, model data excluding post-intervention time period
model2 <- Arima(window(data.ts, end=c(2019,12)), order=c(2,1,0), seasonal=list(order=c(0,1,1), period=12))

# Forecast 12 months post-intervention and convert to time series object
fc <- forecast(model2, h=12)
fc.ts <- ts(as.numeric(fc$mean), start=c(2016,1), frequency=12)

# Combine with observed data
data.ts.2 <- ts.union(data.ts, fc.ts)
data.ts.2

# Plot
plot(data.ts.2, type="l", plot.type="s", col=c('blue','red'), xlab="Month", ylab="Dispensings", linetype=c("solid","dashed"), ylim=c(0,40000))
abline(v=2014, lty="dashed", col="gray")


#############################################################################

#Delete days from publication date

data = data |> 
  mutate(article_date = floor_date(as_date(article_date), "month")) |> 
  group_by(article_date) |> 
  mutate(acceptance_delay = mean(acceptance_delay, na.rm = TRUE))

print(colnames(data))
class(data$article_date)
class(data$acceptance_delay)

data <- data[data$article_date >= as.Date("2016-01-01"), ]

data |> 
  distinct(article_date, keep.all = T) |>
  arrange(article_date) |>
  print(n = 200)

#stop("temporary")

#Create time series data

ts_data_filtered <- data |> 
  select(article_date, acceptance_delay) |> 
  filter(!is.na(article_date) & !is.na(acceptance_delay)) |> 
  filter(acceptance_delay <= 700 & acceptance_delay >= 10) |> 
  distinct() |> 
  mutate(acceptance_delay = round(acceptance_delay, 0))


#Create a time series object

series_filtered = ts(ts_data_filtered$acceptance_delay, start = c(2016, 1), end = c(2023, 2), frequency = 12)
#plot(series_filtered)
#acf(series_filtered)

# Save the time series plot to a PDF file
pdf("series_filtered_plot.pdf", width = 8, height = 6)
plot(series_filtered, main = "Time Series Plot of Acceptance Delay", xlab = "Time", ylab = "Acceptance Delay")
dev.off()

#########################
# Assumptions
#########################

#### ACF

# Save the ACF plot to a PDF file
pdf("series_filtered_acf.pdf", width = 8, height = 6)
acf(series_filtered, main = "Autocorrelation Function of Acceptance Delay")
dev.off()

#### PACF

pdf("series_filtered_pacf.pdf", width = 8, height = 6)
pacf(series_filtered)
dev.off()

#### Stationarity

#adf test
adf_result <- adf.test(series_filtered)
adf_result_pdf <- "stationarity.pdf"

pdf(adf_result_pdf, width = 8, height = 6)
plot.new()
par(mar=c(5,5,2,2))
output_text <- ""
p_value <- adf_result$p.value
if (p_value < .Machine$double.eps) {
  output_text <- paste(output_text, "The p-value is extremely small (<", .Machine$double.eps, ")\n", sep="")
} else {
  output_text <- paste(output_text, "The p-value is: ", p_value, "\n", sep="")
}
output_text <- paste(output_text, "ADF Test Statistic: ", adf_result$statistic, "\n", sep="")
output_text <- paste(output_text, "Lag order: ", adf_result$parameter, "\n", sep="")
output_text <- paste(output_text, "Alternative Hypothesis: ", adf_result$alternative, "\n", sep="")
text(0.1, 0.9, output_text, adj = c(0, 1), cex = 0.8)
dev.off()

#### Seasonality

#Decompose the time series
#determine if additive or multiplicative

#Additive decomposition
add_decomposed = decompose(series_filtered, type = "additive")
add_residuals = add_decomposed$random

#Multiplicative decomposition
multi_decomposed = decompose(series_filtered, type = "multiplicative")
multi_residuals = multi_decomposed$random

#Loess decomposition
loess_decomposed = stl(series_filtered, s.window = "periodic")
loess_residuals = loess_decomposed$time.series[, 3]

#Standard deviations
sd_loess = sd(na.omit(loess_residuals))
sd_add = sd(na.omit(add_residuals))
sd_multi = sd(na.omit(multi_residuals))


if (sd_add < sd_multi) {
  chosen_decomposition <- add_decomposed
  chosen_type <- "additive"
} else {
  if (sd_loess < min(sd_add, sd_multi)) {
    chosen_decomposition <- loess_decomposed
    chosen_type <- "Loess"
  } else if (sd_add < sd_multi) {
    chosen_decomposition <- add_decomposed
    chosen_type <- "additive"
  } else {
    chosen_decomposition <- multi_decomposed
    chosen_type <- "multiplicative"
  }
}

cat("Chosen decomposition type:", chosen_type, "\n")


pdf("decomposition.pdf", width = 16, height = 8)
par(mfrow = c(4, 1))
plot(chosen_decomposition$trend, main = paste("Trend Component (", chosen_type, ")", sep = ""), ylab = "Trend", xlab = "Time")
plot(chosen_decomposition$seasonal, main = paste("Seasonal Component (", chosen_type, ")", sep = ""), ylab = "Seasonal", xlab = "Time")
plot(chosen_decomposition$random, main = paste("Random Component (", chosen_type, ")", sep = ""), ylab = "Random", xlab = "Time")
plot(series_filtered, main = "Original Time Series", ylab = "Acceptance Delay", xlab = "Time")
par(mfrow = c(1, 1)) 
dev.off()

#stop("Assumptions checked")

#### Seasonal adjustment

ts_adjusted = seasadj(chosen_decomposition)
pdf("seasonal_adjustment.pdf", width = 16, height = 8)
plot(ts_adjusted, main = "Seasonally Adjusted Time Series", ylab = "Acceptance Delay", xlab = "Time")
dev.off()
print("Seasonal Adjusment done")
#### Transform to stationarity if needed

ddseries = diff(series_filtered)
print("ddseries done")
#var(ddseries)
sink("variance.txt")
cat("Variance of the differenced series:", var(ddseries), "\n")
print(summary(ddseries))
sink()

#adf.test(ddseries)
sink("adf_test.txt")
cat("ADF test results for the differenced series:\n")
print(adf.test(ddseries))
sink()

logseries = log(series_filtered)

#plotting autocorrelation function

acf(series_filtered)
sink("acf.txt")
cat("Autocorrelation function of the original series:\n")
print(acf(series_filtered))
sink()

pacf(series_filtered)
sink("pacf.txt")
cat("Partial autocorrelation function of the original series:\n")
print(pacf(series_filtered))
sink()

#modeling the series as a function of time

length(series_filtered)

time = seq(1, 86, by = 1)
time = time - mean(time)
time2 = time^2
time3 = time^3

linear = gls(series_filtered ~ time)
quadratic = gls(series_filtered ~ time + time2)
cubic = gls(series_filtered ~ time + time2 + time3)

models <- list(linear, quadratic, cubic)
aic_values <- sapply(models, AIC)
best_model_index <- which.min(aic_values)
best_model <- models[[best_model_index]]

print(summary(linear))
print(summary(quadratic))
print(summary(cubic))
print(paste("Best model based on AIC: Model", best_model_index))

sink("best_model.txt")
cat("Best model based on AIC:\n")
print(summary(best_model))
sink()


pdf("best_model_plot.pdf", width = 8, height = 6)
plot(best_model, main = "Best Model Fit", xlab = "Time", ylab = "Acceptance Delay")
dev.off()

#interrupted time series

time = seq(1, 86, by = 1)
cutoff_date = as.Date("2019-12-01")

start_date <- as.Date("2016-01-01")
end_date <- as.Date("2023-02-01")
date_sequence <- seq(from = start_date, to = end_date, by = "month")

dates_before = date_sequence[date_sequence > cutoff_date]
dates_after = date_sequence[date_sequence < cutoff_date]
date_before_cutoff = length(dates_before)
date_after_cutoff = length(dates_after)
cat("Number of months before 2020-01:", date_before_cutoff, "\n")
cat("Number of months after 2020-01:", date_after_cutoff, "\n")

event = c(rep(0, times = 48), rep(1, times = 27))
tai = c(rep(0, times = 48), seq(from = 1, to = 27, by = 1))

its = gls(series_filtered ~ time + event + tai)
summary(its)
plot(its)

#estimating seasonal effects

time = seq(1, 106, by = 1)
seas = cycle(series_filtered)
seas = factor(seas)

model = gls(series_filtered ~ time + seas)
summary(model)
plot(model)

#harmonic seasonal model

time = seq(1, 106, by = 1)

sin1 = sin(2 * pi * 1 * time / 12)
cos1 = cos(2 * pi * 1 * time / 12)
sin2 = sin(2 * pi * 2 * time / 12)
cos2 = cos(2 * pi * 2 * time / 12)
sin3 = sin(2 * pi * 3 * time / 12)
cos3 = cos(2 * pi * 3 * time / 12)
sin4 = sin(2 * pi * 4 * time / 12)
cos4 = cos(2 * pi * 4 * time / 12)
sin5 = sin(2 * pi * 5 * time / 12)
cos5 = cos(2 * pi * 5 * time / 12)
sin6 = sin(2 * pi * 6 * time / 12)
cos6 = cos(2 * pi * 6 * time / 12)

model_harmonic = gls(series_filtered ~ time + sin1 + cos1 + sin2 + cos2 + sin3 + cos3 + sin4 + cos4 + sin5 + cos5 + sin6 + cos6)
summary(model_harmonic)
plot(model_harmonic)

#dynamic regression modeling

model = auto.arima(series_filtered, ic = "bic")
summary(model)

#residual analysis

res = residuals(model)
dwtest(model)
Box.test(res, type = "Lj")


#forecasting

forecast = forecast(model, h = 12)
accuracy(forecast)

plot(forecast)


#Needed features
##generalize code: should reflect back and decide on its own what analysis to do, better naming system
##add download functions
##package management
