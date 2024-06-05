# Regression analysis

library(readr)
library(lubridate)
library(nlme)
library(lmtest)
library(dplyr)
library(lme4)
library(segmented)
library(Matrix)
library(ggplot2)
library(car)
library(tools)
#install.packages("caret", repos = "https://cloud.r-project.org/")
library(caret)
#install.packages("ggthemes", repos = "https://cloud.r-project.org/")
library(ggthemes)
#install.packages("papaja", repos = "https://cloud.r-project.org/")
library(papaja)

###################################
##### Load data
###################################

data = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")

#colnames(data)
print(max(data$article_date))

###################################
# Loess regression
###################################

#set.seed(42) # Set seed for reproducibility


data <- data %>%
  filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |> 
  mutate(article_date_numeric = as.numeric(article_date)) |> 
  dplyr::sample_n(1000000)
  
print(max(data$article_date))
#colnames(data)

print("data done")
#calculate_rmse_cv <- function(span, k = 5) {
#  folds <- createFolds(data$acceptance_delay, k = k, list = TRUE)
#  rmse_values <- sapply(folds, function(fold) {
#    train_data <- data[-fold, ]
#    test_data <- data[fold, ]
#    model <- loess(acceptance_delay ~ article_date_numeric, data = train_data, span = span, family = "gaussian", degree = 2)
#    predicted <- predict(model, newdata = test_data)
#    sqrt(mean((test_data$acceptance_delay - predicted)^2))
#  })
#  mean(rmse_values)
#}

#print("rmse function done")


# Span values to test
#span_values <- seq(0.1, 0.2, by = 0.02)
#rmse_values <- numeric(length(span_values))

#print("values given")

# Calculate RMSE for each span value using k-fold cross-validation
#for (i in seq_along(span_values)) {
#  rmse_values[i] <- calculate_rmse_cv(span_values[i])
#}

#print("k-fold cross-validation done")

# Create a dataframe for span and rmse values
#validation_data <- data.frame(span = span_values, rmse = rmse_values)

#print("RMSE done")

# Plot validation error graph
#validation_error = ggplot(validation_data, aes(x = span, y = rmse)) +
#  geom_line() +
#  geom_point() +
#  labs(title = "Validation Error (RMSE) vs. Span",
#       x = "Span",
#       y = "Root Mean Squared Error (RMSE)") +
#  theme_minimal()

#ggsave("validation_error_2.pdf", dpi = 150)

#print("validation error done")

# Determine optimal span (with minimum RMSE)
#optimal_span <- span_values[which.min(rmse_values)]

optimal_span = 0.15

#print(optimal_span)



###################################
# Loess regression for everything
###################################
loess_model <- loess(acceptance_delay ~ article_date_numeric, data = data, span = optimal_span, family = "gaussian", degree = 2)
summary(loess_model)


summary_text <- capture.output(summary(loess_model))
writeLines(summary_text, "loess_summary.txt")
print("loess model done")

# Check assumptions by plotting residuals
data <- data %>%
  mutate(fitted = predict(loess_model),
         residuals = acceptance_delay - fitted) 


# Residual plot
loess_residual_plot = ggplot(data, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.01) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Plot",
       x = "Fitted Values",
       y = "Residuals") +
  theme_apa()
ggsave("loess_residual_plot.pdf", dpi = 150)


y_limits = c(50, 150)

loess_plot = ggplot(data, aes(x = article_date, y = acceptance_delay)) +
  geom_smooth(method = "loess", se = FALSE, span = optimal_span, method.args = list(degree = 2)) +
  labs(title = "LOESS Regression of Acceptance Delay",
       x = "Article Date",
       y = "Acceptance Delay (days)") +
  xlim(as.Date(c("2016-01-01", "2022-11-01"))) +
  coord_cartesian(ylim = y_limits) +
  theme_apa()
ggsave("loess_plot.pdf", dpi = 150)



ggplot(data, aes(x = article_date, y = acceptance_delay)) +
  geom_line(aes(y = fitted), color = "blue") +
  labs(title = "LOESS Regression of Acceptance Delay over Article Date",
       x = "Article Date",
       y = "Acceptance Delay (days)") +
  xlim(as_date(c("2016-01-01", "2022-11-01"))) +
  coord_cartesian(ylim = y_limits) +
  theme_apa()
ggsave("loess_plot_fitted.pdf", dpi = 150)



print("plots done")
###################################
# Loess regression for Covid
###################################
data = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")
data = data |> 
  filter(article_date >= lubridate::as_date('2016-01-01') &
           article_date <= lubridate::as_date('2022-12-01') &
           (ifelse(is_covid, article_date > lubridate::as_date('2019-12-01'), TRUE))) |> 
  mutate(article_date_numeric = as.numeric(article_date)) |> 
  dplyr::sample_n(1000000)

#print(max(data$article_date))
#summary_text <- capture.output(summary(loess_model))
#writeLines(summary_text, "loess_summary_covid.txt")
#print("Covid loess model done")

y_limits = c(0, 150)

combined_loess_plot <- ggplot(data, aes(x = article_date, y = acceptance_delay, colour = is_covid)) +
  geom_smooth(method = "loess", se = FALSE, span = optimal_span, method.args = list(degree = 2)) +
  labs(title = "LOESS Regression of Articles' Acceptance Delay",
       x = "Article Date",
       y = "Acceptance Delay (days)") +
  theme_apa() +
  xlim(as_date(c("2016-01-01", "2022-11-01"))) +
  coord_cartesian(ylim = y_limits) +
  theme(legend.position = "bottom")
ggsave("loess_plot_combined.pdf", dpi = 150)
print("Covid Combined plots done")
###################################
# Loess regression for only psychology
###################################
data = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")
data = data |> 
  filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |> 
  filter(is_psych == TRUE) |> 
  mutate(article_date_numeric = as.numeric(article_date))

#print(max(data$article_date))
#loess_model <- loess(acceptance_delay ~ article_date_numeric, data = data, span = optimal_span, family = "gaussian", degree = 2)
#summary_text <- capture.output(summary(loess_model))
#writeLines(summary_text, "loess_summary_psychology.txt")
#print("Psychology loess model done")

y_limits = c(50, 200)

loess_plot = ggplot(data, aes(x = article_date, y = acceptance_delay)) +
  geom_smooth(method = "loess", se = FALSE, span = optimal_span, method.args = list(degree = 2)) +
  labs(title = "LOESS Regression of Psychology Articles' Acceptance Delay",
       x = "Article Date",
       y = "Acceptance Delay (days)") +
  theme_apa() +
  xlim(as_date(c("2016-01-01", "2022-11-01"))) +
  coord_cartesian(ylim = y_limits)
ggsave("loess_plot_psychology.pdf", dpi = 150)
print("Psychology done")
###################################
# Loess regression for publication delay
###################################
data = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")
data = data |> 
  filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |> 
  mutate(article_date_numeric = as.numeric(article_date)) |> 
  dplyr::sample_n(1000000)

#print(max(data$article_date))
#loess_model <- loess(publication_delay ~ article_date_numeric, data = data, span = optimal_span, family = "gaussian", degree = 2)
#summary(loess_model)
#summary_text <- capture.output(summary(loess_model))
#writeLines(summary_text, "loess_summary_publication_delay.txt")
#print("Publication delay loess model done")

y_limits = c(0, 50)

loess_plot = ggplot(data, aes(x = article_date, y = publication_delay)) +
  geom_smooth(method = "loess", se = FALSE, span = optimal_span, method.args = list(degree = 2)) +
  labs(title = "LOESS Regression of Publication Delay over Article Date",
       x = "Article Date",
       y = "Publication Delay (days)") +
  theme_apa() +
  xlim(as_date(c("2016-01-01", "2022-11-01"))) +
  coord_cartesian(ylim = y_limits)
ggsave("loess_plot_publication_delay.pdf", dpi = 150)
print("Publication Delay done")
###################################
# Loess regression for only megajournals
###################################
data = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")
data = data |> 
  filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |> 
  filter(is_mega == TRUE) |> 
  mutate(article_date_numeric = as.numeric(article_date))

#print(max(data$article_date))
#loess_model <- loess(acceptance_delay ~ article_date_numeric, data = data, span = optimal_span, family = "gaussian", degree = 2)
#summary(loess_model)
#summary_text <- capture.output(summary(loess_model))
#writeLines(summary_text, "loess_summary_mega.txt")
#print("Megajournal loess model done")

y_limits = c(100, 200)

loess_plot = ggplot(data, aes(x = article_date, y = acceptance_delay)) +
  geom_smooth(method = "loess", se = FALSE, span = optimal_span, method.args = list(degree = 2)) +
  labs(title = "LOESS Regression of Megajournal Articles' Acceptance Delay",
       x = "Article Date",
       y = "Acceptance Delay (days)") +
  theme_apa() +
  xlim(as_date(c("2016-01-01", "2022-11-01"))) +
  coord_cartesian(ylim = y_limits)
ggsave("loess_plot_mega.pdf", dpi = 150)
print("Megajournals done")
###################################
# Loess regression for receival
###################################
data = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")
data = data |> 
  filter(received >= lubridate::as_date('2016-01-01') & received <= lubridate::as_date('2022-11-01')) |> 
  mutate(received_numeric = as.numeric(received)) |> 
  dplyr::sample_n(1000000)

#print(max(data$received))
#loess_model <- loess(acceptance_delay ~ received_numeric, data = data, span = optimal_span, family = "gaussian", degree = 2)
#summary(loess_model)
#summary_text <- capture.output(summary(loess_model))
#writeLines(summary_text, "loess_summary_received.txt")
#print("Received loess model done")

y_limits = c(50, 150)

loess_plot = ggplot(data, aes(x = received, y = acceptance_delay)) +
  geom_smooth(method = "loess", se = FALSE, span = optimal_span, method.args = list(degree = 2)) +
  labs(title = "LOESS Regression of Acceptance Delay and Receival",
       x = "Receival Date",
       y = "Acceptance Delay (days)") +
  xlim(as_date(c("2016-01-01", "2022-11-01"))) +
  coord_cartesian(ylim = y_limits) +
  theme_apa()
ggsave("loess_plot_received.pdf", dpi = 150)
print("Received done")
###################################
# Loess regression for Disciplines
###################################
data = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")
data = data |> 
  filter(article_date >= lubridate::as_date('2016-01-01') &
           article_date <= lubridate::as_date('2022-11-01')) |> 
  mutate(article_date_numeric = as.numeric(article_date)) |> 
  dplyr::sample_n(1000000)

#print(max(data$article_date))
#loess_model <- loess(acceptance_delay ~ received_numeric, data = data, span = optimal_span, family = "gaussian", degree = 2)
#summary(loess_model)
#summary_text <- capture.output(summary(loess_model))
#writeLines(summary_text, "loess_summary_received.txt")
#print("Discipline loess model done")

y_limits = c(0, 300)

discipline_loess_plot <- ggplot(data, aes(x = article_date, y = acceptance_delay, colour = discipline)) +
  geom_smooth(method = "loess", se = FALSE, span = optimal_span, method.args = list(degree = 2)) +
  labs(title = "LOESS Regression of Different Disciplines' Acceptance Delay",
       x = "Article Date",
       y = "Acceptance Delay (days)") +
  theme_apa() +
  xlim(as_date(c("2016-01-01", "2022-11-01"))) +
  coord_cartesian(ylim = y_limits) +
  theme(legend.position = "right")
ggsave("loess_plot_discipline.pdf", dpi = 250)
print("Discipline plots done")



stop("Analysis done")


#Needs: better looking plots
#Needs: time series analysis
#Needs: fitted instead of new plot
#Fix: month date is wrong
#Fix: ylim fix

###################################
##### Assumptions
###################################

# Normality of residuals
postscript("normality.ps")
qqnorm(residuals(lm_model), pch = 20, cex = 0.5)
qqline(residuals(lm_model), lwd = 1)
hist(residuals(lm_model), breaks = 30, col = "grey", border = "white")
dev.off()

print("normality checked")

#shapiro.test(residuals(lm_model))

# Assumption 2: Homoscedasticity
pdf("homoscedasticity.pdf", width = 7, height = 7, compress = TRUE)
plot(lm_model, which = 1, cex = 0.5)
bptest(lm_model)
dev.off()

print("homoscedasticity checked")

# Assumption 3: Linearity

pdf("linearity.pdf", width = 7, height = 7, compress = TRUE)
plot(fitted(lm_model), residuals(lm_model), main = "Residuals vs Fitted")
abline(h = 0, col = "red")
dev.off()

# Assumption 4: Independence
pdf("normality")
dwtest(lm_model)
dev.off()

print("normality checked")

# Assumption 5: No multicollinearity
#pdf("multicollinearity.pdf")
#vif(lm_model)
#dev.off()

print("multicollinearity checked")