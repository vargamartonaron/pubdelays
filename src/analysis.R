library(tidyverse)
library(dbscan)
library(ggplot2)
library(FactoMineR)
library(Rtsne)
library(ggridges)
library(cluster)
library(survival)
library(survminer)
library(corrplot)
library(viridis)
library(papaja)

# Load the data
data <- readr::read_csv("../data/processed.csv")

# Inspect the data
glimpse(data)

# Convert dates to Date type if needed
data <- data |>
  mutate(received = as.Date(received),
         article_date = as.Date(article_date),
         total_delay = acceptance_delay + publication_delay)


# Summary statistics for delays
summary_stats <- data %>%
  summarise(
    mean_acceptance_delay = mean(acceptance_delay, na.rm = TRUE),
    median_acceptance_delay = median(acceptance_delay, na.rm = TRUE),
    sd_acceptance_delay = sd(acceptance_delay, na.rm = TRUE),
    mean_publication_delay = mean(publication_delay, na.rm = TRUE),
    median_publication_delay = median(publication_delay, na.rm = TRUE),
    sd_publication_delay = sd(publication_delay, na.rm = TRUE)
  )
sink("../results/summary.txt")
summary_stats
sink()
sink("../results/summary_builtin.txt")
summary(data)
sink()

# -------------------
# correlations

# Add year column
data <- data %>%
  mutate(year = lubridate::year(received)) |>
  filter(year >= 2015)

# Plot yearly trends in delays
ggplot(data, aes(x = year)) +
  geom_line(
    stat = "summary",
    fun = "median",
    aes(y = acceptance_delay, color = "Acceptance Delay")
  ) +
  geom_line(
    stat = "summary",
    fun = "median",
    aes(y = publication_delay, color = "Publication Delay")
  ) +
  labs(
    title = "Yearly Trends in Acceptance and Publication Delays",
    x = "Year",
    y = "Delay (Days)",
    color = "Metric"
  ) +
  theme_apa()

ggsave("../results/yearly_trends.pdf")


# ANOVA for acceptance delay by quartile
anova_result <- aov(acceptance_delay ~ sjr_best_quartile, data = data)
sink("../results/anova_result.txt")
summary(anova_result)
sink()

# Post-hoc test
sink("../results/anova_posthoc.txt")
TukeyHSD(anova_result)
sink()


# Linear regression model for acceptance delay
lm_model <- lm(
  acceptance_delay ~ h_index + sjr + rank + is_psych + is_mega + open_access,
  data = data
)

sink("../results/lm_model.txt")
summary(lm_model)
sink()

rtsne_result <- Rtsne(as.matrix(na.omit(data[, c("acceptance_delay", "publication_delay", "h_index", "sjr", "rank")])), dims = 2, check_duplicates = FALSE, num_threads = 0)
reduced_data <- as_data_frame(rtsne_result$Y)
colnames(reduced_data) <- c("Dim1", "Dim2")
dbscan_result <- dbscan(reduced_data, eps = 0.5, minPts = 10)
reduced_data$cluster <- as_factor(dbscan_result$cluster)

ggplot(reduced_data, aes(x = Dim1, y = Dim2, color = cluster)) +
  geom_point(alpha = 0.5, size = 0.7) +
  scale_color_manual(
    values = c("black", RColorBrewer::brewer.pal(8, "Set1")),
    name = "Cluster",
    labels = c("Noise", paste0("Cluster ", unique(dbscan_result$cluster[dbscan_result$cluster > 0])))
  ) +
  theme_minimal() +
  labs(
    title = "DBSCAN Clustering Visualization (t-SNE Reduced)",
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

ggsave("../results/clusters.pdf")

# Create survival object
surv_obj <- Surv(time = data$acceptance_delay, event = !is.na(data$acceptance_delay))

# Fit Kaplan-Meier curve by quartile
km_fit <- survfit(surv_obj ~ sjr_best_quartile, data = data)

# Plot survival curves
ggsurvplot(km_fit, data = data,
           title = "Survival Analysis: Time to Acceptance by Quartile",
           xlab = "Days to Acceptance",
           ylab = "Proportion Accepted")

ggsave("../results/survplot.pdf")


ggplot(data, aes(x = acceptance_delay, y = as.factor(year), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Delay") +
  labs(title = "Ridgeline Plot of Acceptance Delays Over Time",
       x = "Acceptance Delay (Days)", y = "Year") +
  theme_apa()

ggsave("../results/density_ridgeplot.pdf")
