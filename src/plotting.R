library(readr)
library(ggplot2)
library(papaja)
library(dplyr)
library(tidyr)
setwd('~/pubdelays')

articles <- readr::read_tsv('/users/usumusu/pubdelays/journal_articles.tsv')

acceptance_data <- articles |>
  dplyr::group_by(article_date) |>
  dplyr::filter(lubridate::year(article_date) >= 2016) |>
  dplyr::reframe(delay=median(acceptance_delay, na.rm=T))

  
acceptance_plot <- ggplot(acceptance_data, aes(x = article_date, y = delay, group = 1)) +
  #geom_smooth(method = "loess", se = TRUE, span = 1, color = "black", linewidth = 1.2) +
  geom_point(alpha = 1/ 2000) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')
  labs(y = "Acceptance delay (days)", x = "Year") +
  theme_apa() +
  theme(axis.text.x = element_text(size = 16, family = "Times", hjust = 1),
        axis.text.y = element_text(size = 16, family = "Times"),
        axis.title = element_text(size = 22, family = "Times")) +
  ylim(0, 1000)

  ggsave('acceptance_plot.pdf', width = 7, height = 7, scale = 0.9, dpi = 100)