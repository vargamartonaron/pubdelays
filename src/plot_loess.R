library(tidyverse)
library(ggdark)
library(papaja)
library(viridis)

data <- readr::read_csv("/users/usumusu/pubdelays/data/processed.csv")
data <- data |>
  filter(ifelse(is_covid, article_date > as_date("2019-12-01"), TRUE))

combined_loess_plot <- ggplot(data, aes(x = article_date, y = acceptance_delay, group = is_covid, colour = after_stat(y))) +
			      stat_smooth(geom = "line", method = "loess", colour = "white", alpha = 0.6, span = 0.08, se = FALSE) +
                              geom_smooth(method = "loess", se = FALSE, span = 0.15, linewidth = 2) +
			      scale_color_viridis() +
			      coord_cartesian(ylim = c(20, 150)) +
			      scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2016-01-01", "2022-11-01"))) +
			      labs(title = "Smooth loess trend of acceptance delay in light of covid",
				   x = "Article date",
				   y = "Acceptance delay (day)") +
			      dark_mode(theme_apa(base_family = "Times", base_size = 24)) +
			      theme(legend.position = "none")
ggsave("combined_loess_plot.pdf", width = 16, height = 9, units = "in", dpi = 300)

loess_plot <-  ggplot(data, aes(x = article_date, y = acceptance_delay, colour = after_stat(y))) +
  geom_smooth(method = "loess", se = FALSE, span = 0.15, linewidth = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2019-12-01")), linetype = "dashed", color = "red") +
  labs(title = "Smooth loess trend of acceptance delay",
       x = "Article date",
       y = "Acceptance delay (day)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2016-01-01", "2022-11-01"))) +
  scale_color_viridis() +
  coord_cartesian(ylim = c(80, 150)) +
  dark_mode(theme_apa(base_family = "Times", base_size = 24)) +
  theme(legend.position = "none")

ggsave("loess_plot.pdf", width = 16, height = 9, units = "in", dpi = 300)

loess_plot_blank <-  ggplot(data, aes(x = article_date, y = acceptance_delay, colour = after_stat(y))) +
  geom_smooth(method = "loess", se = FALSE, span = 0.15, linewidth = 2) +
  labs(title = "Smooth loess trend of acceptance delay",
       x = "Article date",
       y = "Acceptance delay (day)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2016-01-01", "2022-11-01"))) +
  scale_color_viridis() +
  coord_cartesian(ylim = c(80, 150)) +
  dark_mode(theme_apa(base_family = "Times", base_size = 24)) +
  theme(legend.position = "none",
  axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = unit(c(0.1, 0.1, 1, 0.1), "inches"))

ggsave("loess_plot_blank.pdf", width = 16, height = 9, units = "in", dpi = 300)

loess_plot_pub <-  ggplot(data, aes(x = article_date, y = publication_delay, colour = after_stat(y))) +
  geom_smooth(method = "loess", se = FALSE, span = 0.15, linewidth = 2) +
  labs(title = "Smooth loess trend of publication delay",
       x = "Article date",
       y = "Publicaiton delay (day)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2016-01-01", "2022-11-01"))) +
  scale_color_viridis() +
  coord_cartesian(ylim = c(0, 50)) +
  dark_mode(theme_apa(base_family = "Times", base_size = 24)) +
  theme(legend.position = "none")

ggsave("loess_plot_pub.pdf", width = 16, height = 9, units = "in", dpi = 300)

discipline_loess_data  <- data |>
mutate(discipline = ifelse(is_psych == TRUE, "Psychology", discipline),
       discipline = ifelse(discipline == "multidisciplinary", "Multidisciplinary", discipline))

discipline_loess_plot <-  ggplot(discipline_loess_data, aes(x = article_date, y = acceptance_delay, colour = discipline)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.15) +
  labs(title = "Smooth loess trend of acceptance delay among disciplines",
       x = "Article date",
       y = "Acceptance delay (day)",
       colour = "Discipline") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2016-01-01", "2022-11-01"))) +
  scale_color_viridis(discrete = TRUE) +
  coord_cartesian(ylim = c(70, 250)) +
  dark_mode(theme_apa(base_family = "Times", base_size = 24))

ggsave("discipline_loess_plot.pdf", width = 16, height = 9, units = "in", dpi = 300)
