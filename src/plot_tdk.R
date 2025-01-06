library(readr)
library(lubridate)
library(dplyr)
library(stringr)
#install.packages("ggdist", repos = "https://cloud.r-project.org/")
library(ggdist)
#install.packages("ggplot2", repos = "https://cloud.r-project.org/")
library(ggplot2)
#install.packages("tidyr", repos = "https://cloud.r-project.org/")
library(tidyr)
#install.packages("ggdist", repos = "https://cloud.r-project.org/")
library(ggdist)
#install.packages("ggridges", repos = "https://cloud.r-project.org/")
library(ggridges)


data = readr::read_csv("/users/zsimi/processed.csv")
colnames(data)

source(here::here("src/analysis/utils.R"))
#plot_visuals <- function(title, subtitle, tag, n, with_legend = FALSE, legend_name = NULL, x_axis_name = NULL, y_axis_name = NULL, use_fill = TRUE)

data = data |> 
  filter(article_date > as_date("2014-12-31"),
         article_date < as_date("2024-12-01"))



############### Rainplot for acceptance delay


rainplot_data <- data |> 
  mutate(article_date_year = floor_date(as_date(article_date), "year"),
         article_date_year = str_replace(article_date_year, "-01-01", ""),
         article_date_year = factor(article_date_year, levels = sort(unique(article_date_year), decreasing = TRUE))) |> 
  filter(!is.na(acceptance_delay)) |> 
  mutate(threshold = quantile(acceptance_delay, 0.99)) |> 
  filter(acceptance_delay <= threshold)

rainplot_dots_data = rainplot_data |> 
  sample_n(500000)

rainplot <- rainplot_data |>
  ggplot(aes(x = as.factor(article_date_year), y = acceptance_delay, fill = as.factor(article_date_year))) +
  ggdist::stat_halfeye(
    adjust = 1,
    width = 0.6,
    justification = -.1,
    .width = 0.5,
    point_colour = NA,
    position=position_dodge(.7)
  ) +
  ggdist::stat_dots(
    data = rainplot_dots_data,
    aes(x = as.factor(article_date_year), y = acceptance_delay, fill = as.factor(article_date_year)),
    side = "left",
    justification = 1.1,
    binwidth = 0.01,
    overflow = "compress",
    alpha = 0.1,
    position=position_dodge(.7)
  ) +
  geom_boxplot(
    width = 0.2,
    outlier.color = NA,
    alpha = 1,
    fill = "white",
    color = "black",
    lwd = 0.5,
    position=position_dodge(.7)
  ) +
  plot_visuals("Ábra 3", "Elfogadási késések eloszlása évekre lebontva", "", 10, F, NULL, "Megjelenési év", "Elfogadási késés", T) +
  coord_flip()


ggsave(width = 6, height = 4, dpi = 600, "plots/rainplot.pdf")
print("rainplot done")

############### Histogram for publication delay


data |> 
  mutate(
    article_date_year = floor_date(as_date(article_date), "year"),
    article_date_year = str_replace(article_date_year, "-01-01", ""),
    article_date_year = factor(article_date_year, levels = sort(unique(article_date_year), decreasing = TRUE))
  ) |> 
  filter(!is.na(publication_delay),
         publication_delay <= 75) |> 
  ggplot(aes(x = publication_delay, y = factor(article_date_year), fill = factor(article_date_year))) + 
  geom_density_ridges() +
  plot_visuals(
        "Ábra 5",
        "Publikálási késések eloszlása évekre lebontva",
        "",
        10,
        F,
        NULL,
        "Publikálási késés",
        "Megjelenési év",
        TRUE
      )

ggsave(width = 6, height = 4, dpi = 600, "plots/publication_density.pdf")
print("publication density done")






############### Better acceptance delay plot + quartile

acceptance_data <- data |>
  dplyr::mutate(article_date_month = lubridate::floor_date(as_date(article_date), "month")) |> 
  filter(sjr_best_quartile %in% c("Q1", "Q2", "Q3", "Q4")) |> 
  dplyr::group_by(article_date_month, sjr_best_quartile) |>
  dplyr::reframe(
    delay = median(
      acceptance_delay,
      na.rm = TRUE
    )
  ) |>
  tidyr::drop_na(delay)

quantile_data = data |>
  filter(sjr_best_quartile %in% c("Q1", "Q2", "Q3", "Q4"))

results <- data.frame(Degree = integer(), Adjusted_R2 = numeric(), AIC = numeric())

for (degree in 1:5) {  # Test polynomials up to degree 5
  model <- lm(acceptance_delay ~ poly(as.numeric(article_date), degree), data = data)
  adj_r2 <- summary(model)$adj.r.squared
  aic_value <- AIC(model)
  results <- rbind(results, data.frame(Degree = degree, Adjusted_R2 = adj_r2, AIC = aic_value))
}

print(results)

# Visualizing the fit
best_degree <- results |> arrange(-Adjusted_R2) |> slice(1) |> pull(Degree)

acceptance_plot <- ggplot(acceptance_data,
                          aes(
                            x = as_date(article_date_month),
                            y = delay, color = sjr_best_quartile
                          )
) +
  geom_point(alpha = 0.8, size = 1) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  geom_smooth(
    data = quantile_data,
    aes(x = article_date, y = acceptance_delay, color = sjr_best_quartile),
    method = "lm", formula = y ~ poly(x, best_degree), se = TRUE) +
  plot_visuals("Ábra 4", "Elfogadási késések változása kvartilisenként", "", 4, T, "Legjobb kvartilis", "Megjelenési év", "Elfogadási késés", F)

ggsave(width = 6, height = 4, dpi = 600, "plots/acceptance_plot.pdf")
print("acceptance plot done")






############### Better publication delay plot + quartile

publication_data <- data |>
  dplyr::mutate(article_date_month = lubridate::floor_date(as_date(article_date), "month")) |> 
  filter(sjr_best_quartile %in% c("Q1", "Q2", "Q3", "Q4")) |> 
  dplyr::group_by(article_date_month, sjr_best_quartile) |>
  dplyr::reframe(
    delay = median(
      publication_delay,
      na.rm = TRUE
    )
  ) |>
  tidyr::drop_na(delay)

results <- data.frame(Degree = integer(), Adjusted_R2 = numeric(), AIC = numeric())

for (degree in 1:5) {  # Test polynomials up to degree 5
  model <- lm(publication_delay ~ poly(as.numeric(article_date), degree), data = data)
  adj_r2 <- summary(model)$adj.r.squared
  aic_value <- AIC(model)
  results <- rbind(results, data.frame(Degree = degree, Adjusted_R2 = adj_r2, AIC = aic_value))
}

print(results)

# Visualizing the fit
best_degree <- results |> arrange(-Adjusted_R2) |> slice(1) |> pull(Degree)

publication_plot <- ggplot(publication_data,
                           aes(
                             x = as_date(article_date_month),
                             y = delay, color = sjr_best_quartile
                           )
) +
  geom_point(alpha = 0.8, size = 1) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  geom_hline(yintercept = 14,
             linetype = "dashed",
             color = "#BF616A",
             linewidth = 1) +
  geom_hline(yintercept = 21,
             linetype = "dashed",
             color = "#BF616A",
             linewidth = 1) +
  geom_smooth(
    data = quantile_data,
    aes(x = article_date, y = publication_delay, color = sjr_best_quartile),
    method = "lm", formula = y ~ poly(x, best_degree), se = TRUE) +
  plot_visuals("Ábra 6", "Publikációs késések változása kvartilisenként", "", 4, T, "Legjobb kvartilis", "Megjelenési év", "Publikációs késés", F)

ggsave(width = 6, height = 4, dpi = 600, "plots/publication_plot.pdf")
print("publication plot done")



############### Covid vs non-covid plot

covid_data <- data |>
  dplyr::mutate(article_date_month = lubridate::floor_date(as_date(article_date), "month")) |> 
  filter(!is.na(is_covid)) |> 
  filter(
    (is_covid == TRUE & article_date > as_date("2019-11-01")) | 
      is_covid == FALSE) |> 
  dplyr::group_by(article_date_month, is_covid) |>
  dplyr::reframe(
    delay = median(
      acceptance_delay,
      na.rm = TRUE
    )
  ) |>
  tidyr::drop_na(delay)

covid_plot <- ggplot(covid_data,
                     aes(
                       x = as_date(article_date_month),
                       y = delay, color = is_covid
                     )
) +
  geom_point(alpha = 0.8, size = 1) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  geom_smooth(
    data = covid_data,
    aes(x = article_date_month, y = delay, color = is_covid),
    method = "lm", formula = y ~ poly(x, 4), se = TRUE) +
  plot_visuals("Ábra 7", "Covid és nem Covid cikkek elfogadási késésének változása", "", 2, T, "Covid státusz", "Megjelenési év", "Elfogadási késés", F) +
  scale_color_manual(values = c("TRUE" = "#88CCEE", "FALSE" = "#332288"),
                     labels = c("Hamis", "Igaz"))



ggsave(width = 6, height = 4, dpi = 600, "plots/covid_plot.pdf")
print("covid plot done")


############### Only Covid plot + quantiles

covid_quartile_data <- data |>
  filter(sjr_best_quartile %in% c("Q1", "Q2", "Q3", "Q4")) |> 
  filter(is_covid == TRUE) |> 
  filter(acceptance_delay > 0) |> 
  filter(article_date > as_date("2019-11-01")) |> 
  dplyr::group_by(sjr_best_quartile)


covid_quartile_data |> 
  ggplot(aes(x = article_date, y = acceptance_delay, colour = sjr_best_quartile)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), se = TRUE) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  ylim(0, 150)
  plot_visuals("Ábra 9", "Covid cikkek elfogadási késésének változása kvartilisekre lebontva", "", 4, T, "Legjobb kvartilis", "Megjelenési év", "Elfogadási késés", F)

ggsave(width = 6, height = 4, dpi = 600, "plots/covid_quantile_plot.pdf")
print("covid quantile plot done")


############### Violin plot
############### Covid vs non-covid

violin_data = data |> 
  mutate(article_date_year = floor_date(as_date(article_date), "year"),
         article_date_year = str_replace(article_date_year, "-01-01", "")) |> 
  filter(!is.na(is_covid)) |> 
  filter(
    (is_covid == TRUE & article_date > as_date("2019-11-01")) | 
      is_covid == FALSE) |> 
  filter(article_date_year == 2020)


violinplot <- violin_data |>
  ggplot(aes(x = as.factor(is_covid), y = acceptance_delay, fill = as.factor(is_covid))) +
  geom_violin(alpha = 0.8) + 
  geom_boxplot(
    fill = "white", 
    width = 0.1,
    outlier.color = "NA",
    alpha = 0.7 
  ) +
  # scale_fill_manual(
  #   values = c("lightgreen", "darkgreen"),
  #   labels = NULL
  # ) +
  scale_x_discrete(#name = "COVID státusz",
    labels = c("FALSE" = "Hamis", "TRUE" = "Igaz")) + 
  scale_y_continuous(name = "Elfogadási késés") + 
  plot_visuals("Ábra 8", "Covid és nem Covid cikkek elfogadási késének eloszlása 2020-ban", "", 2, F, NULL, "Covid státusz", "Elfogadási késés", T)

ggsave(width = 6, height = 4, dpi = 600, "plots/violinplot.pdf")
print("violinplot done")

#Better discipline violin plot



discipline_plot <- data |>
  filter(!is.na(discipline)) |> 
  mutate(discipline = case_when(
    discipline == "health_sciences" ~ "Egészségtudományok",
    discipline == "life_sciences" ~ "Élettudományok",
    discipline == "multidisciplinary" ~ "Multidiszciplináris",
    discipline == "physical_sciences" ~ "Fizikai tudományok",
    discipline == "social_sciences_and_humanities" ~ "Társadalomtudományok\n és bölcsészet",
    TRUE ~ discipline
  )) |> 
  ggplot(aes(x = as.factor(discipline), y = acceptance_delay, fill = as.factor(discipline))) +
  geom_violin(alpha = 0.8) + 
  geom_boxplot(
    fill = "white", 
    width = 0.1,
    outlier.color = "NA",
    alpha = 0.7 
  ) +
  coord_flip() +
  plot_visuals("Ábra 11", "Cikkek elfogadási késének eloszlása diszciplinák szerint", "", 5, F, NULL, "Megjelenési év", "Elfogadási késés", T)

ggsave(width = 6, height = 4, dpi = 600, "plots/discipline_violin_plot.pdf")
print("Discipline violin plot done")

#Better discipline plot

acceptance_data_discipline <- data |>
  filter(!is.na(discipline)) |> 
  dplyr::mutate(
    article_date_month = lubridate::floor_date(as_date(article_date), "month")) |> 
  dplyr::group_by(
    article_date_month,
    discipline
  ) |>
  dplyr::reframe(
    delay = median(
      acceptance_delay,
      na.rm = TRUE
    )
  ) |>
  tidyr::drop_na(delay)

discipline_plot <- ggplot(
  acceptance_data_discipline,
  aes(
    x = as_date(article_date_month),
    y = delay, group = discipline,
    colour = discipline
  )
) +
  geom_smooth(
    show.legend = TRUE,
    na.rm = TRUE,
    span = 3,
    se = TRUE
  ) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  plot_visuals("Ábra 10", "Cikkek elfogadási késének eloszlása diszciplinák szerint", "", 5, T, "Diszciplina", "Megjelenési év", "Elfogadási késés", F) +
  scale_colour_manual(values = c("health_sciences" = "#88CCEE", "life_sciences" = "#332288", "multidisciplinary" = "#CC6677", "physical_sciences" = "#AA4499", "social_sciences_and_humanities" = "#44AA99"),
                      labels = c("Egészségtudományok", "Élettudományok", "Multidiszciplináris", "Fizikai tudományok", "Társadalomtudományok\nés bölcsészet"))

ggsave(width = 6, height = 4, dpi = 600, "plots/discipline_plot.pdf")
print("Discipline plot done")



#Megajournal vs q1 and Q2

megajournal_plot = data |> 
  filter(`sjr_best_quartile` %in% c("Q1", "Q2")) |> 
  ggplot(aes(x = article_date, y = acceptance_delay, colour = as.factor(is_mega))) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 5), se = TRUE) +
  plot_visuals("Ábra 12", "Elfogadási késések változása megafolyóiratokra levetítve","", 2, TRUE, "Megafolyóirat", "Megjelenési év","Elfogadási késés", F
  ) +
  scale_color_manual(values = c("TRUE" = "#88CCEE", "FALSE" = "#332288"),
                     labels = c("Hamis", "Igaz"))

ggsave(width = 6, height = 4, dpi = 600, "plots/megajournal_plot.pdf")
print("Megajournal plot done")


#Retractions

retraction_data = data |> 
  filter(str_detect(publication_types, "Retracted"))

nrow(retraction_data)

retraction_data_table = retraction_data |> 
  summarise(
    n_total = n(),
    mean = mean(acceptance_delay, na.rm = TRUE),
    by_quartile = retraction_data |> 
      group_by(sjr_best_quartile) |> 
      summarise(
        n_quartile = n(),
        mean_quartile = mean(acceptance_delay, na.rm = TRUE)
      ),
    by_discipline = retraction_data |> 
      group_by(discipline) |> 
      summarise(
        n_discipline = n(),
        mean_discipline = mean(acceptance_delay, na.rm = TRUE)
      )
  )

write.csv(retraction_data_table, "tables/retraction_data.csv")

retraction_plot = retraction_data |> 
  filter(`sjr_best_quartile` %in% c("Q1", "Q2", "Q3", "Q4")) |> 
  ggplot(aes(x = article_date, y = acceptance_delay, colour = as.factor(sjr_best_quartile))) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 5), se = TRUE) +
  geom_smooth(data = data, aes(x = article_date, y = acceptance_delay, linetype = "Összes"),  # Add an indication with line type
              colour = "#999933",          # Use a distinct color for clarity
              method = "lm", 
              formula = y ~ poly(x, 5), 
              linewidth = 1.3,
              se = FALSE) +
  scale_linetype_manual(values = c("Összes" = "dashed")) +
  plot_visuals("Ábra 13", "Visszavont cikkek elfogadási késének eloszlása kvartilisek szerint", "", 4, T, "Legjobb kavrtilis", "Megjelenési év", "Elfogadási késés", F) +
  labs(linetype = "") 

ggsave(width = 6, height = 4, dpi = 600, "plots/retraction.pdf")


