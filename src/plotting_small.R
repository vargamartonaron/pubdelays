articles = articles |> 
  #Cut the first 10000 rows
  #dplyr::slice(1:10000) |>
  tidyr::drop_na(article_date) |>
  #only preserve years and months from the date variable
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-"))


acceptance_data <- articles |>
  dplyr::group_by(article_date_month) |>
  dplyr::reframe(delay = median(acceptance_delay, na.rm=T)) |>
  # dplyr::filter(article_date_month >= lubridate::as_date('2016-01') & article_date_month <= lubridate::as_date('2022-12')) |>
  tidyr::drop_na(delay) |>
  filter(delay <= 700)

acceptance_data_journal <- articles |>
  dplyr::group_by(article_date_month, journal_title) |>
  dplyr::reframe(delay = median(acceptance_delay, na.rm=T)) |>
  # dplyr::filter(article_date_month >= lubridate::as_date('2016-01') & article_date_month <= lubridate::as_date('2022-12')) |>
  tidyr::drop_na(delay)

acceptance_plot <- ggplot(acceptance_data, aes(x = as_date(article_date_month), y = delay)) +
  geom_point() +
  scale_x_date(date_breaks = '1 years', date_labels = '%Y') +
  ylim(0,400)+
  labs(y = "Elfogadási késés mediánja (nap)", x = "Dátum", title = "Elfogadási késés") 
acceptance_plot

acceptance_plot_journal <- ggplot(acceptance_data_journal, aes(x = as_date(article_date_month), y = delay)) +
  geom_smooth(show.legend = F, na.rm = TRUE, span = 3, se = T)+
  scale_x_date(date_breaks = '1 years', date_labels = '%Y') +
  ylim(0,400)+
  labs(y = "Elfogadási késés mediánja (nap)", x = "Dátum", title = "Elfogadási késés") 
acceptance_plot_journal

acceptance_plot_journal <- ggplot(acceptance_data, aes(x = delay)) +
  geom_histogram()
acceptance_plot_journal

bin_plot <- ggplot(acceptance_data_journal, aes(x = as_date(article_date_month), y = delay)) +
  geom_bin2d(bins = 40) +
  scale_fill_gradient(low = "#56B1F7",
                      high = "#132B43") +
  geom_density2d() +
  ylim(0, 500)
bin_plot 