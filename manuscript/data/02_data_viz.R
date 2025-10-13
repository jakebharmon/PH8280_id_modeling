# Create Time series plotting data
flu_time_series_gg <- ggplot(data = flu_time_series, aes(x = week_num, y = flu_count, color = source)) +
  geom_point() +
  geom_line() +
  facet_wrap(
    ~ season, 
    #    scale = "free_y", 
    ncol = 3) +
  labs(
    title = "Positive Influenza Clinical/Public Lab Tests from 2015-2020",
    x = "Flu Week",
    y = "Number of Weekly Hospitalizations",
  ) +
  scale_color_viridis(
    discrete = TRUE, 
    option = "D",
    labels = c(
      "clinical" = "Clinical Lab",
      "public" = "Public Health Lab",
      "total" = "Total Lab Results")) 
