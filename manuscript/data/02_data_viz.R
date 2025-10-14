# Create Time series plotting data
flu_time_series_gg <- flu_time_series %>% 
  ggplot(aes(x = week_num, y = amount, group = type, color = type)) +
  geom_point(alpha = 0.7) +
  facet_wrap(
    ~ year, 
    #    scale = "free_y", 
    ncol = 3) +
  labs(
    title = "Positive Influenza Clinical/Public Lab Tests from 2021-2024",
    x = "Flu Week",
    y = "Number of Weekly Hospitalizations"
  ) +
  theme_bw() +
  scale_color_viridis(
    discrete = TRUE, 
    option = "D",
    labels = c(
      "flu_a" = "Influenza A",
      "flu_b" = "Influenza B",
      "total" = "Total Influenza")) 

ggsave("/Users/jake/Documents/PH8280_id_modeling/manuscript/data/flu_time_series.png")
