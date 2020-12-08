


library(ggplot2)



ggplot(data %>% filter(time <= as.Date("2019-12-01")),
       aes(x = time)) +
  geom_line(aes(y = ambient_temperature_raw_c, color = "Ambient Temp")) +
  # geom_line(aes(y = analyzer_temperature_sensor_c, color = "Analyzer Temp sensor")) +
  # geom_line(aes(y = ceps_peltier_temperature_sensor_c, color = "CEPS Peltier temp sensor")) +
  scale_color_brewer(palette = "Set1") +
  # expand_limits(y = c(25,30)) +
  labs(title = "Ambient Temperature",
       y = expression('Temp ('~degree*C*')'),
       x = NULL,
       color = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # guides(color = FALSE) +
  ggthemes::theme_hc(base_size = 14) 