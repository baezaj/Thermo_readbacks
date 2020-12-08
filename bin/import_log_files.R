
# Document setup ----------------------------------------------------------

library(ggthemes)


# data import -------------------------------------------------------------



import_log_files <- function(directory){
  
  # Required libraries
  require(tidyverse)
  require(janitor)
  require(lubridate)

  # Reading the log file names as a list
  filelist <- list.files(directory, full.names = TRUE)
  
  # Importing the log files
  datalist <- lapply(filelist, FUN = function(x){read.table(x, sep = "\t", header = FALSE, fill = TRUE)})
  
  # Removing log files that don't have all the columns
  # should be 48 columns
  val <- ceiling(median(unlist(lapply(datalist, ncol))))
  remove_index <- which(unlist(lapply(datalist, ncol)) != val)
  datalist <- datalist[-remove_index]
  
  # binding all the datasets
  data = do.call("rbind", datalist) 
  
  # removing the rows that used to be column names in each separate dataset
  index <- grep("[a-z]", data[,1])
  names(data) <- data[index[1],]
  data <- data[-index,]
  
  # Cleaning up final table
  data <- data %>% 
    clean_names() %>% 
    mutate(time = NA) %>% 
    select(date, time, everything(), -na)
  
  # Converting to date format using lubridate
  data$time <- ymd_hms(data$date)
  
  # Converting data to numeric
  data[3:ncol(data)] <- mapply(data[3:ncol(data)], FUN = as.numeric)

  return(data)
  
}

data <- import_log_files("data/QE_HFX/")


# Plots -------------------------------------------------------------------



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


ggplot(data %>% filter(time >= as.Date("2020-03-01")), 
       aes(x = time)) +
  geom_line(aes(y = vacuum_1_hv_mbar, color = "HV")) +
  geom_line(aes(y = vacuum_2_uhv_mbar, color = "UHV")) +
  geom_line(aes(y = vacuum_3_fore_mbar, color = "Fore")) +
  scale_y_log10() +
  labs(title = "QE HF-X Vacuum pump pressure",
       y = "Pressure (mbar)",
       color = NULL) +
  scale_color_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_hc(base_size = 14)

names(data)


ggplot(data %>% filter(time >= as.Date("2020-03-01")), aes(x = time)) +
  geom_line(aes(y = turbo1_temp_bearing_r_c, color = "bearing")) +
  geom_line(aes(y = turbo1_temp_motor_r_c, color = "motor")) +
  geom_line(aes(y = turbo1_temp_bottompart_r_c, color = "bottompart")) +
  geom_line(aes(y = turbo1_temp_powerstage_r_c, color = "powerstage")) +
  geom_line(aes(y = turbo1_temp_electronics_r_c, color = "electronics")) +
  geom_line(aes(y = turbo1_volt_r_v, color = "voltage")) +
  geom_line(aes(y = turbo1_curr_r_a, color = "current")) 


ggplot(data %>% filter(time >= as.Date("2020-03-01")), aes(x = time)) +
  geom_line(aes(y = turbo2_temp_bearing_r_c, color = "bearing")) +
  geom_line(aes(y = turbo2_temp_motor_r_c, color = "motor")) +
  geom_line(aes(y = turbo2_temp_bottompart_r_c, color = "bottompart")) +
  geom_line(aes(y = turbo2_temp_powerstage_r_c, color = "powerstage")) +
  geom_line(aes(y = turbo2_temp_electronics_r_c, color = "electronics")) +
  labs(title = "Turbo 2 Temperature readback",
       y = expression('Temp ('~degree*C*')'),
       x = NULL,
       color = NULL)






# 3 Day temp readings
ggplot(data %>% filter(time >= as.Date("2020-03-12")), 
       aes(x = time)) +
  geom_line(aes(y = ambient_temperature_raw_c, color = "Ambient Temp")) +
  geom_line(aes(y = analyzer_temperature_sensor_c, color = "Analyzer Temp sensor")) +
  geom_line(aes(y = ceps_peltier_temperature_sensor_c, color = "CEPS Peltier temp sensor")) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "MS Room Temperature readings",
       subtitle = "HFX",
       y = expression('Temp ('~degree*C*')'),
       x = NULL,
       color = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_hc(base_size = 14)

ggplot(data %>% filter(time >= as.Date("2018-03-01")), aes(x = time)) +
  geom_line(aes(y = up_time_days, color = "up time")) +
  scale_color_brewer(palette = "Set1")

