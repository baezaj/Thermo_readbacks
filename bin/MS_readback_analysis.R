
# Document setup ----------------------------------------------------------


options(stringsAsFactors = FALSE)
library(tidyverse)
library(rio)
library(lubridate)
library(ggthemes)


# data import -------------------------------------------------------------


# list of all files in the data folder
log_directory <- dir("data", full.names = TRUE, recursive = TRUE)

# dataframe used for the for-loop
log_files <- data.frame(files = log_directory, file_number = 1:length(log_directory)) 

# data <- import("Combined_logs/QE_HF_HFX_logs.csv")


# Preparing the document --------------------------------------------------


data <- read.table(log_files$files[1], sep = "\t", header = F, fill = TRUE)

index <- grep("[a-z]", data[,3])

names(data) <- data[index[1],]

data <- data[-index,]


data <- data %>% 
  rename_all(tolower) %>% 
  rename_all(~str_replace_all(., "\\W", "_")) %>% 
  rename_all(~str_replace_all(., "____", "_")) %>% 
  rename_all(~str_replace_all(., "___", "_")) %>% 
  rename_all(~str_replace_all(., "__", "_")) %>% 
  rename_all(~str_replace_all(., "(_)$", "")) %>% 
  select_if(~ !any(is.na(.))) %>% 
  mutate(col_length = ncol(.),
         file_name = log_files$files[1],
         file_number = log_files$file_number[1]) %>% 
  select(date, 
         col_length,
         file_name,
         file_number, 
         everything())

# Coercing to numeric values
data[5:ncol(data)] <- mapply(data[5:ncol(data)], FUN = as.numeric)


### For loop ###
### Adding all the files to the document
system.time(
  for(i in 2:nrow(log_files)){
    
    temp <- read.table(log_files$files[i], sep = "\t", header = F, fill = TRUE)
    
    index <- grep("[a-z]", temp[,3])
    
    # if file has missing column names, skips that file
    if(length(index) == 0){
      next
    }

    names(temp) <- temp[index[1],]
    
    temp <- temp[-index,]
    
    temp <- temp %>% 
      rename_all(tolower) %>% 
      rename_all(~str_replace_all(., "\\W", "_")) %>% 
      rename_all(~str_replace_all(., "____", "_")) %>% 
      rename_all(~str_replace_all(., "___", "_")) %>% 
      rename_all(~str_replace_all(., "__", "_")) %>% 
      rename_all(~str_replace_all(., "(_)$", "")) %>% 
      select_if(~ !any(is.na(.))) %>% 
      mutate(col_length = ncol(.),
             file_name = log_files$files[i],
             file_number = log_files$file_number[i]) %>% 
      select(date, 
             col_length,
             file_name,
             file_number, 
             everything())
    
    # Coercing to numeric values
    temp[5:ncol(temp)] <- mapply(temp[5:ncol(temp)], FUN = as.numeric)
    
    data <- bind_rows(data, temp) 
    
    # write_csv(temp, path = "Combined_logs/QE_HF_HFX_logs.csv", append = TRUE)
    
  }
)


# Formatting --------------------------------------------------------------

# data <- import("Combined_logs/QE_HF_HFX_logs.csv", fill = TRUE)

data$time <- ymd_hms(data$date)

data$Instrument <- unlist(lapply(data$file_name, function(x){
  unlist(strsplit(x, split = "/"))[2]
}))

export(data, file = "Combined_logs/QE_HF_HFX_logs.csv")

# Plots -------------------------------------------------------------------

ggplot(data %>% filter(time >= as.Date("2019-11-01") & time <= as.Date("2019-11-08"), 
                       Instrument == "QE_HFX"),
       aes(x = time)) +
  geom_line(aes(y = ambient_temperature_raw_c, color = "Ambient Temp")) +
  # geom_line(aes(y = analyzer_temperature_sensor_c, color = "Analyzer Temp sensor")) +
  # geom_line(aes(y = ceps_peltier_temperature_sensor_c, color = "CEPS Peltier temp sensor")) +
  scale_color_brewer(palette = "Set1") +
  expand_limits(y = c(25,30)) +
  labs(title = "Ambient Temperature",
       y = expression('Temp ('~degree*C*')'),
       x = NULL) +
  # facet_wrap(~Instrument, nrow = 3, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) +
  theme_hc(base_size = 14) 
ggsave(filename = "figures/20191107_ambient_temp.png")

ggplot(data %>% filter(time >= as.Date("2019-05-01") & time <= as.Date("2019-08-20")), aes(x = time)) +
  geom_line(aes(y = vacuum_1_hv_mbar, color = "HV")) +
  geom_line(aes(y = vacuum_2_uhv_mbar, color = "UHV")) +
  # geom_line(aes(y = vacuum_3_fore_mbar, color = "Fore")) +
  # geom_line(aes(y = vacuum_4_humidity_mbar, color = "humidity")) +
  # geom_line(aes(y = ambient_humidity_result, color = "Ambient Humidity")) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~Instrument, nrow = 3, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_hc(base_size = 14) 


ggplot(data %>% filter(time >= as.Date("2019-03-01") & time <= as.Date("2019-04-17")), aes(x = time)) +
  # geom_line(aes(y = vacuum_1_hv_mbar, color = "HV")) +
  geom_line(aes(y = itv_gas_pressure_actual_bar, color = "UHV")) +
  # geom_line(aes(y = vacuum_3_fore_mbar, color = "Fore")) +
  # geom_line(aes(y = vacuum_4_humidity_mbar, color = "humidity")) +
  # geom_line(aes(y = ambient_humidity_result, color = "Ambient Humidity")) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~Instrument, nrow = 3, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_hc(base_size = 14) 


# ggsave(filename = "figures/20190329_high_vacuum_pump.png")
# ggsave(filename = "figures/20190329_ultra_high_vacuum_pump.png")

ggplot(data %>% filter(time >= as.Date("2018-12-01"), Instrument == "QE_HF"), aes(x = time)) +
  geom_line(aes(y = log10(inj_flat_ampl_vpp), color = "inj_flat_ampl")) +
  geom_line(aes(y = log10(inj_flat_freq_khz), color = "inj_flat_freq")) +
  geom_line(aes(y = log10(bent_flat_freq_khz), color = "bent_flat_freq"))

ggplot(data %>% filter(time >= as.Date("2018-11-25"), Instrument == "QE_HF"), aes(x = time)) +
  geom_line(aes(y = turbo1_temp_bearing_r_c, color = "bearing")) +
  geom_line(aes(y = turbo1_temp_motor_r_c, color = "motor")) +
  geom_line(aes(y = turbo1_temp_bottompart_r_c, color = "bottompart")) +
  geom_line(aes(y = turbo1_temp_powerstage_r_c, color = "powerstage")) +
  geom_line(aes(y = turbo1_temp_electronics_r_c, color = "electronics")) +
  geom_line(aes(y = turbo1_volt_r_v, color = "voltage")) +
  geom_line(aes(y = turbo1_curr_r_a, color = "current"))

ggplot(data %>% filter(time >= as.Date("2018-11-25"), Instrument == "QE_HF"), aes(x = time)) +
  geom_line(aes(y = turbo2_temp_bearing_r_c, color = "bearing")) +
  geom_line(aes(y = turbo2_temp_motor_r_c, color = "motor")) +
  geom_line(aes(y = turbo2_temp_bottompart_r_c, color = "bottompart")) +
  geom_line(aes(y = turbo2_temp_powerstage_r_c, color = "powerstage")) +
  geom_line(aes(y = turbo2_temp_electronics_r_c, color = "electronics")) +
  geom_line(aes(y = turbo2_volt_r_v, color = "voltage")) +
  geom_line(aes(y = turbo2_curr_r_a, color = "current"))
  
ggplot(data %>% filter(time >= as.Date("2018-11-25"), Instrument == "QE_HF"), aes(x = time)) +
  geom_line(aes(y = turbo3_temp_bearing_r_c, color = "bearing")) +
  geom_line(aes(y = turbo3_temp_motor_r_c, color = "motor")) +
  geom_line(aes(y = turbo3_temp_bottompart_r_c, color = "bottompart")) +
  geom_line(aes(y = turbo3_temp_powerstage_r_c, color = "powerstage")) +
  geom_line(aes(y = turbo3_temp_electronics_r_c, color = "electronics")) +
  geom_line(aes(y = turbo3_volt_r_v, color = "voltage")) +
  geom_line(aes(y = turbo3_curr_r_a, color = "current"))


ggplot(data %>% filter(time >= as.Date("2018-11-10"), Instrument == "QE_HF"), aes(x = time)) +
  geom_line(aes(y = (ctrap_rf_amp_vpp), color = "rf_amp")) +
  geom_line(aes(y = (ctrap_amp_current_a), color = "amp")) +
  geom_line(aes(y = (ctrap_freq_mhz), color = "freq"))


# 3 Day readings of HF
ggplot(data %>% filter(time >= as.Date("2018-12-01"), time <= as.Date("2019-20-08")), aes(x = time)) +
  scale_y_log10() +
  geom_line(aes(y = vacuum_2_uhv_mbar, color = "V2-UHV")) +
  geom_line(aes(y = vacuum_1_hv_mbar, color = "V1-HV"))

# geom_line(aes(y = vacuum_4_humidity_mbar, color = "V4-humidity")) +
# scale_color_brewer(palette = "Set1") +
# labs(title = "MS Room Temperature readings",
#      y = expression('Temp ('~degree*C*')'),
#      x = NULL,
#      color = NULL) +
# scale_y_log10() +
# facet_wrap(~Instrument, nrow = 3, scales = "free_y") +
# theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
# theme_hc(base_size = 14) 


# 3 Day temp readings
ggplot(data %>% filter(time >= as.Date("2018-01-01") & time <= as.Date("2019-08-20"), 
                       Instrument != "QE"), 
       aes(x = time)) +
  geom_line(aes(y = ambient_temperature_raw_c, color = "Ambient Temp")) +
  geom_line(aes(y = analyzer_temperature_sensor_c, color = "Analyzer Temp sensor")) +
  geom_line(aes(y = ceps_peltier_temperature_sensor_c, color = "CEPS Peltier temp sensor")) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "MS Room Temperature readings",
       y = expression('Temp ('~degree*C*')'),
       x = NULL,
       color = NULL) +
  facet_wrap(~Instrument, nrow = 3, scales = "free_y") +
  # scale_y_continuous(limits = c(20,60)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_hc(base_size = 14)

ggplot(data %>% filter(time >= as.Date("2018-03-01")), aes(x = time)) +
  geom_line(aes(y = up_time_days, color = "up time")) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~Instrument, nrow = 3)
