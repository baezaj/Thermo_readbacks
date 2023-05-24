
# setup -------------------------------------------------------------------


library(tidyverse)
library(janitor)
library(lubridate)
library(data.table)
library(patchwork)
source("import_exploris_logfiles.R")


# data import -------------------------------------------------------------



## Data is located in the following directory

## C:\ProgramData\Thermo\Exploris\Log

## C:\Thermo\Instruments\TNG\OrbitrapEclipse\3.5\System\logs


directory <- "../Exploris480/Log/"

filelist <- list.files(directory, pattern = "InstrumentTemperature", full.names = TRUE)


# reading many files ------------------------------------------------------


read_logfile_to_df <- function(x, instrument = "480"){
  
  tryCatch(import_exploris_logfiles(x, instrument = instrument),
           error = function(e) NA,
           warning = function(w) NA)
  
}



# importing log files and saving as a list
system.time(
  log <- filelist %>% 
    lapply(read_logfile_to_df) %>% 
    bind_rows()
  
)
# Time to complete - 126 sec


# concatenating the listed dataframes
log <- log %>% 
  mutate(Date = ymd_hms(Date)) %>% 
  filter(!is.na(Date))
  


# plots -------------------------------------------------------------------



### Pressure Readbacks
# Fore pump
p_fore <- log %>% 
  filter(Date >= today() - 365) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = ICB_Gauge_2_FV_RPT010_mbar, color = "2_FV")) +
  theme_bw(base_size = 12) +
  scale_color_manual(values = c("#a6cee3")) +
  guides(color = "none") +
  labs(title = "Fore Vaccuum (FV) pressure",
       x = NULL,
       y = expression("Pressure (mbar)")) +
  scale_x_datetime(breaks = scales::breaks_pretty(12))

# UHV
p_uhv <- log %>% 
  filter(Date >= today() - 365) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = ICB_Gauge_0_UHV_IKR270_mbar, color = "0_UHV")) +
  theme_bw(base_size = 12) +
  scale_color_manual(values = c("#1f78b4")) +
  guides(color = "none") +
  scale_y_log10() +
  labs(title = "Ultra High Vaccuum (UHV) pressure",
       x = NULL,
       y = expression("Pressure (mbar)")) +
  scale_x_datetime(breaks = scales::breaks_pretty(12))


### Temperature Readbacks
# Ambient Temp
t_ambient <- log %>% 
  filter(Date > today() - 365) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = ICB_R_TempRh_C, color = "Ambient Temp")) +
  scale_color_manual(values = c("#fdbf6f")) +
  guides(color = "none") +
  coord_cartesian(ylim = c(25, 50)) +
  scale_x_datetime(breaks = scales::breaks_pretty(12)) +
  theme_bw(base_size = 12) +
  labs(title = "Ambient Temperature",
       y = expression('Temp ('~degree*C*')'),
       x = NULL)

# Orbitrap temp
t_orbi <- log %>% 
  filter(Date > today() - 365) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = CTS_Orbitrap_PT100_Temperature_act_C, color = "Orbi")) +
  scale_color_manual(values = c("#ff7f00")) +
  guides(color = "none") +
  scale_x_datetime(breaks = scales::breaks_pretty(12)) +
  theme_bw(base_size = 12) +
  labs(title = "Orbitrap Temperature",
       y = expression('Temp ('~degree*C*')'),
       x = NULL)

# Adding all the plots
p_fore / p_uhv / t_ambient / t_orbi + plot_annotation(title = "Exploris 480 Readbacks")
