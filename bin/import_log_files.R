
# Import Exploris Log Files -----------------------------------------------

import_exploris_logfiles <- function(file, instrument = NULL){
  
  # Libraries ---------------------------------------------------------------
  
  
  require(tidyverse)
  require(janitor)
  require(lubridate)
  
  
  # Header info -------------------------------------------------------------
  
  
  header480 <- c("Time_sec","Date",
                 # ICB
                 "ICB_R_TempRh_C","ICB_pos_24_V_act_V","ICB_pos_5_V_act_V","ICB_pos_3_3_V_act_V","ICB_pos_2_5_V_act_V","ICB_pos_1_2_V_act_V",
                 "ICB_ICB_Vacuum_Supervision_Status_act_None","ICB_PF_Turbopump_Speed_Hz","ICB_PF_Turbopump_Operating_hours_h",
                 "ICB_PF_R_TPPower_W","ICB_PF_R_TPTempBearing_C","ICB_PF_Turbopump_Error_State_act_None",
                 "ICB_PF_R_TPTempMotor_C","ICB_PF_R_TPTempBottom_C","ICB_PF_R_TPTempPwrStage_C","ICB_PF_R_TPTempElectronic_C","ICB_R_heat_pow_W","ICB_Gauge_0_UHV_IKR270_mbar",
                 "ICB_Gauge_1_HCD_RPT010_mbar","ICB_Gauge_2_FV_RPT010_mbar","ICB_Gauge_3_IS5_RPT010_mbar","ICB_R_Fan0_speed_rpm",
                 "ICB_R_Fan1_speed_rpm","ICB_R_Fan2_speed_rpm","ICB_R_Fan3_speed_rpm","ICB_Analog_In_A_act_V","ICB_Analog_In_B_act_V",
                 # MB
                 "MB_Board_Free_RAM_MByte",
                 # IOS
                 "IOS_R_RF1offset_V","IOS_Funnel_RF_Amplitude_Vpp","IOS_Funnel_RF_Frequency_kHz","IOS_R_RF2phaseA_DC_V",
                 "IOS_R_RF2phaseB_DC_V","IOS_Inj_Flat_RF_Amplitude_Vpp","IOS_Inj_Flat_RF_Frequency_kHz","IOS_Inj_Flat_RF_Current_A",
                 "IOS_R_DC3_V","IOS_R_DC4_V","IOS_Bent_Flat_RF_Amplitude_Vpp","IOS_Bent_Flat_RF_Frequency_kHz","IOS_HVV_Positive_V",
                 "IOS_HVV_Negative_V","IOS_HV_Positive_V","IOS_HV_Negative_V",
                 # SB
                 "SB_pos_24_V_Supply_act_V","SB_pos_15_V_Analog_Supply_act_V","SB_neg_8_V_Supply_act_V","SB_pos_5_V_Analog_Supply_act_V",
                 "SB_pos_5_V_Digital_Supply_act_V","SB_neg_5_V_Analog_Supply_act_V","SB_pos_3_3_V_Digital_Supply_act_V",
                 "SB_pos_2_5_V_Digital_Supply_act_V","SB_pos_1_2_V_Digital_Supply_act_V","SB_Ion_Transfer_Tube_Heater_Temperature_act_C",
                 "SB_R_VaporizerTemp_C","SB_Source_ID_kOhm","SB_Cover_Light_Sensor_act_percent",
                 # DAQ
                 "DAQ_Status_None",
                 # OTS
                 "OTS_HV_Offset_High_V","OTS_HV_Offset_Low_V","OTS_HV_Focus_Lens_V","OTS_V_Lens_V","OTS_Deflector_High_V",
                 "OTS_Deflector_Low_V","OTS_CE_Inject_Pos_V","OTS_CE_Inject_Neg_V","OTS_HV_Monitor_kV","OTS_CEmeas_Vpos_kV",
                 "OTS_CEmeas_Vneg_kV","OTS_PCB_Temp_Top_act_C","OTS_PCB_Temp_Center_act_C","OTS_PCB_Temp_Bottom_act_C",
                 # QS
                 "QS_Quad_Detector_Temp_C","QS_RF_Amplitude_Vpp","QS_Modulation_Voltage_V","QS_RF_Current_A",
                 "QS_Rod_Driver_Voltage_V","QS_HV_Negative_V","QS_HV_Positive_V",
                 # CTS
                 "CTS_Fan1_Speed_rpm","CTS_Fan2_Speed_rpm","CTS_CTrap_RF_Frequency_MHz","CTS_Orbitrap_PT100_Temperature_act_C",
                 "CTS_CTrap_RF_Amplitude_V","CTS_RF_Power_W","CTS_RF_Current_mA","CTS_RF_Supply_Voltage_V",
                 "CTS_Demodulator_Voltage_V","CTS_Z_Lens_V","CTS_HV_Positive_V","CTS_HV_Negative_V",
                 "NA")
  
  
  header240 <- c("Time_sec","Date",
                 # ICB
                 "ICB_R_TempRh_C","ICB_pos_24_V_act_V","ICB_pos_5_V_act_V","ICB_pos_3_3_V_act_V",
                 "ICB_pos_2_5_V_act_V","ICB_pos_1_2_V_act_V","ICB_ICB_Vacuum_Supervision_Status_act_None",
                 "ICB_PF_Turbopump_Speed_Hz","ICB_PF_Turbopump_Operating_hours_h","ICB_PF_R_TPPower_W",
                 "ICB_PF_R_TPTempBearing_C","ICB_PF_Turbopump_Error_State_act_None","ICB_PF_R_TPTempMotor_C",
                 "ICB_PF_R_TPTempBottom_C","ICB_PF_R_TPTempPwrStage_C","ICB_PF_R_TPTempElectronic_C",
                 "ICB_R_heat_pow_W","ICB_Gauge_0_UHV_IKR270_mbar","ICB_Gauge_1_HCD_RPT010_mbar",
                 "ICB_Gauge_2_FV_RPT010_mbar","ICB_Gauge_3_IS5_RPT010_mbar","ICB_R_Fan0_speed_rpm",
                 "ICB_R_Fan1_speed_rpm","ICB_R_Fan2_speed_rpm","ICB_R_Fan3_speed_rpm","ICB_Analog_In_A_act_V",
                 "ICB_Analog_In_B_act_V",
                 # MB
                 "MB_Board_Free_RAM_MByte",
                 # IOS
                 "IOS_R_RF1offset_V","IOS_S_Lens_RF_Amplitude_Vpp","IOS_S_Lens_RF_Frequency_kHz","IOS_R_RF2phaseA_DC_V",
                 "IOS_R_RF2phaseB_DC_V","IOS_Inj_Flat_RF_Amplitude_Vpp","IOS_Inj_Flat_RF_Frequency_kHz",
                 "IOS_Inj_Flat_RF_Current_A","IOS_R_DC3_V","IOS_R_DC4_V","IOS_Bent_Flat_RF_Amplitude_Vpp",
                 "IOS_Bent_Flat_RF_Frequency_kHz","IOS_HVV_Positive_V","IOS_HVV_Negative_V","IOS_HV_Positive_V",
                 "IOS_HV_Negative_V",
                 # SB
                 "SB_pos_24_V_Supply_act_V","SB_pos_15_V_Analog_Supply_act_V","SB_neg_8_V_Supply_act_V",
                 "SB_pos_5_V_Analog_Supply_act_V","SB_pos_5_V_Digital_Supply_act_V","SB_neg_5_V_Analog_Supply_act_V",
                 "SB_pos_3_3_V_Digital_Supply_act_V","SB_pos_2_5_V_Digital_Supply_act_V",
                 "SB_pos_1_2_V_Digital_Supply_act_V","SB_Ion_Transfer_Tube_Heater_Temperature_act_C",
                 "SB_R_VaporizerTemp_C","SB_Source_ID_kOhm","SB_Cover_Light_Sensor_act_percent","SB_pos_48_V_act_V",
                 "SB_pos_5_V_act_V","SB_HV_Supply_Voltage_act_V","SB_R_heat_ris_pow_W",
                 "SB_Oven_Heater_Temperature_act_C","SB_Split_Heater_Temperature_act_C","SB_R_PressRegVolt_V",
                 # DAQ
                 "DAQ_Status_None",
                 # OTS
                 "OTS_HV_Offset_High_V","OTS_HV_Offset_Low_V","OTS_HV_Focus_Lens_V","OTS_V_Lens_V","OTS_Deflector_High_V",
                 "OTS_Deflector_Low_V","OTS_CE_Inject_Pos_V","OTS_CE_Inject_Neg_V","OTS_HV_Monitor_kV","OTS_CEmeas_Vpos_kV",
                 "OTS_CEmeas_Vneg_kV","OTS_PCB_Temp_Top_act_C","OTS_PCB_Temp_Center_act_C","OTS_PCB_Temp_Bottom_act_C",
                 # QS
                 "QS_Quad_Detector_Temp_C","QS_RF_Amplitude_Vpp","QS_Modulation_Voltage_V","QS_RF_Current_A",
                 "QS_Rod_Driver_Voltage_V","QS_HV_Negative_V","QS_HV_Positive_V",
                 # CTS
                 "CTS_Fan1_Speed_rpm","CTS_Fan2_Speed_rpm","CTS_CTrap_RF_Frequency_MHz",
                 "CTS_Orbitrap_PT100_Temperature_act_C","CTS_CTrap_RF_Amplitude_V","CTS_RF_Power_W",
                 "CTS_RF_Current_mA","CTS_RF_Supply_Voltage_V","CTS_Demodulator_Voltage_V","CTS_Z_Lens_V",
                 "CTS_HV_Positive_V","CTS_HV_Negative_V",
                 "NA")
  
  
  
  # Error Handling ----------------------------------------------------------
  
  # To make sure an instrument is added
  if(is.null(instrument)){
    stop("Error: Instrument should be either 240 or 480.")
  }
  
  # To make sure the correct instrument is added
  if(!instrument %in% c("240", "480")){
    stop("Error: Instrument should be either 240 or 480")
  }
  
  # Using common header for function
  if(instrument == "480"){
    header <- header480
  }
  # Contd
  if(instrument == "240"){
    header <- header240
  }
  
  
  # Exceptions --------------------------------------------------------------
  
  
  # Reading the data to check dimensions
  temp_file <- read.delim2(file, sep = "\t", header = FALSE)
  
  
  if(ncol(temp_file) != length(header)){
    
    data <- data.frame(matrix(nrow = 1, ncol = length(header)))
    
    names(data) <- header
    
    data$file <- basename(file)
    
  } else {
    
    # Reading the log file using the most up-to-date version
    data <- read.delim2(file, sep = "\t", header = FALSE)
    
    # Adding the column names
    names(data) <- header
    
    # indexing all the rows that contain the header rows (It's not always the top row)
    index <- which(grepl("[A-z]", data[,1]))
    
    # This statement is for special cases
    # Sometimes, the log file does not have header info. This would be missed without the if statement
    if(length(index) > 0){
      
      # Now I can remove the rows with header info
      data <- data[-index,]
      
    }
    
    # Creating a vector of date-time. I need to deal with this separately
    date_column <- data$Date
    
    # Removing the date-time from the dataframe
    data$Date <- NA
    
    # Need to convert the entire dataframe to numeric
    data <- as.data.frame(sapply(data, FUN = as.numeric))
    
    # Now that the dataframe is numeric, I can add back the date-time
    data$Date <- date_column
    
    # add file name
    data$file <- basename(file)
    
  }
  
  # return dataframe
  return(data)
  
}
