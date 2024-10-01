setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\CCRG_PurpleAir")
rm(list = ls())
library(httr);library(jsonlite);library(dplyr)

# PurpleAir Data Cleaning --------------------------------------------------------------------------
# Loop and read in TEMPERATURE files from purpleair -- collected later and separately from initial API call in separate script due to requirements for SE-specific correction factor 
folder_path_temp <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\tempdata"
file_names_temp <- list.files(path = folder_path_temp, pattern = "\\.csv$", full.names = TRUE)
data_frames_temp <- list()
for (i in seq_along(file_names_temp)) {
  # Read the CSV file
  data <- read.csv(file_names_temp[i])
  
  # Remove everything but time_stamp and temperature columns
  data <- data %>% dplyr::select(data.1, data.2) %>% rename(time_stamp = data.1, temperature = data.2)
  
  # Convert the 'time_stamp' column from Unix time to POSIXct
  data$time_stamp <- as.POSIXct(data$time_stamp, origin = "1970-01-01 00:00:00")
  data$time_stamp <- format(data$time_stamp, "%Y-%m-%d")
  
  # Remove duplicate rows
  data <- data[!duplicated(data), ]
  
  # Extract the first 6 characters of the file name
  truncated_file_name <- paste0(substr(basename(file_names_temp[i]), 3, 8), "_temp")
  
  # Store the data frame and truncated file name in the list
  data_frames_temp[[i]] <- list(truncated_file_name = truncated_file_name, data = data)
}

# Combining sensor values spread across 2 excel files per API call data size limits
combined_data_frames_temp <- list()
for (i in seq_along(data_frames_temp)) {
  # Extract the first four characters of the truncated_file_name
  key <- substr(data_frames_temp[[i]]$truncated_file_name, 1, 4)
  
  # Check if this key already exists in the combined_data_frames list
  if (!key %in% names(combined_data_frames_temp)) {
    # If not, create a new entry in the list
    combined_data_frames_temp[[key]] <- data_frames_temp[[i]]$data
  } else {
    # If yes, combine the current data frame with the existing one
    combined_data_frames_temp[[key]] <- rbind(combined_data_frames_temp[[key]], data_frames_temp[[i]]$data)
  }
}

# convert temperature to celsius -- applied on 9/30/2024 per conversation with Jen and Martine on SE-specific correction factor unit requirements
for (i in seq_along(combined_data_frames_temp)) {
  combined_data_frames_temp[[i]]$temperature <- (combined_data_frames_temp[[i]]$temperature - 32) * 5/9
}

# Loop and read in PM2.5 files (original scrape -- and clean) 
folder_path <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\pmdata"
file_names <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
data_frames <- list()
for (i in seq_along(file_names)) {
  data <- read.csv(file_names[i])
  data <- data %>% dplyr::select(data.1, data.2, data.5, data.6) %>% 
                          rename(time_stamp = data.1, temperature = data.2, pm2.5_a = data.5, pm2.5_b = data.6)
  
  # Convert the 'time_stamp' column from Unix time to POSIXct
  data$time_stamp <- as.POSIXct(data$time_stamp, origin = "1970-01-01 00:00:00")
  data$time_stamp <- format(data$time_stamp, "%Y-%m-%d")
  
  # Remove duplicate rows
 # data <- data[!duplicated(data), ]
  
  # Extract the first 6 characters of the file name
  truncated_file_name <- paste0(substr(basename(file_names[i]), 3, 8), "_pm")
  
  # Store the data frame and truncated file name in the list
  data_frames[[i]] <- list(truncated_file_name = truncated_file_name, data = data)
}

# Combining sensor values spread across 2 excel files per limitations in API call limits
# RH is incorrectly labeled as temperature in these data frames and naming convention is altered later to save effort of changing each input file
combined_data_frames <- list()
for (i in seq_along(data_frames)) {
  # Extract the first four characters of the truncated_file_name
  key <- substr(data_frames[[i]]$truncated_file_name, 1, 4)
  
  # Check if this key already exists in the combined_data_frames list
  if (!key %in% names(combined_data_frames)) {
    # If not, create a new entry in the list
    combined_data_frames[[key]] <- data_frames[[i]]$data
  } else {
    # If yes, combine the current data frame with the existing one
    combined_data_frames[[key]] <- rbind(combined_data_frames[[key]], data_frames[[i]]$data)
  }
}

# Merging the temperature data with the pm + RH data
all_purple_air_data <- list()
merge_by_key_and_time_stamp <- function(df1, df2) {
  merged_list <- list()
  for (key in union(names(df1), names(df2))) {
    if (key %in% names(df1) & key %in% names(df2)) {
      merged_list[[key]] <- merge(df1[[key]], df2[[key]], by = "time_stamp", all = TRUE)
    } else if (key %in% names(df1)) {
      merged_list[[key]] <- df1[[key]]
    } else {
      merged_list[[key]] <- df2[[key]]
    }
  }
  return(merged_list)
}

# Merge the two lists by their keys and time_stamp
all_purple_air_data <- merge_by_key_and_time_stamp(combined_data_frames, combined_data_frames_temp)
all_purple_air_data <- lapply(all_purple_air_data, function(df) {
                              df %>% 
                              rename(humidity = temperature.x, temperature = temperature.y) #this is where previously incorrect naming conventions were corrected
})
df_names <- names(all_purple_air_data)

# Apply correction factors for measurement errors humidity/temperature (hygroscopic growth)
corrected_purple_air_data <- list()
corrected_purple_air_data.1 <- list()

# For loop to perform the correction factors from the literature
for (i in seq_along(all_purple_air_data)) {
  data <- all_purple_air_data[[i]]  # Extract the dataframe
  
  # CONUS correction factor (Barkjohn et al 2021)
  if ("pm2.5_a" %in% colnames(data) && "pm2.5_b" %in% colnames(data)) {
    data$sensor_dif <- data$pm2.5_a - data$pm2.5_b
    data$channel_comp <- ((data$pm2.5_a - data$pm2.5_b) * 2) / (data$pm2.5_a + data$pm2.5_b)
    data$pm2.5_avg_raw <- (data$pm2.5_a + data$pm2.5_b) / 2
    
    # Remove rows where sensor_dif > 5 or channel_comparison > .61
    data <- data[data$sensor_dif <= 5 & data$sensor_dif >= -5 & data$channel_comp <= 0.61 & data$channel_comp >= -0.61, ]
    data$pm2.5_avg <- (data$pm2.5_a + data$pm2.5_b) / 2
  }
  if ("pm2.5_avg" %in% colnames(data) && "humidity" %in% colnames(data)) {
    data$CONUS_pm2.5 <- 0.524 * data$pm2.5_avg - 0.0862 * data$humidity + 5.75
  }
  
  # SE specific correction factor (Mathieu-Campbell et al 2024)
  data.1 <- all_purple_air_data[[i]]  # Extract the dataframe
  
  # Remove points > 1000 and <= 1.5
  data.1 <- data.1[data.1$pm2.5_a <= 1000 & data.1$pm2.5_a >= 1.5 &
                     data.1$pm2.5_b <= 1000 & data.1$pm2.5_b >= 1.5, ]
  
  data.1$sensor_dif <- data.1$pm2.5_a - data.1$pm2.5_b
  data.1$channel_comp <- ((data.1$pm2.5_a - data.1$pm2.5_b) * 2) / (data.1$pm2.5_a + data.1$pm2.5_b)
  
  # Filter based on conditions
 data.1 <- subset(data.1, 
                   !(pm2.5_a <= 25 & pm2.5_b <= 25 & (sensor_dif >= 5 | channel_comp >= 0.2 | channel_comp <= -0.2)) &
                   !(pm2.5_a >= 25 & pm2.5_b >= 25 & (channel_comp >= 0.2 | channel_comp <= -0.2)))
  
  # Calculate pm2.5_avg_SE (raw data but with stricter cleaning procedure) and SE.pm2.5 (cf applied)
 data.1$pm2.5_avg_SE <- (data.1$pm2.5_a + data.1$pm2.5_b) / 2
 # Multilinear Regression Model 4 approach 
 # data.1$SE.pm2.5 <-  4.3295358 + (0.4182906*data.1$pm2.5_avg_SE) - (0.0445768* data.1$humidity) + (0.0752867*data.1$temperature)
 # Semi-Supervised Clustered approach to account for high humidity
 data.1 <- data.1 %>% mutate(SE.pm2.5 = case_when(
                                       humidity <= 50 ~ 2.738732 + (0.425834*pm2.5_avg_SE) - (0.008944*humidity) + (0.079210*temperature),
                                       humidity > 50 ~ 7.230374 + (0.412683*pm2.5_avg_SE) - (0.085278*humidity) + (0.070655*temperature)))
  # Remove duplicate rows
  data <- data[!duplicated(data), ]
  data.1 <- data.1[!duplicated(data.1), ]
  
  # Extract the last 6 characters of the file name
 # truncated_file_name <- substr(basename(file_names[i]), nchar(basename(file_names[i])) - 9, nchar(basename(file_names[i])))
  
  corrected_purple_air_data[df_names[i]] <- list(data = data)
  corrected_purple_air_data.1[df_names[i]] <- list(data = data.1)
}


# Merging different correction factors into single dfs 
sensor.5822 <- left_join(corrected_purple_air_data[["5822"]], corrected_purple_air_data.1[["5822"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)
sensor.5838 <- left_join(corrected_purple_air_data[["5838"]], corrected_purple_air_data.1[["5838"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)
sensor.1318 <- left_join(corrected_purple_air_data[["1318"]], corrected_purple_air_data.1[["1318"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)
sensor.1334 <- left_join(corrected_purple_air_data[["1334"]], corrected_purple_air_data.1[["1334"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)
sensor.1344 <- left_join(corrected_purple_air_data[["1344"]], corrected_purple_air_data.1[["1344"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)
sensor.1348 <- left_join(corrected_purple_air_data[["1348"]], corrected_purple_air_data.1[["1348"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)
sensor.1358 <- left_join(corrected_purple_air_data[["1358"]], corrected_purple_air_data.1[["1358"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)
sensor.1362 <- left_join(corrected_purple_air_data[["1362"]], corrected_purple_air_data.1[["1362"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)
sensor.1378 <- left_join(corrected_purple_air_data[["1378"]], corrected_purple_air_data.1[["1378"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)
sensor.1562 <- left_join(corrected_purple_air_data[["1562"]], corrected_purple_air_data.1[["1562"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)
sensor.1680 <- left_join(corrected_purple_air_data[["1680"]], corrected_purple_air_data.1[["1680"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)
sensor.1806 <- left_join(corrected_purple_air_data[["1806"]], corrected_purple_air_data.1[["1806"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)
sensor.9875 <- left_join(corrected_purple_air_data[["9875"]], corrected_purple_air_data.1[["9875"]], by = c("time_stamp", "temperature", "humidity"), keep = F) %>% dplyr::select(time_stamp, humidity, temperature, pm2.5_avg, CONUS_pm2.5, pm2.5_avg_SE, SE.pm2.5)

# Filling in missing humidity, temperature values and recalculating for sensors where Bosch measurements went out
# 1318 has missing humidity and temperature values -- as well as values humidity = 100 and temp = -223 for all after 10-22-2022, using RH values from nearest sensor (1344) operating at the time to fill in these NAs 
library(lubridate)
merged_data <- full_join(sensor.1318, sensor.1344, by = "time_stamp", suffix = c("_1318", "_1344"))
cutoff_date <- ymd("2022-10-21")
merged_data <- merged_data %>%
  mutate(humidity_1318 = if_else(time_stamp > cutoff_date, humidity_1344, humidity_1318),
         temperature_1318 = if_else(time_stamp > cutoff_date, temperature_1344, temperature_1318))
new_RH_values_1318 <- merged_data %>% dplyr::select(time_stamp, humidity_1318, temperature_1318)
sensor.1318.1 <- full_join(new_RH_values_1318, sensor.1318, by = "time_stamp") %>%
  dplyr::select(-humidity, -temperature) %>%
  rename(humidity = humidity_1318, temperature = temperature_1318)
sensor.1318.1 <- sensor.1318.1 %>%
  mutate(CONUS_pm2.5 = 0.524 * pm2.5_avg - 0.0862 * humidity + 5.75) %>% 
  mutate(SE.pm2.5 = case_when(
    humidity <= 50 ~ 2.738732 + (0.425834*pm2.5_avg_SE) - (0.008944*humidity) + (0.079210*temperature),
    humidity > 50 ~ 7.230374 + (0.412683*pm2.5_avg_SE) - (0.085278*humidity) + (0.070655*temperature)))
 # mutate(SE.pm2.5 = 4.3295358 + (0.4182906*pm2.5_avg_SE) - (0.0445768* humidity) + (0.0752867*temperature))
sensor.1318 <- sensor.1318.1

# same issue for 9875
merged_data <- full_join(sensor.9875, sensor.1344, by = "time_stamp", suffix = c("_9875", "_1344"))
cutoff_date <- ymd("2022-10-21")
merged_data <- merged_data %>%
  mutate(humidity_9875 = if_else(time_stamp > cutoff_date, humidity_1344, humidity_9875),
         temperature_9875 = if_else(time_stamp > cutoff_date, temperature_1344, temperature_9875))
new_RH_values_9875 <- merged_data %>% dplyr::select(time_stamp, humidity_9875, temperature_9875)
sensor.9875.1 <- full_join(new_RH_values_9875, sensor.9875, by = "time_stamp") %>%
  dplyr::select(-humidity, -temperature) %>%
  rename(humidity = humidity_9875, temperature = temperature_9875)
sensor.9875.1 <- sensor.9875.1 %>%
  mutate(CONUS_pm2.5 = 0.524 * pm2.5_avg - 0.0862 * humidity + 5.75) %>% 
  mutate(SE.pm2.5 = case_when(
  humidity <= 50 ~ 2.738732 + (0.425834*pm2.5_avg_SE) - (0.008944*humidity) + (0.079210*temperature),
  humidity > 50 ~ 7.230374 + (0.412683*pm2.5_avg_SE) - (0.085278*humidity) + (0.070655*temperature)))
 # mutate(SE.pm2.5 = 4.3295358 + (0.4182906*pm2.5_avg_SE) - (0.0445768* humidity) + (0.0752867*temperature))
sensor.9875 <- sensor.9875.1

# 1348 has missing humidity and temp values, using values from nearest sensor (5838) operating at the time to fill in these NAs 
merged_data <- full_join(sensor.1348, sensor.5838, by = "time_stamp", suffix = c("_1348", "_5838"))
merged_data <- merged_data %>% mutate(humidity_1348 = coalesce(humidity_1348, humidity_5838), temperature_1348 = coalesce(temperature_1348, temperature_5838))
new.RH.values.1348 <- merged_data %>% dplyr::select(time_stamp, humidity_1348, temperature_1348)
sensor.1348.1 <- full_join(new.RH.values.1348, sensor.1348, by = "time_stamp") %>% dplyr::select(-humidity, -temperature) %>% rename(humidity = humidity_1348, temperature = temperature_1348)
sensor.1348.1 <- sensor.1348.1 %>% 
  mutate(CONUS_pm2.5 = 0.524 * pm2.5_avg - 0.0862 * humidity + 5.75) %>% 
  mutate(SE.pm2.5 = case_when(
    humidity <= 50 ~ 2.738732 + (0.425834*pm2.5_avg_SE) - (0.008944*humidity) + (0.079210*temperature),
    humidity > 50 ~ 7.230374 + (0.412683*pm2.5_avg_SE) - (0.085278*humidity) + (0.070655*temperature)))
 # mutate(SE.pm2.5 = 4.3295358 + (0.4182906*pm2.5_avg_SE) - (0.0445768* humidity) + (0.0752867*temperature))
sensor.1348 <- sensor.1348.1

# 1680 -- using values from nearest sensor (5838) operating at the time to fill in these NAs 
merged_data <- full_join(sensor.1680, sensor.5838, by = "time_stamp", suffix = c("_1680", "_5838"))
merged_data <- merged_data %>% mutate(humidity_1680 = coalesce(humidity_1680, humidity_5838), temperature_1680 = coalesce(temperature_1680, temperature_5838))
new.RH.values.1680 <- merged_data %>% dplyr::select(time_stamp, humidity_1680 , temperature_1680)
sensor.1680.1 <- full_join(new.RH.values.1680, sensor.1680, by = "time_stamp") %>% dplyr::select(-humidity, -temperature) %>% rename(humidity = humidity_1680, temperature = temperature_1680)
sensor.1680.1 <- sensor.1680.1 %>%  
  mutate(CONUS_pm2.5 = 0.524 * pm2.5_avg - 0.0862 * humidity + 5.75) %>% 
  mutate(SE.pm2.5 = case_when(
    humidity <= 50 ~ 2.738732 + (0.425834*pm2.5_avg_SE) - (0.008944*humidity) + (0.079210*temperature),
     humidity > 50 ~ 7.230374 + (0.412683*pm2.5_avg_SE) - (0.085278*humidity) + (0.070655*temperature)))
 # mutate(SE.pm2.5 = 4.3295358 + (0.4182906*pm2.5_avg_SE) - (0.0445768* humidity) + (0.0752867*temperature))
# renaming to original file name 
sensor.1680 <- sensor.1680.1

# 5822 -- using values from nearest sensor (1334) operating at the time to fill in these NAs 
merged_data <- full_join(sensor.5822, sensor.1334, by = "time_stamp", suffix = c("_5822", "_1334"))
merged_data <- merged_data %>% mutate(humidity_5822 = coalesce(humidity_5822, humidity_1334), temperature_5822 = coalesce(temperature_5822, temperature_1334))
new.RH.values.5822 <- merged_data %>% dplyr::select(time_stamp, humidity_5822, temperature_5822)
sensor.5822.1 <- full_join(new.RH.values.5822, sensor.5822, by = "time_stamp") %>% dplyr::select(-humidity, -temperature) %>% rename(humidity = humidity_5822, temperature = temperature_5822)
sensor.5822.1 <- sensor.5822.1 %>% 
  mutate(CONUS_pm2.5 = 0.524 * pm2.5_avg - 0.0862 * humidity + 5.75) %>% 
  mutate(SE.pm2.5 = case_when(
  humidity <= 50 ~ 2.738732 + (0.425834*pm2.5_avg_SE) - (0.008944*humidity) + (0.079210*temperature),
  humidity > 50 ~ 7.230374 + (0.412683*pm2.5_avg_SE) - (0.085278*humidity) + (0.070655*temperature)))
  #mutate(SE.pm2.5 = 4.3295358 + (0.4182906*pm2.5_avg_SE) - (0.0445768* humidity) + (0.0752867*temperature))
sensor.5822 <- sensor.5822.1

# Compiling purpleair data back into a list 
all_objects <- ls()
sensor_objects <- grep("^sensor\\.\\w{4}$", all_objects, value = TRUE)

# List of dataframes to combine
sensor_names <- c("sensor.1318", "sensor.1344", "sensor.1348", "sensor.1358", 
                  "sensor.1362", "sensor.1378", "sensor.1680", "sensor.1806", 
                  "sensor.5822", "sensor.5838", "sensor.9875", "sensor.1334", "sensor.1562")

# Initialize an empty list to store combined dataframes
purpleair_data <- list()

# Loop through each dataframe, add a new column "Name" with the sensor name
for (sensor_name in sensor_names) {
  df <- get(sensor_name) # Get dataframe by name
  df <- df %>%
    mutate(Name = gsub("sensor\\.", "", sensor_name)) # Extract sensor number and add as new column
  purpleair_data[[sensor_name]] <- df # Add modified dataframe to combined list
}

compiled_purpleair_data <- bind_rows(purpleair_data) %>% 
  dplyr::select(Name, time_stamp, CONUS_pm2.5, SE.pm2.5, pm2.5_avg) %>% 
  rename(date = time_stamp, pm2.5_CONUS = CONUS_pm2.5, pm2.5_SE = SE.pm2.5)  %>%
  mutate(across(-c(date, Name), ~ifelse(. < 0, 0, .))) %>% mutate(date = as.Date(date))

# CyAN Cyanobacterial Index (digital number DN) data from SEADAS Pixel Extraction ---------------------------------------------- 
# write for loop to read in TIFF files and then extract information I need 
## I used this to find the X and Y pixel bounds in UTM so that I could write the code to automatically scrape all of pixel data I needed using python
# library(raster)
# str_name <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\SeaDAS files\\CyanoIndices\\L2022157.L3m_DAY_CYAN_CI_cyano_CYAN_CONUS_300m_8_3.tif"
# imported_raster <- raster(str_name)
# imported_raster # see properties of the file 

library(tidyverse);library(dplyr)

# read in all of the SeaDAS files 
# Specify the folder path containing your text files
# as of 08-27-2024 I updated the CyAN data to be at a higher resolution (smaller areas per Zorbas et al., 2023)
# as of 08-28-2024 I am compositing images into weekly maximums for each pixel before finding averages 
#folder_path <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\SeaDAS files\\Band_1_py_2"

# Initialize an empty list to store data frames
#result_list <- list()

# Iterate over each file in the folder
#files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)

# Loop through each file
#for (file in files) {tryCatch({
    # Read the file, skipping lines until line 7
    #df <- read_delim(file, delim = "\t", col_names = TRUE, skip = 0)
    
    # Check if 'Name' column exists in the dataframe
   # if ('Name' %in% colnames(df)) {
      # Select and store the desired columns
     # selected_columns <- df %>% dplyr::select(Name, band_1)
      
      # Extract the desired part from the original filename to use as the new filename
     # new_filename <- gsub("_Derived from .*", "", basename(file))
      
      # Use the filename (without extension) as the list element name
     # result_list[[new_filename]] <- selected_columns
   # } else {
   #   warning(paste("Skipping file", basename(file), "as it does not contain 'Name' column."))
  #  }
 # }, error = function(e) {
  #  warning(paste("Error reading", basename(file), ":", e$message))
 # })
  
# trying to read in only the weekly composites instead of the daily data 
library(dplyr)
library(readr)
library(stringr)

# Define the folder path
folder_path <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\SeaDAS files\\Band_1_py_2"

# Initialize an empty list to store weekly results
weekly_result_list <- list()

# List all files in the folder
files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)

# Extract Julian dates and sort files by Julian date and size indicator
files_info <- data.frame(
  file_path = files,
  julian_date = as.numeric(str_extract(basename(files), "\\d{7}")), # Extract Julian dates
  size_type = str_extract(basename(files), "_[SML]") # Extract the _S, _M, or _L indicator
)

# Function to calculate the weekly maximum for 'band_1', considering special values
calc_weekly_max <- function(band_values) {
  if (any(band_values == 254)) {
    return(254)
  } else if (all(band_values == 255)) {
    return(255)
  } else {
    return(max(band_values[band_values < 254]))
  }
}

# Separate files by size type and process each group independently
size_types <- unique(files_info$size_type)

for (size in size_types) {
  # Filter files for the current size type
  size_group <- files_info %>% filter(size_type == size) %>% arrange(julian_date)
  
  # Process files in groups of 7 to create weekly composites
  for (i in seq(1, nrow(size_group), by = 7)) {
    # Get the group of 7 files
    file_group <- size_group$file_path[i:min(i + 6, nrow(size_group))]
    
    # Initialize a dataframe to store combined data
    combined_data <- data.frame()
    
    # Process each file in the group
    for (file in file_group) {
      tryCatch({
        # Read the file
        df <- read_delim(file, delim = "\t", col_names = TRUE, skip = 0)
        
        # Check if 'Name' column exists in the dataframe
        if ('Name' %in% colnames(df)) {
          # Append to combined data
          combined_data <- bind_rows(combined_data, df)
        } else {
          warning(paste("Skipping file", basename(file), "as it does not contain 'Name' column."))
        }
      }, error = function(e) {
        warning(paste("Error reading", basename(file), ":", e$message))
      })
    }
    
    # Group by all columns except 'band_1' and calculate weekly max for 'band_1'
    weekly_data <- combined_data %>%
      group_by(across(-band_1)) %>%
      summarise(band_1 = calc_weekly_max(band_1), .groups = 'drop') %>%
      arrange(Name)
    
    # Create a name for the weekly composite based on the first and last Julian date in the group
    week_name <- paste0("Week_", size_group$julian_date[i], "_to_", size_group$julian_date[min(i + 6, nrow(size_group))], size)
    
    # Store the weekly data in the list
    weekly_result_list[[week_name]] <- weekly_data
  }
}

# Creating three separate lists for S, M, and L pixel coverages ---------------------------------------------------
# Create empty lists to store dataframes
S_list <- list()
M_list <- list()
L_list <- list()

# Loop through the list of dataframes and sort them into respective lists
for (df_name in names(weekly_result_list)) {
  if (grepl("_S|_S_*", df_name, ignore.case = FALSE)) {
    S_list[[df_name]] <- weekly_result_list[[df_name]]
  } else if (grepl("_M|_M_*", df_name, ignore.case = FALSE)) {
    M_list[[df_name]] <- weekly_result_list[[df_name]]
  } else if (grepl("_L|_L_*", df_name, ignore.case = FALSE)) {
    L_list[[df_name]] <- weekly_result_list[[df_name]]
  }
}

# Adding dates into each dataframe for easier manipulation later on 
# Iterate over each data frame in the list
for (df_name in names(S_list)) {
  # Extract the date from the dataframe name
  date <- gsub("[^0-9]", "", df_name)  # Extract only digits from the dataframe name
  
  # Convert the extracted date to a proper date format (YYYY-MM-DD)
  date <- as.Date(as.character(date), format = "%Y%j")  # Convert to date format
  
  # Add a new column named 'date' to the dataframe with the extracted date
  S_list[[df_name]]$date <- format(date, "%Y-%m-%d")  # Format date as YYYY-MM-DD
}

for (df_name in names(M_list)) {
  # Extract the date from the dataframe name
  date <- gsub("[^0-9]", "", df_name)  # Extract only digits from the dataframe name
  
  # Convert the extracted date to a proper date format (YYYY-MM-DD)
  date <- as.Date(as.character(date), format = "%Y%j")  # Convert to date format
  
  # Add a new column named 'date' to the dataframe with the extracted date
  M_list[[df_name]]$date <- format(date, "%Y-%m-%d")  # Format date as YYYY-MM-DD
}

for (df_name in names(L_list)) {
  # Extract the date from the dataframe name
  date <- gsub("[^0-9]", "", df_name)  # Extract only digits from the dataframe name
  
  # Convert the extracted date to a proper date format (YYYY-MM-DD)
  date <- as.Date(as.character(date), format = "%Y%j")  # Convert to date format
  
  # Add a new column named 'date' to the dataframe with the extracted date
  L_list[[df_name]]$date <- format(date, "%Y-%m-%d")  # Format date as YYYY-MM-DD
}

# Calculate cyanobacterial indices, chlorophyll a and cyano cell count averages (measures of Intensity) ----------
# this is working fine, but the issue is that a lot of sensors are dropping due to missing values which will need to be addressed later on
S_intensity <- list()  
for (df_name in names(S_list)) {
  # Calculate summary statistics
  summary_df <- S_list[[df_name]] %>%
    mutate(pixel_type = case_when(
      band_1 >= 0 & band_1 <= 253 ~ "valid",
      band_1 == 254 ~ "land",
      band_1 == 255 ~ "invalid")) %>%
    mutate(CI_cyano = 10^(3/250*band_1-4.2),
           cyano_cell_count = CI_cyano*100000000,
           chlorophyll = 6620*CI_cyano-3.07) %>%
    filter(pixel_type == "valid") %>%
    group_by(Name, date) %>%
    summarize(
      avg_cyano_cell_count = mean(cyano_cell_count, na.rm = TRUE),
      median_cyano_cell_count = median(cyano_cell_count, na.rm = TRUE),
      max_cyano_cell_count = max(cyano_cell_count, na.rm = TRUE),
      avg_chlorophyll = mean(chlorophyll, na.rm = TRUE),
      median_chlorophyll = median(chlorophyll, na.rm = TRUE),
      max_chlorophyll = max(chlorophyll, na.rm = TRUE)) %>%
    mutate_at(vars(c(avg_cyano_cell_count, median_cyano_cell_count, max_cyano_cell_count)), ~ifelse(. <= 6310, 0, .)) %>%
    mutate_at(vars(c(avg_chlorophyll, median_chlorophyll, max_chlorophyll)), ~ifelse(. <= 0, 0, .)) %>%
    ungroup()  %>% 
    as.data.frame() %>%
    mutate(Name = gsub("^sensor_", "", Name))
  # Store the summary data frame in the list with the same name as the data frame
  S_intensity[[df_name]] <- summary_df }

# Define the complete set of Name values
all_names <- as.character(c(1562, 5822, 1334, 1680, 1358, 5838, 1348, 1378, 1362, 1318, 9875, 1344, 1806))

# Fill in the missing values for each sensor with NA
for (i in seq_along(S_intensity)) {
  S_intensity[[i]] <- S_intensity[[i]] %>%
    complete(`Name` = all_names) %>%
    fill(date) %>%
    fill(date, .direction = "up")
}

all_S_cyano_values_wide <- bind_rows(S_intensity) %>% mutate(Name = as.character(Name), date = as.Date(date)) %>% drop_na(date)

M_intensity <- list()  
for (df_name in names(M_list)) {
  # Calculate summary statistics
  summary_df <- M_list[[df_name]] %>%
    mutate(pixel_type = case_when(
      band_1 >= 0 & band_1 <= 253 ~ "valid",
      band_1 == 254 ~ "land",
      band_1 == 255 ~ "invalid")) %>%
    mutate(CI_cyano = 10^(3/250*band_1-4.2),
           cyano_cell_count = CI_cyano*100000000,
           chlorophyll = 6620*CI_cyano-3.07) %>%
    filter(pixel_type == "valid") %>%
    group_by(Name, date) %>%
    summarize(
      avg_cyano_cell_count = mean(cyano_cell_count, na.rm = TRUE),
      median_cyano_cell_count = median(cyano_cell_count, na.rm = TRUE),
      max_cyano_cell_count = max(cyano_cell_count, na.rm = TRUE),
      avg_chlorophyll = mean(chlorophyll, na.rm = TRUE),
      median_chlorophyll = median(chlorophyll, na.rm = TRUE),
      max_chlorophyll = max(chlorophyll, na.rm = TRUE)) %>%
    mutate_at(vars(c(avg_cyano_cell_count, median_cyano_cell_count, max_cyano_cell_count)), ~ifelse(. <= 6310, 0, .)) %>%
    mutate_at(vars(c(avg_chlorophyll, median_chlorophyll, max_chlorophyll)), ~ifelse(. <= 0, 0, .)) %>%
    ungroup()  %>% 
    as.data.frame() %>%
    mutate(Name = gsub("^sensor_", "", Name))
  # Store the summary data frame in the list with the same name as the data frame
  M_intensity[[df_name]] <- summary_df }

# Fill in the missing values for each sensor with NA
for (i in seq_along(M_intensity)) {
  M_intensity[[i]] <- M_intensity[[i]] %>%
    complete(Name = all_names) %>%
    fill(date) %>%
    fill(date, .direction = "up")
}

all_M_cyano_values_wide <- bind_rows(M_intensity) %>% mutate(Name = as.character(Name), date = as.Date(date)) %>% drop_na(date)


L_intensity <- list()  
for (df_name in names(L_list)) {
  # Calculate summary statistics
  summary_df <- L_list[[df_name]] %>%
    mutate(pixel_type = case_when(
      band_1 >= 0 & band_1 <= 253 ~ "valid",
      band_1 == 254 ~ "land",
      band_1 == 255 ~ "invalid")) %>%
    mutate(CI_cyano = 10^(3/250*band_1-4.2),
           cyano_cell_count = CI_cyano*100000000,
           chlorophyll = 6620*CI_cyano-3.07) %>%
    filter(pixel_type == "valid") %>%
    group_by(Name, date) %>%
    summarize(
      avg_cyano_cell_count = mean(cyano_cell_count, na.rm = TRUE),
      median_cyano_cell_count = median(cyano_cell_count, na.rm = TRUE),
      max_cyano_cell_count = max(cyano_cell_count, na.rm = TRUE),
      avg_chlorophyll = mean(chlorophyll, na.rm = TRUE),
      median_chlorophyll = median(chlorophyll, na.rm = TRUE),
      max_chlorophyll = max(chlorophyll, na.rm = TRUE)) %>%
    mutate_at(vars(c(avg_cyano_cell_count, median_cyano_cell_count, max_cyano_cell_count)), ~ifelse(. <= 6310, 0, .)) %>%
    mutate_at(vars(c(avg_chlorophyll, median_chlorophyll, max_chlorophyll)), ~ifelse(. <= 0, 0, .)) %>%
    ungroup()  %>% 
    as.data.frame() %>%
    mutate(Name = gsub("^sensor_", "", Name))
  # Store the summary data frame in the list with the same name as the data frame
  L_intensity[[df_name]] <- summary_df }

# Fill in the missing values for each sensor with NA
for (i in seq_along(L_intensity)) {
  L_intensity[[i]] <- L_intensity[[i]] %>%
    complete(Name = all_names) %>%
    fill(date) %>%
    fill(date, .direction = "up")
}

all_L_cyano_values_wide <- bind_rows(L_intensity) %>% mutate(Name = as.character(Name), date = as.Date(date)) %>% drop_na(date)

# Determining Spatial Coverage ---------------------------------------
# set a different threshold for a bloom ? i.e., 0 is a valid measurement but not a 'bloom', start with ~10,000 cells or a cyano index of X?
# a cyanobacterial index of 17 ~10K cells/mL, is about lowest detection (10092.53), a cyanobacterial index of 100 = 100,000 cells/mL (~3.6 ug/L chla), and what about chlorophyll? 40 ug/L threshold for Chowan per NC-DEQ, this is a CIcyano of ~168 (which is ~650K cells/mL). Range of CIcyano is between 10K to 7M cells/mL

# Function to calculate daily spatial coverage of blooms 
process_dataframe <- function(df) {
  df1 <- df %>%
    mutate(pixel_type = case_when(
      band_1 >= 0 & band_1 <= 253 ~ "valid",
      band_1 == 254 ~ "land",
      band_1 == 255 ~ "invalid")) %>%
    mutate(bloom_status = case_when(
      band_1 >= 0 & band_1 < 100 ~ "non-bloom", # this is where I can change the value for bloom threshold, see notes above
      band_1 >= 100 & band_1 < 254 ~ "bloom",
      band_1 == 254 ~ "land",
      band_1 == 255 ~ "invalid")) %>%
    mutate(pixel_assignment = case_when(
      pixel_type == "invalid" ~ "water",
      pixel_type == "valid" ~ "water",
      pixel_type == "land" ~ "land"))
  
  df2 <- df1 %>%
    group_by(date, Name, pixel_assignment) %>%
    summarise(water_pixels_total = n(), .groups = 'drop') %>% 
    complete(Name, pixel_assignment = c("water", "land"), fill = list(water_pixels_total = 0)) %>%
    filter(pixel_assignment != "land") %>% 
    mutate(water_surface_area = water_pixels_total * 300 * 300)
  
  df3 <- df1 %>%
    group_by(date, Name, bloom_status) %>%
    summarise(bloom_pixels = n(), .groups = 'drop') %>%
    complete(date, Name, bloom_status = c("bloom", "non-bloom", "land", "invalid"), fill = list(bloom_pixels = 0)) %>%
    filter(bloom_status != "land") %>% 
    pivot_wider(names_from = bloom_status, values_from = bloom_pixels)
  
  df4 <- df3 %>% 
    full_join(df2, by = c("date","Name")) %>%
    mutate(percent_bloom = signif((bloom / water_pixels_total), digits = 3)) %>% 
    mutate(surface_area_bloom = signif((percent_bloom * water_surface_area), digits = 3))
  
  df5 <- df4 %>% dplyr::select(date, Name, percent_bloom, surface_area_bloom)
  
  return(df5)
}

# Calculate Spatial Coverage using function for each individual dataframe (single dates)
processed_list_S <- lapply(S_list, process_dataframe)
processed_list_M <- lapply(M_list, process_dataframe)
processed_list_L <- lapply(L_list, process_dataframe)
# Combine all processed dataframes into a single dataframe (all dates per S, M, L)
all_S_spatialcov_wide <- bind_rows(processed_list_S) %>% mutate(Name = as.character(Name), date = as.Date(date)) %>%
  mutate(Name = gsub("^sensor_", "", Name))
all_M_spatialcov_wide <- bind_rows(processed_list_M) %>% mutate(Name = as.character(Name), date = as.Date(date)) %>%
  mutate(Name = gsub("^sensor_", "", Name))
all_L_spatialcov_wide <- bind_rows(processed_list_L) %>% mutate(Name = as.character(Name), date = as.Date(date)) %>%
  mutate(Name = gsub("^sensor_", "", Name))

# Combining Spatial Coverage data with Intensity data ------------------------------------------
# wide
all_S_cyano_values_wide <- full_join(all_S_cyano_values_wide, all_S_spatialcov_wide, by = c("Name", "date"))
all_M_cyano_values_wide <- full_join(all_M_cyano_values_wide, all_M_spatialcov_wide, by = c("Name", "date"))
all_L_cyano_values_wide <- full_join(all_L_cyano_values_wide, all_L_spatialcov_wide, by = c("Name", "date"))

#1562, 1344, 1358 why are these printed twice?  
# long
all_S_cyano_values_long <- all_S_cyano_values_wide %>% 
  pivot_longer(cols = c(-Name,-date),
               names_to = "variable",
               values_to = "value") %>% drop_na(date)
all_M_cyano_values_long <- all_M_cyano_values_wide %>% 
  pivot_longer(cols = c(-Name,-date),
               names_to = "variable",
               values_to = "value") %>% drop_na(date)
all_L_cyano_values_long <- all_L_cyano_values_wide %>% 
  pivot_longer(cols = c(-Name,-date),
               names_to = "variable",
               values_to = "value") %>% drop_na(date)

# Generate site description data for each sensor at each resolution ----------------------------------------

# Function to calculate descriptors of data completeness for each sensor/ spatial resolution
site_descriptors <- function(df) {
  df1 <- df %>% mutate(pixel_type = case_when(
      band_1 >= 0 & band_1 <= 253 ~ "valid",
      band_1 == 254 ~ "land",
      band_1 == 255 ~ "invalid"))
  
  df2 <- df1 %>%
    group_by(date, Name, pixel_type) %>%
    summarise(data_completeness = n(), .groups = 'drop') %>% 
    complete(date, Name, pixel_type = c("valid", "invalid", "land"), fill = list(data_completeness = 0)) 
  
  df3 <- df2 %>% pivot_wider(names_from = pixel_type, values_from = data_completeness) %>% 
    mutate(percent_water = ifelse(invalid+valid == 0, 0, signif((invalid+valid)/(invalid+valid+land), digits = 3)),
           percent_complete = ifelse(valid+land == 0, 0, signif((valid+land)/(invalid+valid+land), digits = 3)),
           percent_water_complete = ifelse(valid == 0, 0, signif(valid/(invalid+valid), digits = 3)),
           percent_land = ifelse(land == 0, 0, signif(land/(invalid+valid+land), digits = 3))) %>%
    mutate(Name = as.character(Name)) %>% 
    dplyr::select(-invalid,-valid,-land)

  return(df3)
}

# Calculate site/ data descriptors
site_descriptors_S <- lapply(S_list, site_descriptors)
data_completeness_S <- bind_rows(site_descriptors_S) %>% mutate(Name = as.character(Name), date = as.Date(date))
avg_data_completeness_S <- data_completeness_S %>% 
  group_by(Name) %>% 
  summarise(across(c("percent_water", "percent_complete", "percent_water_complete", "percent_land"), c(mean,sd), na.rm = T)) %>%
  rename_with(~ gsub("_1$", "_mean", .x), ends_with("_1")) %>%
  rename_with(~ gsub("_2$", "_sd", .x), ends_with("_2"))

site_descriptors_M <- lapply(M_list, site_descriptors)
data_completeness_M <- bind_rows(site_descriptors_M) %>% mutate(Name = as.character(Name), date = as.Date(date))
avg_data_completeness_M <- data_completeness_M %>% 
  group_by(Name) %>% 
  summarise(across(c("percent_water", "percent_complete", "percent_water_complete", "percent_land"), c(mean,sd), na.rm = T)) %>%
  rename_with(~ gsub("_1$", "_mean", .x), ends_with("_1")) %>%
  rename_with(~ gsub("_2$", "_sd", .x), ends_with("_2"))

site_descriptors_L <- lapply(L_list, site_descriptors)
data_completeness_L <- bind_rows(site_descriptors_L) %>% mutate(Name = as.character(Name), date = as.Date(date))
avg_data_completeness_L <- data_completeness_L %>% 
  group_by(Name) %>% 
  summarise(across(c("percent_water", "percent_complete", "percent_water_complete", "percent_land"), c(mean,sd), na.rm = T)) %>%
  rename_with(~ gsub("_1$", "_mean", .x), ends_with("_1")) %>%
  rename_with(~ gsub("_2$", "_sd", .x), ends_with("_2"))

write.csv(avg_data_completeness_S, 'C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\avg_data_completeness_S.csv')
write.csv(avg_data_completeness_M, 'C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\avg_data_completeness_M.csv')
write.csv(avg_data_completeness_L, 'C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\avg_data_completeness_L.csv')

# Combining Air Quality data with Water Quality data -----------------------------------------------
# longer for visualizations
compiled_purpleair_data_longer <- compiled_purpleair_data %>% 
                                  pivot_longer(cols = c(-Name,-date), names_to = "variable", values_to = "value")
compiled_purpleair_data_longer$date <- as.Date(compiled_purpleair_data_longer$date)

combined_water_air_S_long <- rbind(compiled_purpleair_data_longer, all_S_cyano_values_long) %>% 
  mutate(water.proximity = case_when(
    Name == "1318" ~ "<1km",
    Name == "1334" ~ "<1km",
    Name == "1344" ~ ">1km",
    Name == "1348" ~ "<.5km",
    Name == "1358" ~ "control",
    Name == "1362" ~ "<.5km",
    Name == "1378" ~ "<.5km",
    Name == "1562" ~ "control",
    Name == "1680" ~ "<.5km",
    Name == "1806" ~ "<1km",
    Name == "5822" ~ "<.5km",
    Name == "5838" ~ "<1km",
    Name == "9875" ~ "<.5km")) %>% mutate(date = as.Date(date))
combined_water_air_M_long <- rbind(compiled_purpleair_data_longer, all_M_cyano_values_long) %>% 
  mutate(water.proximity = case_when(
    Name == "1318" ~ "<1km",
    Name == "1334" ~ "<1km",
    Name == "1344" ~ ">1km",
    Name == "1348" ~ "<.5km",
    Name == "1358" ~ "control",
    Name == "1362" ~ "<.5km",
    Name == "1378" ~ "<.5km",
    Name == "1562" ~ "control",
    Name == "1680" ~ "<.5km",
    Name == "1806" ~ "<1km",
    Name == "5822" ~ "<.5km",
    Name == "5838" ~ "<1km",
    Name == "9875" ~ "<.5km")) %>% mutate(date = as.Date(date))
combined_water_air_L_long <- rbind(compiled_purpleair_data_longer, all_L_cyano_values_long) %>% 
  mutate(water.proximity = case_when(
    Name == "1318" ~ "<1km",
    Name == "1334" ~ "<1km",
    Name == "1344" ~ ">1km",
    Name == "1348" ~ "<.5km",
    Name == "1358" ~ "control",
    Name == "1362" ~ "<.5km",
    Name == "1378" ~ "<.5km",
    Name == "1562" ~ "control",
    Name == "1680" ~ "<.5km",
    Name == "1806" ~ "<1km",
    Name == "5822" ~ "<.5km",
    Name == "5838" ~ "<1km",
    Name == "9875" ~ "<.5km")) %>% mutate(date = as.Date(date))

# wider for other comparisons / viz
combined_water_air_S_wide <- full_join(compiled_purpleair_data, all_S_cyano_values_wide, by = c("Name", "date")) %>% 
  mutate(water.proximity = case_when(
    Name == "1318" ~ "<1km",
    Name == "1334" ~ "<1km",
    Name == "1344" ~ ">1km",
    Name == "1348" ~ "<.5km",
    Name == "1358" ~ "control",
    Name == "1362" ~ "<.5km",
    Name == "1378" ~ "<.5km",
    Name == "1562" ~ "control",
    Name == "1680" ~ "<.5km",
    Name == "1806" ~ "<1km",
    Name == "5822" ~ "<.5km",
    Name == "5838" ~ "<1km",
    Name == "9875" ~ "<.5km"))
combined_water_air_M_wide <- full_join(compiled_purpleair_data, all_M_cyano_values_wide, by = c("Name", "date")) %>% 
  mutate(water.proximity = case_when(
    Name == "1318" ~ "<1km",
    Name == "1334" ~ "<1km",
    Name == "1344" ~ ">1km",
    Name == "1348" ~ "<.5km",
    Name == "1358" ~ "control",
    Name == "1362" ~ "<.5km",
    Name == "1378" ~ "<.5km",
    Name == "1562" ~ "control",
    Name == "1680" ~ "<.5km",
    Name == "1806" ~ "<1km",
    Name == "5822" ~ "<.5km",
    Name == "5838" ~ "<1km",
    Name == "9875" ~ "<.5km"))
combined_water_air_L_wide <- full_join(compiled_purpleair_data, all_L_cyano_values_wide, by = c("Name", "date"))%>% 
  mutate(water.proximity = case_when(
    Name == "1318" ~ "<1km",
    Name == "1334" ~ "<1km",
    Name == "1344" ~ ">1km",
    Name == "1348" ~ "<.5km",
    Name == "1358" ~ "control",
    Name == "1362" ~ "<.5km",
    Name == "1378" ~ "<.5km",
    Name == "1562" ~ "control",
    Name == "1680" ~ "<.5km",
    Name == "1806" ~ "<1km",
    Name == "5822" ~ "<.5km",
    Name == "5838" ~ "<1km",
    Name == "9875" ~ "<.5km"))

# Adding meteorological data to the dataframes ---------------------------------------------
weather.csv.folder <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\WeatherData"
met.files <- list.files(weather.csv.folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
# Loop through each file, read it, and store it in the list
for (file in met.files) {
  # Extract filename without extension
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the CSV file and store it as a dataframe with the filename as the dataframe name
  assign(file_name, read.csv(file))
  
  # Append the dataframe name to the list
  dataframes[[file_name]] <- get(file_name)
  
  print(names(dataframes))
}

# Assigning weather to specific sensors 
met.5838 <- Edenton %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.5838$Name <- "5838" 
met.1348 <- Edenton %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.1348$Name <- "1348" 
met.5822 <- `Arrowhead Beach` %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.5822$Name <- "5822" 
met.1334 <- `Arrowhead Beach` %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.1334$Name <- "1334" 
met.1344 <- `Elizabeth City` %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.1344$Name <- "1344" 
met.9875 <- `Elizabeth City` %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.9875$Name <- "9875" 
met.1318 <- `Elizabeth City` %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.1318$Name <- "1318" 
met.1806 <- `OBX` %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.1806$Name <- "1806" 
met.1362 <- `Nixonton` %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.1362$Name <- "1362" 
met.1378 <- `Hertford` %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.1378$Name <- "1378" 
met.1358 <- `Windsor` %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.1358$Name <- "1358"
met.1562 <- `Murfreesboro` %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.1562$Name <- "1562"
met.1680 <- `Edenhouse` %>% rename(Name = name, date = datetime) %>% mutate(Name = as.character(Name), date = as.Date(date))
met.1680$Name <- "1680"

# combining individual met data
all.met.data <- rbind(met.1334,met.1348,met.5822,met.5838,met.1344,met.9875,met.1318,met.1806,met.1362,met.1378,met.1358,met.1562, met.1680) %>% dplyr::select(Name, date, tempmax, temp, humidity, precip, windgust, windspeed, winddir, sealevelpressure, cloudcover, solarradiation, solarenergy)

# adding met data to air_water_dataframes
combined_water_air_met_S_wide <- full_join(combined_water_air_S_wide, all.met.data, by = c("Name", "date"))
combined_water_air_met_M_wide <- full_join(combined_water_air_M_wide, all.met.data, by = c("Name", "date"))
combined_water_air_met_L_wide <- full_join(combined_water_air_L_wide, all.met.data, by = c("Name", "date"))

# Reading in EPA PM data and Ozone data ------------------------------------------------------
EPA.csv.folder <- "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\OzoneData_EPA_PM"
EPA.files <- list.files(EPA.csv.folder, pattern = "\\.csv$", full.names = TRUE)
dataframes.1 <- list()
# Loop through each file, read it, and store it in the list
for (file in EPA.files) {
  # Extract filename without extension
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the CSV file and store it as a dataframe with the filename as the dataframe name
  assign(file_name, read.csv(file))
  
  # Append the dataframe name to the list
  dataframes.1[[file_name]] <- get(file_name)
}
print(names(dataframes.1))

# combining years 
Greenville_PM_all <- rbind(Greenville_PM_2022, Greenville_PM_2023)
Hampton_PM_all <- rbind(Hampton_PM_2022, Hampton_PM_2023)
RoanokeRapids_PM_all <- rbind(RoanokeRapids_PM_2022, RoanokeRapids_PM_2023)
Jamesville_O3_all <- rbind(Jamesville_Ozone_2022, Jamesville_Ozone_2023)
Suffolk_O3_all <- rbind(Suffolk_Ozone_2022, Suffolk_Ozone_2023)
IMPROVE_network_all <- rbind(IMPROVE_network_2022, IMPROVE_network_2023)

# cleaning the data and selecting important columns
Greenville_PM <- Greenville_PM_all %>% dplyr::select(`Parameter.Name`, `Duration.Description`, `Date..Local.`, `Arithmetic.Mean`) %>% filter(`Parameter.Name` == "PM2.5 - Local Conditions", `Duration.Description` == "24-HR BLK AVG") %>% mutate( `Parameter.Name` = as.character(`Parameter.Name`), `Date..Local.` = as.Date(`Date..Local.`, "%Y-%m-%d"), `Arithmetic.Mean` = as.numeric(`Arithmetic.Mean`)) %>% dplyr::select(-`Duration.Description`, -`Parameter.Name`) %>% rename(date = `Date..Local.`, `EPA_PM2.5` = `Arithmetic.Mean`) %>% distinct()

Hampton_PM <- Hampton_PM_all %>% dplyr::select(`Parameter.Name`, `Duration.Description`, `Date..Local.`, `Arithmetic.Mean`) %>% filter(`Parameter.Name` == "PM2.5 - Local Conditions", `Duration.Description` == "24-HR BLK AVG") %>% mutate( `Parameter.Name` = as.character(`Parameter.Name`), `Date..Local.` = as.Date(`Date..Local.`, "%Y-%m-%d"), `Arithmetic.Mean` = as.numeric(`Arithmetic.Mean`)) %>% dplyr::select(-`Duration.Description`, -`Parameter.Name`) %>% rename(date = `Date..Local.`, `EPA_PM2.5` = `Arithmetic.Mean`) %>% distinct()

RoanokeRapids_PM <- RoanokeRapids_PM_all %>% dplyr::select(`Parameter.Name`, `Duration.Description`, `Date..Local.`, `Arithmetic.Mean`) %>% filter(`Parameter.Name` == "PM2.5 - Local Conditions", `Duration.Description` == "24-HR BLK AVG") %>% mutate( `Parameter.Name` = as.character(`Parameter.Name`), `Date..Local.` = as.Date(`Date..Local.`, "%Y-%m-%d"), `Arithmetic.Mean` = as.numeric(`Arithmetic.Mean`)) %>% dplyr::select(-`Duration.Description`, -`Parameter.Name`) %>% rename(date = `Date..Local.`, `EPA_PM2.5` = `Arithmetic.Mean`) %>% distinct()

triangulated_EPA_PM <- rbind(Greenville_PM, Hampton_PM, RoanokeRapids_PM) %>%
  group_by(date) %>%
  dplyr::summarise(`EPA_PM2.5` = mean(`EPA_PM2.5`, na.rm=T))

Jamesville_O3 <- Jamesville_O3_all %>% dplyr::select(`Parameter.Name`, `Duration.Description`, `Date..Local.`, `Arithmetic.Mean`) %>% filter(`Parameter.Name` == "Ozone", `Duration.Description` == "8-HR RUN AVG BEGIN HOUR") %>% mutate( `Parameter.Name` = as.character(`Parameter.Name`), `Date..Local.` = as.Date(`Date..Local.`, "%Y-%m-%d"), `Arithmetic.Mean` = as.numeric(`Arithmetic.Mean`)) %>% dplyr::select(-`Duration.Description`, -`Parameter.Name`) %>% rename(date = `Date..Local.`, `Ozone` = `Arithmetic.Mean`) %>% distinct()

Suffolk_O3 <- Suffolk_O3_all %>% dplyr::select(`Parameter.Name`, `Duration.Description`, `Date..Local.`, `Arithmetic.Mean`) %>% filter(`Parameter.Name` == "Ozone", `Duration.Description` == "8-HR RUN AVG BEGIN HOUR") %>% mutate( `Parameter.Name` = as.character(`Parameter.Name`), `Date..Local.` = as.Date(`Date..Local.`, "%Y-%m-%d"), `Arithmetic.Mean` = as.numeric(`Arithmetic.Mean`)) %>% dplyr::select(-`Duration.Description`, -`Parameter.Name`) %>% rename(date = `Date..Local.`, `Ozone` = `Arithmetic.Mean`) %>% distinct()

est_chowan_albemarle_o3 <- rbind(Jamesville_O3, Suffolk_O3) %>%
  group_by(date) %>%
  dplyr::summarise(`Ozone` = mean(`Ozone`, na.rm=T))

# selecting other air quality pollutants that might tell us a bit about the source
IMPROVE_network <- IMPROVE_network_all %>% dplyr::select(`Parameter.Name`, Date, `Arithmetic.Mean`) %>% filter(`Parameter.Name` == "Sulfate PM2.5 LC" | `Parameter.Name` == "Total Nitrate PM2.5 LC" | `Parameter.Name` == "Nitrite PM2.5 LC" | `Parameter.Name` == "Phosphorus PM2.5 LC") %>% mutate( `Parameter.Name` = as.character(`Parameter.Name`), Date = as.Date(Date, "%m/%d/%Y"), `Arithmetic.Mean` = as.numeric(`Arithmetic.Mean`)) %>% pivot_wider(names_from = `Parameter.Name`, values_from = `Arithmetic.Mean`) %>% rename(SO4 = `Sulfate PM2.5 LC`, Total_NOx =  `Total Nitrate PM2.5 LC`, NO2 = `Nitrite PM2.5 LC`, PO4 = `Phosphorus PM2.5 LC`, date = Date)

CO_from_Hampton <- Hampton_PM_all %>% dplyr::select(`Parameter.Name`, `Duration.Description`, `Date..Local.`, `Arithmetic.Mean`) %>% filter(`Parameter.Name` == "Carbon monoxide", `Duration.Description` == "8-HR RUN AVG END HOUR") %>% mutate( `Parameter.Name` = as.character(`Parameter.Name`), `Date..Local.` = as.Date(`Date..Local.`, "%Y-%m-%d"), `Arithmetic.Mean` = as.numeric(`Arithmetic.Mean`)) %>% dplyr::select(-`Duration.Description`, -`Parameter.Name`) %>% rename(date = `Date..Local.`, `CO` = `Arithmetic.Mean`) %>% distinct()

CO_SO2_from_Hampton <- Hampton_PM_all %>% dplyr::select(`Parameter.Name`, `Date..Local.`, `Arithmetic.Mean`) %>% filter(`Parameter.Name` == "Carbon monoxide") %>% mutate( `Parameter.Name` = as.character(`Parameter.Name`), `Date..Local.` = as.Date(`Date..Local.`, "%m/%d/%Y"), `Arithmetic.Mean` = as.numeric(`Arithmetic.Mean`)) %>% pivot_wider(names_from = `Parameter.Name`, values_from = `Arithmetic.Mean`) %>% rename(CO = `Carbon monoxide`, date = `Date..Local.`) ## ENSURE DATES ARE READING IN CORRECTLY TOMORROW FOR ALL WEATHER DATA 

all.EPA.data <- full_join(est_chowan_albemarle_o3, triangulated_EPA_PM, by = "date")
all.EPA.data <- full_join(all.EPA.data, IMPROVE_network, by = "date")
all.EPA.data <- full_join(all.EPA.data, CO_from_Hampton, by = "date")

# adding EPA data to air_water_met_dataframes
combined_water_air_met_epa_S_daily <- full_join(combined_water_air_met_S_wide, all.EPA.data, by = c("date")) %>% filter(date > "2022-05-30")
combined_water_air_met_epa_M_daily <- full_join(combined_water_air_met_M_wide, all.EPA.data, by = c("date")) %>% filter(date > "2022-05-30")
combined_water_air_met_epa_L_daily <- full_join(combined_water_air_met_L_wide, all.EPA.data, by = c("date")) %>% filter(date > "2022-05-30")

# Collapsing into weekly averages for all environmental data ----------------------------------
library(lubridate)
calc_weekly_avgs <- function(df) {
  df %>% 
  mutate(week = floor_date(df$date, "week")) %>% 
  group_by(Name, week) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            across(where(is.character), first),
            across(where(is.Date), first)) %>% 
  drop_na(week, `pm2.5_CONUS`, avg_cyano_cell_count) 
  }

combined_water_air_met_epa_S_weekly <- calc_weekly_avgs(combined_water_air_met_epa_S_daily)
combined_water_air_met_epa_M_weekly <- calc_weekly_avgs(combined_water_air_met_epa_M_daily)
combined_water_air_met_epa_L_weekly <- calc_weekly_avgs(combined_water_air_met_epa_L_daily)

# Generating imputed values --------------------------------------------
library(zoo)
# this function works if you separate out the specific sensor individually, which is what I ended up doing for my California project
impute_all_columns_single_sensor <- function(data) {
  # Find the index of the "date" column
  date_col_index <- which(names(data) == "date")
  
  # List to store imputed dataframes for each column
  imputed_data_list <- list()
  
  # Group the data by Name
  grouped_data <- data #%>% group_by(Name)
  
  # Iterate through each group
  imputed_data <- grouped_data %>%
    group_modify(~ {
      # Get the group's data
      group_data <- .x
      
      # Create a full sequence of dates
      full_dates <- seq(min(group_data$date), max(group_data$date), by = "day")
      
      # Create a full dataframe with all dates
      full_group_data <- data.frame(date = full_dates)
      
      # Merge with the original data to ensure all dates are included
      merged_data <- merge(full_group_data, group_data, by = "date", all.x = TRUE)
      
      # Iterate through columns following "date"
      for (col_index in (date_col_index + 1):ncol(data)) {
        col_name <- names(data)[col_index]
        
        # Ensure the column name is not NA
        if (!is.na(col_name)) {
          # Convert columns into vectors
          values <- merged_data[[col_name]]
          index <- merged_data$date
          
          # Check if there are at least two non-NA values for interpolation
          if (sum(!is.na(values)) >= 2) {
            # Convert the column to a zoo time-series object
            ts_data <- zoo(values, index)
            
            # Perform linear interpolation for imputation
            imputed_data <- try(na.approx(ts_data, xout = full_dates, na.rm = FALSE), silent=TRUE)
            
            # Check if interpolation was successful
            if (inherits(imputed_data, "try-error")) {
              message(paste("Skipping column", col_name, "in group", unique(group_data$Name)))
            } else {
              # Assign the imputed values back to the group's data
              merged_data[[col_name]] <- coredata(imputed_data)
            }
          } else {
            message(paste("Skipping column", col_name, "in group", unique(group_data$Name), "due to insufficient data"))
          }
        }
      }
      
      # Remove duplicates based on Date
      merged_data <- merged_data %>%
        distinct(date, .keep_all = TRUE)
      
      # Update the group data with imputed values
      group_data <- merged_data[merged_data$date %in% group_data$date, ]
      
      return(group_data)
    })
  
  return(ungroup(imputed_data))
}

# Usage of Function -- Imputations appear to be consistent between S M and L so not randomized each time which is good 
# only was able to get this to work for individual sensors to ensure values are based on specific measurements by those sensors
imputs.1318_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "1318")
imputs.1318_S <- impute_all_columns_single_sensor(imputs.1318_S)
imputs.1318_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "1318") 
imputs.1318_M <- impute_all_columns_single_sensor(imputs.1318_M)
imputs.1318_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "1318") 
imputs.1318_L <- impute_all_columns_single_sensor(imputs.1318_L)

imputs.1344_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "1344") 
imputs.1344_S <- impute_all_columns_single_sensor(imputs.1344_S)
imputs.1344_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "1344") 
imputs.1344_M <- impute_all_columns_single_sensor(imputs.1344_M)
imputs.1344_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "1344") 
imputs.1344_L <- impute_all_columns_single_sensor(imputs.1344_L)

imputs.1348_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "1348") 
imputs.1348_S <- impute_all_columns_single_sensor(imputs.1348_S)
imputs.1348_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "1348") 
imputs.1348_M <- impute_all_columns_single_sensor(imputs.1348_M)
imputs.1348_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "1348") 
imputs.1348_L <- impute_all_columns_single_sensor(imputs.1348_L)

imputs.1358_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "1358") 
imputs.1358_S <- impute_all_columns_single_sensor(imputs.1358_S)
imputs.1358_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "1358") 
imputs.1358_M <- impute_all_columns_single_sensor(imputs.1358_M)
imputs.1358_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "1358") 
imputs.1358_L <- impute_all_columns_single_sensor(imputs.1358_L)

imputs.1362_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "1362") 
imputs.1362_S <- impute_all_columns_single_sensor(imputs.1362_S)
imputs.1362_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "1362") 
imputs.1362_M <- impute_all_columns_single_sensor(imputs.1362_M)
imputs.1362_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "1362") 
imputs.1362_L <- impute_all_columns_single_sensor(imputs.1362_L)

imputs.1378_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "1378") 
imputs.1378_S <- impute_all_columns_single_sensor(imputs.1378_S)
imputs.1378_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "1378") 
imputs.1378_M <- impute_all_columns_single_sensor(imputs.1378_M)
imputs.1378_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "1378") 
imputs.1378_L <- impute_all_columns_single_sensor(imputs.1378_L)

imputs.1680_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "1680") 
imputs.1680_S <- impute_all_columns_single_sensor(imputs.1680_S)
imputs.1680_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "1680") 
imputs.1680_M <- impute_all_columns_single_sensor(imputs.1680_M)
imputs.1680_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "1680") 
imputs.1680_L <- impute_all_columns_single_sensor(imputs.1680_L)

imputs.1806_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "1806") 
imputs.1806_S <- impute_all_columns_single_sensor(imputs.1806_S)
imputs.1806_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "1806") 
imputs.1806_M <- impute_all_columns_single_sensor(imputs.1806_M)
imputs.1806_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "1806") 
imputs.1806_L <- impute_all_columns_single_sensor(imputs.1806_L)

imputs.5822_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "5822") 
imputs.5822_S <- impute_all_columns_single_sensor(imputs.5822_S)
imputs.5822_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "5822") 
imputs.5822_M <- impute_all_columns_single_sensor(imputs.5822_M)
imputs.5822_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "5822") 
imputs.5822_L <- impute_all_columns_single_sensor(imputs.5822_L)

imputs.5838_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "5838") 
imputs.5838_S <- impute_all_columns_single_sensor(imputs.5838_S)
imputs.5838_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "5838") 
imputs.5838_M <- impute_all_columns_single_sensor(imputs.5838_M)
imputs.5838_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "5838") 
imputs.5838_L <- impute_all_columns_single_sensor(imputs.5838_L)

imputs.9875_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "9875") 
imputs.9875_S <- impute_all_columns_single_sensor(imputs.9875_S)
imputs.9875_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "9875") 
imputs.9875_M <- impute_all_columns_single_sensor(imputs.9875_M)
imputs.9875_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "9875") 
imputs.9875_L <- impute_all_columns_single_sensor(imputs.9875_L)

imputs.1334_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "1334") 
imputs.1334_S <- impute_all_columns_single_sensor(imputs.1334_S)
imputs.1334_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "1334") 
imputs.1334_M <- impute_all_columns_single_sensor(imputs.1334_M)
imputs.1334_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "1334") 
imputs.1334_L <- impute_all_columns_single_sensor(imputs.1334_L)

imputs.1562_S <- combined_water_air_met_epa_S_daily %>% filter(Name == "1562") 
imputs.1562_S <- impute_all_columns_single_sensor(imputs.1562_S)
imputs.1562_M <- combined_water_air_met_epa_M_daily %>% filter(Name == "1562") 
imputs.1562_M <- impute_all_columns_single_sensor(imputs.1562_M)
imputs.1562_L <- combined_water_air_met_epa_L_daily %>% filter(Name == "1562") 
imputs.1562_L <- impute_all_columns_single_sensor(imputs.1562_L)

combined_water_air_met_epa_S_imputations <- rbind(imputs.1318_S, imputs.1334_S, imputs.1344_S, imputs.1348_S, imputs.1358_S, imputs.1362_S, imputs.1378_S, imputs.1562_S, imputs.1680_S, imputs.1806_S, imputs.5822_S, imputs.5838_S, imputs.9875_S) %>% drop_na(`pm2.5_CONUS`, `pm2.5_SE`, avg_cyano_cell_count) 
combined_water_air_met_epa_M_imputations <- rbind(imputs.1318_M, imputs.1334_M, imputs.1344_M, imputs.1348_M, imputs.1358_M, imputs.1362_M, imputs.1378_M, imputs.1562_M, imputs.1680_M, imputs.1806_M, imputs.5822_M, imputs.5838_M, imputs.9875_M) %>% drop_na(`pm2.5_CONUS`, `pm2.5_SE`, avg_cyano_cell_count) 
combined_water_air_met_epa_L_imputations <- rbind(imputs.1318_L, imputs.1334_L, imputs.1344_L, imputs.1348_L, imputs.1358_L, imputs.1362_L, imputs.1378_L, imputs.1562_L, imputs.1680_L, imputs.1806_L, imputs.5822_L, imputs.5838_L, imputs.9875_L) %>% drop_na(`pm2.5_CONUS`, `pm2.5_SE`, avg_cyano_cell_count) 

# combining S, M, and L cyanobacterial coverage spatial areas ---------------------------
rename.cols.fxn.L <- function(dataframe.name) {
  dataframe.name <- dataframe.name %>% 
    rename(avg_cyano_cell_count_L = avg_cyano_cell_count, 
           median_cyano_cell_count_L = median_cyano_cell_count, 
           max_cyano_cell_count_L = max_cyano_cell_count,
           avg_chlorophyll_L = avg_chlorophyll, 
           median_chlorophyll_L = median_chlorophyll, 
           max_chlorophyll_L = max_chlorophyll,
           percent_bloom_L = percent_bloom,
           surface_area_bloom_L = surface_area_bloom)
}
combined_water_air_met_epa_L_daily <- rename.cols.fxn.L(combined_water_air_met_epa_L_daily)
combined_water_air_met_epa_L_weekly <- rename.cols.fxn.L(combined_water_air_met_epa_L_weekly)
combined_water_air_met_epa_L_imputations <- rename.cols.fxn.L(combined_water_air_met_epa_L_imputations)

rename.cols.fxn.M <- function(dataframe.name) {
  dataframe.name <- dataframe.name %>% 
    rename(avg_cyano_cell_count_M = avg_cyano_cell_count, 
           median_cyano_cell_count_M = median_cyano_cell_count, 
           max_cyano_cell_count_M = max_cyano_cell_count,
           avg_chlorophyll_M = avg_chlorophyll, 
           median_chlorophyll_M = median_chlorophyll, 
           max_chlorophyll_M = max_chlorophyll,
           percent_bloom_M = percent_bloom,
           surface_area_bloom_M = surface_area_bloom)
}
combined_water_air_met_epa_M_daily <- rename.cols.fxn.M(combined_water_air_met_epa_M_daily)
combined_water_air_met_epa_M_weekly <- rename.cols.fxn.M(combined_water_air_met_epa_M_weekly)
combined_water_air_met_epa_M_imputations <- rename.cols.fxn.M(combined_water_air_met_epa_M_imputations)

rename.cols.fxn.S <- function(dataframe.name) {
  dataframe.name <- dataframe.name %>% 
    rename(avg_cyano_cell_count_S = avg_cyano_cell_count, 
           median_cyano_cell_count_S = median_cyano_cell_count, 
           max_cyano_cell_count_S = max_cyano_cell_count,
           avg_chlorophyll_S = avg_chlorophyll, 
           median_chlorophyll_S = median_chlorophyll, 
           max_chlorophyll_S = max_chlorophyll,
           percent_bloom_S = percent_bloom,
           surface_area_bloom_S = surface_area_bloom)
}
combined_water_air_met_epa_S_daily <- rename.cols.fxn.S(combined_water_air_met_epa_S_daily)
combined_water_air_met_epa_S_weekly <- rename.cols.fxn.S(combined_water_air_met_epa_S_weekly)
combined_water_air_met_epa_S_imputations <- rename.cols.fxn.S(combined_water_air_met_epa_S_imputations)

daily_cyano_S_selections <- combined_water_air_met_epa_S_daily %>% 
  dplyr::select(date, Name, avg_cyano_cell_count_S, median_cyano_cell_count_S, max_cyano_cell_count_S,   avg_chlorophyll_S, median_chlorophyll_S, max_chlorophyll_S, percent_bloom_S, surface_area_bloom_S)
daily_cyano_M_selections <- combined_water_air_met_epa_M_daily %>% 
  dplyr::select(date, Name, avg_cyano_cell_count_M, median_cyano_cell_count_M, max_cyano_cell_count_M,   avg_chlorophyll_M, median_chlorophyll_M, max_chlorophyll_M, percent_bloom_M, surface_area_bloom_M)
combo <- full_join(daily_cyano_S_selections, daily_cyano_M_selections, by = c("date", "Name"))
COMPLETE.daily.data <- full_join(combined_water_air_met_epa_L_daily, combo, by = c("date", "Name"))

weekly_cyano_S_selections <- combined_water_air_met_epa_S_weekly %>% 
  dplyr::select(date, Name, avg_cyano_cell_count_S, median_cyano_cell_count_S, max_cyano_cell_count_S,   avg_chlorophyll_S, median_chlorophyll_S, max_chlorophyll_S, percent_bloom_S, surface_area_bloom_S)
weekly_cyano_M_selections <- combined_water_air_met_epa_M_weekly %>% 
  dplyr::select(date, Name, avg_cyano_cell_count_M, median_cyano_cell_count_M, max_cyano_cell_count_M,   avg_chlorophyll_M, median_chlorophyll_M, max_chlorophyll_M, percent_bloom_M, surface_area_bloom_M)
combo <- full_join(weekly_cyano_S_selections, weekly_cyano_M_selections, by = c("date", "Name"))
COMPLETE.weekly.data <- full_join(combined_water_air_met_epa_L_weekly, combo, by = c("date", "Name"))

imputations_cyano_S_selections <- combined_water_air_met_epa_S_imputations %>% 
  dplyr::select(date, Name, avg_cyano_cell_count_S, median_cyano_cell_count_S, max_cyano_cell_count_S,   avg_chlorophyll_S, median_chlorophyll_S, max_chlorophyll_S, percent_bloom_S, surface_area_bloom_S)
imputations_cyano_M_selections <- combined_water_air_met_epa_M_imputations %>% 
  dplyr::select(date, Name, avg_cyano_cell_count_M, median_cyano_cell_count_M, max_cyano_cell_count_M,   avg_chlorophyll_M, median_chlorophyll_M, max_chlorophyll_M, percent_bloom_M, surface_area_bloom_M)
combo <- full_join(imputations_cyano_S_selections, imputations_cyano_M_selections, by = c("date", "Name"))
COMPLETE.imputations.data <- full_join(combined_water_air_met_epa_L_imputations, combo, by = c("date", "Name"))

# Reading out complete cleaned datasets for analyses --------------------
write.csv(COMPLETE.daily.data, 'C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\COMPLETE.daily.data.csv')
write.csv(COMPLETE.weekly.data, 'C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\COMPLETE.weekly.data.csv')
write.csv(COMPLETE.imputations.data, 'C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\COMPLETE.imputations.data.csv')



# Time Series of PM with air pollutant tracers --------------------------------------------------------
ggplot(combined_water_air_met_epa_S_daily) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = CO * 50, fill = "black")) +
  scale_y_continuous(name = "PM2.5 CONUS", sec.axis = sec_axis(~./50, name = "EPA Carbon Monoxide")) +
  labs(x = "Date") +
  theme_minimal()

ggplot(combined_water_air_met_epa_S_daily) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = Ozone * 500), color = "red") +
  # geom_point(aes(x = date, y = SO4 * 10 ),color = "darkgray", shape = 3, size =3) +
  scale_y_continuous(name = "PM2.5 CONUS", sec.axis = sec_axis(~./500, name = "EPA Ozone")) +
  labs(x = "Date") +
  theme_minimal()

ggplot(combined_water_air_met_epa_S_daily) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_point(aes(x = date, y = SO4 * 10 ),color = "black", shape = 2, size =2) +
  scale_y_continuous(name = "PM2.5 CONUS", sec.axis = sec_axis(~./10, name = "EPA SO4")) +
  labs(x = "Date") +
  theme_minimal()

ggplot(combined_water_air_met_epa_S_daily) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_point(aes(x = date, y = PO4 * 5000 ),color = "black", shape = 3, size =2) +
  scale_y_continuous(name = "PM2.5 CONUS", sec.axis = sec_axis(~./5000, name = "EPA PO4")) +
  labs(x = "Date") +
  theme_minimal()

ggplot(combined_water_air_met_epa_S_daily) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_point(aes(x = date, y = Total_NOx * 50 ),color = "black", shape = 4, size =2) +
  scale_y_continuous(name = "PM2.5 CONUS", sec.axis = sec_axis(~./50, name = "EPA NOx")) +
  labs(x = "Date") +
  theme_minimal() +
  ylim(0,50)

ggplot(combined_water_air_met_epa_S_daily) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = CO * 50, fill = "black")) +
  geom_line(aes(x = date, y = Ozone * 500), color = "red") +
  geom_point(aes(x = date, y = Total_NOx * 50 ), color = "darkgray", shape = 5, size = 1) +
  geom_point(aes(x = date, y = PO4 * 50000), color = "darkgray", shape = 4, size = 1) +
  geom_point(aes(x = date, y = SO4 * 50 ), color = "darkgray", shape = 3, size = 1) +
  # scale_y_continuous(name = "PM2.5 CONUS", sec.axis = sec_axis(~./50, name = " EPA")) +
  labs(x = "Date") +
  theme_minimal() +
  ylim(0,50)

# Comparing EPA data to purpleair data -------------------------------
# making dfs for this comparison
not.RH.corr.pm <- full_join(all.EPA.data, compiled_purpleair_data_NOT_RH_CORR, by = "date") %>% drop_na()
RH.corr.pm <- full_join(all.EPA.data, compiled_purpleair_data, by = "date") #%>% drop_na(pm2.5_avg)

# regression and change fill to sensor ID 
pm.comparison.RH.corr <- ggplot(RH.corr.pm) + 
  geom_point(aes(y = `EPA_PM2.5`, x = pm2.5_CONUS, color = Name)) + 
  geom_smooth(aes(y = `EPA_PM2.5`, x = pm2.5_CONUS), method='lm')+
  xlim(0,40) + 
  ylim(0,40) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1) +
  labs(title = "CONUS corrected by RH")
  
pm.comparison.NOT.RH.corr <- ggplot(not.RH.corr.pm) + 
  geom_point(aes(y = `EPA_PM2.5`, x = pm2.5, color = Name)) + 
  geom_smooth(aes(y = `EPA_PM2.5`, x = pm2.5), method='lm')+
  xlim(0,40) + 
  ylim(0,40) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1) +
  labs(title = "not corrected by RH")

pm.comparison.SE.RH.corr <- ggplot(RH.corr.pm) + 
  geom_point(aes(y = `EPA_PM2.5`, x = pm2.5_SE, color = Name)) + 
  geom_smooth(aes(y = `EPA_PM2.5`, x = pm2.5_SE), method='lm')+
  xlim(0,40) + 
  ylim(0,40) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1) +
  labs(title = "SE corrected by RH")

library(patchwork);library(purrr)
pm.comparison.SE.RH.corr + pm.comparison.RH.corr + pm.comparison.NOT.RH.corr  

# single points for each sensor to see how each is performing --------------------------------------
sensor_count <- RH.corr.pm %>% group_by(Name) %>% summarize(count = n()) 

RH.corr.pm.single.point <- RH.corr.pm %>% 
  dplyr::select(Name, `pm2.5_CONUS`, `EPA_PM2.5`) %>%
  group_by(Name) %>%
  mutate(mean_EPA = mean(`EPA_PM2.5`), sd_EPA = sd(`EPA_PM2.5`), mean_purp = mean(`pm2.5_CONUS`), sd_purp = sd(`pm2.5_CONUS`)) %>%
  dplyr::select(-`EPA_PM2.5`, -`pm2.5_CONUS`) %>%
  mutate(xmin = (mean_purp-sd_purp), xmax = (mean_purp+sd_purp), ymin = (mean_EPA-sd_EPA), ymax = (mean_EPA+sd_EPA)) %>% 
  unique() %>% 
  ungroup() %>% 
  full_join(sensor_count) %>% 
  mutate(count = as.numeric(count)) %>% drop_na()

SE.RH.corr.pm.single.point <- RH.corr.pm %>% 
  dplyr::select(Name, `pm2.5_SE`, `EPA_PM2.5`) %>%
  group_by(Name) %>%
  mutate(mean_EPA = mean(`EPA_PM2.5`), sd_EPA = sd(`EPA_PM2.5`), mean_purp = mean(`pm2.5_SE`), sd_purp = sd(`pm2.5_SE`)) %>%
  dplyr::select(-`EPA_PM2.5`, -`pm2.5_SE`) %>%
  mutate(xmin = (mean_purp-sd_purp), xmax = (mean_purp+sd_purp), ymin = (mean_EPA-sd_EPA), ymax = (mean_EPA+sd_EPA)) %>% 
  unique() %>% 
  ungroup() %>% 
  full_join(sensor_count) %>% 
  mutate(count = as.numeric(count)) %>% drop_na()

not.RH.corr.pm.single.point <- not.RH.corr.pm %>% 
  dplyr::select(Name, `pm2.5`, `EPA_PM2.5`) %>%
  group_by(Name) %>%
  mutate(mean_EPA = mean(`EPA_PM2.5`), sd_EPA = sd(`EPA_PM2.5`), mean_purp = mean(`pm2.5`), sd_purp = sd(`pm2.5`)) %>%
  dplyr::select(-`EPA_PM2.5`, -`pm2.5`) %>%
  mutate(xmin = (mean_purp-sd_purp), xmax = (mean_purp+sd_purp), ymin = (mean_EPA-sd_EPA), ymax = (mean_EPA+sd_EPA)) %>% 
  unique() %>% 
  ungroup() %>% 
  full_join(sensor_count) %>% 
  mutate(count = as.numeric(count)) %>% drop_na()

RH.corr.fig <- ggplot(RH.corr.pm.single.point, aes()) +
    geom_errorbar(aes(x = mean_purp, ymin = ymin, ymax = ymax), width = 0.1, color = "gray") +
    geom_errorbarh(aes(y = mean_EPA, xmin = xmin, xmax = xmax), height = 0.1, color = "gray") +
    geom_point(aes(x = mean_purp, y = mean_EPA, color = Name, size = count)) +
    #xlim(0, 12) + 
    #ylim(0, 12) + 
    scale_size(range = c(2, 8)) + 
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1) +
    theme_minimal() + 
    labs(title = "CONUS Humidity Corrected PurpleAir PM2.5") + 
    theme(legend.position = "none")

SE.RH.corr.fig <- ggplot(SE.RH.corr.pm.single.point, aes()) +
  geom_errorbar(aes(x = mean_purp, ymin = ymin, ymax = ymax), width = 0.1, color = "gray") +
  geom_errorbarh(aes(y = mean_EPA, xmin = xmin, xmax = xmax), height = 0.1, color = "gray") +
  geom_point(aes(x = mean_purp, y = mean_EPA, color = Name, size = count)) +
  #xlim(0, 12) + 
  #ylim(0, 12) + 
  scale_size(range = c(2, 8)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1) +
  theme_minimal() + 
  labs(title = "SE Humidity Corrected PurpleAir PM2.5") + 
  theme(legend.position = "none")

not.RH.corr.fig <- ggplot(not.RH.corr.pm.single.point, aes()) +
  geom_errorbar(aes(x = mean_purp, ymin = ymin, ymax = ymax), width = 0.1, color = "gray") +
  geom_errorbarh(aes(y = mean_EPA, xmin = xmin, xmax = xmax), height = 0.1, color = "gray") +
  geom_point(aes(x = mean_purp, y = mean_EPA, color = Name, size = count)) +
  #xlim(0, 15) + 
 # ylim(0, 15) + 
  scale_size(range = c(2, 8)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1) +
  theme_minimal() + 
  labs(title = "Non-RH Corrected PurpleAir PM2.5")

SE.RH.corr.fig + RH.corr.fig + not.RH.corr.fig

# Wind Roses to examine general wind direction for each sensor location --------------------------------------
install.packages("openair")
library(openair)
windRose(combined_water_air_met_epa_S_weekly, ws="windspeed", wd="winddir", ws.int = 3, type= "Name", cols = "heat", paddle = FALSE, 
         key = list(plot.style = c("ticks", "border"),
                    fit = "all", height = 0.5,
                    space = "bottom"))
windRose(weekly_avg_met, ws="windspeed", wd="winddir", ws.int = 3, type= "site", cols = "heat", paddle = FALSE)

# Simple regression examining PM2.5 as a function of cyano cell counts -----------------------------------------
# cyano cell counts
ggplot(combined_water_air_met_epa_S_weekly) +
  geom_point(aes(x = `pm2.5_CONUS`, y = `avg_cyano_cell_count`, color = Name)) +
  ylim(0,20000) + 
  xlim(0,10) +
  facet_wrap(~Name) + 
  geom_smooth(aes(x = `pm2.5_CONUS`, y = `avg_cyano_cell_count`, color = Name),  method = "lm", se = TRUE)

ggplot(combined_water_air_met_epa_M_weekly) +
  geom_point(aes(x = `pm2.5_CONUS`, y = `avg_cyano_cell_count`, color = Name)) +
  ylim(0,20000) + 
  xlim(0,10) +
  facet_wrap(~Name) + 
  geom_smooth(aes(x = `pm2.5_CONUS`, y = `avg_cyano_cell_count`, color = Name),  method = "lm", se = TRUE)

ggplot(combined_water_air_met_epa_L_weekly) +
  geom_point(aes(x = `pm2.5_CONUS`, y = `avg_cyano_cell_count`, color = Name)) +
  ylim(0,20000) + 
  xlim(0,10) +
  facet_wrap(~Name) + 
  geom_smooth(aes(x = `pm2.5_CONUS`, y = `avg_cyano_cell_count`, color = Name),  method = "lm", se = TRUE)

# percent coverage of bloom
ggplot(combined_water_air_met_epa_S_weekly) +
  geom_point(aes(x = `pm2.5_CONUS`, y = `percent_bloom`, color = Name)) +
  ylim(0,.05) + 
  xlim(0,10) +
  facet_wrap(~Name) + 
  geom_smooth(aes(x = `pm2.5_CONUS`, y = `percent_bloom`, color = Name),  method = "lm", se = TRUE)

ggplot(combined_water_air_met_epa_M_weekly) +
  geom_point(aes(x = `pm2.5_CONUS`, y = `percent_bloom`, color = Name)) +
  ylim(0,.05) + 
  xlim(0,10) +
  facet_wrap(~Name) + 
  geom_smooth(aes(x = `pm2.5_CONUS`, y = `percent_bloom`, color = Name),  method = "lm", se = TRUE)

ggplot(combined_water_air_met_epa_L_weekly) +
  geom_point(aes(x = `pm2.5_CONUS`, y = `percent_bloom`, color = Name)) +
  ylim(0,.05) + 
  xlim(0,10) +
  facet_wrap(~Name) + 
  geom_smooth(aes(x = `pm2.5_CONUS`, y = `percent_bloom`, color = Name),  method = "lm", se = TRUE)

# Principal Component analyses --------------------------------------
library(corrr);library(ggcorrplot);library(factoextra);library(FactoMineR)
pca.1 <- combined_water_air_met_epa_M_imputations %>% select(-where(~ all(. == 0 | is.na(.)))) %>% select("pm2.5", "avg_cyano_cell_count", "max_cyano_cell_count", "percent_bloom", "temp", "humidity", "windspeed", "winddir", "solarradiation", "Ozone") %>% drop_na()
normalized.pca.data <- scale(pca.1)
pca.corr.matrix <- cor(normalized.pca.data)
pca.matrix.df <- as.data.frame(pca.corr.matrix)
ggcorrplot(pca.corr.matrix)
data.pca <- princomp(pca.corr.matrix)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = T)
fviz_pca_biplot(data.pca, col.var = "cos2", repel = TRUE, labelsize = 5 , pointsize = 2, font.family = "Arial", arrowsize = 1.25)

# Playing around with plots ------------------------------------------------------------------
# color palettes and other theme specifications
location.color.palette <- c("1318" = "deepskyblue",
                            "3_1318" = "deepskyblue",
                            "1334" = "deepskyblue",
                            "1344" = "cadetblue2",
                            "3_1344" = "cadetblue2",
                            "1_1334" = "deepskyblue",
                            "1348" = "blue",
                            "2_1348" = "blue",
                            "1358" = "black",
                            "1362" = "blue",
                            "1378" = "blue",
                            "1680" = "blue",
                            "1806" = "blue",
                            "1_5822" = "blue",
                            "5822" = "blue",
                            "5838" = "deepskyblue",
                            "2_5838" = "deepskyblue",
                            "9875" = "blue",
                            "3_9875" = "blue")

proximity.color.palette <- c("<1km" = "deepskyblue",
                             ">1km"= "cadetblue2",
                             "<.5km" = "blue",
                             "control" = "black")

# selecting data of interest
pm2.5df <- combined_water_air_L_long %>% 
           filter(variable == "pm2.5") %>% #selecting variable of interest for plot
           filter(#date > "2023-01-01" & 
                    date <= "2023-06-01" | 
                    date >= "2023-06-14") %>% 
           na.omit()

pm2.5df.2023 <- combined_water_air_L_long %>% 
  filter(variable == "pm2.5") %>% #selecting variable of interest for plot
  filter(date > "2023-01-01" & 
    date <= "2023-06-01" | 
      date >= "2023-06-14") %>% 
  na.omit()

pm2.5df.2022 <- combined_water_air_L_long %>% 
  filter(variable == "pm2.5") %>% #selecting variable of interest for plot
  filter(date < "2023-01-01" & 
           date <= "2023-06-01" | 
           date >= "2023-06-14") %>% 
  na.omit()

# Compiled years ----------------------------------------------------------------------------
pm2.5df.ordered <- pm2.5df %>% mutate(Name = recode(Name,
                                                    "5822" = "1_5822",
                                                    "1334" = "1_1334",
                                                    "1348" = "2_1348", 
                                                    "5838" = "2_5838",
                                                    "1344" = "3_1344",
                                                    "9875" = "3_9875",
                                                    "1318" = "3_1318"))
# stats test to go with boxplot data
library(multcompView)
anova.PM2.5.sensor <- aov(value ~ Name, data = pm2.5df.ordered)
summary(anova.PM2.5.sensor)

tukey.sensor <- TukeyHSD(anova.PM2.5.sensor)
print(tukey.sensor)

cld.sensor <- multcompLetters4(anova.PM2.5.sensor, tukey.sensor)
print(cld.sensor)

tk.sensor <- group_by(pm2.5df.ordered, Name) %>% 
      summarise(mean = mean(value), quant = quantile(value, probs = 0.75)) %>% 
      arrange(desc(mean))
cld.sensor <- as.data.frame.list(cld.sensor$Name)


  
# plots 
ggplot(pm2.5df.ordered, aes(x = Name, y = value, fill = Name)) +
  geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.1) + 
  geom_boxplot(alpha = 0.5) +
  labs(x = "Sensor ID") +
  ylab(expression(bold(PM[2.5]~mass~concentration~(µg~m^bold("-3"))))) +
  ggtitle("PM2.5 Mass Grouped by Sensor") +
  scale_fill_manual(values = location.color.palette) +
  scale_color_manual(values = location.color.palette) +
  theme(axis.text.x = element_text(size=15, face="bold", color = "black"),
        axis.text.y = element_text(size=15, face="bold", color = "black"),
        axis.title.x = element_text(size=15, face="bold", color = "black"),  
        axis.title.y = element_text(size=15, face="bold", color = "black"),
        plot.title = element_text(size=20, face="bold", color = "black", hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = 'gray', linetype = "dashed"), 
        panel.grid.major.x = element_blank()) +
  ylim(0,20) +
  geom_text(data = tk.sensor, aes(x = Name, y = quant, label = cld.sensor$Letters), size = 6, vjust=-1, hjust = -0.2 ,face = "bold")

# grouped by water proximity
anova.PM2.5.prox <- aov(value ~ water.proximity, data = pm2.5df)
summary(anova.PM2.5.prox)

tukey.prox <- TukeyHSD(anova.PM2.5.prox)
print(tukey.prox)

cld.prox <- multcompLetters4(anova.PM2.5.prox, tukey.prox)
print(cld.prox)

tk.prox <- group_by(pm2.5df, water.proximity) %>% 
  summarise(mean = mean(value), quant = quantile(value, probs = 0.75)) %>% 
  arrange(desc(mean))
cld.prox <- as.data.frame.list(cld.prox$water.proximity)

# plots 
ggplot(pm2.5df, aes(x = water.proximity, y = value, fill = water.proximity)) +
  #geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.25) + 
  geom_boxplot(alpha = 0.5) +
  labs(x = "Proximity to Waterfront") +
  ylab(expression(bold(PM[2.5]~mass~concentration~(µg~m^bold("-3"))))) +
  ggtitle("Annual PM2.5 Mass May-August 2022 and 2023") +
  scale_fill_manual(values = proximity.color.palette) +
  scale_color_manual(values = proximity.color.palette) +
  theme(axis.text.x = element_text(size=15, face="bold", color = "black"),
        axis.text.y = element_text(size=15, face="bold", color = "black"),
        axis.title.x = element_text(size=15, face="bold", color = "black"),  
        axis.title.y = element_text(size=15, face="bold", color = "black"),
        plot.title = element_text(size=20, face="bold", color = "black", hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = 'gray', linetype = "dashed"), 
        panel.grid.major.x = element_blank()) +
  ylim(0,20) +
  geom_text(data = tk.prox, aes(x = water.proximity, y = quant, label = cld.prox$Letters), size = 5, vjust=-1, hjust = -0.2)

# 2022 DATA ONLY --------------------------------------------------------------------------------
# stats test to go with boxplot data
library(multcompView)
anova.PM2.5.sensor.22 <- aov(value ~ Name, data = pm2.5df.2022)
summary(anova.PM2.5.sensor.22)

tukey.sensor.22 <- TukeyHSD(anova.PM2.5.sensor.22)
print(tukey.sensor.22)

cld.sensor.22 <- multcompLetters4(anova.PM2.5.sensor.22, tukey.sensor.22)
print(cld.sensor.22)

tk.sensor.22 <- group_by(pm2.5df.2022, Name) %>% 
      summarise(mean = mean(value), quant = quantile(value, probs = 0.75)) %>% 
      arrange(desc(mean))
cld.sensor.22 <- as.data.frame.list(cld.sensor.22$Name)

# plots 
ggplot(pm2.5df.2022, aes(x = Name, y = value, fill = Name)) +
  #geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.25) + 
  geom_boxplot(alpha = 0.5) +
  labs(x = "Sensor ID") +
  ylab(expression(bold(PM[2.5]~mass~concentration~(µg~m^bold("-3"))))) +
  ggtitle("PM2.5 Mass May-August 2023") +
  scale_fill_manual(values = location.color.palette) +
  scale_color_manual(values = location.color.palette) +
  theme(axis.text.x = element_text(size=15, face="bold", color = "black"),
        axis.text.y = element_text(size=15, face="bold", color = "black"),
        axis.title.x = element_text(size=15, face="bold", color = "black"),  
        axis.title.y = element_text(size=15, face="bold", color = "black"),
        plot.title = element_text(size=20, face="bold", color = "black", hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = 'gray', linetype = "dashed"), 
        panel.grid.major.x = element_blank()) +
  ylim(0,20) +
  geom_text(data = tk.sensor.22, aes(x = Name, y = quant, label = cld.sensor.22$Letters), size = 3, vjust=-1, hjust = -0.2)

# grouped by water proximity
anova.PM2.5.prox.22 <- aov(value ~ water.proximity, data = pm2.5df.2022)
summary(anova.PM2.5.prox.22)

tukey.prox.22 <- TukeyHSD(anova.PM2.5.prox.22)
print(tukey.prox.22)

cld.prox.22 <- multcompLetters4(anova.PM2.5.prox.22, tukey.prox.22)
print(cld.prox.22)

tk.prox.22 <- group_by(pm2.5df.2022, water.proximity) %>% 
  summarise(mean = mean(value), quant = quantile(value, probs = 0.75)) %>% 
  arrange(desc(mean))
cld.prox.22 <- as.data.frame.list(cld.prox.22$water.proximity)

# plots 
ggplot(pm2.5df.2022, aes(x = water.proximity, y = value, fill = water.proximity)) +
  #geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.25) + 
  geom_boxplot(alpha = 0.5) +
  labs(x = "Proximity to Waterfront") +
  ylab(expression(bold(PM[2.5]~mass~concentration~(µg~m^bold("-3"))))) +
  ggtitle("Annual PM2.5 Mass May-August 2022") +
  scale_fill_manual(values = proximity.color.palette) +
  scale_color_manual(values = proximity.color.palette) +
  theme(axis.text.x = element_text(size=15, face="bold", color = "black"),
        axis.text.y = element_text(size=15, face="bold", color = "black"),
        axis.title.x = element_text(size=15, face="bold", color = "black"),  
        axis.title.y = element_text(size=15, face="bold", color = "black"),
        plot.title = element_text(size=20, face="bold", color = "black", hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = 'gray', linetype = "dashed"), 
        panel.grid.major.x = element_blank()) +
  ylim(0,20) +
  geom_text(data = tk.prox.22, aes(x = water.proximity, y = quant, label = cld.prox.22$Letters), size = 5, vjust=-1, hjust = -0.2)

# 2023 DATA ONLY ------------------------------------------------------------------------
# stats test to go with boxplot data
anova.PM2.5.sensor.23 <- aov(value ~ Name, data = pm2.5df.2023)
summary(anova.PM2.5.sensor.23)

tukey.sensor.23 <- TukeyHSD(anova.PM2.5.sensor.23)
print(tukey.sensor.23)

cld.sensor.23 <- multcompLetters4(anova.PM2.5.sensor.23, tukey.sensor.23)
print(cld.sensor.23)

tk.sensor.23 <- group_by(pm2.5df.2023, Name) %>% 
  summarise(mean = mean(value), quant = quantile(value, probs = 0.75)) %>% 
  arrange(desc(mean))
cld.sensor.23 <- as.data.frame.list(cld.sensor.23$Name)

# plots 
ggplot(pm2.5df.2023, aes(x = Name, y = value, fill = Name)) +
  #geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.25) + 
  geom_boxplot(alpha = 0.5) +
  labs(x = "Sensor ID") +
  ylab(expression(bold(PM[2.5]~mass~concentration~(µg~m^bold("-3"))))) +
  ggtitle("PM2.5 Mass May-August 2023") +
  scale_fill_manual(values = location.color.palette) +
  scale_color_manual(values = location.color.palette) +
  theme(axis.text.x = element_text(size=15, face="bold", color = "black"),
        axis.text.y = element_text(size=15, face="bold", color = "black"),
        axis.title.x = element_text(size=15, face="bold", color = "black"),  
        axis.title.y = element_text(size=15, face="bold", color = "black"),
        plot.title = element_text(size=20, face="bold", color = "black", hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = 'gray', linetype = "dashed"), 
        panel.grid.major.x = element_blank()) +
  ylim(0,20) +
  geom_text(data = tk.sensor.23, aes(x = Name, y = quant, label = cld.sensor.23$Letters), size = 3, vjust=-1, hjust = -0.2)

# grouped by water proximity
anova.PM2.5.prox.23 <- aov(value ~ water.proximity, data = pm2.5df.2023)
summary(anova.PM2.5.prox.23)

tukey.prox.23 <- TukeyHSD(anova.PM2.5.prox.23)
print(tukey.prox.23)

cld.prox.23 <- multcompLetters4(anova.PM2.5.prox.23, tukey.prox.23)
print(cld.prox.23)

tk.prox.23 <- group_by(pm2.5df.2023, water.proximity) %>% 
  summarise(mean = mean(value), quant = quantile(value, probs = 0.75)) %>% 
  arrange(desc(mean))
cld.prox.23 <- as.data.frame.list(cld.prox.23$water.proximity)

# plots 
ggplot(pm2.5df.2023, aes(x = water.proximity, y = value, fill = water.proximity)) +
  #geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.25) + 
  geom_boxplot(alpha = 0.5) +
  labs(x = "Proximity to Waterfront") +
  ylab(expression(bold(PM[2.5]~mass~concentration~(µg~m^bold("-3"))))) +
  ggtitle("Annual PM2.5 Mass May-August 2023") +
  scale_fill_manual(values = proximity.color.palette) +
  scale_color_manual(values = proximity.color.palette) +
  theme(axis.text.x = element_text(size=15, face="bold", color = "black"),
        axis.text.y = element_text(size=15, face="bold", color = "black"),
        axis.title.x = element_text(size=15, face="bold", color = "black"),  
        axis.title.y = element_text(size=15, face="bold", color = "black"),
        plot.title = element_text(size=20, face="bold", color = "black", hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = 'gray', linetype = "dashed"), 
        panel.grid.major.x = element_blank()) +
  ylim(0,20) +
  geom_text(data = tk.prox.23, aes(x = water.proximity, y = quant, label = cld.prox.23$Letters), size = 5, vjust=-1, hjust = -0.2)

# comparing PM to water data via simple linear regressions  ----------------------------------------------
S_cyano_pm <- combined_water_air_S_wide %>% 
  filter(#date < "2023-01-01" & 
         date <= "2023-06-01" | 
         date >= "2023-06-14") %>% 
  filter(avg_cyano_cell_count != 0)

M_cyano_pm <- combined_water_air_M_wide %>% 
  filter(#date < "2023-01-01" & 
    date <= "2023-06-01" | 
      date >= "2023-06-14") %>% 
  filter(avg_cyano_cell_count != 0)

L_cyano_pm <- combined_water_air_L_wide %>% 
  filter(#date < "2023-01-01" & 
    date <= "2023-06-01" | 
      date >= "2023-06-14") %>% 
  filter(avg_cyano_cell_count != 0)

# linear regression plots compiled sensors 
library(patchwork)
S.plot <- ggplot(S_cyano_pm, aes(x = `avg_cyano_cell_count`, y = `pm2.5`)) +
  geom_point() + 
  geom_smooth(method = "glm", se = FALSE) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "<0.5 km resolution") +
  xlab(expression(bold(cyanobacterial~cell~count~(cells~ml^bold("-1"))))) +
  ylab(expression(bold(PM[2.5]~mass~concentration~(µg~m^bold("-3"))))) +
  theme_minimal() 
fit <- lm(pm2.5 ~ avg_cyano_cell_count, data = S_cyano_pm)
eq <- as.character(paste("R^2 =", round(summary(fit)$r.squared, 2)))
# Add R^2 value as a label to the plot
S.plot.1 <- S.plot + annotate("text", x = Inf, y = -Inf, hjust = 1.5, vjust = -15, label = eq, size = 6)
S.plot.1

M.plot <- ggplot(M_cyano_pm, aes(x = `avg_cyano_cell_count`, y = `pm2.5`)) +
  geom_point()+  # Scatter plot
  geom_smooth(method = "glm", se = FALSE) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "1 km resolution") +
  xlab(expression(bold(cyanobacterial~cell~count~(cells~ml^bold("-1"))))) +
  ylab(expression(bold(PM[2.5]~mass~concentration~(µg~m^bold("-3"))))) +
  theme_minimal() 
fit <- lm(pm2.5 ~ avg_cyano_cell_count, data = M_cyano_pm)
eq <- as.character(paste("R^2 =", round(summary(fit)$r.squared, 2)))
# Add R^2 value as a label to the plot
M.plot.1 <- M.plot + annotate("text", x = Inf, y = -Inf, hjust = 1.5, vjust = -15, label = eq, size = 6)
M.plot.1

L.plot <- ggplot(L_cyano_pm, aes(x = `avg_cyano_cell_count`, y = `pm2.5`)) +
  geom_point() + 
  xlim(6500,75000) +  # Scatter plot
  geom_smooth(method = "glm", se = FALSE) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = ">1 km resolution") +
  xlab(expression(bold(cyanobacterial~cell~count~(cells~ml^bold("-1"))))) +
  ylab(expression(bold(PM[2.5]~mass~concentration~(µg~m^bold("-3"))))) +
  theme_minimal() 
fit <- lm(pm2.5 ~ avg_cyano_cell_count, data = L_cyano_pm)
eq <- as.character(paste("R^2 =", round(summary(fit)$r.squared, 2)))
# Add R^2 value as a label to the plot
L.plot.1 <- L.plot + annotate("text", x = Inf, y = -Inf, hjust = 1.5, vjust = -15, label = eq, size = 6)
L.plot.1

S.plot.1 + M.plot.1 + L.plot.1

# stratifying data by individual sensors
ggplot(S_cyano_pm, aes(x = `avg_cyano_cell_count`, y = `pm2.5`, color = Name)) +
  geom_point() +   # Axis labels
  theme_minimal() + 
  geom_smooth(method = "glm", se = FALSE) 

ggplot(M_cyano_pm, aes(x = `avg_cyano_cell_count`, y = `pm2.5`, color = Name)) +
  geom_point() +   # Axis labels
  theme_minimal() + 
  geom_smooth(method = "glm", se = FALSE) 

ggplot(L_cyano_pm, aes(x = `avg_cyano_cell_count`, y = `pm2.5`, color = Name)) +
  geom_point() +   # Axis labels
  theme_minimal()+ 
  xlim(6500,75000) + 
  geom_smooth(method = "glm", se = FALSE) 

# cleaning data for daily time series stratified by sensor

# time series
sensor.AB <- L_cyano_pm %>% filter(Name == "1318" & date >= "2023-01-01")
sensor.AB$date <- as.Date(sensor.AB$date)
# Plot with two y-axes
ggplot(sensor.AB, aes(x = date)) +
  geom_line(aes(y = pm2.5, color = "pm2.5")) +
  geom_line(aes(y = avg_cyano_cell_count / 1000, color = "avg_cyano_cell_count")) +
  scale_y_continuous(
    name = "pm2.5",
    limits = c(0, 60),
    sec.axis = sec_axis(~ .*1000, name = "avg_cyano_cell_count")
  ) +
  labs(x = "Date") +
  scale_color_manual(values = c("pm2.5" = "blue", "avg_cyano_cell_count" = "red")) +
  theme_minimal()


ggplot(sensor.AB, aes(x=date, y=pm2.5)) + 
         geom_line()





