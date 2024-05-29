setwd("Q:\\My Drive\\Code Repositories\\R\\CCRG\\CCRG_PurpleAir")
rm(list = ls())
library(httr);library(jsonlite);library(dplyr)

# PurpleAir Data Cleaning --------------------------------------------------------------------------
folder_path <- "Q:\\My Drive\\Code Repositories\\R\\CCRG\\purpleairdata\\cleaned_sensor_data"

# Get the list of file names in the folder
file_names <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Create an empty list to store the data frames
data_frames <- list()

# Loop through each file name and read the CSV file
## compare a and b channels and remove channels where a and b disagree according to Barkjohn et al., 2021
### apply relative humidity correction factor to pm1 pm2.5 and pm10 readings, although not sure if the correction factor varies for pm1 and pm10
for (i in seq_along(file_names)) {
  # Read the CSV file
  data <- read.csv(file_names[i])
  
  # Convert the 'time_stamp' column from Unix time to POSIXct
  data$time_stamp <- as.POSIXct(data$time_stamp, origin = "1970-01-01 00:00:00")
  
  # Convert the 'time_stamp' column to the desired format
  data$time_stamp <- format(data$time_stamp, "%Y-%m-%d")
  
  # Perform calculations on columns 'pm1_a' and 'pm1_b'
  if ("pm1_a" %in% colnames(data) && "pm1_b" %in% colnames(data)) {
    data$sensor_dif <- data$pm1_a - data$pm1_b
    data$channel_comp <- ((data$pm1_a - data$pm1_b) * 2) / (data$pm1_a + data$pm1_b)
    
    # Remove rows where sensor_dif > 5 or channel_comp > 2
    data <- data[data$sensor_dif <= 5 & data$sensor_dif >= -5 & data$channel_comp <= 0.61 & data$channel_comp >= -0.61, ]
    
    #find average of a and b sensors
    data$pm1_avg <- ((data$pm1_a + data$pm1_b)) / 2
  }
  
  # Perform calculations on columns 'pm2.5_a' and 'pm2.5_b'
  if ("pm2.5_a" %in% colnames(data) && "pm2.5_b" %in% colnames(data)) {
    data$sensor_dif <- data$pm2.5_a - data$pm2.5_b
    data$channel_comp <- ((data$pm2.5_a - data$pm2.5_b) * 2) / (data$pm2.5_a + data$pm2.5_b)
    
    # Remove rows where sensor_dif > 5 or channel_comp > 2
    data <- data[data$sensor_dif <= 5 & data$sensor_dif >= -5 & data$channel_comp <= 0.61 & data$channel_comp >= -0.61, ]
    
    #find average of a and b sensors
    data$pm2.5_avg <- ((data$pm2.5_a + data$pm2.5_b)) / 2
  }
  
  # Perform calculations on columns 'pm10_a' and 'pm10_b'
  if ("pm10_a" %in% colnames(data) && "pm10_b" %in% colnames(data)) {
    data$sensor_dif <- data$pm10_a - data$pm10_b
    data$channel_comp <- ((data$pm10_a - data$pm10_b) * 2) / (data$pm10_a + data$pm10_b)
    
    # Remove rows where sensor_dif > 5 or channel_comp > 2
    data <- data[data$sensor_dif <= 5 & data$sensor_dif >= -5 & data$channel_comp <= 0.61 & data$channel_comp >= -0.61, ]
    
    #find average of a and b sensors
    data$pm10_avg <- ((data$pm10_a + data$pm10_b)) / 2
  }
  
  # Calculate the 'corrected_by_RH' column for pm1
  if ("pm1_avg" %in% colnames(data) && "humidity" %in% colnames(data)) {
    data$pm1_corrected_by_RH <- 0.524 * data$pm1_avg - 0.0862 * data$humidity + 5.75
  }
  
  # Calculate the 'corrected_by_RH' column for pm2.5
  if ("pm2.5_avg" %in% colnames(data) && "humidity" %in% colnames(data)) {
    data$pm2.5_corrected_by_RH <- 0.524 * data$pm2.5_avg - 0.0862 * data$humidity + 5.75
  }
  
  # Calculate the 'corrected_by_RH' column for pm2.5
  if ("pm10_avg" %in% colnames(data) && "humidity" %in% colnames(data)) {
    data$pm10_corrected_by_RH <- 0.524 * data$pm10_avg - 0.0862 * data$humidity + 5.75
  }
  
  # Remove duplicate rows
  data <- data[!duplicated(data), ]
  
  # Extract the last 6 characters of the file name
  truncated_file_name <- substr(basename(file_names[i]), nchar(basename(file_names[i])) - 9, nchar(basename(file_names[i])))
  
  # Store the data frame and truncated file name in the list
  data_frames[[i]] <- list(truncated_file_name = truncated_file_name, data = data)

}

# Create an empty list to store the data frames
result_data_frames <- list()

# Loop through each element in data_frames
for (i in seq_along(data_frames)) {
  # Extract the truncated file name and data from the list
  truncated_file_name <- data_frames[[i]]$truncated_file_name
  data <- data_frames[[i]]$data
  
  # Assign the data frame to a variable with the truncated file name
  assign(truncated_file_name, data)
  
  # Store the data frame in the result_data_frames list
  result_data_frames[[i]] <- get(truncated_file_name)
}

# Now result_data_frames is a list where each element is a data frame named according to the truncated file name
# Now to manually merge files because ChatGPT couldnt figure it out
sensor.1318 <- rbind(`151318.csv`, `1318_2.csv`) %>% distinct(time_stamp, .keep_all = TRUE)
sensor.1334 <- rbind(`151334.csv`, `1334_2.csv`) %>% distinct(time_stamp, .keep_all = TRUE)
sensor.1344 <- rbind(`151344.csv`, `1344_2.csv`) %>% distinct(time_stamp, .keep_all = TRUE)
sensor.5822 <- rbind(`145822.csv`, `5822_2.csv`) %>% distinct(time_stamp, .keep_all = TRUE)
sensor.1348 <- rbind(`151348.csv`, `1348_2.csv`) %>% distinct(time_stamp, .keep_all = TRUE)
sensor.1358 <- rbind(`151358.csv`, `1358_2.csv`) %>% distinct(time_stamp, .keep_all = TRUE)
sensor.1362 <- rbind(`151362.csv`, `1362_2.csv`) %>% distinct(time_stamp, .keep_all = TRUE)
sensor.5838 <- rbind(`145838.csv`, `5838_2.csv`) %>% distinct(time_stamp, .keep_all = TRUE)
sensor.1806 <- rbind(`151806.csv`, `1806_2.csv`) %>% distinct(time_stamp, .keep_all = TRUE)
sensor.9875 <- `9875_2.csv` %>% distinct(time_stamp, .keep_all = TRUE)
sensor.1378 <- `1378_2.csv` %>% distinct(time_stamp, .keep_all = TRUE)
sensor.1680 <- `1680_2.csv` %>% distinct(time_stamp, .keep_all = TRUE)
sensor.1806 <- `1806_2.csv` %>% distinct(time_stamp, .keep_all = TRUE)

## filling in NA values with values from nearest sensors on those dates
library(dplyr)

# 1318 has missing humidity values, using RH values from nearest sensor (1344) operating at the time to fill in these NAs 
# Left join to merge data based on time_stamp
merged_data <- left_join(sensor.1318, sensor.1344, by = "time_stamp", suffix = c("_1318", "_1344"))
# Use coalesce to fill in missing values in humidity_1318 with values from humidity_1344
merged_data <- merged_data %>%
  mutate(humidity_1318 = coalesce(humidity_1318, humidity_1344))
# Select the columns you need
new.RH.values.1318 <- merged_data %>% select(time_stamp, humidity_1318)
# use left_join to add the humidity column back in to sensor.1318
sensor.1318.1 <- left_join(new.RH.values.1318, sensor.1318, by = "time_stamp") %>% select(-humidity) %>% rename(humidity = humidity_1318)
#re-calculate adjusted values for PM based on new RH values inputted
sensor.1318.1 <- sensor.1318.1 %>% mutate(pm1_corrected_by_RH = 0.524 * pm1_avg - 0.0862 * humidity + 5.75, pm2.5_corrected_by_RH = 0.524 * pm2.5_avg - 0.0862 * humidity + 5.75, pm10_corrected_by_RH = 0.524 * pm10_avg - 0.0862 * humidity + 5.75)
# renaming to original file name 
sensor.1318 <- sensor.1318.1

# 1348 has missing humidity values, using RH values from nearest sensor (5838) operating at the time to fill in these NAs 
# Left join to merge data based on time_stamp
merged_data <- left_join(sensor.1348, sensor.5838, by = "time_stamp", suffix = c("_1348", "_5838"))
# Use coalesce to fill in missing values in humidity_1348 with values from humidity_5838
merged_data <- merged_data %>%
  mutate(humidity_1348 = coalesce(humidity_1348, humidity_5838))
# Select the columns you need
new.RH.values.1348 <- merged_data %>% select(time_stamp, humidity_1348)
# use left_join to add the humidity column back in to sensor.1348
sensor.1348.1 <- left_join(new.RH.values.1348, sensor.1348, by = "time_stamp") %>% select(-humidity) %>% rename(humidity = humidity_1348)
#re-calculate adjusted values for PM based on new RH values inputted
sensor.1348.1 <- sensor.1348.1 %>% mutate(pm1_corrected_by_RH = 0.524 * pm1_avg - 0.0862 * humidity + 5.75, pm2.5_corrected_by_RH = 0.524 * pm2.5_avg - 0.0862 * humidity + 5.75, pm10_corrected_by_RH = 0.524 * pm10_avg - 0.0862 * humidity + 5.75)
# renaming to original file name 
sensor.1348 <- sensor.1348.1

# 1680 has missing humidity values, using RH values from nearest sensor (5838) operating at the time to fill in these NAs 
# Left join to merge data based on time_stamp
merged_data <- left_join(sensor.1680, sensor.5838, by = "time_stamp", suffix = c("_1680", "_5838"))
# Use coalesce to fill in missing values in humidity_1680 with values from humidity_5838
merged_data <- merged_data %>%
  mutate(humidity_1680 = coalesce(humidity_1680, humidity_5838))
# Select the columns you need
new.RH.values.1680 <- merged_data %>% select(time_stamp, humidity_1680)
# use left_join to add the humidity column back in to sensor.1680
sensor.1680.1 <- left_join(new.RH.values.1680, sensor.1680, by = "time_stamp") %>% select(-humidity) %>% rename(humidity = humidity_1680)
#re-calculate adjusted values for PM based on new RH values inputted
sensor.1680.1 <- sensor.1680.1 %>% mutate(pm1_corrected_by_RH = 0.524 * pm1_avg - 0.0862 * humidity + 5.75, pm2.5_corrected_by_RH = 0.524 * pm2.5_avg - 0.0862 * humidity + 5.75, pm10_corrected_by_RH = 0.524 * pm10_avg - 0.0862 * humidity + 5.75) 
# renaming to original file name 
sensor.1680 <- sensor.1680.1

# 5822 has missing humidity values, using RH values from nearest sensor (1334) operating at the time to fill in these NAs 
# Left join to merge data based on time_stamp
merged_data <- left_join(sensor.5822, sensor.1334, by = "time_stamp", suffix = c("_5822", "_1334"))
# Use coalesce to fill in missing values in humidity_5822 with values from humidity_1334
merged_data <- merged_data %>%
  mutate(humidity_5822 = coalesce(humidity_5822, humidity_1334))
# Select the columns you need
new.RH.values.5822 <- merged_data %>% select(time_stamp, humidity_5822)
# use left_join to add the humidity column back in to sensor.5822
sensor.5822.1 <- left_join(new.RH.values.5822, sensor.5822, by = "time_stamp") %>% select(-humidity) %>% rename(humidity = humidity_5822)
#re-calculate adjusted values for PM based on new RH values inputted
sensor.5822.1 <- sensor.5822.1 %>% mutate(pm1_corrected_by_RH = 0.524 * pm1_avg - 0.0862 * humidity + 5.75, pm2.5_corrected_by_RH = 0.524 * pm2.5_avg - 0.0862 * humidity + 5.75, pm10_corrected_by_RH = 0.524 * pm10_avg - 0.0862 * humidity + 5.75) 
# renaming to original file name 
sensor.5822 <- sensor.5822.1

# List all objects in the environment
all_objects <- ls()

# Filter objects that start with "Sensor." followed by four letters
sensor_objects <- grep("^sensor\\.\\w{4}$", all_objects, value = TRUE)

# Create a list combining all the matching dataframes
pa.sensor.list <- lapply(sensor_objects, get)

# If you want to name the list with the object names
names(pa.sensor.list) <- sensor_objects

# CyAN Cyanobacterial Index (digital number DN) data from SEADAS Pixel Extraction ---------------------------------------------- 
library(tidyverse);library(dplyr)

# read in all of the SeaDAS files 
# Specify the folder path containing your text files
folder_path <- "Q:\\My Drive\\Code Repositories\\R\\CCRG\\SeaDAS files\\extracted_DNs"

# Initialize an empty list to store data frames
result_list <- list()

# Iterate over each file in the folder
files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)

# Filter files that contain the word "Derived" in the name
filtered_files <- files[grep("GeoTIFF", files)]

for (file in filtered_files) {
  
  # Read the file, skipping lines until line 7
  df <- read_delim(file, delim = "\t", skip = 6)
  
  # Select and store the desired columns
  selected_columns <- df %>% select(Name = `Name`, band_1)
  
  # Extract the desired part from the original filename to use as the new filename
  new_filename <- gsub("_Derived from .*", "", basename(file))
  
  # Use the filename (without extension) as the list element name
  result_list[[new_filename]] <- selected_columns
}



# Creating three separate lists for S, M, and L pixel coverages ---------------------------------------------------
# Create empty lists to store dataframes
S_list <- list()
M_list <- list()
L_list <- list()

# Loop through the list of dataframes and sort them into respective lists
for (df_name in names(result_list)) {
  if (grepl("_S|_S_*", df_name, ignore.case = FALSE)) {
    S_list[[df_name]] <- result_list[[df_name]]
  } else if (grepl("_M|_M_*", df_name, ignore.case = FALSE)) {
    M_list[[df_name]] <- result_list[[df_name]]
  } else if (grepl("_L|_L_*", df_name, ignore.case = FALSE)) {
    L_list[[df_name]] <- result_list[[df_name]]
  }
}

# Adding dates into each dataframe for easier manipulation later on -------------------------------------------
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

# Calculate cyanobacterial indices, chlorophyll a and cyano cell count averages ----------
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
    as.data.frame()
  # Store the summary data frame in the list with the same name as the data frame
  S_intensity[[df_name]] <- summary_df }

# Define the complete set of Name values
all_names <- c(1562, 5822, 1334, 1680, 1358, 5838, 1348, 1378, 1362, 1318, 9875, 1344, 1806)

# Fill in the missing values for each sensor with NA
for (i in seq_along(S_intensity)) {
  S_intensity[[i]] <- S_intensity[[i]] %>%
    complete(Name = all_names) %>%
    fill(date) %>%
    fill(date, .direction = "up")
}

all_S_cyano_values_wide <- bind_rows(S_intensity)
all_S_cyano_values_wide$Name <- as.character(all_S_cyano_values_wide$Name)

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
    as.data.frame()
  # Store the summary data frame in the list with the same name as the data frame
  M_intensity[[df_name]] <- summary_df }

# Fill in the missing values for each sensor with NA
for (i in seq_along(M_intensity)) {
  M_intensity[[i]] <- M_intensity[[i]] %>%
    complete(Name = all_names) %>%
    fill(date) %>%
    fill(date, .direction = "up")
}

all_M_cyano_values_wide <- bind_rows(M_intensity)
all_M_cyano_values_wide$Name <- as.character(all_M_cyano_values_wide$Name)

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
    as.data.frame()
  # Store the summary data frame in the list with the same name as the data frame
  L_intensity[[df_name]] <- summary_df }

# Fill in the missing values for each sensor with NA
for (i in seq_along(L_intensity)) {
  L_intensity[[i]] <- L_intensity[[i]] %>%
    complete(Name = all_names) %>%
    fill(date) %>%
    fill(date, .direction = "up")
}

all_L_cyano_values_wide <- bind_rows(L_intensity)
all_L_cyano_values_wide$Name <- as.character(all_L_cyano_values_wide$Name)

# pivot_longer for visualizations and combine list into single df -----------------------------

# Create an empty list to store the result
S_intensity_longer <- list()

# Loop over each dataframe in the S_intensity list
for (i in seq_along(S_intensity)) {
  # Pivot longer to make every column after "date" into a new column
  pivoted_df <- S_intensity[[i]] %>%
    pivot_longer(cols = c(-Name,-date),
                 names_to = "variable",
                 values_to = "value")
  
  # Store the pivoted dataframe in the S_intensity_longer list
  S_intensity_longer[[i]] <- pivoted_df
}

# completed final large dataframe all bound together
all_S_cyano_values <- bind_rows(S_intensity_longer)

# Create an empty list to store the result
M_intensity_longer <- list()

# Loop over each dataframe in the M_intensity list
for (i in seq_along(M_intensity)) {
  # Pivot longer to make every column after "date" into a new column
  pivoted_df <- M_intensity[[i]] %>%
    pivot_longer(cols = c(-Name,-date),
                 names_to = "variable",
                 values_to = "value")
  
  # Store the pivoted dataframe in the M_intensity_longer list
  M_intensity_longer[[i]] <- pivoted_df
}

# completed final large dataframe all bound together
all_M_cyano_values <- bind_rows(M_intensity_longer)

# Create an empty list to store the result
L_intensity_longer <- list()

# Loop over each dataframe in the L_intensity list
for (i in seq_along(L_intensity)) {
  # Pivot longer to make every column after "date" into a new column
  pivoted_df <- L_intensity[[i]] %>%
    pivot_longer(cols = c(-Name,-date),
                 names_to = "variable",
                 values_to = "value")
  
  # Store the pivoted dataframe in the L_intensity_longer list
  L_intensity_longer[[i]] <- pivoted_df
}

# completed final large dataframe all bound together
all_L_cyano_values <- bind_rows(L_intensity_longer)

# Generate data availability summaries for each sensor at each resolution ----------------------------------------
summary_data_frames_S <- list()
# Iterate over each data frame in the result list
for (df_name in names(S_list)) {
  # Group by "Name" and summarize counts for each category of "band_1"
  summary_df <- S_list[[df_name]] %>%
    group_by(Name, date) %>%
    summarize(`0-253` = sum(band_1 >= 0 & band_1 <= 253),
              `254` = sum(band_1 == 254),
              `255` = sum(band_1 == 255)) %>%
    # Add a new column "water" that sums counts for categories 0-253 and 255
    mutate(water = `0-253` + `255`,
           land = `254`,
           water_surface_area = round((`0-253`+`255`)/(`0-253`+`255`+`254`),2),
           percent_valid_pixels = round(`0-253` / `water`, 2)) %>%
    arrange(Name) %>%
    as.data.frame()
  
  # Store the summary data frame in the list with the same name as the data frame
  summary_data_frames_S[[df_name]] <- summary_df
}

summary_data_frames_M <- list()
# Iterate over each data frame in the result list
for (df_name in names(M_list)) {
  # Group by "Name" and summarize counts for each category of "band_1"
  summary_df <- M_list[[df_name]] %>%
    group_by(Name, date) %>%
    summarize(`0-253` = sum(band_1 >= 0 & band_1 <= 253),
              `254` = sum(band_1 == 254),
              `255` = sum(band_1 == 255)) %>%
    # Add a new column "water" that sums counts for categories 0-253 and 255
    mutate(water = `0-253` + `255`,
           land = `254`,
           water_surface_area = round((`0-253`+`255`)/(`0-253`+`255`+`254`),2),
           percent_valid_pixels = round(`0-253` / `water`, 2)) %>%
    arrange(Name) %>%
    as.data.frame()
  
  # Store the summary data frame in the list with the same name as the data frame
  summary_data_frames_M[[df_name]] <- summary_df
}

summary_data_frames_L <- list()
# Iterate over each data frame in the result list
for (df_name in names(L_list)) {
  # Group by "Name" and summarize counts for each category of "band_1"
  summary_df <- L_list[[df_name]] %>%
    group_by(Name, date) %>%
    summarize(`0-253` = sum(band_1 >= 0 & band_1 <= 253),
              `254` = sum(band_1 == 254),
              `255` = sum(band_1 == 255)) %>%
    # Add a new column "water" that sums counts for categories 0-253 and 255
    mutate(water = `0-253` + `255`,
           land = `254`,
           water_surface_area = round((`0-253`+`255`)/(`0-253`+`255`+`254`),2),
           percent_valid_pixels = round(`0-253` / `water`, 2)) %>%
    arrange(Name) %>%
    as.data.frame()
  
  # Store the summary data frame in the list with the same name as the data frame
  summary_data_frames_L[[df_name]] <- summary_df
}


# Step 1: Extract unique sensor names from the first dataframe in S_intensity
unique_sensor_names <- unique(L_intensity[[1]]$Name)





# Combining Air Quality data with Water Quality data -----------------------------------------------
# List of dataframes to combine
sensor_names <- c("sensor.1318", "sensor.1344", "sensor.1348", "sensor.1358", 
                  "sensor.1362", "sensor.1378", "sensor.1680", "sensor.1806", 
                  "sensor.5822", "sensor.5838", "sensor.9875")

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
          select(Name, time_stamp, pm1_corrected_by_RH, pm2.5_corrected_by_RH, pm10_corrected_by_RH) %>% 
          rename(date = time_stamp, pm1 = pm1_corrected_by_RH, pm2.5 = pm2.5_corrected_by_RH, 
                 pm10 = pm10_corrected_by_RH)  %>%
          mutate(across(-c(date, Name), ~ifelse(. < 0, 0, .)))

# longer for visualizations
compiled_purpleair_data_longer <- compiled_purpleair_data %>% 
                                  pivot_longer(cols = c(-Name,-date), names_to = "variable", values_to = "value")
compiled_purpleair_data_longer$date <- as.Date(compiled_purpleair_data_longer$date)

combined_water_air_S_long <- rbind(compiled_purpleair_data_longer, all_S_cyano_values) %>% 
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
combined_water_air_M_long <- rbind(compiled_purpleair_data_longer, all_M_cyano_values) %>% 
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
combined_water_air_L_long <- rbind(compiled_purpleair_data_longer, all_L_cyano_values) %>% 
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

# wider for other comparisons / viz
combined_water_air_S_wide <- left_join(compiled_purpleair_data, all_S_cyano_values_wide, by = c("Name", "date")) %>% 
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
combined_water_air_M_wide <- left_join(compiled_purpleair_data, all_M_cyano_values_wide, by = c("Name", "date")) %>% 
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
combined_water_air_L_wide <- left_join(compiled_purpleair_data, all_L_cyano_values_wide, by = c("Name", "date"))%>% 
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

# Playing around with plots ------------------------------------------------------------------
# color palettes and other theme specifications
location.color.palette <- c("1318" = "deepskyblue",
                            "1344" = "cadetblue2",
                            "1348" = "blue",
                            "1358" = "black",
                            "1362" = "blue",
                            "1378" = "blue",
                            "1680" = "blue",
                            "1806" = "blue",
                            "5822" = "blue",
                            "5838" = "deepskyblue",
                            "9875" = "blue")

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
# stats test to go with boxplot data
library(multcompView)
anova.PM2.5.sensor <- aov(value ~ Name, data = pm2.5df)
summary(anova.PM2.5.sensor)

tukey.sensor <- TukeyHSD(anova.PM2.5.sensor)
print(tukey.sensor)

cld.sensor <- multcompLetters4(anova.PM2.5.sensor, tukey.sensor)
print(cld.sensor)

tk.sensor <- group_by(pm2.5df, Name) %>% 
      summarise(mean = mean(value), quant = quantile(value, probs = 0.75)) %>% 
      arrange(desc(mean))
cld.sensor <- as.data.frame.list(cld.sensor$Name)

# plots 
ggplot(pm2.5df, aes(x = Name, y = value, fill = Name)) +
  #geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.25) + 
  geom_boxplot(alpha = 0.5) +
  labs(x = "Sensor ID") +
  ylab(expression(bold(PM[2.5]~mass~concentration~(µg~m^bold("-3"))))) +
  ggtitle("PM2.5 Mass May-August 2022 and 2023") +
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
  geom_text(data = tk.sensor, aes(x = Name, y = quant, label = cld.sensor$Letters), size = 3, vjust=-1, hjust = -0.2)

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

class(sensor.AB$date)



