setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\CCRG_PurpleAir")
rm(list = ls())
library(httr);library(jsonlite);library(dplyr);library(ggplot2)

# Read in the data ------------------------------------
daily_data <- read.csv("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\COMPLETE.daily.data.csv") %>% select(-X) %>% mutate(Name = as.character(Name), date = as.Date(date))
imputed_data <- read.csv("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\COMPLETE.imputations.data.csv") %>% select(-X) %>% mutate(Name = as.character(Name), date = as.Date(date))

# Cleaning the dataframes prior to any analyses ---------------------------
library(lubridate)

# convert daily data to weekly data to avoid sensors dropping otherwise 
# Group by the start of each week and calculate the weekly averages for numeric columns
weekly_data <- daily_data %>%
  mutate(week = floor_date(date, unit = "week")) %>%
  group_by(Name,week) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),  
    across(where(~ !is.numeric(.x)), ~ first(.x))) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))

# adding a season column 
daily_data <- daily_data %>% 
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ "Winter",
    month(date) %in% c(3, 4, 5) ~ "Spring",
    month(date) %in% c(6, 7, 8) ~ "Summer",
    month(date) %in% c(9, 10, 11) ~ "Fall",
    TRUE ~ NA_character_)) %>% 
  mutate(bloom.season = case_when(
    month(date) %in% c(1, 2, 3, 4, 10, 11, 12) ~ "off_season",
    month(date) %in% c(5, 6, 7, 8, 9) ~ "bloom_season",
    TRUE ~ NA_character_)) %>%
  mutate(month = case_when(
    month(date) == 1 ~ "January",
    month(date) == 2 ~ "February",
    month(date) == 3 ~ "March",
    month(date) == 4 ~ "April",
    month(date) == 5 ~ "May",
    month(date) == 6 ~ "June",
    month(date) == 7 ~ "July",
    month(date) == 8 ~ "August",
    month(date) == 9 ~ "September",
    month(date) == 10 ~ "October",
    month(date) == 11 ~ "November",
    month(date) == 12 ~ "December",
    TRUE ~ NA_character_)) %>%
  mutate(Year = case_when(
    year(date) == 2022 ~ "2022",
    year(date) == 2023 ~ "2023",
    TRUE ~ NA_character_)) %>%
  mutate(Hotspot = case_when(
    Name %in% c("5822", "1334") ~ "Arrowhead Beach",
    Name %in% c("1344", "9875", "1318") ~ "Elizabeth City",
    Name %in% c("1358", "1562") ~ "No Hotspot",
    Name %in% c("5838", "1348", "1680") ~ "Edenton",
    Name == "1378" ~ "Hertford",
    Name == "1362" ~ "Nixonton",
    Name == "1806" ~ "Outer Banks",
    TRUE ~ NA_character_)) %>%
  mutate(winddir_cardinal = case_when(
    (0 <= winddir & winddir <= 22.5) | (337.5 < winddir & winddir <= 360) ~ "N",
    22.5 < winddir & winddir <= 67.5 ~ "NE",
    67.5 < winddir & winddir <= 112.5 ~ "E",
    112.5 < winddir & winddir <= 157.5 ~ "SE",
    157.5 < winddir & winddir <= 202.5 ~ "S",
    202.5 < winddir & winddir <= 247.5 ~ "SW",
    247.5 < winddir & winddir <= 292.5 ~ "W",
    292.5 < winddir & winddir <= 337.5 ~ "NW",
    TRUE ~ NA_character_)) %>%
  mutate(relative_winddir = case_when(
    Name == "5822" & winddir_cardinal %in% c("SW", "W") ~ "onshore",
    Name == "1334" & winddir_cardinal %in% c("SW", "W") ~ "onshore",
    Name == "1680" & winddir_cardinal %in% c("N", "NE", "E", "SE") ~ "onshore",
    Name == "5838" & winddir_cardinal %in% c("SW", "S", "SE", "E") ~ "onshore",
    Name == "1348" & winddir_cardinal %in% c("SW", "S", "SE", "E") ~ "onshore",
    Name == "1378" & winddir_cardinal %in% c("SW", "S", "SE") ~ "onshore",
    Name == "1362" & winddir_cardinal %in% c("SW", "S", "SE") ~ "onshore",
    Name == "1318" & winddir_cardinal %in% c("S", "SE", "E") ~ "onshore",
    Name == "9875" & winddir_cardinal %in% c("S", "SE", "E") ~ "onshore",
    Name == "1344" & winddir_cardinal %in% c("S") ~ "onshore",
    Name == "1806" & winddir_cardinal %in% c("N", "NW", "W", "SW", "S") ~ "onshore",
    TRUE ~ "offshore")) %>%
  mutate(cyano_case_relevancy = case_when(
    date >= "2022-05-31" & date <= "2022-07-01" ~ "relevant",
    date >= "2022-07-14" & date <= "2022-08-14" ~ "relevant",
    date >= "2023-05-14" & date <= "2023-06-21" ~ "relevant",
    date >= "2023-07-07" & date <= "2023-07-14" ~ "relevant",
    date >= "2023-07-21" & date <= "2023-09-07" ~ "relevant",
    TRUE ~ "not-relevant")) %>%
  mutate(known_PM_days = case_when(
    date >= "2022-07-04" & date <= "2022-07-07" ~ "known_PM_source",
    date >= "2023-07-04" & date <= "2023-07-07" ~ "known_PM_source",
    date >= "2023-06-05" & date <= "2023-06-09" ~ "known_PM_source",
    date >= "2023-06-24" & date <= "2023-07-01" ~ "known_PM_source",
    date >= "2023-07-12" & date <= "2023-07-19" ~ "known_PM_source",
    date >= "2022-08-15" & date <= "2022-08-17" ~ "known_PM_source",
    TRUE ~ "unknown_PM_source" ))

weekly_data <- weekly_data %>% 
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ "Winter",
    month(date) %in% c(3, 4, 5) ~ "Spring",
    month(date) %in% c(6, 7, 8) ~ "Summer",
    month(date) %in% c(9, 10, 11) ~ "Fall",
    TRUE ~ NA_character_)) %>% 
  mutate(bloom.season = case_when(
    month(date) %in% c(1, 2, 3, 4, 10, 11, 12) ~ "off_season",
    month(date) %in% c(5, 6, 7, 8, 9) ~ "bloom_season",
    TRUE ~ NA_character_)) %>%
  mutate(month = case_when(
    month(date) == 1 ~ "January",
    month(date) == 2 ~ "February",
    month(date) == 3 ~ "March",
    month(date) == 4 ~ "April",
    month(date) == 5 ~ "May",
    month(date) == 6 ~ "June",
    month(date) == 7 ~ "July",
    month(date) == 8 ~ "August",
    month(date) == 9 ~ "September",
    month(date) == 10 ~ "October",
    month(date) == 11 ~ "November",
    month(date) == 12 ~ "December",
    TRUE ~ NA_character_)) %>%
  mutate(Year = case_when(
    year(date) == 2022 ~ "2022",
    year(date) == 2023 ~ "2023",
    TRUE ~ NA_character_)) %>%
  mutate(Hotspot = case_when(
    Name %in% c("5822", "1334") ~ "Arrowhead Beach",
    Name %in% c("1344", "9875", "1318") ~ "Elizabeth City",
    Name %in% c("1358", "1562") ~ "Terrestrial Influence",
    Name %in% c("5838", "1348", "1680") ~ "Edenton",
    Name == "1378" ~ "Hertford",
    Name == "1362" ~ "Nixonton",
    Name == "1806" ~ "Outer Banks",
    TRUE ~ NA_character_)) %>%
  mutate(winddir_cardinal = case_when(
    (0 <= winddir & winddir <= 22.5) | (337.5 < winddir & winddir <= 360) ~ "N",
    22.5 < winddir & winddir <= 67.5 ~ "NE",
    67.5 < winddir & winddir <= 112.5 ~ "E",
    112.5 < winddir & winddir <= 157.5 ~ "SE",
    157.5 < winddir & winddir <= 202.5 ~ "S",
    202.5 < winddir & winddir <= 247.5 ~ "SW",
    247.5 < winddir & winddir <= 292.5 ~ "W",
    292.5 < winddir & winddir <= 337.5 ~ "NW",
    TRUE ~ NA_character_)) %>%
  mutate(relative_winddir = case_when(
    Name == "5822" & winddir_cardinal %in% c("SW", "W") ~ "onshore",
    Name == "1334" & winddir_cardinal %in% c("SW", "W") ~ "onshore",
    Name == "1680" & winddir_cardinal %in% c("N", "NE", "E", "SE") ~ "onshore",
    Name == "5838" & winddir_cardinal %in% c("SW", "S", "SE", "E") ~ "onshore",
    Name == "1348" & winddir_cardinal %in% c("SW", "S", "SE", "E") ~ "onshore",
    Name == "1378" & winddir_cardinal %in% c("SW", "S", "SE") ~ "onshore",
    Name == "1362" & winddir_cardinal %in% c("SW", "S", "SE") ~ "onshore",
    Name == "1318" & winddir_cardinal %in% c("S", "SE", "E") ~ "onshore",
    Name == "9875" & winddir_cardinal %in% c("S", "SE", "E") ~ "onshore",
    Name == "1344" & winddir_cardinal %in% c("S") ~ "onshore",
    Name == "1806" & winddir_cardinal %in% c("N", "NW", "W", "SW", "S") ~ "onshore",
    TRUE ~ "offshore"
  )) %>%
  mutate(cyano_case_relevancy = case_when(
    date >= "2022-05-31" & date <= "2022-07-01" ~ "relevant",
    date >= "2022-07-14" & date <= "2022-08-14" ~ "relevant",
    date >= "2023-05-14" & date <= "2023-06-21" ~ "relevant",
    date >= "2023-07-07" & date <= "2023-07-14" ~ "relevant",
    date >= "2023-07-21" & date <= "2023-09-07" ~ "relevant",
    TRUE ~ "not-relevant")) %>%
  mutate(known_PM_days = case_when(
    date >= "2022-07-04" & date <= "2022-07-07" ~ "known_PM_source",
    date >= "2023-07-04" & date <= "2023-07-07" ~ "known_PM_source",
    date >= "2023-06-05" & date <= "2023-06-09" ~ "known_PM_source",
    date >= "2023-06-24" & date <= "2023-07-01" ~ "known_PM_source",
    date >= "2023-07-12" & date <= "2023-07-19" ~ "known_PM_source",
    date >= "2022-08-15" & date <= "2022-08-17" ~ "known_PM_source",
    TRUE ~ "unknown_PM_source" ))

imputed_data <- imputed_data %>% 
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ "Winter",
    month(date) %in% c(3, 4, 5) ~ "Spring",
    month(date) %in% c(6, 7, 8) ~ "Summer",
    month(date) %in% c(9, 10, 11) ~ "Fall",
    TRUE ~ NA_character_)) %>% 
  mutate(bloom.season = case_when(
    month(date) %in% c(1, 2, 3, 4, 10, 11, 12) ~ "off_season",
    month(date) %in% c(5, 6, 7, 8, 9) ~ "bloom_season",
    TRUE ~ NA_character_)) %>%
  mutate(month = case_when(
    month(date) == 1 ~ "January",
    month(date) == 2 ~ "February",
    month(date) == 3 ~ "March",
    month(date) == 4 ~ "April",
    month(date) == 5 ~ "May",
    month(date) == 6 ~ "June",
    month(date) == 7 ~ "July",
    month(date) == 8 ~ "August",
    month(date) == 9 ~ "September",
    month(date) == 10 ~ "October",
    month(date) == 11 ~ "November",
    month(date) == 12 ~ "December",
    TRUE ~ NA_character_)) %>%
  mutate(Year = case_when(
    year(date) == 2022 ~ "2022",
    year(date) == 2023 ~ "2023",
    TRUE ~ NA_character_)) %>%
  mutate(Hotspot = case_when(
    Name %in% c("5822", "1334") ~ "Arrowhead Beach",
    Name %in% c("1344", "9875", "1318") ~ "Elizabeth City",
    Name %in% c("1358", "1562") ~ "Terrestrial Influence",
    Name %in% c("5838", "1348", "1680") ~ "Edenton",
    Name == "1378" ~ "Hertford",
    Name == "1362" ~ "Nixonton",
    Name == "1806" ~ "Outer Banks",
    TRUE ~ NA_character_)) %>%
  mutate(winddir_cardinal = case_when(
    (0 <= winddir & winddir <= 22.5) | (337.5 < winddir & winddir <= 360) ~ "N",
    22.5 < winddir & winddir <= 67.5 ~ "NE",
    67.5 < winddir & winddir <= 112.5 ~ "E",
    112.5 < winddir & winddir <= 157.5 ~ "SE",
    157.5 < winddir & winddir <= 202.5 ~ "S",
    202.5 < winddir & winddir <= 247.5 ~ "SW",
    247.5 < winddir & winddir <= 292.5 ~ "W",
    292.5 < winddir & winddir <= 337.5 ~ "NW",
    TRUE ~ NA_character_)) %>%
  mutate(relative_winddir = case_when(
    Name == "5822" & winddir_cardinal %in% c("SW", "W") ~ "onshore",
    Name == "1334" & winddir_cardinal %in% c("SW", "W") ~ "onshore",
    Name == "1680" & winddir_cardinal %in% c("N", "NE", "E", "SE") ~ "onshore",
    Name == "5838" & winddir_cardinal %in% c("SW", "S", "SE", "E") ~ "onshore",
    Name == "1348" & winddir_cardinal %in% c("SW", "S", "SE", "E") ~ "onshore",
    Name == "1378" & winddir_cardinal %in% c("SW", "S", "SE") ~ "onshore",
    Name == "1362" & winddir_cardinal %in% c("SW", "S", "SE") ~ "onshore",
    Name == "1318" & winddir_cardinal %in% c("S", "SE", "E") ~ "onshore",
    Name == "9875" & winddir_cardinal %in% c("S", "SE", "E") ~ "onshore",
    Name == "1344" & winddir_cardinal %in% c("S") ~ "onshore",
    Name == "1806" & winddir_cardinal %in% c("N", "NW", "W", "SW", "S") ~ "onshore",
    TRUE ~ "offshore"
  )) %>%
  mutate(cyano_case_relevancy = case_when(
    date >= "2022-05-31" & date <= "2022-07-01" ~ "relevant",
    date >= "2022-07-14" & date <= "2022-08-14" ~ "relevant",
    date >= "2023-05-14" & date <= "2023-06-21" ~ "relevant",
    date >= "2023-07-07" & date <= "2023-07-14" ~ "relevant",
    date >= "2023-07-21" & date <= "2023-09-07" ~ "relevant",
    TRUE ~ "not-relevant")) %>%
  mutate(known_PM_days = case_when(
    date >= "2022-07-04" & date <= "2022-07-07" ~ "known_PM_source",
    date >= "2023-07-04" & date <= "2023-07-07" ~ "known_PM_source",
    date >= "2023-06-05" & date <= "2023-06-09" ~ "known_PM_source",
    date >= "2023-06-24" & date <= "2023-07-01" ~ "known_PM_source",
    date >= "2023-07-12" & date <= "2023-07-19" ~ "known_PM_source",
    date >= "2022-08-15" & date <= "2022-08-17" ~ "known_PM_source",
    TRUE ~ "unknown_PM_source" ))

daily_data_cleaned <- daily_data %>% filter(known_PM_days != "known_PM_source")
weekly_data_cleaned <- weekly_data %>% filter(known_PM_days != "known_PM_source")
weekly_data_cleaned_cyano <- weekly_data_cleaned %>% filter(cyano_case_relevancy != "not-relevant")

# color palette for sensors ---------------------------
sensor.name.palette <- c("1562" = '#CC6600',
                         "1334" = '#FFCC00',
                         "5822" = '#FFFF33',
                         "1680" = '#99FF66',
                         "1358" = '#003300',
                         "5838" = '#006633',
                         "1348" = '#539f37',
                         "1378" = '#99FFCC',
                         "1362" = '#99FFFF',
                         "9875" = '#99CCFF',
                         "1318" = '#6699FF',
                         "1344" = '#0000FF',
                         "1806" = '#9999FF')


df <- data.frame(sensor = factor(names(sensor.name.palette), levels = names(sensor.name.palette)), 
                 color = sensor.name.palette)

# Plot the color bar
ggplot(df, aes(x = sensor, y = 1, fill = sensor)) +
  geom_tile(height = 0.15) +  # Reduce the height of the tiles
  scale_fill_manual(values = sensor.name.palette) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"))  +
  labs(title = "Sensor Color Palette") +
  coord_fixed(ratio = 10) 

# Create a sequence of colors for the gradient from black to gray
gradient_colors <- colorRampPalette(c("black", "gray"))(100)

# Create a dataframe with the colors and positions
df <- data.frame(x = 1:100, 
                 color = gradient_colors)

# Plot the gradient color bar
ggplot(df, aes(x = x, y = 1, fill = color)) +
  geom_tile() +
  scale_fill_identity() +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "Black to Gray Gradient") +
  coord_fixed(ratio = 10)  # Adjust the ratio to make the bar look better

# Time Series of PM with air pollutant tracers --------------------------------------------------------
mean.CO <- mean(daily_data$CO, na.rm = T) # in ppm
sd.CO <- sd(weekly_data$CO, na.rm = T) # in ppm
mean.Ozone <- mean(daily_data$Ozone, na.rm = T) # in ppm
sd.Ozone <- sd(daily_data$Ozone, na.rm = T) # in ppm
mean.Total_NOx <- mean(daily_data$Total_NOx, na.rm = T) # in ppb
sd.Total_NOx <- sd(daily_data$Total_NOx, na.rm = T) # in ppb
mean.PO4 <- mean(daily_data$PO4, na.rm = T) # in ug m-3
sd.PO4 <- sd(daily_data$PO4, na.rm = T) # in ug m-3
mean.SO4 <- mean(daily_data$SO4, na.rm = T) # in ppb
sd.SO4 <- sd(daily_data$SO4, na.rm = T) # in ppb
mean.PM <- mean(daily_data$pm2.5_CONUS, na.rm = T) # in ug m-3
sd.PM <- sd(daily_data$pm2.5_CONUS, na.rm = T) # in ug m-3

CO.timeseries <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = CO * 40), color = "black") +
  geom_abline(intercept = mean.CO* 40, slope = 0, color = "black", linetype = "dashed") +
  geom_abline(intercept = (mean.CO*40)+(sd.CO*40), slope = 0, color = "black", linetype = "dotted") +
  geom_abline(intercept = (mean.CO*40)-(sd.CO*40), slope = 0, color = "black", linetype = "dotted") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./40, name = "Carbon Monoxide (ppm)")) + 
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank())

O3.timeseries <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = Ozone * 400), color = "black") +
  geom_abline(intercept = mean.Ozone* 400, slope = 0, color = "black", linetype = "dashed") +
  geom_abline(intercept = (mean.Ozone*400)+(sd.Ozone*400), slope = 0, color = "black", linetype = "dotted") +
  geom_abline(intercept = (mean.Ozone*400)-(sd.Ozone*400), slope = 0, color = "black", linetype = "dotted") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./400, name = "Ozone (ppm)")) + 
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank())

NOx.timeseries <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = Total_NOx* 20), color = "black") +
  geom_abline(intercept = mean.Total_NOx* 20, slope = 0, color = "black", linetype = "dashed") +
  geom_abline(intercept = (mean.Total_NOx*20)+(sd.Total_NOx*20), slope = 0, color = "black", linetype = "dotted") +
  geom_abline(intercept = (mean.Total_NOx*20)-(sd.Total_NOx*20), slope = 0, color = "black", linetype = "dotted") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./20, name = "Total NOx (ppb)")) + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

PO4.timeseries <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = PO4 * 4000), color = "black") +
  geom_abline(intercept = mean.PO4 * 4000, slope = 0, color = "black", linetype = "dashed") +
  geom_abline(intercept = (mean.PO4 * 4000)+(sd.PO4 * 4000), slope = 0, color = "black", linetype = "dotted") +
  geom_abline(intercept = (mean.PO4 * 4000)-(sd.PO4 * 4000), slope = 0, color = "black", linetype = "dotted") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./4000, name = "Phosphate (ug m-3)")) +    theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) 

SO4.timeseries <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = SO4 * 10), color = "black") +
  geom_abline(intercept = mean.SO4 * 10, slope = 0, color = "black", linetype = "dashed") +
  geom_abline(intercept = (mean.SO4 * 10)+(sd.SO4 * 10), slope = 0, color = "black", linetype = "dotted") +
  geom_abline(intercept = (mean.SO4 * 10)-(sd.SO4 * 10), slope = 0, color = "black", linetype = "dotted") +
  theme_minimal() +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./10, name = "Sulfate (ppb)")) +  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") 

PM.only.timeseries <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_abline(intercept = mean.PM, slope = 0, color = "black", linetype = "dashed") +
  geom_abline(intercept = (mean.PM)+(sd.PM), slope = 0, color = "black", linetype = "dotted") +
  geom_abline(intercept = (mean.PM)-(sd.PM), slope = 0, color = "black", linetype = "dotted") +
  geom_abline(intercept = 9, slope = 0, color = "red") +
  theme_minimal() +
  scale_color_manual(values = sensor.name.palette) +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())

library(patchwork)
CO.timeseries / O3.timeseries / NOx.timeseries / PO4.timeseries / SO4.timeseries / PM.only.timeseries

# HAP Time series with seasonal averages -------------------------------------  
library(tidyverse)
weekly_data$week <- as.Date(weekly_data$week)

annual_avgs <- daily_data %>%
  summarise(CO_mean = mean(CO, na.rm = TRUE), # in ppm
            CO_sd = sd(CO, na.rm = TRUE),
            Ozone_mean = mean(Ozone, na.rm = TRUE), # in ppb
            Ozone_sd = sd(Ozone, na.rm = TRUE),
            SO4_mean = mean(SO4, na.rm = TRUE), # in ppb
            SO4_sd = sd(SO4, na.rm = TRUE),
            PO4_mean = mean(PO4, na.rm = TRUE), # in ug m-3
            PO4_sd = sd(PO4, na.rm = TRUE),
            Total_NOx_mean = mean(Total_NOx, na.rm = TRUE), # in ppb
            Total_NOx_sd = sd(Total_NOx, na.rm = TRUE),
            PM_mean = mean(pm2.5_CONUS, na.rm = TRUE), # in ug m-3
            PM_sd = sd(pm2.5_CONUS, na.rm = TRUE),
            EPA_PM_mean = mean(EPA_PM2.5, na.rm = TRUE), # in ug m-3
            EPA_PM_sd = sd(EPA_PM2.5, na.rm = TRUE)) %>% 
  mutate(time_period = "annual") 

seasonal_avgs <- daily_data %>%
  group_by(season) %>%
  summarise(CO_mean = mean(CO, na.rm = TRUE),
            CO_sd = sd(CO, na.rm = TRUE),
            Ozone_mean = mean(Ozone, na.rm = TRUE),
            Ozone_sd = sd(Ozone, na.rm = TRUE),
            SO4_mean = mean(SO4, na.rm = TRUE),
            SO4_sd = sd(SO4, na.rm = TRUE),
            PO4_mean = mean(PO4, na.rm = TRUE),
            PO4_sd = sd(PO4, na.rm = TRUE),
            Total_NOx_mean = mean(Total_NOx, na.rm = TRUE),
            Total_NOx_sd = sd(Total_NOx, na.rm = TRUE),
            PM_mean = mean(pm2.5_CONUS, na.rm = TRUE), # in ug m-3
            PM_sd = sd(pm2.5_CONUS, na.rm = TRUE),
            EPA_PM_mean = mean(EPA_PM2.5, na.rm = TRUE), # in ug m-3
            EPA_PM_sd = sd(EPA_PM2.5, na.rm = TRUE)) %>% 
  rename(time_period = season) 

monthly_avgs <- daily_data %>%
  group_by(month) %>%
  summarise(CO_mean = mean(CO, na.rm = TRUE),
            CO_sd = sd(CO, na.rm = TRUE),
            Ozone_mean = mean(Ozone, na.rm = TRUE),
            Ozone_sd = sd(Ozone, na.rm = TRUE),
            SO4_mean = mean(SO4, na.rm = TRUE),
            SO4_sd = sd(SO4, na.rm = TRUE),
            PO4_mean = mean(PO4, na.rm = TRUE),
            PO4_sd = sd(PO4, na.rm = TRUE),
            Total_NOx_mean = mean(Total_NOx, na.rm = TRUE),
            Total_NOx_sd = sd(Total_NOx, na.rm = TRUE),
            PM_mean = mean(pm2.5_CONUS, na.rm = TRUE), # in ug m-3
            PM_sd = sd(pm2.5_CONUS, na.rm = TRUE),
            EPA_PM_mean = mean(EPA_PM2.5, na.rm = TRUE), # in ug m-3
            EPA_PM_sd = sd(EPA_PM2.5, na.rm = TRUE)) %>% 
  rename(time_period = month) 

all_avgs <- rbind(monthly_avgs, seasonal_avgs, annual_avgs)

summer.only <- all_avgs %>% filter(time_period == "Summer")
fall.only <- all_avgs %>% filter(time_period == "Fall")
winter.only <- all_avgs %>% filter(time_period == "Winter")
spring.only <- all_avgs %>% filter(time_period == "Spring")

CO.timeseries.seasonal <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) +
  geom_line(aes(x = date, y = CO * 40), color = "black") +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey90")) +
  scale_color_manual(values = sensor.name.palette) +
    scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./40, name = "Carbon Monoxide (ppm)")) +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) +
  geom_segment(aes(x = as.Date('2022-6-01'), xend = as.Date('2022-8-31'), 
                   y = summer.only$CO_mean*40, yend = summer.only$CO_mean*40), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-6-01'), xend = as.Date('2022-8-31'), 
                   y = summer.only$CO_mean*40+summer.only$CO_sd*40, yend = summer.only$CO_mean*40+summer.only$CO_sd*40), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2022-9-01'), xend = as.Date('2022-11-30'), 
                   y = fall.only$CO_mean*40, yend = fall.only$CO_mean*40), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-9-01'), xend = as.Date('2022-11-30'), 
                   y = fall.only$CO_mean*40+fall.only$CO_sd*40, yend = fall.only$CO_mean*40+fall.only$CO_sd*40), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2022-12-01'), xend = as.Date('2023-02-28'), 
                   y = winter.only$CO_mean*40, yend = winter.only$CO_mean*40), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-12-01'), xend = as.Date('2023-02-28'), 
                   y = winter.only$CO_mean*40+winter.only$CO_sd*40, yend = winter.only$CO_mean*40+winter.only$CO_sd*40), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-03-01'), xend = as.Date('2023-05-31'), 
                   y = spring.only$CO_mean*40, yend = spring.only$CO_mean*40), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-03-01'), xend = as.Date('2023-05-31'), 
                   y = spring.only$CO_mean*40+spring.only$CO_sd*40, yend = spring.only$CO_mean*40+spring.only$CO_sd*40), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-06-01'), xend = as.Date('2023-08-31'), 
                   y = summer.only$CO_mean*40, yend = summer.only$CO_mean*40), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-06-01'), xend = as.Date('2023-08-31'), 
                   y = summer.only$CO_mean*40+summer.only$CO_sd*40, yend = summer.only$CO_mean*40+summer.only$CO_sd*40), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-09-01'), xend = as.Date('2023-11-30'), 
                   y = fall.only$CO_mean*40, yend = fall.only$CO_mean*40), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-09-01'), xend = as.Date('2023-11-30'), 
                   y = fall.only$CO_mean*40+fall.only$CO_sd*40, yend = fall.only$CO_mean*40+fall.only$CO_sd*40), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-12-01'), xend = as.Date('2023-12-31'), 
                   y = winter.only$CO_mean*40, yend = winter.only$CO_mean*40), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-12-01'), xend = as.Date('2023-12-31'), 
                   y = winter.only$CO_mean*40+winter.only$CO_sd*40, yend = winter.only$CO_mean*40+winter.only$CO_sd*40), 
               color = "black", linetype = "dotted") 

O3.timeseries.seasonal <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) +
  geom_line(aes(x = date, y = Ozone * 400), color = "black") +
  theme_minimal() +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./400, name = "Ozone (ppm)")) +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  theme(panel.grid.major = element_line(colour = "grey90")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) +
  geom_segment(aes(x = as.Date('2022-6-01'), xend = as.Date('2022-8-31'), 
                   y = summer.only$Ozone_mean*400, yend = summer.only$Ozone_mean*400), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-6-01'), xend = as.Date('2022-8-31'), 
                   y = summer.only$Ozone_mean*400+summer.only$Ozone_sd*400, yend = summer.only$Ozone_mean*400+summer.only$Ozone_sd*400), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2022-9-01'), xend = as.Date('2022-11-30'), 
                   y = fall.only$Ozone_mean*400, yend = fall.only$Ozone_mean*400), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-9-01'), xend = as.Date('2022-11-30'), 
                   y = fall.only$Ozone_mean*400+fall.only$Ozone_sd*400, yend = fall.only$Ozone_mean*400+fall.only$Ozone_sd*400), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-03-01'), xend = as.Date('2023-05-31'), 
                   y = spring.only$Ozone_mean*400, yend = spring.only$Ozone_mean*400), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-03-01'), xend = as.Date('2023-05-31'), 
                   y = spring.only$Ozone_mean*400+spring.only$Ozone_sd*400, yend = spring.only$Ozone_mean*400+spring.only$Ozone_sd*400), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-06-01'), xend = as.Date('2023-08-31'), 
                   y = summer.only$Ozone_mean*400, yend = summer.only$Ozone_mean*400), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-06-01'), xend = as.Date('2023-08-31'), 
                   y = summer.only$Ozone_mean*400+summer.only$Ozone_sd*400, yend = summer.only$Ozone_mean*400+summer.only$Ozone_sd*400), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-09-01'), xend = as.Date('2023-11-30'), 
                   y = fall.only$Ozone_mean*400, yend = fall.only$Ozone_mean*400), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-09-01'), xend = as.Date('2023-11-30'), 
                   y = fall.only$Ozone_mean*400+fall.only$Ozone_sd*400, yend = fall.only$Ozone_mean*400+fall.only$Ozone_sd*400), 
               color = "black", linetype = "dotted") 
  
NOx.timeseries.seasonal <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) +
  geom_line(aes(x = date, y = Total_NOx * 20), color = "black") +
  theme_minimal() +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./20, name = "NOx (ppb)")) +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major = element_line(colour = "grey90")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) +
  geom_segment(aes(x = as.Date('2022-6-01'), xend = as.Date('2022-8-31'), 
                   y = summer.only$Total_NOx_mean*20, yend = summer.only$Total_NOx_mean*20), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-6-01'), xend = as.Date('2022-8-31'), 
                   y = summer.only$Total_NOx_mean*20+summer.only$Total_NOx_sd*20, yend = summer.only$Total_NOx_mean*20+summer.only$Total_NOx_sd*20), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2022-9-01'), xend = as.Date('2022-11-30'), 
                   y = fall.only$Total_NOx_mean*20, yend = fall.only$Total_NOx_mean*20), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-9-01'), xend = as.Date('2022-11-30'), 
                   y = fall.only$Total_NOx_mean*20+fall.only$Total_NOx_sd*20, yend = fall.only$Total_NOx_mean*20+fall.only$Total_NOx_sd*20), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2022-12-01'), xend = as.Date('2023-02-28'), 
                   y = winter.only$Total_NOx_mean*20, yend = winter.only$Total_NOx_mean*20), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-12-01'), xend = as.Date('2023-02-28'), 
                   y = winter.only$Total_NOx_mean*20+winter.only$Total_NOx_sd*20, yend = winter.only$Total_NOx_mean*20+winter.only$Total_NOx_sd*20), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-03-01'), xend = as.Date('2023-05-31'), 
                   y = spring.only$Total_NOx_mean*20, yend = spring.only$Total_NOx_mean*20), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-03-01'), xend = as.Date('2023-05-31'), 
                   y = spring.only$Total_NOx_mean*20+spring.only$Total_NOx_sd*20, yend = spring.only$Total_NOx_mean*20+spring.only$Total_NOx_sd*20), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-06-01'), xend = as.Date('2023-08-31'), 
                   y = summer.only$Total_NOx_mean*20, yend = summer.only$Total_NOx_mean*20), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-06-01'), xend = as.Date('2023-08-31'), 
                   y = summer.only$Total_NOx_mean*20+summer.only$Total_NOx_sd*20, yend = summer.only$Total_NOx_mean*20+summer.only$Total_NOx_sd*20), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-09-01'), xend = as.Date('2023-11-30'), 
                   y = fall.only$Total_NOx_mean*20, yend = fall.only$Total_NOx_mean*20), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-09-01'), xend = as.Date('2023-11-30'), 
                   y = fall.only$Total_NOx_mean*20+fall.only$Total_NOx_sd*20, yend = fall.only$Total_NOx_mean*20+fall.only$Total_NOx_sd*20), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-12-01'), xend = as.Date('2023-12-31'), 
                   y = winter.only$Total_NOx_mean*20, yend = winter.only$Total_NOx_mean*20), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-12-01'), xend = as.Date('2023-12-31'), 
                   y = winter.only$Total_NOx_mean*20+winter.only$Total_NOx_sd*20, yend = winter.only$Total_NOx_mean*20+winter.only$Total_NOx_sd*20), 
               color = "black", linetype = "dotted") 

PO4.timeseries.seasonal <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) +
  geom_line(aes(x = date, y = PO4 * 4000), color = "black") +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey90")) +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./4000, name = "PO4 (ug m-3)")) +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) +
  geom_segment(aes(x = as.Date('2022-6-01'), xend = as.Date('2022-8-31'), 
                   y = summer.only$PO4_mean*4000, yend = summer.only$PO4_mean*4000), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-6-01'), xend = as.Date('2022-8-31'), 
                   y = summer.only$PO4_mean*4000+summer.only$PO4_sd*4000, yend = summer.only$PO4_mean*4000+summer.only$PO4_sd*4000), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2022-9-01'), xend = as.Date('2022-11-30'), 
                   y = fall.only$PO4_mean*4000, yend = fall.only$PO4_mean*4000), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-9-01'), xend = as.Date('2022-11-30'), 
                   y = fall.only$PO4_mean*4000+fall.only$PO4_sd*4000, yend = fall.only$PO4_mean*4000+fall.only$PO4_sd*4000), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2022-12-01'), xend = as.Date('2023-02-28'), 
                   y = winter.only$PO4_mean*4000, yend = winter.only$PO4_mean*4000), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-12-01'), xend = as.Date('2023-02-28'), 
                   y = winter.only$PO4_mean*4000+winter.only$PO4_sd*4000, yend = winter.only$PO4_mean*4000+winter.only$PO4_sd*4000), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-03-01'), xend = as.Date('2023-05-31'), 
                   y = spring.only$PO4_mean*4000, yend = spring.only$PO4_mean*4000), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-03-01'), xend = as.Date('2023-05-31'), 
                   y = spring.only$PO4_mean*4000+spring.only$PO4_sd*4000, yend = spring.only$PO4_mean*4000+spring.only$PO4_sd*4000), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-06-01'), xend = as.Date('2023-08-31'), 
                   y = summer.only$PO4_mean*4000, yend = summer.only$PO4_mean*4000), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-06-01'), xend = as.Date('2023-08-31'), 
                   y = summer.only$PO4_mean*4000+summer.only$PO4_sd*4000, yend = summer.only$PO4_mean*4000+summer.only$PO4_sd*4000), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-09-01'), xend = as.Date('2023-11-30'), 
                   y = fall.only$PO4_mean*4000, yend = fall.only$PO4_mean*4000), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-09-01'), xend = as.Date('2023-11-30'), 
                   y = fall.only$PO4_mean*4000+fall.only$PO4_sd*4000, yend = fall.only$PO4_mean*4000+fall.only$PO4_sd*4000), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-12-01'), xend = as.Date('2023-12-31'), 
                   y = winter.only$PO4_mean*4000, yend = winter.only$PO4_mean*4000), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-12-01'), xend = as.Date('2023-12-31'), 
                   y = winter.only$PO4_mean*4000+winter.only$PO4_sd*4000, yend = winter.only$PO4_mean*4000+winter.only$PO4_sd*4000), 
               color = "black", linetype = "dotted") 

SO4.timeseries.seasonal <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) +
  geom_line(aes(x = date, y = SO4 * 10), color = "black") +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey90")) +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./10, name = "SO4 (ppb)")) +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank()) +
  geom_segment(aes(x = as.Date('2022-6-01'), xend = as.Date('2022-8-31'), 
                   y = summer.only$SO4_mean*10, yend = summer.only$SO4_mean*10), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-6-01'), xend = as.Date('2022-8-31'), 
                   y = summer.only$SO4_mean*10+summer.only$SO4_sd*10, yend = summer.only$SO4_mean*10+summer.only$SO4_sd*10), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2022-9-01'), xend = as.Date('2022-11-30'), 
                   y = fall.only$SO4_mean*10, yend = fall.only$SO4_mean*10), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-9-01'), xend = as.Date('2022-11-30'), 
                   y = fall.only$SO4_mean*10+fall.only$SO4_sd*10, yend = fall.only$SO4_mean*10+fall.only$SO4_sd*10), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2022-12-01'), xend = as.Date('2023-02-28'), 
                   y = winter.only$SO4_mean*10, yend = winter.only$SO4_mean*10), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-12-01'), xend = as.Date('2023-02-28'), 
                   y = winter.only$SO4_mean*10+winter.only$SO4_sd*10, yend = winter.only$SO4_mean*10+winter.only$SO4_sd*10), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-03-01'), xend = as.Date('2023-05-31'), 
                   y = spring.only$SO4_mean*10, yend = spring.only$SO4_mean*10), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-03-01'), xend = as.Date('2023-05-31'), 
                   y = spring.only$SO4_mean*10+spring.only$SO4_sd*10, yend = spring.only$SO4_mean*10+spring.only$SO4_sd*10), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-06-01'), xend = as.Date('2023-08-31'), 
                   y = summer.only$SO4_mean*10, yend = summer.only$SO4_mean*10), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-06-01'), xend = as.Date('2023-08-31'), 
                   y = summer.only$SO4_mean*10+summer.only$SO4_sd*10, yend = summer.only$SO4_mean*10+summer.only$SO4_sd*10), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-09-01'), xend = as.Date('2023-11-30'), 
                   y = fall.only$SO4_mean*10, yend = fall.only$SO4_mean*10), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-09-01'), xend = as.Date('2023-11-30'), 
                   y = fall.only$SO4_mean*10+fall.only$SO4_sd*10, yend = fall.only$SO4_mean*10+fall.only$SO4_sd*10), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-12-01'), xend = as.Date('2023-12-31'), 
                   y = winter.only$SO4_mean*10, yend = winter.only$SO4_mean*10), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-12-01'), xend = as.Date('2023-12-31'), 
                   y = winter.only$SO4_mean*10+winter.only$SO4_sd*10, yend = winter.only$SO4_mean*10+winter.only$SO4_sd*10), 
               color = "black", linetype = "dotted") 

PM.only.timeseries.seasonal <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) +
  geom_segment(aes(x = as.Date('2022-6-01'), xend = as.Date('2022-8-31'), 
                   y = summer.only$PM_mean, yend = summer.only$PM_mean), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-6-01'), xend = as.Date('2022-8-31'), 
                   y = summer.only$PM_mean+summer.only$PM_sd, yend = summer.only$PM_mean+summer.only$PM_sd), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2022-9-01'), xend = as.Date('2022-11-30'), 
                   y = fall.only$PM_mean, yend = fall.only$PM_mean), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-9-01'), xend = as.Date('2022-11-30'), 
                   y = fall.only$PM_mean+fall.only$PM_sd, yend = fall.only$PM_mean+fall.only$PM_sd), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2022-12-01'), xend = as.Date('2023-02-28'), 
                   y = winter.only$PM_mean, yend = winter.only$PM_mean), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2022-12-01'), xend = as.Date('2023-02-28'), 
                   y = winter.only$PM_mean+winter.only$PM_sd, yend = winter.only$PM_mean+winter.only$PM_sd), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-03-01'), xend = as.Date('2023-05-31'), 
                   y = spring.only$PM_mean, yend = spring.only$PM_mean), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-03-01'), xend = as.Date('2023-05-31'), 
                   y = spring.only$PM_mean+spring.only$PM_sd, yend = spring.only$PM_mean+spring.only$PM_sd), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-06-01'), xend = as.Date('2023-08-31'), 
                   y = summer.only$PM_mean, yend = summer.only$PM_mean), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-06-01'), xend = as.Date('2023-08-31'), 
                   y = summer.only$PM_mean+summer.only$PM_sd, yend = summer.only$PM_mean+summer.only$PM_sd), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-09-01'), xend = as.Date('2023-11-30'), 
                   y = fall.only$PM_mean, yend = fall.only$PM_mean), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-09-01'), xend = as.Date('2023-11-30'), 
                   y = fall.only$PM_mean+fall.only$PM_sd, yend = fall.only$PM_mean+fall.only$PM_sd), 
               color = "black", linetype = "dotted") +
  geom_segment(aes(x = as.Date('2023-12-01'), xend = as.Date('2023-12-31'), 
                   y = winter.only$PM_mean, yend = winter.only$PM_mean), 
               color = "black", linetype = "dashed") +
  geom_segment(aes(x = as.Date('2023-12-01'), xend = as.Date('2023-12-31'), 
                   y = winter.only$PM_mean+winter.only$PM_sd, yend = winter.only$PM_mean+winter.only$PM_sd), 
               color = "black", linetype = "dotted") +
  #geom_abline(intercept = 9, slope = 0, color = "red") +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey90")) +
  scale_color_manual(values = sensor.name.palette) +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())

CO.timeseries.seasonal / NOx.timeseries.seasonal / O3.timeseries.seasonal / PO4.timeseries.seasonal / SO4.timeseries.seasonal / PM.only.timeseries.seasonal

# HAP time series with year specific seasonal averages ----------------------------------
annual_avgs <- daily_data %>%
  group_by(Year) %>%
  summarise(CO_mean = mean(CO, na.rm = TRUE), # in ppm
            CO_sd = sd(CO, na.rm = TRUE),
            Ozone_mean = mean(Ozone, na.rm = TRUE), # in ppb
            Ozone_sd = sd(Ozone, na.rm = TRUE),
            SO4_mean = mean(SO4, na.rm = TRUE), # in ppb
            SO4_sd = sd(SO4, na.rm = TRUE),
            PO4_mean = mean(PO4, na.rm = TRUE), # in ug m-3
            PO4_sd = sd(PO4, na.rm = TRUE),
            Total_NOx_mean = mean(Total_NOx, na.rm = TRUE), # in ppb
            Total_NOx_sd = sd(Total_NOx, na.rm = TRUE),
            PM_mean = mean(pm2.5_CONUS, na.rm = TRUE), # in ug m-3
            PM_sd = sd(pm2.5_CONUS, na.rm = TRUE),
            EPA_PM_mean = mean(EPA_PM2.5, na.rm = TRUE), # in ug m-3
            EPA_PM_sd = sd(EPA_PM2.5, na.rm = TRUE)) %>% 
  mutate(time_period = "annual") %>% drop_na(Year)

seasonal_avgs <- daily_data %>%
  group_by(season, Year) %>%
  summarise(CO_mean = mean(CO, na.rm = TRUE),
            CO_sd = sd(CO, na.rm = TRUE),
            Ozone_mean = mean(Ozone, na.rm = TRUE),
            Ozone_sd = sd(Ozone, na.rm = TRUE),
            SO4_mean = mean(SO4, na.rm = TRUE),
            SO4_sd = sd(SO4, na.rm = TRUE),
            PO4_mean = mean(PO4, na.rm = TRUE),
            PO4_sd = sd(PO4, na.rm = TRUE),
            Total_NOx_mean = mean(Total_NOx, na.rm = TRUE),
            Total_NOx_sd = sd(Total_NOx, na.rm = TRUE),
            PM_mean = mean(pm2.5_CONUS, na.rm = TRUE), # in ug m-3
            PM_sd = sd(pm2.5_CONUS, na.rm = TRUE),
            EPA_PM_mean = mean(EPA_PM2.5, na.rm = TRUE), # in ug m-3
            EPA_PM_sd = sd(EPA_PM2.5, na.rm = TRUE)) %>% 
  rename(time_period = season) %>% drop_na(Year)

monthly_avgs <- daily_data %>%
  group_by(month, Year) %>%
  summarise(CO_mean = mean(CO, na.rm = TRUE),
            CO_sd = sd(CO, na.rm = TRUE),
            Ozone_mean = mean(Ozone, na.rm = TRUE),
            Ozone_sd = sd(Ozone, na.rm = TRUE),
            SO4_mean = mean(SO4, na.rm = TRUE),
            SO4_sd = sd(SO4, na.rm = TRUE),
            PO4_mean = mean(PO4, na.rm = TRUE),
            PO4_sd = sd(PO4, na.rm = TRUE),
            Total_NOx_mean = mean(Total_NOx, na.rm = TRUE),
            Total_NOx_sd = sd(Total_NOx, na.rm = TRUE),
            PM_mean = mean(pm2.5_CONUS, na.rm = TRUE), # in ug m-3
            PM_sd = sd(pm2.5_CONUS, na.rm = TRUE),
            EPA_PM_mean = mean(EPA_PM2.5, na.rm = TRUE), # in ug m-3
            EPA_PM_sd = sd(EPA_PM2.5, na.rm = TRUE)) %>% 
  rename(time_period = month) %>% drop_na(Year)

# Sensitivity Analysis for Correction Factors of PurpleAir data ------------------------
# join with EPA data 
EPA_airqual_data <- daily_data %>% select(Name, date, EPA_PM2.5)

# Read in the data 
daily_data_current_workflow <- read.csv("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\compiled_purpleair_data_currentwf.csv") %>% select(-X) %>% mutate(Name = as.character(Name), date = as.Date(date)) %>% left_join(EPA_airqual_data, by = c("Name", "date"))
daily_data_no_cleaning <- read.csv("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\compiled_purpleair_data_no_removal.csv") %>% select(-X) %>% mutate(Name = as.character(Name), date = as.Date(date)) %>% left_join(EPA_airqual_data, by = c("Name", "date"))
daily_data_most_stringent <- read.csv("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\compiled_purpleair_data_most_stringent.csv") %>% select(-X) %>% mutate(Name = as.character(Name), date = as.Date(date)) %>% left_join(EPA_airqual_data, by = c("Name", "date"))
daily_data_MLR_model4 <- read.csv("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\compiled_purpleair_data_Model4_MLR.csv") %>% select(-X) %>% mutate(Name = as.character(Name), date = as.Date(date)) %>% left_join(EPA_airqual_data, by = c("Name", "date"))

# Comparing EPA data to purpleair data -------------------------------
range_summary <- daily_data %>%
  #group_by(Name) %>%
  summarize(
    raw_PM2.5_min = min(pm2.5_avg, na.rm = TRUE),
    raw_PM2.5_max = max(pm2.5_avg, na.rm = TRUE),
    frm_PM2.5_min = min(EPA_PM2.5, na.rm = TRUE),
    frm_PM2.5_max = max(EPA_PM2.5, na.rm = TRUE),
    CONUS_PM2.5_min = min(pm2.5_CONUS, na.rm = TRUE),
    CONUS_PM2.5_max = max(pm2.5_CONUS, na.rm = TRUE),
    SE_PM2.5_min = min(pm2.5_SE, na.rm = TRUE),
    SE_PM2.5_max = max(pm2.5_SE, na.rm = TRUE))

overall_stats <- daily_data %>%
  #group_by(Name) %>% 
  summarize(
    raw_PM2.5_mean = mean(pm2.5_avg, na.rm = TRUE),
    raw_PM2.5_sd = sd(pm2.5_avg, na.rm = TRUE),
    frm_PM2.5_mean = mean(EPA_PM2.5, na.rm = TRUE),
    frm_PM2.5_sd = sd(EPA_PM2.5, na.rm = TRUE),
    CONUS_PM2.5_mean = mean(pm2.5_CONUS, na.rm = TRUE),
    CONUS_PM2.5_sd = sd(pm2.5_CONUS, na.rm = TRUE),
    SE_PM2.5_mean = mean(pm2.5_SE, na.rm = TRUE),
    SE_PM2.5_sd = sd(pm2.5_SE, na.rm = TRUE)) 

grouped_stats <- daily_data %>%
  group_by(Name) %>% 
  summarize(
    raw_PM2.5_mean = mean(pm2.5_avg, na.rm = TRUE),
    raw_PM2.5_sd = sd(pm2.5_avg, na.rm = TRUE),
    frm_PM2.5_mean = mean(EPA_PM2.5, na.rm = TRUE),
    frm_PM2.5_sd = sd(EPA_PM2.5, na.rm = TRUE),
    CONUS_PM2.5_mean = mean(pm2.5_CONUS, na.rm = TRUE),
    CONUS_PM2.5_sd = sd(pm2.5_CONUS, na.rm = TRUE),
    SE_PM2.5_mean = mean(pm2.5_SE, na.rm = TRUE),
    SE_PM2.5_sd = sd(pm2.5_SE, na.rm = TRUE))

grouped_stats_v1 <- daily_data_current_workflow %>%
  group_by(Name) %>% 
  summarize(
    raw_PM2.5_mean = mean(pm2.5_avg, na.rm = TRUE),
    raw_PM2.5_sd = sd(pm2.5_avg, na.rm = TRUE),
    frm_PM2.5_mean = mean(EPA_PM2.5, na.rm = TRUE),
    frm_PM2.5_sd = sd(EPA_PM2.5, na.rm = TRUE),
    CONUS_PM2.5_mean = mean(pm2.5_CONUS, na.rm = TRUE),
    CONUS_PM2.5_sd = sd(pm2.5_CONUS, na.rm = TRUE),
    SE_PM2.5_mean = mean(pm2.5_SE, na.rm = TRUE),
    SE_PM2.5_sd = sd(pm2.5_SE, na.rm = TRUE))

grouped_stats_v2 <- daily_data_no_cleaning %>%
  group_by(Name) %>% 
  summarize(
    raw_PM2.5_mean = mean(pm2.5_avg, na.rm = TRUE),
    raw_PM2.5_sd = sd(pm2.5_avg, na.rm = TRUE),
    frm_PM2.5_mean = mean(EPA_PM2.5, na.rm = TRUE),
    frm_PM2.5_sd = sd(EPA_PM2.5, na.rm = TRUE),
    CONUS_PM2.5_mean = mean(pm2.5_CONUS, na.rm = TRUE),
    CONUS_PM2.5_sd = sd(pm2.5_CONUS, na.rm = TRUE),
    SE_PM2.5_mean = mean(pm2.5_SE, na.rm = TRUE),
    SE_PM2.5_sd = sd(pm2.5_SE, na.rm = TRUE))

grouped_stats_v3 <- daily_data_most_stringent %>%
  group_by(Name) %>% 
  summarize(
    raw_PM2.5_mean = mean(pm2.5_avg, na.rm = TRUE),
    raw_PM2.5_sd = sd(pm2.5_avg, na.rm = TRUE),
    frm_PM2.5_mean = mean(EPA_PM2.5, na.rm = TRUE),
    frm_PM2.5_sd = sd(EPA_PM2.5, na.rm = TRUE),
    CONUS_PM2.5_mean = mean(pm2.5_CONUS, na.rm = TRUE),
    CONUS_PM2.5_sd = sd(pm2.5_CONUS, na.rm = TRUE),
    SE_PM2.5_mean = mean(pm2.5_SE, na.rm = TRUE),
    SE_PM2.5_sd = sd(pm2.5_SE, na.rm = TRUE))

grouped_stats_v4 <- daily_data_MLR_model4 %>%
  group_by(Name) %>% 
  summarize(
    raw_PM2.5_mean = mean(pm2.5_avg, na.rm = TRUE),
    raw_PM2.5_sd = sd(pm2.5_avg, na.rm = TRUE),
    frm_PM2.5_mean = mean(EPA_PM2.5, na.rm = TRUE),
    frm_PM2.5_sd = sd(EPA_PM2.5, na.rm = TRUE),
    CONUS_PM2.5_mean = mean(pm2.5_CONUS, na.rm = TRUE),
    CONUS_PM2.5_sd = sd(pm2.5_CONUS, na.rm = TRUE),
    SE_PM2.5_mean = mean(pm2.5_SE, na.rm = TRUE),
    SE_PM2.5_sd = sd(pm2.5_SE, na.rm = TRUE))

pm.comparison.RH.corr <- ggplot(daily_data) + 
  geom_point(aes(y = `EPA_PM2.5`, x = pm2.5_CONUS, color = Name)) + 
  geom_smooth(aes(y = `EPA_PM2.5`, x = pm2.5_CONUS), method='lm')+
  xlim(0,40) + 
  ylim(0,40) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
  labs(title = "CONUS corrected by RH")

pm.comparison.NOT.RH.corr <- ggplot(daily_data) + 
  geom_point(aes(y = `EPA_PM2.5`, x = pm2.5_avg, color = Name)) + 
  geom_smooth(aes(y = `EPA_PM2.5`, x = pm2.5_avg), method='lm')+
  xlim(0,40) + 
  ylim(0,40) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
  labs(title = "not corrected by RH")

pm.comparison.SE.RH.corr <- ggplot(daily_data) + 
  geom_point(aes(y = `EPA_PM2.5`, x = pm2.5_SE, color = Name)) + 
  geom_smooth(aes(y = `EPA_PM2.5`, x = pm2.5_SE), method='lm')+
  xlim(0,40) + 
  ylim(0,40) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
  labs(title = "SE corrected by RH")

library(purrr)
pm.comparison.SE.RH.corr + pm.comparison.RH.corr + pm.comparison.NOT.RH.corr  

# single point comparison for each sensor 
# swapping daily_data for daily_data_current_workflow, daily_data_most_stringent, daily_data_no_cleaning, daily_data_MLR_model4
library(tidyr)
sensor_count <- daily_data %>% drop_na(`pm2.5_CONUS`) %>% group_by(Name) %>% summarize(count = n())

RH.corr.pm.single.point <- daily_data %>% drop_na(pm2.5_CONUS) %>%
  dplyr::select(Name, `pm2.5_CONUS`, `EPA_PM2.5`) %>%
  group_by(Name) %>%
  mutate(mean_EPA = mean(`EPA_PM2.5`), sd_EPA = sd(`EPA_PM2.5`), mean_purp = mean(`pm2.5_CONUS`), sd_purp = sd(`pm2.5_CONUS`)) %>%
  dplyr::select(-`EPA_PM2.5`, -`pm2.5_CONUS`) %>%
  mutate(xmin = (mean_purp-sd_purp), xmax = (mean_purp+sd_purp), ymin = (mean_EPA-sd_EPA), ymax = (mean_EPA+sd_EPA)) %>% 
  unique() %>% 
  ungroup() %>% 
  full_join(sensor_count) %>% 
  mutate(count = as.numeric(count))

SE.RH.corr.pm.single.point <- daily_data %>% drop_na(pm2.5_SE) %>%
  dplyr::select(Name, `pm2.5_SE`, `EPA_PM2.5`) %>%
  group_by(Name) %>%
  mutate(mean_EPA = mean(`EPA_PM2.5`), sd_EPA = sd(`EPA_PM2.5`), mean_purp = mean(`pm2.5_SE`), sd_purp = sd(`pm2.5_SE`)) %>%
  dplyr::select(-`EPA_PM2.5`, -`pm2.5_SE`) %>%
  mutate(xmin = (mean_purp-sd_purp), xmax = (mean_purp+sd_purp), ymin = (mean_EPA-sd_EPA), ymax = (mean_EPA+sd_EPA)) %>% 
  unique() %>% 
  ungroup() %>% 
  full_join(sensor_count) %>% 
  mutate(count = as.numeric(count))
 
not.RH.corr.pm.single.point <- daily_data %>% drop_na(pm2.5_avg) %>%
  dplyr::select(Name, `pm2.5_avg`, `EPA_PM2.5`) %>%
  group_by(Name) %>%
  mutate(mean_EPA = mean(`EPA_PM2.5`), sd_EPA = sd(`EPA_PM2.5`), mean_purp = mean(`pm2.5_avg`), sd_purp = sd(`pm2.5_avg`)) %>%
  dplyr::select(-`EPA_PM2.5`, -`pm2.5_avg`) %>%
  mutate(xmin = (mean_purp-sd_purp), xmax = (mean_purp+sd_purp), ymin = (mean_EPA-sd_EPA), ymax = (mean_EPA+sd_EPA)) %>% 
  unique() %>% 
  ungroup() %>% 
  full_join(sensor_count) %>% 
  mutate(count = as.numeric(count), diff = mean_EPA - mean_purp) 

# Performing linear regression
model.RH.corr <- lm(mean_EPA~ mean_purp, data = RH.corr.pm.single.point)
summary(model.RH.corr)

model.SE.corr <- lm(mean_EPA~ mean_purp, data = SE.RH.corr.pm.single.point)
summary(model.SE.corr)

model.not.corr <- lm(mean_EPA~ mean_purp, data = not.RH.corr.pm.single.point)
summary(model.not.corr)

RH.corr.fig <- ggplot(RH.corr.pm.single.point, aes()) +
  geom_errorbar(aes(x = mean_purp, ymin = ymin, ymax = ymax), width = 0.2, color = "gray") +
  geom_errorbarh(aes(y = mean_EPA, xmin = xmin, xmax = xmax), height = 0.2, color = "gray") +
  geom_point(aes(x = mean_purp, y = mean_EPA, color = Name, size = count)) +
  coord_cartesian(ylim = c(0, 15), xlim = c(0, 20)) +
  scale_size(range = c(2, 8)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1) +
  theme_minimal() + 
  labs(title = "CONUS Humidity Corrected PurpleAir PM2.5") + 
  theme(legend.position = "none") + 
  scale_color_manual(values = sensor.name.palette)  +  
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())

SE.RH.corr.fig <- ggplot(SE.RH.corr.pm.single.point, aes()) +
  geom_errorbar(aes(x = mean_purp, ymin = ymin, ymax = ymax), width = 0.2, color = "gray") +
  geom_errorbarh(aes(y = mean_EPA, xmin = xmin, xmax = xmax), height = 0.2, color = "gray") +
  geom_point(aes(x = mean_purp, y = mean_EPA, color = Name, size = count)) +
  coord_cartesian(ylim = c(0, 15), xlim = c(0, 20)) + 
  scale_size(range = c(2, 8)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1) +
  theme_minimal() + 
  labs(title = "SE Humidity Corrected PurpleAir PM2.5") + 
  theme(legend.position = "none") + 
  scale_color_manual(values = sensor.name.palette) 

not.RH.corr.fig <- ggplot(not.RH.corr.pm.single.point, aes()) +
  geom_errorbar(aes(x = mean_purp, ymin = ymin, ymax = ymax), width = 0.2, color = "gray") +
  geom_errorbarh(aes(y = mean_EPA, xmin = xmin, xmax = xmax), height = 0.2, color = "gray") +
  geom_point(aes(x = mean_purp, y = mean_EPA, color = Name, size = count)) +
  coord_cartesian(ylim = c(0, 15), xlim = c(0, 20)) + 
  scale_size(range = c(2, 8)) + 
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1) +
  theme_minimal() + 
  labs(title = "Non-RH Corrected PurpleAir PM2.5") + 
  scale_color_manual(values = sensor.name.palette)  +  
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
  guides(alpha = "none")

not.RH.corr.fig + RH.corr.fig + SE.RH.corr.fig 

# Wind Roses to examine general wind direction for each sensor location --------------------------------------
library(openair)
sample.windrose <- weekly_data %>% filter(Name == "1680")
custom_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35)
windRose(sample.windrose, 
         ws = "windspeed", 
         wd = "winddir", 
         ws.int = 3, 
         type = "Name", 
         cols = "heat", 
         paddle = FALSE, 
         breaks = custom_breaks,
         key = list(fit = "all", height = 0.5, space = "bottom"))

# Simple regression examining PM2.5 as a function of cyano cell counts -----------------------------------------
daily_data_cleaned <- daily_data %>% filter(known_PM_days != "known_PM_source")

# to select variable of interest
df <- weekly_data %>% filter(cyano_case_relevancy == "not-relevant")

ggplot(daily_data_cleaned) +
  geom_point(aes(x = `pm2.5_CONUS`, y = `avg_cyano_cell_count_L`, color = Name)) +
  ylim(0,20000) + 
  xlim(0,10) +
  facet_wrap(~Name) + 
  geom_smooth(aes(x = `pm2.5_CONUS`, y = `avg_cyano_cell_count_L`, color = Name),  method = "lm", se = TRUE) + 
  scale_color_manual(values = sensor.name.palette) + labs(title = "PM as a function of cyanobacteria, relevant")



# Principal Component analyses --------------------------------------
library(corrr);library(ggcorrplot);library(factoextra);library(FactoMineR)
pca.1 <- daily_data_cleaned %>% dplyr::select(-where(~ all(. == 0 | is.na(.)))) %>% dplyr::select("pm2.5_CONUS", "avg_cyano_cell_count_S", "temp", "humidity", "windspeed", "winddir", "solarradiation", "Ozone") %>% drop_na()

normalized.pca.data <- scale(pca.1)
pca.corr.matrix <- cor(normalized.pca.data)
pca.matrix.df <- as.data.frame(pca.corr.matrix)
ggcorrplot(pca.corr.matrix)
data.pca <- princomp(pca.corr.matrix)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = T)
fviz_pca_biplot(data.pca, col.var = "cos2", repel = TRUE, labelsize = 5 , pointsize = 2, font.family = "Arial", arrowsize = 1.25)

# GAMs ---------------------------------------------------------------------------
library(stats)
library(AICcmodavg)
library(mgcv)
library(bbmle) 
library(gamlss)
library(gamlss.dist)

# Histograms
ggplot(weekly_data_cleaned) + geom_histogram(aes(x = avg_cyano_cell_count_S))
# swapped variable here and found that everything other than cyanobacterial indicators were normally distributed (enough)
# Model looking at cyanobacterial intensity alone
ggplot(weekly_data_cleaned) + geom_histogram(aes(avg_cyano_cell_count_S))

# first order of operations is figuring out how to handle all of the zeroes 
# need to add a value and then log transform the data with many zeros 
model.s1 <- gamlss(pm2.5_CONUS ~ avg_cyano_cell_count_S, 
                family = ZAGA,  method = RS(), data = na.omit(weekly_data_cleaned))
model.m1 <- gamlss(pm2.5_CONUS ~ avg_cyano_cell_count_M, 
                family = ZAGA,  method = RS(), data = na.omit(weekly_data_cleaned))
model.l1 <- gamlss(pm2.5_CONUS ~ avg_cyano_cell_count_L, 
                family = ZAGA,  method = RS(), data = na.omit(weekly_data_cleaned))

# old normal distribution models 
#model.s1 <- gam(pm2.5_CONUS ~ s(avg_cyano_cell_count_S), family = gaussian, method = "REML", data = weekly_data_cleaned)
#model.m1 <- gam(pm2.5_CONUS ~ s(avg_cyano_cell_count_M), family = gaussian, method = "REML", data = weekly_data_cleaned)
#model.l1 <- gam(pm2.5_CONUS ~ s(avg_cyano_cell_count_L), family = gaussian, method = "REML", data = weekly_data_cleaned)

model.sml4 <- gamlss(pm2.5_CONUS ~ avg_cyano_cell_count_S*avg_cyano_cell_count_M*avg_cyano_cell_count_L, family = ZAGA,  method = RS(), data = na.omit(weekly_data_cleaned))

AIC(model.s1, model.m1, model.l1, model.sml4)
BIC(model.s1, model.m1, model.l1, model.sml4)

# Models looking at cyanobacterial spatial coverage alone
model.s2 <- gamlss(pm2.5_CONUS ~ surface_area_bloom_S, 
                   family = ZAGA,  method = RS(), data = na.omit(weekly_data_cleaned))
model.m2 <- gamlss(pm2.5_CONUS ~ surface_area_bloom_M, 
                   family = ZAGA,  method = RS(), data = na.omit(weekly_data_cleaned))
model.l2 <- gamlss(pm2.5_CONUS ~ surface_area_bloom_L, 
                   family = ZAGA,  method = RS(), data = na.omit(weekly_data_cleaned))

AIC(model.s2, model.m2, model.l2)
BIC(model.s2, model.m2, model.l2)

#model.s2 <- gam(pm2.5_CONUS ~ s(surface_area_bloom_S), family = gaussian, method = "REML", data = weekly_data_cleaned)
#model.m2 <- gam(pm2.5_CONUS ~ s(surface_area_bloom_M), family = gaussian, method = "REML", data = weekly_data_cleaned)
#model.l2 <- gam(pm2.5_CONUS ~ s(surface_area_bloom_L), family = gaussian, method = "REML",data = weekly_data_cleaned)

# Examining interaction between cyanobacterial intensity and spatial extent
model.s3 <- gamlss(pm2.5_CONUS ~ surface_area_bloom_S*avg_cyano_cell_count_S, 
                   family = ZAGA,  method = RS(), data = na.omit(weekly_data_cleaned))
model.m3 <- gamlss(pm2.5_CONUS ~ surface_area_bloom_M*avg_cyano_cell_count_M, 
                   family = ZAGA,  method = RS(), data = na.omit(weekly_data_cleaned))
model.l3 <- gamlss(pm2.5_CONUS ~ surface_area_bloom_L*avg_cyano_cell_count_L, 
                   family = ZAGA,  method = RS(), data = na.omit(weekly_data_cleaned))

AIC(model.s3, model.m3, model.l3)
BIC(model.s3, model.m3, model.l3)

weekly_data_cleaned_no_na <- weekly_data_cleaned %>% drop_na()
weekly_data_cleaned_no_na$ZI_predicted_cyano <- fitted(model.l1)

# Individual air pollutant models 
model.so4 <- gam(pm2.5_CONUS ~ s(SO4), family = gaussian, method = "REML", data = weekly_data_cleaned)
model.po4 <- gam(pm2.5_CONUS ~ s(PO4), family = gaussian, method = "REML", data = weekly_data_cleaned)
model.co <- gam(pm2.5_CONUS ~ s(CO), family = gaussian, method = "REML", data = weekly_data_cleaned)
model.nox <- gam(pm2.5_CONUS ~ s(Total_NOx), family = gaussian,method = "REML", data = weekly_data_cleaned)
model.o3 <- gam(pm2.5_CONUS ~ s(Ozone), family = gaussian, method = "REML", data = weekly_data_cleaned) 

AIC(model.so4, model.po4, model.co, model.nox, model.o3) 
BIC(model.so4, model.po4, model.co, model.nox, model.o3)# if you do this with the cleaned vs uncleaned data (removal of big wildfire days), the results are virtually the same. Bit better fit when cleaned
gam.check(model.so4)
# ozone has the strongest relationship with PM, however, this model is not a good fit 

# Examining additive effects of key pollutants to identify interactions
model.frm.1 <- gam(pm2.5_CONUS ~ s(Ozone) + s(SO4), family = gaussian, method = "REML", data = weekly_data_cleaned)
model.frm.2 <- gam(pm2.5_CONUS ~ s(Ozone) + s(PO4), family = gaussian, method = "REML", data = weekly_data_cleaned)
model.frm.3 <- gam(pm2.5_CONUS ~ te(Ozone, Total_NOx), family = gaussian, method = "REML", data = weekly_data_cleaned)
model.frm.3b <- gam(pm2.5_CONUS ~ s(Ozone) + s(Total_NOx), family = gaussian, method = "REML", data = weekly_data_cleaned)
model.frm.4 <- gam(pm2.5_CONUS ~ s(Ozone) + s(CO), family = gaussian, method = "REML", data = weekly_data_cleaned)
model.frm.5 <- gam(pm2.5_CONUS ~ s(Ozone) + te(SO4, CO), family = gaussian, method = "REML", data = weekly_data_cleaned)
model.frm.5b <- gam(pm2.5_CONUS ~ s(Ozone) + s(SO4) + s(CO), family = gaussian, method = "REML", data = weekly_data_cleaned)
model.frm.6 <- gam(pm2.5_CONUS ~ te(Ozone, Total_NOx) + s(SO4) + s(CO), family = gaussian, method = "REML", data = weekly_data_cleaned)

AIC(model.frm.1, model.frm.2, model.frm.3, model.frm.3b, model.frm.4, model.frm.5, model.frm.5b, model.frm.6) 
BIC(model.frm.1, model.frm.2, model.frm.3, model.frm.3b, model.frm.4, model.frm.5, model.frm.5b, model.frm.6) 
gam.check(model.frm.5b) # okay now we are starting to enter into a realm where the model is actually fitting a bit better (although not perfectly) -- the best fit for the pollutant data demonstrated that ozone when considered with the interactive effects between SO2 and CO is best fit (not the interactions between NOx and Ozone, surprisingly)

# Testing models with all of the pollutants together
model.frm.7 <- gam(pm2.5_CONUS ~ s(Ozone) + te(SO4, CO) + s(Total_NOx), family = gaussian, method = "REML", data = weekly_data_cleaned)
model.frm.8 <- gam(pm2.5_CONUS ~ s(Ozone) + te(SO4, CO) + s(Total_NOx) + s(PO4), family = gaussian, method = "REML", data = weekly_data_cleaned)

AIC(model.frm.5, model.frm.7, model.frm.8) 
BIC(model.frm.5, model.frm.7, model.frm.8) 
gam.check(model.frm.8) # getting a better fit still with these models, but not too much more explained with the inclusion of NOx and PO4 -- especially PO4 which edf, K, and low p value bad sign -- versions 5 or 7 are likely fine, BIC actually prefers version 5
summary(model.frm.8)

# Meteorological data models
ws.mod <- gam(pm2.5_CONUS ~ s(windspeed), family = gaussian, method = "REML", data = weekly_data_cleaned)
wd.mod <- gam(pm2.5_CONUS ~ s(winddir), family = gaussian, method = "REML", data = weekly_data_cleaned)
sun.mod <- gam(pm2.5_CONUS ~ s(solarradiation), family = gaussian, method = "REML", data = weekly_data_cleaned)
temp.mod <- gam(pm2.5_CONUS ~ s(temp), family = gaussian,method = "REML", data = weekly_data_cleaned)
rh.mod <- gam(pm2.5_CONUS ~ s(humidity), family = gaussian, method = "REML", data = weekly_data_cleaned)

AIC(ws.mod, wd.mod, sun.mod, temp.mod, rh.mod)
BIC(ws.mod, wd.mod, sun.mod, temp.mod, rh.mod)
gam.check(temp.mod) # not a good fit for this alone, as suspected

# Examining suspected (known) interactions between weather variables before moving on 
met.mod1 <- gam(pm2.5_CONUS ~ te(temp, humidity), family = gaussian, method = "REML", data = weekly_data_cleaned)
met.mod1b <- gam(pm2.5_CONUS ~ s(temp) + s(humidity), family = gaussian, method = "REML", data = weekly_data_cleaned)
met.mod2 <- gam(pm2.5_CONUS ~ te(solarradiation, temp), family = gaussian, method = "REML", data = weekly_data_cleaned)
met.mod2b <- gam(pm2.5_CONUS ~ s(solarradiation) + s(temp), family = gaussian, method = "REML", data = weekly_data_cleaned)
met.mod3 <- gam(pm2.5_CONUS ~ te(winddir, windspeed), family = gaussian, method = "REML", data = weekly_data_cleaned)
met.mod3b <- gam(pm2.5_CONUS ~ s(winddir) + s(windspeed), family = gaussian, method = "REML", data = weekly_data_cleaned)
met.mod1c <- gam(pm2.5_CONUS ~ te(humidity, solarradiation, temp), family = gaussian, method = "REML", data = weekly_data_cleaned)

AIC(met.mod1, met.mod1b, met.mod2, met.mod2b, met.mod3, met.mod3b, met.mod1c)
BIC(met.mod1, met.mod1b, met.mod2, met.mod2b, met.mod3, met.mod3b, met.mod1c)
gam.check(met.mod2)# not a great fit here either, do not consider any as interactive

met.mod4 <- gam(pm2.5_CONUS ~ s(solarradiation) + te(temp, humidity), family = gaussian, method = "REML", data = weekly_data_cleaned)
met.mod5 <- gam(pm2.5_CONUS ~ s(humidity) + te(temp, solarradiation), family = gaussian, method = "REML", data = weekly_data_cleaned)
met.mod6 <- gam(pm2.5_CONUS ~ s(solarradiation) + te(temp, humidity) + s(winddir), family = gaussian, method = "REML", data = weekly_data_cleaned)
met.mod7 <- gam(pm2.5_CONUS ~ s(solarradiation) + te(temp, humidity) + s(windspeed), family = gaussian, method = "REML", data = weekly_data_cleaned)
met.mod8 <- gam(pm2.5_CONUS ~ s(solarradiation) + te(temp, humidity) + s(winddir) + s(windspeed), family = gaussian, method = "REML", data = weekly_data_cleaned)

AIC(met.mod1, met.mod2, met.mod4, met.mod5, met.mod6, met.mod7, met.mod8)
BIC(met.mod1, met.mod2, met.mod4, met.mod5, met.mod6, met.mod7, met.mod8)
gam.check(met.mod7) # better fit than met mod.2, but still not a great fit model. solar radiation and temperature, indicative of secondary aerosol formation. Wind direction is also very important 
summary(met.mod8)

# now adding together met data, air pollution data, and cyanobacteria data into a single model
# best met model: met.mod5 s(solarradiation) + s(temp) + s(windspeed)
# best cyano model: model.s3 te(avg_cyano_cell_count_S, surface_area_bloom_S)
# best frm pollutant model: model.frm.5 s(Ozone) + te(SO4, CO)

#all.mod1 <- gam(pm2.5_CONUS ~ s(solarradiation) + te(temp, humidity) + s(windspeed) + s(avg_cyano_cell_count_S), family = gaussian, method = "REML", data = weekly_data_cleaned)
#all.mod2 <- gam(pm2.5_CONUS ~ s(solarradiation) + te(temp, humidity) + s(windspeed) + s(Ozone) + te(SO4, CO) + s(Total_NOx), family = gaussian, method = "REML", data = weekly_data_cleaned)
#all.mod3 <- gam(pm2.5_CONUS ~ s(Ozone) + te(SO4, CO) + s(Total_NOx) + s(PO4) + s(avg_cyano_cell_count_S), family = gaussian, method = "REML", data = weekly_data_cleaned)
#all.mod4 <- gam(pm2.5_CONUS ~ s(solarradiation) + te(temp, humidity) + s(windspeed) + s(avg_cyano_cell_count_S) + s(Ozone) + te(SO4, CO) + s(Total_NOx) + s(PO4), family = gaussian, method = "REML", data = weekly_data_cleaned)
#all.mod5 <- gam(pm2.5_CONUS ~ Name + s(solarradiation) + te(temp, humidity) + te(windspeed, avg_cyano_cell_count_S) + s(Ozone) + te(SO4, CO) + s(Total_NOx) + s(PO4), family = gaussian, method = "REML", data = weekly_data_cleaned)

all.mod1 <- gam(pm2.5_CONUS ~ s(solarradiation) + te(temp, humidity) + s(windspeed) + s(ZI_predicted_cyano), family = gaussian, method = "REML", data = weekly_data_cleaned_no_na)

all.mod2 <- gam(pm2.5_CONUS ~ s(solarradiation) + te(temp, humidity) + s(windspeed) + s(Ozone) + te(SO4, CO, k=10) + s(PO4) + s(Total_NOx), family = gaussian, method = "REML", data = weekly_data_cleaned_no_na)

all.mod3 <- gam(pm2.5_CONUS ~ s(Ozone) + te(SO4, CO, k=10) + s(ZI_predicted_cyano) + s(PO4) + s(Total_NOx), family = gaussian, method = "REML", data = weekly_data_cleaned_no_na)

all.mod4 <- gam(pm2.5_CONUS ~ s(solarradiation) + te(temp, humidity) + s(windspeed) + s(ZI_predicted_cyano) + s(Ozone) + te(SO4, CO, k=10) + s(PO4) + s(Total_NOx), family = gaussian, method = "REML", data = weekly_data_cleaned_no_na)

all.mod5 <- gam(pm2.5_CONUS ~ Name + s(solarradiation) + te(temp, humidity) + te(windspeed, ZI_predicted_cyano) + s(Ozone) + te(SO4, CO, k=10) + s(PO4) + s(Total_NOx), family = gaussian, method = "REML", data = weekly_data_cleaned_no_na)

all.mod6 <- gam(pm2.5_CONUS ~ Name + s(solarradiation) + te(temp, humidity) + te(Ozone, ZI_predicted_cyano)  + te(SO4, CO, k=10) + s(PO4) + s(Total_NOx), family = gaussian, method = "REML", data = weekly_data_cleaned_no_na)

AIC(all.mod1, all.mod2, all.mod3, all.mod4, all.mod5, all.mod6)
BIC(all.mod1, all.mod2, all.mod3, all.mod4, all.mod5, all.mod6)
gam.check(all.mod5)
summary(all.mod5)

# Checking parametric vs non parametric 
all.mod4.gam <- gam(pm2.5_CONUS ~ s(solarradiation) + s(temp) + s(windspeed) + te(avg_cyano_cell_count_S, surface_area_bloom_S) + s(Ozone) + te(SO4, CO), family = gaussian, method = "REML", data = weekly_data_cleaned)
all.mod4.glm <- glm(pm2.5_CONUS ~ solarradiation + temp + windspeed + avg_cyano_cell_count_S*surface_area_bloom_S + Ozone + SO4* CO, family = gaussian, data = weekly_data_cleaned)
AIC(all.mod4.gam, all.mod4.glm)
BIC(all.mod4.gam, all.mod4.glm) #both BIC and AIC are much lower for the GAM when compared to the GLM, and the residuals plot revealed some curvature for higher values, indicating non-linearity... might need to revisit previous statistical testing and use nonparametric tests

# PM BOXPLOTS ------------------------------------------------------------------
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

proximity.color.palette <- c("<.5km"= "gray85",
                             ">.5km" = "darkgray",
                             "control" = "black")

# Boxplot examining PM under different groupings ----------------------------
# Years looped together
library(multcompView)
weekly_data_1 <- weekly_data_cleaned %>% drop_na(pm2.5_CONUS) %>%
mutate(water.proximity = recode(water.proximity, 
                                ">1km" = ">0.5km", 
                                "<1km" = ">0.5km")) 


anova.PM2.5.sensor <- aov(pm2.5_CONUS ~ Name, data = weekly_data_1)
summary(anova.PM2.5.sensor)

tukey.sensor <- TukeyHSD(anova.PM2.5.sensor)
print(tukey.sensor)

cld.sensor <- multcompLetters4(anova.PM2.5.sensor, tukey.sensor)
print(cld.sensor)

tk.sensor <- group_by(weekly_data_1, Name) %>% 
  summarise(mean = mean(pm2.5_CONUS), quant = quantile(pm2.5_CONUS, probs = 0.75)) %>% 
  arrange(desc(mean))
cld.sensor <- as.data.frame.list(cld.sensor$Name) 

weekly_data_1$Name <- factor(weekly_data_1$Name , levels = c("1562", "5822", "1334", "1680", "1358", "5838", "1348", "1378", "1362", "9875", "1318", "1344", "1806"))
#weekly_data_1$Name <- factor(weekly_data_1$Name , levels = c("1806", "1680", "1378", "5822", "1334", "1362", "1348", "5838", "9875", "1318", "1344", "1358", "1562"))

# Boxplot showing PM averages for each sensor 
ggplot(weekly_data_1, aes(x = Name, y = pm2.5_CONUS, fill = Name)) +
  geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.3) + 
  geom_boxplot(alpha = 0.9) +
  labs(x = "Sensor ID") +
  ylab(expression(bold(PM[2.5]~mass~concentration~(µg~m^bold("-3"))))) +
  ggtitle("PM2.5 Mass Grouped by Sensor") +
  scale_fill_manual(values = sensor.name.palette) +
  scale_color_manual(values = sensor.name.palette) +
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

# Boxplots grouped by water proximity
anova.PM2.5.prox <- aov(pm2.5_CONUS ~ water.proximity, data = weekly_data_1)
summary(anova.PM2.5.prox)

tukey.prox <- TukeyHSD(anova.PM2.5.prox)
print(tukey.prox)

cld.prox <- multcompLetters4(anova.PM2.5.prox, tukey.prox)
print(cld.prox)

tk.prox <- group_by(weekly_data_1, water.proximity) %>% 
  summarise(mean = mean(pm2.5_CONUS), quant = quantile(pm2.5_CONUS, probs = 0.75)) %>% 
  arrange(desc(mean))
cld.prox <- as.data.frame.list(cld.prox$water.proximity)

# plots 
ggplot(weekly_data_1, aes(x = water.proximity, y = pm2.5_CONUS, fill = water.proximity)) +
  #geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.25) + 
  geom_boxplot(alpha = 0.7) +
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

# Fall only analyses
weekly_data_fall <- weekly_data_cleaned %>% drop_na(pm2.5_CONUS) %>%
  mutate(water.proximity = recode(water.proximity, 
                                  ">1km" = ">0.5km", 
                                  "<1km" = ">0.5km"))  %>%  
  filter(grepl("-09-|-10-|-11-", week))
anova.PM2.5.prox <- aov(pm2.5_CONUS ~ water.proximity, data = weekly_data_fall)
summary(anova.PM2.5.prox)

tukey.prox <- TukeyHSD(anova.PM2.5.prox)
print(tukey.prox)

cld.prox <- multcompLetters4(anova.PM2.5.prox, tukey.prox)
print(cld.prox)

tk.prox <- group_by(weekly_data_fall, water.proximity) %>% 
  summarise(mean = mean(pm2.5_CONUS), quant = quantile(pm2.5_CONUS, probs = 0.75)) %>% 
  arrange(desc(mean))
cld.prox <- as.data.frame.list(cld.prox$water.proximity)

fall <- ggplot(weekly_data_fall, aes(x = water.proximity, y = pm2.5_CONUS, fill = water.proximity)) +
  #geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.25) + 
  geom_boxplot(alpha = 0.7) +
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

weekly_data_winter <- weekly_data_cleaned %>% drop_na(pm2.5_CONUS) %>%
  mutate(water.proximity = recode(water.proximity, 
                                  ">1km" = ">0.5km", 
                                  "<1km" = ">0.5km"))  %>%  
  filter(grepl("-12-|-01-|-02-", week))
anova.PM2.5.prox <- aov(pm2.5_CONUS ~ water.proximity, data = weekly_data_winter)
summary(anova.PM2.5.prox)

tukey.prox <- TukeyHSD(anova.PM2.5.prox)
print(tukey.prox)

cld.prox <- multcompLetters4(anova.PM2.5.prox, tukey.prox)
print(cld.prox)

tk.prox <- group_by(weekly_data_winter, water.proximity) %>% 
  summarise(mean = mean(pm2.5_CONUS), quant = quantile(pm2.5_CONUS, probs = 0.75)) %>% 
  arrange(desc(mean))
cld.prox <- as.data.frame.list(cld.prox$water.proximity)

winter <- ggplot(weekly_data_winter, aes(x = water.proximity, y = pm2.5_CONUS, fill = water.proximity)) +
  #geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.25) + 
  geom_boxplot(alpha = 0.7) +
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

weekly_data_spring <- weekly_data_cleaned %>% drop_na(pm2.5_CONUS) %>%
  mutate(water.proximity = recode(water.proximity, 
                                  ">1km" = ">0.5km", 
                                  "<1km" = ">0.5km"))  %>%  
  filter(grepl("-03-|-04-|-05-", week))
anova.PM2.5.prox <- aov(pm2.5_CONUS ~ water.proximity, data = weekly_data_spring)
summary(anova.PM2.5.prox)

tukey.prox <- TukeyHSD(anova.PM2.5.prox)
print(tukey.prox)

cld.prox <- multcompLetters4(anova.PM2.5.prox, tukey.prox)
print(cld.prox)

tk.prox <- group_by(weekly_data_spring, water.proximity) %>% 
  summarise(mean = mean(pm2.5_CONUS), quant = quantile(pm2.5_CONUS, probs = 0.75)) %>% 
  arrange(desc(mean))
cld.prox <- as.data.frame.list(cld.prox$water.proximity)


spring <- ggplot(weekly_data_spring, aes(x = water.proximity, y = pm2.5_CONUS, fill = water.proximity)) +
  #geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.25) + 
  geom_boxplot(alpha = 0.7) +
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

weekly_data_summer <- weekly_data_cleaned %>% drop_na(pm2.5_CONUS) %>%
  mutate(water.proximity = recode(water.proximity, 
                                  ">1km" = ">0.5km", 
                                  "<1km" = ">0.5km"))  %>%  
  filter(grepl("-06-|-07-|-08-", week))

anova.PM2.5.prox <- aov(pm2.5_CONUS ~ water.proximity, data = weekly_data_summer)
summary(anova.PM2.5.prox)

tukey.prox <- TukeyHSD(anova.PM2.5.prox)
print(tukey.prox)

cld.prox <- multcompLetters4(anova.PM2.5.prox, tukey.prox)
print(cld.prox)

tk.prox <- group_by(weekly_data_summer, water.proximity) %>% 
  summarise(mean = mean(pm2.5_CONUS), quant = quantile(pm2.5_CONUS, probs = 0.75)) %>% 
  arrange(desc(mean))
cld.prox <- as.data.frame.list(cld.prox$water.proximity)

summer <- ggplot(weekly_data_summer, aes(x = water.proximity, y = pm2.5_CONUS, fill = water.proximity)) +
  #geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.25) + 
  geom_boxplot(alpha = 0.7) +
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

winter + spring + summer + fall

# WHO THRESHOLD PLOT 
weekly_data_bloom_threshold <- weekly_data_cleaned %>% 
                    mutate(WHO.bloom = case_when(
                      avg_cyano_cell_count_S >= 100000 |  
                      avg_cyano_cell_count_M >= 100000 | 
                      avg_cyano_cell_count_L >= 100000 ~ "Bloom",
                      avg_cyano_cell_count_S < 100000 |
                      avg_cyano_cell_count_M < 100000 | 
                      avg_cyano_cell_count_L < 100000 |
                      is.na(avg_cyano_cell_count_S) |
                      is.na(avg_cyano_cell_count_M) |
                      is.na(avg_cyano_cell_count_L) ~ "Non-Bloom")) %>%
  drop_na(WHO.bloom, pm2.5_CONUS) 

weekly_data_bloom_threshold$Name <- factor(weekly_data_bloom_threshold$Name , levels = c("1562", "5822", "1334", "1680", "1358", "5838", "1348", "1378", "1362", "9875", "1318", "1344", "1806"))

library(ggpubr)
ggplot(weekly_data_bloom_threshold, aes(x = WHO.bloom, y = pm2.5_CONUS, fill = Name)) +
  #geom_point(position = position_jitter(width = 0.3), aes(color = Name), alpha = 0.25) +
  geom_boxplot(alpha = 0.9) +
  ylab(expression(bold(PM[2.5]~mass~concentration~(µg~m^bold("-3"))))) +
  scale_fill_manual(values = sensor.name.palette) +
  scale_color_manual(values = sensor.name.palette) +
  theme(axis.text.x = element_text(size=15, face="bold", color = "black"),
        axis.text.y = element_text(size=15, face="bold", color = "black"),
        axis.title.x = element_text(size=15, face="bold", color = "black"),  
        axis.title.y = element_text(size=15, face="bold", color = "black"),
        plot.title = element_text(size=20, face="bold", color = "black", hjust = 0.5), 
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = 'gray', linetype = "dashed"), 
        panel.grid.major.x = element_blank()) + 
  facet_wrap(~Name) + 
  ylim(0,15) 

# now need to run stats for those 
WHO.stats <- weekly_data_bloom_threshold %>% 
  group_by(Name, WHO.bloom) %>%
  summarise(
    count = n(),
    median = median(pm2.5_CONUS, na.rm = TRUE),
    IQR = IQR(pm2.5_CONUS, na.rm = TRUE))

weekly_data_bloom_threshold_noinland <- weekly_data_bloom_threshold %>% filter(Name == "5822")
  #filter(Name != "1562", Name != "1680", Name != "1358")
  
# Group by 'Name' and run Wilcoxon test for each group
wilcox_results <- weekly_data_bloom_threshold_noinland %>%
  group_by(Name) %>%
  do(test = wilcox.test(pm2.5_CONUS ~ WHO.bloom, data = weekly_data_bloom_threshold_noinland))  # Run Wilcoxon test
wilcox_results %>%
  mutate(p_value = test$p.value, 
         statistic = test$statistic)

library(dplyr)

# Define the function to run the Wilcoxon test for a given Name
run_wilcox_test <- function(data, name) {
  filtered_data <- data %>% filter(Name == name)
  
  if (n_distinct(filtered_data$WHO.bloom) == 2) {
    test_result <- wilcox.test(pm2.5_CONUS ~ WHO.bloom, data = filtered_data, exact = FALSE)
    return(data.frame(Name = name, p_value = test_result$p.value, statistic = test_result$statistic))
  } else {
    return(data.frame(Name = name, p_value = NA, statistic = NA))
  }
}

# Get unique names from the data
unique_names <- unique(weekly_data_bloom_threshold$Name)

# Run the Wilcoxon test for each Name and store results in a dataframe
results <- do.call(rbind, lapply(unique_names, function(name) run_wilcox_test(weekly_data_bloom_threshold, name)))

# Print the results dataframe
print(results)


# WIND DIRECTION PLOT
ggplot(weekly_data_cleaned) + 
  geom_boxplot(aes(x = `relative_winddir`, y = `pm2.5_CONUS`, color = Name)) + 
  facet_wrap(~Name) + 
  scale_color_manual(values = sensor.name.palette) +
  ylim(0,15) + 
  theme_bw()


# comparing PM to water data via simple linear regressions  ---------------------------------
library(ggpmisc)
corr.S <- ggplot(weekly_data, aes(x = avg_cyano_cell_count_S, y = surface_area_bloom_S))+
  geom_point() + 
  theme_bw()+
  geom_smooth(method = "glm", se = FALSE) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x = "right", label.y = "top",
               formula = y ~ x, parse = TRUE)

corr.M <-ggplot(weekly_data, aes(x = avg_cyano_cell_count_M, y = surface_area_bloom_M))+
  geom_point() + 
  geom_smooth(method = "glm", se = FALSE) + 
  theme_bw()+
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x = "right", label.y = "top",
               formula = y ~ x, parse = TRUE)

corr.L <- ggplot(weekly_data, aes(x = avg_cyano_cell_count_L, y = surface_area_bloom_L))+
  geom_point() + 
  theme_bw()+
  geom_smooth(method = "glm", se = FALSE)+
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x = "right", label.y = "top",
               formula = y ~ x, parse = TRUE)

corr.S + corr.M + corr.L








