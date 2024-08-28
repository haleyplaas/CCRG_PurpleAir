setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\CCRG_PurpleAir")
rm(list = ls())
library(httr);library(jsonlite);library(dplyr);library(ggplot2)

# Read in the data ------------------------------------
daily_data <- read.csv("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\COMPLETE.daily.data.csv") %>% select(-X) %>% mutate(Name = as.character(Name), date = as.Date(date))
weekly_data <- read.csv("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\COMPLETE.weekly.data.csv") %>% select(-X) %>% mutate(Name = as.character(Name), date = as.Date(date))
imputed_data <- read.csv("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\COMPLETE.imputations.data.csv") %>% select(-X) %>% mutate(Name = as.character(Name), date = as.Date(date))

# color palette for sensors, based on regional groupings and also distance from shoreline (darker = further from shore, similar hue = proximate to each other) ---------------------------
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


# Adding columns with specifications per stratified analyses --------------------------------
library(lubridate)
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
    Name == "1344" & winddir_cardinal %in% c("SW", "W") ~ "onshore",
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
    Name == "1344" & winddir_cardinal %in% c("SW", "W") ~ "onshore",
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

# same plots but with moving averages (based on seasons and Months) 
library(tidyverse)
weekly_data$week <- as.Date(weekly_data$week)

# Time series with seasonal averages -------------------------------------
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

# Time series with year specific seasonal averages ----------------------------------
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

# Comparing EPA data to purpleair data -------------------------------
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

SE.RH.corr.fig + RH.corr.fig + not.RH.corr.fig

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

# Boxplot examining PM under different groupings ----------------------------
ggplot(daily_data_cleaned) + 
  geom_boxplot(aes(x = `relative_winddir`, y = `pm2.5_CONUS`, color = Name)) + 
  facet_wrap(~Name) + 
  scale_color_manual(values = sensor.name.palette) +
  ylim(0,20) + 
  theme_bw()

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

# GLMs ---------------------------------------------------------------------------
library(stats)
library(AICcmodavg)

daily_data_cleaned <- daily_data %>% filter(known_PM_days != "known_PM_source")
weekly_data_cleaned <- weekly_data %>% filter(known_PM_days != "known_PM_source")
  
model.1 <- glm(pm2.5_CONUS ~ SO4 + PO4 + CO + Total_NOx + Ozone + avg_cyano_cell_count_S + percent_bloom_S, family = gaussian, data = weekly_data_cleaned)
summary(model.1) 
model.2 <- glm(pm2.5_CONUS ~ SO4 + PO4 + CO + Total_NOx + Ozone + avg_cyano_cell_count_M + percent_bloom_M, family = gaussian, data = weekly_data_cleaned)
summary(model.2) 
model.3 <- glm(pm2.5_CONUS ~ SO4 + PO4 + CO + Total_NOx + Ozone + avg_cyano_cell_count_L + percent_bloom_L, family = gaussian, data = weekly_data_cleaned)
summary(model.3) 

# Variables to consider: 
# SO4 + PO4 + CO + Total_NOx + Ozone + avg_cyano_cell_count_X + percent_bloom_X + windspeed + winddir + solarradiation

model.s <- glm(pm2.5_CONUS ~ avg_cyano_cell_count_S, family = gaussian, data = weekly_data_cleaned)
model.m <- glm(pm2.5_CONUS ~ avg_cyano_cell_count_M, family = gaussian, data = weekly_data_cleaned)
model.l <- glm(pm2.5_CONUS ~ avg_cyano_cell_count_L, family = gaussian, data = weekly_data_cleaned)

model.s2 <- glm(pm2.5_CONUS ~ avg_cyano_cell_count_S*percent_bloom_S, family = gaussian, data = weekly_data_cleaned)
model.m2 <- glm(pm2.5_CONUS ~ avg_cyano_cell_count_M*percent_bloom_M, family = gaussian, data = weekly_data_cleaned)
model.l2 <- glm(pm2.5_CONUS ~ avg_cyano_cell_count_L*percent_bloom_L, family = gaussian, data = weekly_data_cleaned)

model.s3 <- glm(pm2.5_CONUS ~ percent_bloom_S, family = gaussian, data = weekly_data_cleaned)
model.m3 <- glm(pm2.5_CONUS ~ percent_bloom_M, family = gaussian, data = weekly_data_cleaned)
model.l3 <- glm(pm2.5_CONUS ~ percent_bloom_L, family = gaussian, data = weekly_data_cleaned)
?glm()
cyano.names <- c('small', "medium", "large")

cyano.models <- list(model.s, model.m, model.l)
cyano.models2 <- list(model.s2, model.m2, model.l2)
cyano.models3 <- list(model.s3, model.m3, model.l3)

aictab(cand.set = cyano.models, modnames = cyano.names) # cyanobacteria seem to have a bigger impact closer to the source 
aictab(cand.set = cyano.models2, modnames = cyano.names) 
aictab(cand.set = cyano.models3, modnames = cyano.names) # similar results for difference between percent coverage and intensity 

model.so4 <- glm(pm2.5_CONUS ~ SO4, family = gaussian, data = weekly_data_cleaned)
model.po4 <- glm(pm2.5_CONUS ~ PO4, family = gaussian, data = weekly_data_cleaned)
model.co <- glm(pm2.5_CONUS ~ CO, family = gaussian, data = weekly_data_cleaned)
model.nox <- glm(pm2.5_CONUS ~ Total_NOx, family = gaussian, data = weekly_data_cleaned)
model.o3 <- glm(pm2.5_CONUS ~ Ozone, family = gaussian, data = weekly_data_cleaned) 

airpollutant.names <- c('so4', "po4", "co", "nox", "o3")
airpollutant.models <- list(model.so4, model.po4, model.co, model.nox, model.o3)

aictab(cand.set = airpollutant.models, modnames = airpollutant.names) # if you do this with the cleaned vs uncleaned data (removal of big wildfire days), the results are virtually the same. Bit better fit when cleaned

model.1 <- glm(pm2.5_CONUS ~ Ozone + SO4, family = gaussian, data = weekly_data_cleaned)
model.2 <- glm(pm2.5_CONUS ~ Ozone + PO4, family = gaussian, data = weekly_data_cleaned)
model.3 <- glm(pm2.5_CONUS ~ Ozone*Total_NOx, family = gaussian, data = weekly_data_cleaned)
model.4 <- glm(pm2.5_CONUS ~ Ozone + CO, family = gaussian, data = weekly_data_cleaned)

airpollutant.names.mixed <- c('o3+so4', "o3+po4", "o3+co", "o3*nox", "o3")
airpollutant.models.mixed <- list(model.1, model.2, model.3, model.4, model.o3)

aictab(cand.set = airpollutant.models.mixed, modnames = airpollutant.names.mixed)

model.5 <- glm(pm2.5_CONUS ~ Ozone*Total_NOx + SO4, family = gaussian, data = weekly_data_cleaned)
model.6 <- glm(pm2.5_CONUS ~ Ozone*Total_NOx + PO4, family = gaussian, data = weekly_data_cleaned)
model.7 <- glm(pm2.5_CONUS ~ Ozone*Total_NOx + CO, family = gaussian, data = weekly_data_cleaned)
model.8 <- glm(pm2.5_CONUS ~ Ozone*Total_NOx + SO4 + PO4, family = gaussian, data = weekly_data_cleaned)
model.9 <- glm(pm2.5_CONUS ~ Ozone*Total_NOx + PO4 + CO, family = gaussian, data = weekly_data_cleaned)
model.10 <- glm(pm2.5_CONUS ~ Ozone*Total_NOx + SO4 + PO4 + CO, family = gaussian, data = weekly_data_cleaned)
model.11 <- glm(pm2.5_CONUS ~ Ozone*Total_NOx + SO4 + PO4 + CO + avg_cyano_cell_count_S, family = gaussian, data = weekly_data_cleaned)
model.12 <- glm(pm2.5_CONUS ~ Ozone*Total_NOx + SO4 + PO4 + CO + avg_cyano_cell_count_S*percent_bloom_S, family = gaussian, data = weekly_data_cleaned)

airpollutant.names.mixed <- c("o3*nox+so4", "o3*nox+po4", "o3*nox+co", 'o3*nox+so4+po4', "o3*nox+po4+co", "o3*nox+so4+po4+co", "all+cyano", "cyano_w_%")
airpollutant.models.mixed <- list(model.5, model.6, model.7, model.8, model.9, model.10, model.11, model.12) # adding in cyanobacteria does make a difference in the fit of the model, but considering percent coverage doesnt matter as much (percent coverage and cell count are too dependent and essentially tell the same story)

aictab(cand.set = airpollutant.models.mixed, modnames = airpollutant.names.mixed)

ws.mod <- glm(pm2.5_CONUS ~ windspeed, family = gaussian, data = weekly_data_cleaned)
wd.mod <- glm(pm2.5_CONUS ~ winddir, family = gaussian, data = weekly_data_cleaned)
sun.mod <- glm(pm2.5_CONUS ~ solarradiation, family = gaussian, data = weekly_data_cleaned)
temp.mod <- glm(pm2.5_CONUS ~ temp, family = gaussian, data = weekly_data_cleaned)
rh.mod <- glm(pm2.5_CONUS ~ humidity, family = gaussian, data = weekly_data_cleaned)
# dist.mod <- glm(pm2.5_CONUS ~ water.proximity, family = binomial, data = weekly_data_cleaned) 
# card.wind.mod <- glm(pm2.5_CONUS ~ relative.winddir, family = binomial, data = weekly_data_cleaned)

met.names <- c("ws", "wd", "sun", 'temp', "rh")
met.models <- list(ws.mod, wd.mod, sun.mod, temp.mod, rh.mod)

aictab(cand.set = met.models, modnames = met.names)

met.mod1 <- glm(pm2.5_CONUS ~ temp*humidity, family = gaussian, data = weekly_data_cleaned)
met.mod2 <- glm(pm2.5_CONUS ~ temp*humidity + windspeed, family = gaussian, data = weekly_data_cleaned)
met.mod3 <- glm(pm2.5_CONUS ~ solarradiation + windspeed, family = gaussian, data = weekly_data_cleaned)
met.mod4 <- glm(pm2.5_CONUS ~ temp*humidity + windspeed + solarradiation, family = gaussian, data = weekly_data_cleaned)
met.mod5 <- glm(pm2.5_CONUS ~ temp*humidity + windspeed + solarradiation + winddir, family = gaussian, data = weekly_data_cleaned)

met.names <- c("sun", 'temp', "temp+sun", "temp+ws", "sun+ws", "sun+temp+ws", "all")
met.models <- list(sun.mod, temp.mod, met.mod1, met.mod2, met.mod3, met.mod4, met.mod5)

aictab(cand.set = met.models, modnames = met.names) # best fit is temp*humidity + ws -- all and sun+temp+ws is pretty similar but more effective if you leaved out sunlight and winddir

model.a <- glm(pm2.5_CONUS ~ Ozone*Total_NOx + SO4 + PO4 + CO + avg_cyano_cell_count_S, family = gaussian, data = weekly_data_cleaned)
model.b <- glm(pm2.5_CONUS ~ Ozone*Total_NOx + SO4 + PO4 + CO + avg_cyano_cell_count_S + temp*humidity + windspeed, family = gaussian, data = weekly_data_cleaned)
model.c <- glm(pm2.5_CONUS ~ Ozone*Total_NOx + SO4 + PO4 + CO + temp*humidity + windspeed, family = gaussian, data = weekly_data_cleaned)
model.d <- glm(pm2.5_CONUS ~ Ozone*Total_NOx + SO4 + PO4 + CO + avg_cyano_cell_count_L*percent_bloom_L + temp*humidity + windspeed, family = gaussian, data = weekly_data_cleaned)

mods.names <- c("without", 'with_met', "withuot_cyano")
models <- list(model.a, model.b, model.c)

aictab(cand.set = models, modnames = mods.names) # best fit is temp*humidity + ws -- all and sun+temp+ws is pretty similar but more effective if you leaved out sunlight and winddir
summary(model.b)

residuals <- rstandard(model.11, type = 'deviance')
length(residuals)
x_values <- seq_along(residuals)

# Plot using scatter.smooth
scatter.smooth(x_values, residuals, col = 'gray')

# Final list of important models to show 
# 

# Progression of models to show 
  


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





