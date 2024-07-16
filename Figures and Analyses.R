setwd("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\CCRG_PurpleAir")
rm(list = ls())
library(httr);library(jsonlite);library(dplyr);library(ggplot2)

daily_data <- read.csv("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\COMPLETE.daily.data.csv") %>% select(-X) %>% mutate(Name = as.character(Name), date = as.Date(date))
weekly_data <- read.csv("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\COMPLETE.weekly.data.csv") %>% select(-X) %>% mutate(Name = as.character(Name), date = as.Date(date))
imputed_data <- read.csv("C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\all_compiled_data\\COMPLETE.imputations.data.csv") %>% select(-X) %>% mutate(Name = as.character(Name), date = as.Date(date))

# color palette for sensors, based on regional groupings and also distance from shoreline (darker = further from shore, similar hue = proximate to each other)
sensor.name.palette <- c("1358" = '#003300',
                         "1348" = '#539f37',
                         "5838" = '#006633',
                         "1680" = '#99FF66',
                         "5822" = '#FFFF33',
                         "1334" = '#FFCC00',
                         "1562" = '#CC6600',
                         "1378" = '#99FFCC',
                         "1362" = '#99FFFF',
                         "9875" = '#99CCFF',
                         "1318" = '#6699FF',
                         "1344" = '#0000FF',
                         "1806" = '#9999FF')

# Time Series of PM with air pollutant tracers --------------------------------------------------------

mean.CO <- mean(weekly_data$CO, na.rm = T) # in ppm
mean.Ozone <- mean(weekly_data$Ozone, na.rm = T) # in ppm
mean.Total_NOx <- mean(weekly_data$Total_NOx, na.rm = T) # in ppb
mean.PO4 <- mean(weekly_data$PO4, na.rm = T) # in ug m-3
mean.SO4 <- mean(weekly_data$SO4, na.rm = T) # in ppb
mean.PM <- mean(weekly_data$pm2.5_CONUS, na.rm = T) # in ug m-3

CO.timeseries <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = CO * 40), color = "black") +
  geom_abline(intercept = mean.CO* 40, slope = 0, color = "black", linetype = "dashed") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./40, name = "Carbon Monoxide (ppm)")) + 
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank())

O3.timeseries <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = Ozone * 400), color = "black") +
  geom_abline(intercept = mean.Ozone* 400, slope = 0, color = "black", linetype = "dashed") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./400, name = "Ozone (ppm)")) + 
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank())

NOx.timeseries <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = Total_NOx* 20), color = "black") +
  geom_abline(intercept = mean.Total_NOx* 20, slope = 0, color = "black", linetype = "dashed") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./20, name = "Total NOx (ppb)")) + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

PO4.timeseries <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = PO4 * 4000), color = "black") +
  geom_abline(intercept = mean.PO4 * 4000, slope = 0, color = "black", linetype = "dashed") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./4000, name = "Phosphate (ug m-3)")) +    theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) 

SO4.timeseries <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_line(aes(x = date, y = SO4 * 10), color = "black") +
  geom_abline(intercept = mean.SO4 * 10, slope = 0, color = "black", linetype = "dashed") +
  theme_minimal() +
  scale_color_manual(values = sensor.name.palette) +
  scale_y_continuous(name = "PM2.5 Mass Concentration", sec.axis = sec_axis(~./10, name = "Sulfate (ppb)")) +  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank()) + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") 

PM.only.timeseries <- ggplot(weekly_data) + 
  geom_point(aes(x = date, y = pm2.5_CONUS, color = Name)) + 
  geom_abline(intercept = mean.PM, slope = 0, color = "black", linetype = "dashed") +
  geom_abline(intercept = 9, slope = 0, color = "black") +
  theme_minimal() +
  scale_color_manual(values = sensor.name.palette) +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(),)

CO.timeseries / O3.timeseries / NOx.timeseries / PO4.timeseries / SO4.timeseries / PM.only.timeseries



# Comparing EPA data to purpleair data -------------------------------
# making dfs for this comparison
not.RH.corr.pm <- full_join(all.EPA.data, compiled_purpleair_data_NOT_RH_CORR, by = "date") %>% drop_na()
RH.corr.pm <- full_join(all.EPA.data, compiled_purpleair_data, by = "date") %>% drop_na()

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





