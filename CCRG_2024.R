setwd("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG")
rm(list = ls())
library(httr);library(jsonlite);library(dplyr)

read_api_key <- "AF9F770D-16A5-11EE-A77F-42010A800009" #Read API Key 
write_api_key <- "1180D5E8-16AC-11EE-A77F-42010A800009" #Write API Key

date_string <- "2024-01-01" #input date here 
# Convert the date string to a UNIX timestamp
unix_timestamp <- as.POSIXct(date_string, format = "%Y-%m-%d", tz = "UTC")
# Extract the numeric value of the UNIX timestamp
unix_timestamp_numeric <- as.numeric(unix_timestamp)
# Print the UNIX timestamp
print(unix_timestamp_numeric)

# API Calls to Retrieve PurpleAir Sensor PM data 6/1/2022-Present --------------------------------------------------------------------------------------
## AFTER SUCCESSFUL RETRIEVAL, DO NOT RUN THE SCRIPT AGAIN TO DUE TO LIMITATIONS ON CALLS BY PURPLEAIR 
#group_ID <- '2171' #Group name CCRG, added sensors to group by sensor INDEX (not ID): 
#1. OBX 151806, 2. N.ELIZ 151344, 3. MID.ELIZ 179875, 4. PH.ELIZ 151318, 5. NIX 151362, 6. HERT 151378, 7. EDE.HM 151348, 8. EDE.Q 145838, 9.COLE 151680, 10. ARR.C 145822, 11. ARR.M 151334, 12. WIND 151358, 13. MURF 151562

#requesting the following data (between 6/1/2022 and 5/31/2023 -- starting there): PM1, PM2.5, PM10 (from both channels a and b) and RH

#1. OBX 151806 
#sensor151806 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151806/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151806_list <- fromJSON(rawToChar(sensor151806$content), flatten = TRUE)
#maxlength = max(lengths(sensor151806_list))
#sensor151806_list2 = lapply(sensor151806_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151806_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151806.csv", row.names=FALSE)

#sensor151806 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151806/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151806_list <- fromJSON(rawToChar(sensor151806$content), flatten = TRUE)
#maxlength = max(lengths(sensor151806_list))
#sensor151806_list2 = lapply(sensor151806_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151806_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151806_2.csv", row.names=FALSE)

#2. N.ELIZ 151344
#sensor151344 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151344/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151344_list <- fromJSON(rawToChar(sensor151344$content), flatten = TRUE)
#maxlength = max(lengths(sensor151344_list))
#sensor151344_list2 = lapply(sensor151344_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151344_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151344.csv", row.names=FALSE)

#sensor151344 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151344/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151344_list <- fromJSON(rawToChar(sensor151344$content), flatten = TRUE)
#maxlength = max(lengths(sensor151344_list))
#sensor151344_list2 = lapply(sensor151344_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151344_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151344_2.csv", row.names=FALSE)

#3. MID.ELIZ 179875 
#sensor179875 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/179875/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor179875_list <- fromJSON(rawToChar(sensor179875$content), flatten = TRUE)
#maxlength = max(lengths(sensor179875_list))
#sensor179875_list2 = lapply(sensor179875_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor179875_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/179875.csv", row.names=FALSE) 

#sensor179875 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/179875/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor179875_list <- fromJSON(rawToChar(sensor179875$content), flatten = TRUE)
#maxlength = max(lengths(sensor179875_list))
#sensor179875_list2 = lapply(sensor179875_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor179875_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/179875_2.csv", row.names=FALSE) 
#4. PH.ELIZ 151318
#sensor151318 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151318/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151318_list <- fromJSON(rawToChar(sensor151318$content), flatten = TRUE)
#maxlength = max(lengths(sensor151318_list))
#sensor151318_list2 = lapply(sensor151318_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151318_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151318.csv", row.names=FALSE)

#sensor151318 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151318/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151318_list <- fromJSON(rawToChar(sensor151318$content), flatten = TRUE)
#maxlength = max(lengths(sensor151318_list))
#sensor151318_list2 = lapply(sensor151318_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151318_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151318_2.csv", row.names=FALSE)

#5. NIX 151362 
#sensor151362 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151362/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151362_list <- fromJSON(rawToChar(sensor151362$content), flatten = TRUE)
#maxlength = max(lengths(sensor151362_list))
#sensor151362_list2 = lapply(sensor151362_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151362_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151362.csv", row.names=FALSE)

#sensor151362 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151362/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151362_list <- fromJSON(rawToChar(sensor151362$content), flatten = TRUE)
#maxlength = max(lengths(sensor151362_list))
#sensor151362_list2 = lapply(sensor151362_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151362_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151362_2.csv", row.names=FALSE)

#6. HERT 151378 (?)
#sensor151378 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151378/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151378_list <- fromJSON(rawToChar(sensor151378$content), flatten = TRUE)
#maxlength = max(lengths(sensor151378_list))
#sensor151378_list2 = lapply(sensor151378_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151378_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151378.csv", row.names=FALSE)

#sensor151378 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151378/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151378_list <- fromJSON(rawToChar(sensor151378$content), flatten = TRUE)
#maxlength = max(lengths(sensor151378_list))
#sensor151378_list2 = lapply(sensor151378_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151378_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151378_2.csv", row.names=FALSE)

#7. EDE.HM 151348 
#sensor151348 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151348/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151348_list <- fromJSON(rawToChar(sensor151348$content), flatten = TRUE)
#maxlength = max(lengths(sensor151348_list))
#sensor151348_list2 = lapply(sensor151348_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151348_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151348.csv", row.names=FALSE)

#sensor151348 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151348/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151348_list <- fromJSON(rawToChar(sensor151348$content), flatten = TRUE)
#maxlength = max(lengths(sensor151348_list))
#sensor151348_list2 = lapply(sensor151348_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151348_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151348_2.csv", row.names=FALSE)

#8. EDE.Q 145838 
#sensor145838 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/145838/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor145838_list <- fromJSON(rawToChar(sensor145838$content), flatten = TRUE)
#maxlength = max(lengths(sensor145838_list))
#sensor145838_list2 = lapply(sensor145838_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor145838_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/145838.csv", row.names=FALSE)

#sensor145838 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/145838/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor145838_list <- fromJSON(rawToChar(sensor145838$content), flatten = TRUE)
#maxlength = max(lengths(sensor145838_list))
#sensor145838_list2 = lapply(sensor145838_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor145838_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/145838_2.csv", row.names=FALSE)

#9.COLE 151680 
#sensor151680 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151680/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151680_list <- fromJSON(rawToChar(sensor151680$content), flatten = TRUE)
#maxlength = max(lengths(sensor151680_list))
#sensor151680_list2 = lapply(sensor151680_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151680_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151680.csv", row.names=FALSE)

#sensor151680 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151680/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151680_list <- fromJSON(rawToChar(sensor151680$content), flatten = TRUE)
#maxlength = max(lengths(sensor151680_list))
#sensor151680_list2 = lapply(sensor151680_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151680_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151680_2.csv", row.names=FALSE)

#10. ARR.C 145822 
#sensor145822 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/145822/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor145822_list <- fromJSON(rawToChar(sensor145822$content), flatten = TRUE)
#maxlength = max(lengths(sensor145822_list))
#sensor145822_list2 = lapply(sensor145822_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor145822_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/145822.csv", row.names=FALSE)

#sensor145822 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/145822/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor145822_list <- fromJSON(rawToChar(sensor145822$content), flatten = TRUE)
#maxlength = max(lengths(sensor145822_list))
#sensor145822_list2 = lapply(sensor145822_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor145822_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/145822_2.csv", row.names=FALSE)

#11. ARR.M 151334
#sensor151334 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151334/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151334_list <- fromJSON(rawToChar(sensor151334$content), flatten = TRUE)
#maxlength = max(lengths(sensor151334_list))
#sensor151334_list2 = lapply(sensor151334_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151334_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151334.csv", row.names=FALSE)

#sensor151334 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151334/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151334_list <- fromJSON(rawToChar(sensor151334$content), flatten = TRUE)
#maxlength = max(lengths(sensor151334_list))
#sensor151334_list2 = lapply(sensor151334_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151334_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151334_2.csv", row.names=FALSE)

#12. WIND 151358 
#sensor151358 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151358/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151358_list <- fromJSON(rawToChar(sensor151358$content), flatten = TRUE)
#maxlength = max(lengths(sensor151358_list))
#sensor151358_list2 = lapply(sensor151358_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151358_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151358.csv", row.names=FALSE)

#sensor151358 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151358/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151358_list <- fromJSON(rawToChar(sensor151358$content), flatten = TRUE)
#maxlength = max(lengths(sensor151358_list))
#sensor151358_list2 = lapply(sensor151358_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151358_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151358_2.csv", row.names=FALSE)

#13. MURF 151562
#sensor151562 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151562/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151562_list <- fromJSON(rawToChar(sensor151562$content), flatten = TRUE)
#maxlength = max(lengths(sensor151562_list))
#sensor151562_list2 = lapply(sensor151562_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151562_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151562.csv", row.names=FALSE)

#sensor151562 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151562/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
#sensor151562_list <- fromJSON(rawToChar(sensor151562$content), flatten = TRUE)
#maxlength = max(lengths(sensor151562_list))
#sensor151562_list2 = lapply(sensor151562_list, function(x) {length(x) = maxlength; return(x)})
#write.csv(sensor151562_list2, "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/151562_2.csv", row.names=FALSE)


# PurpleAir Data Cleaning -----------------------------------------------------------------------------------------------------------------
folder_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/purpleairdata/cleaned_sensor_data"

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


# CyAN Cyanobacterial Index (digital number DN) data from SEADAS Pixel Extraction ---------------------------------------------- 
library(tidyverse);library(dplyr)

####################### read in all of the files
# Specify the folder path containing your text files
folder_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Coding/R/CCRG/SeaDAS files/extracted_DNs"

# Initialize an empty list to store data frames
result_list <- list()

# Iterate over each file in the folder
files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)

# Filter files that contain the word "Derived" in the name
filtered_files <- files[grep("Derived", files)]

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

################ Matrix of all digital number values by pixel for future imputations







################ Generate summaries for each sensor at each resolution 
# Initialize an empty list to store summary data frames
summary_data_frames <- list()

# Iterate over each data frame in the result list
for (df_name in names(result_list)) {
  # Group by "Name" and summarize counts for each category of "band_1"
  summary_df <- result_list[[df_name]] %>%
    group_by(Name) %>%
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
  summary_data_frames[[df_name]] <- summary_df
}

############## Calculating overall water surface area for each sensor at each spatial variance
# Initialize an empty dataframe
water_SA_by_sensor <- data.frame(Name = character())

# Iterate over the first three dataframes in the summary_data_frames list
for (i in 1:min(3, length(summary_data_frames))) {
  # Extract the dataframe name
  df_name <- names(summary_data_frames)[i]
  
  # Extract the water_surface_area column and rename it using the dataframe name
  water_SA_column <- summary_data_frames[[i]] %>%
    select(Name, water_surface_area) %>%
    rename(!!paste0("water_SA_", gsub("L2022152_", "", df_name)) := water_surface_area)
  
  # If it's the first iteration, assign the column to water_SA_by_sensor
  if (i == 1) {
    water_SA_by_sensor <- water_SA_column
  } else {
    # Otherwise, join the column to the existing dataframe
    water_SA_by_sensor <- bind_cols(water_SA_by_sensor, water_SA_column)
  }
}

# Drop the unwanted columns and rename the "Name...1" column to "Sensor_ID" and add categories based on distance from the water
water_SA_by_sensor <- water_SA_by_sensor %>%
  select(-matches("^Name\\.\\.\\.(3|5)$")) %>%
  rename(Sensor_ID = Name...1) %>% 
  mutate(water_influence = case_when(
    water_SA_L == 0 ~ "none",
    water_SA_L <= 0.1 ~ "intermediate", 
    water_SA_L > 0.1 ~ "apparent"))

############## Determining % of data completeness for water pixels based on cloud cover 
# Initialize an empty list to store the dataframes for each entry
data_completeness <- list()

# Initialize an empty vector to store the unique options
possible_options <- character()

# Iterate over each dataframe in the summary_data_frames list
for (file_name in names(summary_data_frames)) {
  # Get the dataframe
  df <- summary_data_frames[[file_name]]
  
  # Get the unique options from the "Name" column
  options <- unique(df$Name)
  
  # Update the possible_options vector
  possible_options <- union(possible_options, options)
}

# Iterate over each possible option
for (option in possible_options) {
  # Initialize an empty dataframe for the current option
  option_df <- data.frame(date = character(), percent_valid = numeric(), pixel_size = character())
  
  # Iterate over each dataframe in the summary_data_frames list
  for (file_name in names(summary_data_frames)) {
    # Get the dataframe
    df <- summary_data_frames[[file_name]]
    
    # Subset the dataframe for the current option
    entry_df <- df[df$Name == option, ]
    
    # If the entry dataframe is not empty, add it to the option dataframe
    if (nrow(entry_df) > 0) {
      # Extract the date from the file name
      date <- substr(file_name, 2, 8)
      
      # Add a row to the option dataframe
      option_df <- rbind(option_df, c(date, entry_df$percent_valid_pixels))
    }
  }
  
  # Rename the first column to "date"
  colnames(option_df)[1] <- "date"
  
  # Add a column for pixel_size repeating "S", "M", and "L" every three rows
  n <- nrow(option_df)
  pixel_size <- rep(c("S", "M", "L"), length.out = n)
  option_df$pixel_size <- pixel_size
  
  # Rename the 7th column to "percent_valid"
  colnames(option_df)[2] <- "percent_valid"
  
  # Add the option dataframe to the data_completeness list
  data_completeness[[option]] <- option_df
}

######## reorganizing the data completedness information
# Initialize an empty list to store the widened dataframes for each entry
data_completeness_wide <- list()

# Iterate over each dataframe in the data_completeness list
for (option in names(data_completeness)) {
  # Get the dataframe
  df <- data_completeness[[option]]
  
  # Pivot wider
  df_wide <- pivot_wider(df, names_from = pixel_size, values_from = percent_valid)
  
  # Add the pivoted dataframe to the data_completeness_wide list
  data_completeness_wide[[option]] <- df_wide
}

# Combine all dataframes into one single dataframe
comprehensive_data_completeness <- do.call(rbind, data_completeness_wide)

# Add Sensor_ID column
comprehensive_data_completeness$Sensor_ID <- rep(names(data_completeness), sapply(data_completeness_wide, nrow))

# Reorder columns
comprehensive_data_completeness <- comprehensive_data_completeness[, c("Sensor_ID", "date", "S", "M", "L")]

################ Calculating cell counts and chlorophyll a concentrations from DNs 
# Initialize an empty list to store summary data frames
summary_count_frames <- list()

# Iterate over each data frame in the result list
for (df_name in names(result_list)) {
  # Add a new columns "CI_cyano" and "cyano_cell_count" converted from digital number values
  summary_df <- result_list[[df_name]] %>%
    mutate(CI_cyano = 10^(3/250*band_1-4.2),
           cyano_cell_count = CI_cyano*100000000) %>%
    select(-CI_cyano) %>%
    as.data.frame()
  
  # Store the summary data frame in the list with the same name as the data frame
  summary_count_frames[[df_name]] <- summary_df
}

# Initialize an empty list to store summary data frames
summary_statistics_data_frames <- list()

# Iterate over each data frame in the result list
for (df_name in names(summary_count_frames)) {
  # Filter values between 0 and 253 for each 'Name' group and remove NA values
  filtered_df <- summary_count_frames[[df_name]] %>%
    filter(cyano_cell_count >= 6309.573 & cyano_cell_count <= 7046930)
  
  # Check if the filtered dataframe is not empty
  if (!is_empty(filtered_df)) {
    # Group by 'Name' and calculate summary statistics
    summary_statistics_df <- filtered_df %>%
      group_by(Name) %>%
      summarise(
        average = round(mean(cyano_cell_count, na.rm = TRUE)),
        median = round(median(cyano_cell_count, na.rm = TRUE)),
        maximum = round(max(cyano_cell_count, na.rm = TRUE))
      ) %>%
      arrange(Name) %>%
      as.data.frame()
    
    # Ensure that each dataframe has 13 rows with NA values where there is no valid data available
    all_names <- unique(summary_count_frames[[df_name]]$Name)
    missing_names <- setdiff(all_names, summary_statistics_df$Name)
    missing_df <- data.frame(Name = missing_names,
                             average = rep(NA, length(missing_names)),
                             median = rep(NA, length(missing_names)),
                             maximum = rep(NA, length(missing_names)))
    summary_statistics_df <- rbind(summary_statistics_df, missing_df)
  } else {
    # Create a dataframe with NA values for summary statistics columns
    all_names <- unique(summary_count_frames[[df_name]]$Name)
    summary_statistics_df <- data.frame(Name = all_names,
                                        average = rep(NA, length(all_names)),
                                        median = rep(NA, length(all_names)),
                                        maximum = rep(NA, length(all_names)))
  }
  
  # Store the summary statistics data frame in the list with the same name as the data frame
  summary_statistics_data_frames[[paste0(df_name, "_summary_statistics")]] <- summary_statistics_df
}

# Initialize an empty list to store the dataframes for each entry
compiled_summary_stats <- list()

# Initialize an empty vector to store the unique options
possible_options <- character()

# Iterate over each dataframe in the summary_statistics_data_frames list
for (file_name in names(summary_statistics_data_frames)) {
  # Get the dataframe
  df <- summary_statistics_data_frames[[file_name]]
  
  # Get the unique options from the "Name" column
  options <- unique(df$Name)
  
  # Update the possible_options vector
  possible_options <- union(possible_options, options)
}

# Iterate over each possible option
for (option in possible_options) {
  # Initialize an empty dataframe for the current option
  option_df <- data.frame(date = character(), average = numeric(), median = numeric(), maximum = numeric(), pixel_size = character())
  
  # Iterate over each dataframe in the summary_statistics_data_frames list
  for (file_name in names(summary_statistics_data_frames)) {
    # Get the dataframe
    df <- summary_statistics_data_frames[[file_name]]
    
    # Subset the dataframe for the current option
    entry_df <- df[df$Name == option, ]
    
    # If the entry dataframe is not empty, add it to the option dataframe
    if (nrow(entry_df) > 0) {
      # Extract the date from the file name
      date <- substr(file_name, 2, 8)
      
      # Add a row to the option dataframe
      option_df <- rbind(option_df, c(date, entry_df$average, entry_df$median, entry_df$maximum))
    }
  }
  
  # Rename the first column to "date"
  colnames(option_df)[1] <- "date"
  
  # Add a column for pixel_size repeating "S", "M", and "L" every three rows
  n <- nrow(option_df)
  pixel_size <- rep(c("S", "M", "L"), length.out = n)
  option_df$pixel_size <- pixel_size
  
  # Rename the 7th column to "percent_valid"
  colnames(option_df)[2] <- "average"
  colnames(option_df)[3] <- "median"
  colnames(option_df)[4] <- "maximum"
  
  # Add the option dataframe to the data_completeness list
  compiled_summary_stats[[option]] <- option_df
}

library(dplyr)
library(tidyr)

# Initialize an empty list to store the rearranged dataframes
rearranged_summary_stats <- list()

# Define the pixel sizes
pixel_sizes <- c("S", "M", "L")

# Iterate over each pixel size
for (pixel_size in pixel_sizes) {
  # Initialize an empty dataframe for the current pixel size
  df_combined <- NULL
  
  # Iterate over each dataframe in the compiled_summary_stats list
  for (sensor_id in names(compiled_summary_stats)) {
    # Get the dataframe
    df <- compiled_summary_stats[[sensor_id]]
    
    # Filter the dataframe for the current pixel size
    df_filtered <- df %>%
      filter(pixel_size == pixel_size)
    
    # Add a column for Sensor_ID
    df_filtered$Sensor_ID <- sensor_id
    
    # Append the filtered dataframe to df_combined
    df_combined <- bind_rows(df_combined, df_filtered)
  }
  
  # Store the combined dataframe in the rearranged_summary_stats list
  rearranged_summary_stats[[pixel_size]] <- df_combined
}

rearranged_summary_stats[["L"]] <- rearranged_summary_stats[["L"]] %>% filter(pixel_size == "L") %>% select(-pixel_size)
rearranged_summary_stats[["M"]] <- rearranged_summary_stats[["M"]] %>% filter(pixel_size == "M") %>% select(-pixel_size)
rearranged_summary_stats[["S"]] <- rearranged_summary_stats[["S"]] %>% filter(pixel_size == "S") %>% select(-pixel_size)

################ Filtering data based on completeness 
complete_cases <- comprehensive_data_completeness %>% filter(S > 0.25) #arbitrarily selected 25% completeness at smallest surface area but subject to change
complete_cases.S <- complete_cases %>% select(Sensor_ID, date, S) %>% rename(data_availability = S) 
complete_cases.M <- complete_cases %>% select(Sensor_ID, date, M) %>% rename(data_availability = M)
complete_cases.L <- complete_cases %>% select(Sensor_ID, date, L) %>% rename(data_availability = L)

water_SA_by_sensor.L <- water_SA_by_sensor %>% select(Sensor_ID, water_SA_L) 
water_SA_by_sensor.L$Sensor_ID <- as.character(water_SA_by_sensor.L$Sensor_ID)
water_SA_by_sensor.M <- water_SA_by_sensor %>% select(Sensor_ID, water_SA_M)
water_SA_by_sensor.M$Sensor_ID <- as.character(water_SA_by_sensor.M$Sensor_ID)
water_SA_by_sensor.S <- water_SA_by_sensor %>% select(Sensor_ID, water_SA_S)
water_SA_by_sensor.S$Sensor_ID <- as.character(water_SA_by_sensor.S$Sensor_ID)

summarized_data.L <- rearranged_summary_stats[["L"]] %>% left_join(complete_cases.L, by = c("Sensor_ID", "date")) %>% select(Sensor_ID, date, everything()) %>% left_join(water_SA_by_sensor.L, by = "Sensor_ID") %>% rename(water_coverage = water_SA_L) %>% mutate(date = as.Date(date, format = "%Y%j")) %>% mutate(date = format(date, "%Y-%m-%d"))
summarized_data.M <- rearranged_summary_stats[["M"]] %>% left_join(complete_cases.M, by = c("Sensor_ID", "date")) %>% select(Sensor_ID, date, everything()) %>% left_join(water_SA_by_sensor.M, by = "Sensor_ID") %>% rename(water_coverage = water_SA_M) %>% mutate(date = as.Date(date, format = "%Y%j")) %>% mutate(date = format(date, "%Y-%m-%d"))
summarized_data.S <- rearranged_summary_stats[["S"]] %>% left_join(complete_cases.S, by = c("Sensor_ID", "date")) %>% select(Sensor_ID, date, everything()) %>% left_join(water_SA_by_sensor.S, by = "Sensor_ID") %>% rename(water_coverage = water_SA_S) %>% mutate(date = as.Date(date, format = "%Y%j")) %>% mutate(date = format(date, "%Y-%m-%d"))

#the next step is going to be finding the % of the total pixels available that have a surface scum present, and also to figure out some way to assign whether or not a "bloom" was occurring within the area. -average cell counts reached a threshold? maximum/ median? certain % of area was covered? 




# COMPARING TO PURPLE AIR DATA --------------------------------------------------------------------------------------
# sensor 5838 has data for the first week of June in 2022. 
practice.pa.data <- sensor.5838 %>% select(time_stamp, pm2.5_corrected_by_RH) %>% mutate(Sensor_ID = "5838", pm2.5_corrected_by_RH = round(pm2.5_corrected_by_RH, 2)) %>% rename(date = time_stamp)
summarized_data.M.5838 <- summarized_data.M %>% filter(Sensor_ID == "5838")
joint.data <- summarized_data.M.5838 %>% left_join(practice.pa.data, by = c("date", "Sensor_ID"))



# VISUALIZATIONS ------------------------------------------------------------------------------------------------------
library(tidyr); library(ggplot2); library(purrr)

create_sensor_list <- function(...) {
  # Use the list() function to create a named list of data frames
  df_list <- list(...)
  
  # Set the names of the list to the names of the data frames
  names(df_list) <- sapply(substitute(list(...))[-1], deparse)
  
  return(df_list)
}

# Use the function to create the list
df_list <- create_sensor_list(sensor.1318, sensor.1334, sensor.1344, sensor.1348, sensor.1358, sensor.1362, sensor.1378, sensor.1680, sensor.1806, sensor.5822, sensor.5838, sensor.9875)

# PM1 
extract_and_rename <- function(df, df_name) {
  # Extract the last four characters from the dataframe name
  suffix <- substr(df_name, max(nchar(df_name) - 3, 1), nchar(df_name))
  
  selected_cols <- df %>%
    select(time_stamp, starts_with("pm1_corrected_by_RH")) %>%
    rename_at(vars(starts_with("pm1_corrected_by_RH")), ~ paste0("sensor_", suffix))
  
  return(selected_cols)
}

# Apply the function to each data frame in the list
modified_dfs <- imap(df_list, extract_and_rename)

# Combine modified data frames into a single data frame, and also changing any negative values to 0
pm1_mass_conc <- modified_dfs %>% reduce(left_join, by = "time_stamp") %>% mutate_all(function(x) ifelse(x < 0, 0, x))

# PM2.5 
extract_and_rename <- function(df, df_name) {
  # Extract the last four characters from the dataframe name
  suffix <- substr(df_name, max(nchar(df_name) - 3, 1), nchar(df_name))
  
  selected_cols <- df %>%
    select(time_stamp, starts_with("pm2.5_corrected_by_RH")) %>%
    rename_at(vars(starts_with("pm2.5_corrected_by_RH")), ~ paste0("sensor_", suffix))
  
  return(selected_cols)
}

# Apply the function to each data frame in the list
modified_dfs <- imap(df_list, extract_and_rename)

# Combine modified data frames into a single data frame, and also changing any negative values to 0
pm2.5_mass_conc <- modified_dfs %>% reduce(left_join, by = "time_stamp") %>% mutate_all(function(x) ifelse(x < 0, 0, x))

# PM10 
extract_and_rename <- function(df, df_name) {
  # Extract the last four characters from the dataframe name
  suffix <- substr(df_name, max(nchar(df_name) - 3, 1), nchar(df_name))
  
  selected_cols <- df %>%
    select(time_stamp, starts_with("pm10_corrected_by_RH")) %>%
    rename_at(vars(starts_with("pm10_corrected_by_RH")), ~ paste0("sensor_", suffix))
  
  return(selected_cols)
}

# Apply the function to each data frame in the list
modified_dfs <- imap(df_list, extract_and_rename)

# Combine modified data frames into a single data frame, and also changing any negative values to 0
pm10_mass_conc <- modified_dfs %>% reduce(left_join, by = "time_stamp") %>% mutate_all(function(x) ifelse(x < 0, 0, x))

# PM2.5 Plot
pm2.5_mass_conc_long <- pm2.5_mass_conc %>%
  pivot_longer(cols = -time_stamp, names_to = "variable", values_to = "PM2.5_Mass_Concentration")

# Calculate the overall average
overall_avg <- pm2.5_mass_conc_long %>%
  summarise(overall_avg = mean(PM2.5_Mass_Concentration, na.rm = TRUE))

# Convert time_stamp to Date if it's not already
pm2.5_mass_conc_long$time_stamp <- as.Date(pm2.5_mass_conc_long$time_stamp)

# Add overall average as a variable
pm2.5_mass_conc_long <- pm2.5_mass_conc_long %>%
  mutate(overall_avg = overall_avg$overall_avg)

# Plot using ggplot2
ggplot(pm2.5_mass_conc_long, aes(x = time_stamp, y = PM2.5_Mass_Concentration, color = variable)) +
  geom_point() +
  geom_line(aes(y = overall_avg), linetype = "dashed", color = "black", size = 1) +
  labs(title = "PM2.5 Mass Concentration Over Time",
       x = "Time Stamp",
       y = "PM2.5 Mass Concentration",
       color = "Variable") +
  theme_minimal()

# Plot using ggplot2
ggplot(pm2.5_mass_conc_long, aes(x = time_stamp, y = PM2.5_Mass_Concentration, color = variable)) +
  geom_point(size = 3) +
  scale_color_manual(values = ifelse(pm1_mass_conc_long$variable %in% c("sensor_1562", "sensor_1358"), "black", "gray")) +
  labs(title = "PM2.5 Mass Concentration Over Time",
       x = "Time Stamp",
       y = "PM2.5 Mass Concentration") +
  theme_minimal()

library(lubridate)

# Create a new variable for the season
pm2.5_mass_conc_long <- pm2.5_mass_conc_long %>%
  mutate(season = ifelse(month(time_stamp) %in% 5:9, "Bloom season", "Non-bloom season"))

# Plot using ggplot2
ggplot(pm2.5_mass_conc_long, aes(x = season, y = PM2.5_Mass_Concentration, fill = season)) +
  geom_boxplot() +
  labs(title = "Boxplot of PM2.5 Mass Concentration by Season",
       x = "Season",
       y = "PM2.5 Mass Concentration") +
  theme_minimal()







