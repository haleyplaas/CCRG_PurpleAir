setwd('C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\CCRG_PurpleAir')
rm(list = ls())
library(httr);library(jsonlite);library(dplyr)

# API Calls to Retrieve PurpleAir Sensor PM data 6/1/2022-Present ----------------------------------------------------------------------
# purple air API
#read_api_key <- "" #Read API Key 
#write_api_key <- "" #Write API Key

date_string <- "2024-01-01" #input date here 
# Convert the date string to a UNIX timestamp
unix_timestamp <- as.POSIXct(date_string, format = "%Y-%m-%d", tz = "UTC")
# Extract the numeric value of the UNIX timestamp
unix_timestamp_numeric <- as.numeric(unix_timestamp)
# Print the UNIX timestamp
print(unix_timestamp_numeric)
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

#requesting humidity data

#1. OBX 151806 -------------------------------
sensor151806 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151806/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151806_list <- fromJSON(rawToChar(sensor151806$content), flatten = TRUE)
maxlength = max(lengths(sensor151806_list))
sensor151806_list2 = lapply(sensor151806_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151806_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151806_temp.csv", row.names=FALSE)

sensor151806 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151806/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151806_list <- fromJSON(rawToChar(sensor151806$content), flatten = TRUE)
maxlength = max(lengths(sensor151806_list))
sensor151806_list2 = lapply(sensor151806_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151806_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151806_2_temp.csv", row.names=FALSE)

#2. N.ELIZ 151344 -----------------------------
sensor151344 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151344/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151344_list <- fromJSON(rawToChar(sensor151344$content), flatten = TRUE)
maxlength = max(lengths(sensor151344_list))
sensor151344_list2 = lapply(sensor151344_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151344_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151344_temp.csv", row.names=FALSE)

sensor151344 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151344/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151344_list <- fromJSON(rawToChar(sensor151344$content), flatten = TRUE)
maxlength = max(lengths(sensor151344_list))
sensor151344_list2 = lapply(sensor151344_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151344_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151344_2_temp.csv", row.names=FALSE)

#3. MID.ELIZ 179875 --------------
sensor179875 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/179875/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor179875_list <- fromJSON(rawToChar(sensor179875$content), flatten = TRUE)
maxlength = max(lengths(sensor179875_list))
sensor179875_list2 = lapply(sensor179875_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor179875_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\179875_temp.csv", row.names=FALSE) 

sensor179875 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/179875/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor179875_list <- fromJSON(rawToChar(sensor179875$content), flatten = TRUE)
maxlength = max(lengths(sensor179875_list))
sensor179875_list2 = lapply(sensor179875_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor179875_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\179875_2_temp.csv", row.names=FALSE) 

#4. PH.ELIZ 151318 -------------------------
sensor151318 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151318/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151318_list <- fromJSON(rawToChar(sensor151318$content), flatten = TRUE)
maxlength = max(lengths(sensor151318_list))
sensor151318_list2 = lapply(sensor151318_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151318_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151318_temp.csv", row.names=FALSE)

sensor151318 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151318/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151318_list <- fromJSON(rawToChar(sensor151318$content), flatten = TRUE)
maxlength = max(lengths(sensor151318_list))
sensor151318_list2 = lapply(sensor151318_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151318_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151318_2_temp.csv", row.names=FALSE)

#5. NIX 151362 ------------------------------
sensor151362 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151362/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151362_list <- fromJSON(rawToChar(sensor151362$content), flatten = TRUE)
maxlength = max(lengths(sensor151362_list))
sensor151362_list2 = lapply(sensor151362_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151362_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151362_temp.csv", row.names=FALSE)

sensor151362 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151362/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151362_list <- fromJSON(rawToChar(sensor151362$content), flatten = TRUE)
maxlength = max(lengths(sensor151362_list))
sensor151362_list2 = lapply(sensor151362_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151362_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151362_2_temp.csv", row.names=FALSE)

#6. HERT 151378 (still isnt working for first one, try and redo via purple air -----------------------------------
sensor151378 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151378/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2Cpm10.0_cf_1_a%2C%20pm10.0_cf_1_b%2C%20temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151378_list <- fromJSON(rawToChar(sensor151378$content), flatten = TRUE)
str(sensor151378_list)
sensor151378_list2 = lapply(sensor151378_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151378_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151378.csv", row.names=FALSE)

sensor151378 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151378/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151378_list <- fromJSON(rawToChar(sensor151378$content), flatten = TRUE)
maxlength = max(lengths(sensor151378_list))
sensor151378_list2 = lapply(sensor151378_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151378_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151378_2_temp.csv", row.names=FALSE)

#7. EDE.HM 151348 -----------------------------------
sensor151348 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151348/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=humidity%2C%20pm1.0_cf_1_a%2C%20pm1.0_cf_1_b%2C%20pm2.5_cf_1_a%2C%20pm2.5_cf_1_b%2C%20pm10.0_cf_1_a%2C%20pm10.0_cf_1_b%2C%20temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151348_list <- fromJSON(rawToChar(sensor151348$content), flatten = TRUE)
maxlength = max(lengths(sensor151348_list))
sensor151348_list2 = lapply(sensor151348_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151348_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151348.csv", row.names=FALSE)

sensor151348 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151348/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151348_list <- fromJSON(rawToChar(sensor151348$content), flatten = TRUE)
maxlength = max(lengths(sensor151348_list))
sensor151348_list2 = lapply(sensor151348_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151348_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151348_2_temp.csv", row.names=FALSE)

#8. EDE.Q 145838 ---------------------------------------------
sensor145838 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/145838/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor145838_list <- fromJSON(rawToChar(sensor145838$content), flatten = TRUE)
maxlength = max(lengths(sensor145838_list))
sensor145838_list2 = lapply(sensor145838_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor145838_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\145838_temp.csv", row.names=FALSE)

sensor145838 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/145838/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor145838_list <- fromJSON(rawToChar(sensor145838$content), flatten = TRUE)
maxlength = max(lengths(sensor145838_list))
sensor145838_list2 = lapply(sensor145838_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor145838_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\145838_2_temp.csv", row.names=FALSE)

#9.COLE 151680 --------------------------------------
sensor151680 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151680/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151680_list <- fromJSON(rawToChar(sensor151680$content), flatten = TRUE)
maxlength = max(lengths(sensor151680_list))
sensor151680_list2 = lapply(sensor151680_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151680_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151680_temp.csv", row.names=FALSE)

# this one is also being weird ---------------
sensor151680 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151680/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151680_list <- fromJSON(rawToChar(sensor151680$content), flatten = TRUE)
maxlength = max(lengths(sensor151680_list))
sensor151680_list2 = lapply(sensor151680_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151680_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151680_2_temp.csv", row.names=FALSE)

#10. ARR.C 145822 ------------------------
sensor145822 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/145822/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor145822_list <- fromJSON(rawToChar(sensor145822$content), flatten = TRUE)
maxlength = max(lengths(sensor145822_list))
sensor145822_list2 = lapply(sensor145822_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor145822_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\145822_temp.csv", row.names=FALSE)

sensor145822 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/145822/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor145822_list <- fromJSON(rawToChar(sensor145822$content), flatten = TRUE)
maxlength = max(lengths(sensor145822_list))
sensor145822_list2 = lapply(sensor145822_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor145822_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\145822_2_temp.csv", row.names=FALSE)

#11. ARR.M 151334 ------------------------------------
sensor151334 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151334/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151334_list <- fromJSON(rawToChar(sensor151334$content), flatten = TRUE)
maxlength = max(lengths(sensor151334_list))
sensor151334_list2 = lapply(sensor151334_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151334_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151334_temp.csv", row.names=FALSE)

sensor151334 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151334/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151334_list <- fromJSON(rawToChar(sensor151334$content), flatten = TRUE)
maxlength = max(lengths(sensor151334_list))
sensor151334_list2 = lapply(sensor151334_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151334_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151334_2_temp.csv", row.names=FALSE)

#12. WIND 151358 -----------------------------
sensor151358 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151358/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151358_list <- fromJSON(rawToChar(sensor151358$content), flatten = TRUE)
maxlength = max(lengths(sensor151358_list))
sensor151358_list2 = lapply(sensor151358_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151358_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151358_temp.csv", row.names=FALSE)

sensor151358 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151358/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151358_list <- fromJSON(rawToChar(sensor151358$content), flatten = TRUE)
maxlength = max(lengths(sensor151358_list))
sensor151358_list2 = lapply(sensor151358_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151358_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151358_2_temp.csv", row.names=FALSE)

#13. MURF 151562 ---------------------------
sensor151562 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151562/history?start_timestamp=1654041600&end_timestamp=1685577600&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151562_list <- fromJSON(rawToChar(sensor151562$content), flatten = TRUE)
maxlength = max(lengths(sensor151562_list))
sensor151562_list2 = lapply(sensor151562_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151562_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151562_temp.csv", row.names=FALSE)

sensor151562 <- httr:: GET((url = 'https://api.purpleair.com/v1/sensors/151562/history?start_timestamp=1685577600&end_timestamp=1704067200&average=1440&fields=temperature'), config = add_headers('X-API-Key' = 'AF9F770D-16A5-11EE-A77F-42010A800009'))
sensor151562_list <- fromJSON(rawToChar(sensor151562$content), flatten = TRUE)
maxlength = max(lengths(sensor151562_list))
sensor151562_list2 = lapply(sensor151562_list, function(x) {length(x) = maxlength; return(x)})
write.csv(sensor151562_list2, "C:\\Users\\heplaas\\OneDrive - North Carolina State University\\Code Repositories\\R\\CCRG\\purpleairdata\\151562_2_temp.csv", row.names=FALSE)

