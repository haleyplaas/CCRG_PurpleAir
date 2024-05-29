# ------------------- DOWNLOADING CYANOBACTERIAL INDICES FROM CYAN USING EARTHDATA AND OCLI ------- #
# Load necessary packages into R
library(sys); library(getPass); library(httr); library(dplyr)

# STEP 1. --------------------------------- SET UP ENVIRONMENT -------------------------------------------- #
# dl_dir <- Sys.getenv("HOME") # will find you your home directory -- only folder location where this will work
# need to establish wd, download directory, and location of netrc file as the same folder. The script I found on NASA's
# chat boards were automatically creating the .netrc file on my base C://, which I cannot access/ modify since I have a 
# university computer -- so I changed the directory manually here. IT MUST BE YOUR HOME DIRECTORY TO WORK. On my UNC work 
# computer, this is my OneDrive/ Documents folder

setwd("C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Documents")

# STEP 2. ----------------- CREATE NETRC AND COOKIES .TXT FILES ------------------------------------------ #
# These files must be created outside of R and put into the same file as the working directory 
## to set up a .netrc file on a windows computer, you do not need to use terminal or cURL. 
### All you need to do is create a .txt file with a single line that reads: 
#### machine urs.earthdata.nasa.gov login MY LOGIN password MY PASSWORD

netrc <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Documents/.netrc"
netrc_path <- netrc # pass the netrc info down to another line of code I found online, didnt want to fix what wasnt broken
cat(readLines(netrc), sep = "\n") #confirm the user and password is reading correctly in the .netrc

## Same for a Netscape HTTP Cookie File (where your specific cookies data can be stored as data is scraped from the web)
### create a .txt file with a single line that reads: 
#### urs.earthdata.nasa.gov TRUE / FALSE 0 USER_TOKEN <MY BEARER TOKEN>

cookie_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Documents/.urs_cookies.txt" 
set_config(config(followlocation=1,netrc=1,netrc_file=netrc_path,cookie=cookie_path,cookiefile=cookie_path,cookiejar=cookie_path))

# STEP 3. ------------------------------------CHECK/ CREATE .NETRC FILE--------------------------------------- #
# If you already have a .netrc file with your Earthdata Login credentials stored in your home
# directory, this portion will be skipped. Otherwise you will be prompted for your NASA Earthdata
# Login Username/Password and a netrc file will be created to store your credentials (in home dir)
## I used this to check to ensure my .netrc file was being read properly. Once it was, 
## I was NOT prompted for my credentials. When prompted for my credentials, the url download wouldn't work
if (file.exists(netrc) == FALSE || grepl("urs.earthdata.nasa.gov", readLines(netrc)) == FALSE) {
  netrc_conn <- file(netrc)
  
  # User will be prompted for NASA Earthdata Login Username and Password below
  writeLines(c("machine urs.earthdata.nasa.gov",
               sprintf("login %s", getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")),
               sprintf("password %s", getPass(msg = "Enter NASA Earthdata Login Password:"))), netrc_conn)
  close(netrc_conn)
}

# STEP 4. ---------------------------CONNECT TO DATA POOL AND DOWNLOAD FILES------------------------------ #
## SINGLE FILE DOWNLOAD -- use this to check if it is working 
# replace downloaded file path with the folder you want to save the data in with the NAME OF YOUR FILE to be saved
## replace LINK TO DATA and APP KEY with your specific link to the .tif file and application key from earthdata
#downloaded_file_path <- "C:/Users/hplaas/OneDrive - University of North Carolina at Chapel Hill/Documents/SeaDAS files/CyanoIndices/NAME OF YOUR FILE.tif"
#httr::GET(url = "{LINK TO DATA}?appkey={MY APP KEY}", write_disk(downloaded_file_path, overwrite = TRUE))

## MULTIPLE FILE DOWNLOAD (multiple dates at once)
# Function to download and save a file
download_and_save <- function(url, app_key, save_dir) {
  # Extract the file name from the URL
  file_name <- basename(url)
  
  # Set up the download path
  downloaded_file_path <- file.path(save_dir, file_name)
  
  # Construct the full URL with the app key
  full_url <- paste0(url, "?appkey=", app_key)
  
  # Download and save the file
  httr::GET(url = full_url, write_disk(downloaded_file_path, overwrite = TRUE))
}

# Read the list of URLs from the file
## the url_file should be a .txt file (in the working directory) that has the urls to each file to download in a single line
url_file <- "{YOUR .TXT FILE}.txt"
urls <- readLines(url_file)

# Loop through each URL and download
for (url in urls) {
  download_and_save(url, "{MY APP KEY}", "{DIRECTORY TO STORE FILES IN}")
}
