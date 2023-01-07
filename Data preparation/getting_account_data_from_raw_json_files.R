##############################################
#                                            #
#  GETTING ACCOUNT DATA FROM RAW JSON FILES  #
#                                            #
##############################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 06/01/23

# DESCRIPTION: This file takes the raw JSON files which contain all the scraped tweets (provided by Digital Society Project) and extracts
# a set of variables about each Twitter account. It then combines them together and saves the data in a Rda and CSV file.

library(jsonlite)
library(dplyr)
library(lubridate)
library(stringr)
library(parallel)
library(tidyr)
library(utils)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())


### STEP 1: SET UP FUNCTION TO READ JSON FILES ###


# Get path to raw JSON files on Univeristy of Nottingham OneDrive
dir<- choose.dir(caption = "Select directory containing Twitter scrapes")

# Get list of JSON files. There is one for each Twitter account
files<- list.files(dir, recursive = T)
files<- paste0(dir, "/", files)

# Loop over files
for (file in files){
  
  # Read first line of JSON (ie: first Tweet)
  raw_string<- readLines(con = file, n = 1)
  combined_string<- paste0("[", raw_string, "]")
  df<- fromJSON(combined_string)
  
  # Flatten nested account data
  account<- flatten(df$user[1,], recursive = T)
  
  # Add extracted account data to dataframe to output
  if(exists("accounts_df")){
    # Bind new data to existing dataframe, filling with NAs where variables are missing
    accounts_df<- plyr::rbind.fill(accounts_df, account )
  } else {
    accounts_df<- account
  }
  
}


# Remove nested list variables
accounts_df<- accounts_df%>%
  select(-c(withheld_in_countries, entities.description.urls, entities.url.urls))

# Get data folder on University of Nottingham OneDrive
data_dir<- choose.dir(caption = "Select data directory on University of Nottingham OneDrive under Twitter Scrapes > Datasets")

# Save as Rda file
save(accounts_df, file = paste0(data_dir, "\\Account Data\\Rda Formatted Data\\Account Data.Rda"))

# Save as CSV file
write.csv(accounts_df, file = paste0(data_dir, "\\Account Data\\CSV Formatted Data\\Account Data.csv"), row.names = F, fileEncoding = "UTF8")
