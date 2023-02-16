#########################
#                       #
#  EXTRACTING HASHTAGS  #
#                       #
#########################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 08/01/23

# DESCRIPTION: This file extracts the hashtags used in each Tweet stored in the raw JSON files and creates a list of hashtags used by each BIOID.
# It runs the extraction in parallel across multiple cores in our to speed up processing.


library(jsonlite)
library(dplyr)
library(lubridate)
library(stringr)
library(parallel)
library(tidyr)
library(utils)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())


### STEP 1: SET UP FUNCTION TO EXTRACT HASHTAGS ###


# Get directory containing scraped Tweets on University of Nottingham OneDrive
dir<- choose.dir(caption = "Select directory containing scraped Tweets")

# Create list of paths to the files within the directory. Each file is the Tweets from one account.
files<- list.files(dir, recursive = T)
files<- paste0(dir, "/", files)

# Define a function to extract the hashtags from each JSON file
extract_hashtags<- function(json_paths){
  
  # Load libraries. This is necessary as the function will be being run in separate instances of R.
  library(dplyr)
  library(jsonlite)
  library(jqr)
  
  # Loop over files
  for (path in json_paths){
    
    # Read in JSON file as string
    raw_string<- readLines(con = path)
    
    # Concatenate the JSON elements into a JSON array
    combined_string<- paste0("[", paste0(raw_string, collapse = ","), "]")
    
    # Parse JSON string using JQ interface to keep only relevant variables
    combined_string<- combined_string%>%
      jq('.[] | {id_str: .id_str, created_at: .created_at, htags: .entities.hashtags[].text, user_id_str: .user.id_str, user_screen_name: .user.screen_name}')
    
    # Re-concatenate the parsed JSON elements into a JSON array
    combined_string<- paste0("[", paste0(combined_string, collapse = ","), "]")
    
    # Convert JSON array to R dataframe
    df<- fromJSON(combined_string)
    
    if(exists("hashtag_df")){
      hashtag_df<- rbind(hashtag_df, df)
    } else {
      hashtag_df<- df
    }
    
  }  
  return(hashtag_df)
}



htag_df<- extract_hashtags(json_paths = files)


### STEP 2: RUN FUNCTION IN PARALLEL ACROSS MULTIPLE CORES ###


# Set number of cores to use
n_cores<- parallel::detectCores() - 1

# Create a cluster of cores
cl<- makeCluster(n_cores)

# Create a list of file ranges for each core to use
#file_ranges<- list(c(1,499), c(500,999), c(1000, 1499), c(1500, 1999), c(2000,2499), c(2500,2999), c(3000,3785))

# Export relevant R objects to the cluster
clusterExport(cl = cl, varlist = c("extract_hashtags", "files"))

# Apply the extract_hashtags function across the files
results<- parLapply(cl, files, extract_hashtags)

# Stop the cluster of cores
stopCluster(cl)

# Save the results of the parLapply function (as a backup)
save(results, file = "parLapply_results_hashtag_extraction.Rda")


### STEP 3: JOIN HASHTAGS TO BIOIDS AND SAVE ###


# Bind all the results together as one datafrmae 
df<- do.call("rbind", results)

# Read in lookup between BIOIDs and Twitter handles
lookup<- read.csv("..\\Lookups\\BIOID_to_twitter_handle_lookup.csv")

# Create lowercase account handles in both the hashtag data and the lookup
df<- df%>%
  mutate(lower_user_screen_name = str_to_lower(user_screen_name))

lookup<- lookup%>%
  mutate(Lower_Account_Handle = str_to_lower(Account_Handle))

# Check for handles in the hashtag data that are not present in the lookup.
# These should be handles that were originally included in the data collection but which have seubsequently been excluded.
df%>%
  filter(!lower_user_screen_name %in% lookup$Lower_Account_Handle)%>%
  group_by(lower_user_screen_name)%>%
  slice_max(order_by = row_number(), n = 1)%>%
  dplyr::select(user_screen_name)

# Join the BIOID to the hashtag data
df<- left_join(df, dplyr::select(lookup, BIOID, Lower_Account_Handle), by = c("lower_user_screen_name" = "Lower_Account_Handle"))

# Filter out hashtags not linked to a BIOID
df<- df%>%
  filter(!is.na(BIOID))

# Select relevant variables and rename to more informative names
df<- df%>%
  dplyr::select(BIOID, user_screen_name, user_id_str, id_str, htags, created_at)%>%
  rename(tweet_id_str = id_str,
         hashtags = htags,
         tweet_date = created_at)

# Rename dataframe to more informative name
hashtag_df<- df

# Save the hashtag data as an Rda and CSV file
save(hashtag_df, file = "Hashtag Data.Rda")
write.csv(hashtag_df, file = "Hashtag Data.csv", row.names = F, fileEncoding = "UTF-8")
