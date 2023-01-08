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
extract_hashtags<- function(row_range){
  
  # Load the relevant packages (required since the function will be applied in separate instances of R)
  library(jsonlite)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(parallel)
  library(tidyr)
  
  # Create a list of file indexes to loop over
  range<- seq(file_range[1], file_range[2])
  
  # Loop over files
  for (i in range){
    
    # Read in JSON file
    file<- files[i]
    raw_string<- readLines(con = file)
    combined_string<- paste0("[", paste0(raw_string, collapse = ","), "]")
    df<- fromJSON(combined_string)
    
    # Get account ID and handle
    user_id_str<- df$user$id_str[1]
    user_screen_name<- df$user$screen_name[1]
    
    # Get hashtag entities, where available
    if( !is.null(df$entities$hashtags)){
      # Get the list of dataframes containing the hashtags for each Tweet
      htags<- df$entities$hashtags
      # Combine the list of dataframes into one dataframe
      htags<- do.call("rbind", htags)
      # Combine all the hashtags into one text string, separated by spaces
      htags<- paste0(htags$text, collapse = " ")
    } else {
      # If no hashtags available, set htags to NA
      htags<- NA
    }
    
    # Create a dataframe containing the account ID and handle and the hashtag string
    out_df<- data.frame(user_id_str = user_id_str, user_screen_name = user_screen_name, htags = htags)
    
    # Add the created dataframe to a dataframe to output from the function
    if(exists("hashtag_df")){
      hashtag_df<- rbind(hashtag_df, out_df)
    } else {
      hashtag_df<- out_df
    }
    
  }  
  return(hashtag_df)
}


### STEP 2: RUN FUNCTION IN PARALLEL ACROSS MULTIPLE CORES ###


# Set number of cores to use
n_cores<- parallel::detectCores() - 1

# Create a cluster of cores
cl<- makeCluster(7)

# Create a list of file ranges for each core to use
file_ranges<- list(c(1,499), c(500,999), c(1000, 1499), c(1500, 1999), c(2000,2499), c(2500,2999), c(3000,3785))

# Export relevant R objects to the cluster
clusterExport(cl = cl, varlist = c("extract_hashtags", "file_ranges", "files"))

# Apply the extract_hashtags function across the files
results<- parLapply(cl, rows, extract_hashtags)

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

# Check all handles in the hashtag data are present in the lookup, allowing for differences in capitalisation
table(df$lower_user_screen_name %in% lookup$Lower_Account_Handle)

# Join the BIOID to the hashtag data
df<- left_join(df, select(lookup, BIOID, Lower_Account_Handle), by = c("lower_user_screen_name" = "Lower_Account_Handle"))

# Select relevant variables and rename to more informative names
df<- df%>%
  select(BIOID, user_screen_name, user_id_str, htags)%>%
  rename(hashtags = htags)

# Rename dataframe to more informative name
hashtag_df<- df

# Save the hashtag data as an Rda and CSV file
save(hashtag_df, file = "Hashtag Data.Rda")
write.csv(hashtag_df, file = "Hashtag Data.csv", row.names = F, fileEncoding = "UTF-8")
