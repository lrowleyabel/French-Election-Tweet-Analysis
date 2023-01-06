############################################
#                                          #
#  GETTING TWEET DATA FROM RAW JSON FILES  #
#                                          #
############################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 06/01/23

# DESCRIPTION: This file takes the raw JSON files which contain all the scraped tweets (provided by Digital Society Project) and extracts
# a set of variables for each tweet. It does this using parallel workers across multiple cores to speed up the extraction.
# It then combines them together in a series of Rda and CSV files.

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

# Create list of variables to extract from JSON files
keep_cols<- c("created_at", "id_str", "full_text", "favorite_count", "retweet_count", "is_quote_status", "is_retweet", "user_id_str", "user_screen_name")

# Define function that can loop over a list of files to extract these variables. This function
# will be run in parallel across different cores to speed up processing of files.
read_tweet_json<- function(file_range){
  
  # Load packages. Required as function will be running in different instances of R.
  library(jsonlite)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(parallel)
  library(tidyr)
  
  # Nest within tryCatch so that if an error occurs on one core it returns NULL rather than stopping all cores 
  output_df<- tryCatch(
    {
      # Ignore any warnings so that they don't trigger the tryCatch warning handler
      suppressWarnings({
        
        # Loop over the file range passed to the function
        for (i in file_range){
          
          # Read in file
          file<- files[i]
          raw_string<- readLines(con = file)
          combined_string<- paste0("[", paste0(raw_string, collapse = ","), "]")
          df<- fromJSON(combined_string)
          
          # Get nested key user info and add as normal variable
          df$user_screen_name<- df$user$screen_name
          df$user_id_str<- df$user$id_str
          
          # Create variable indicating if Tweet is a Retweet
          if(!"retweeted_status" %in% colnames(df)){
            df$is_retweet<- FALSE
          } else {
            df$is_retweet<- lapply(df$retweeted_status[[1]], FUN = function(x){return(!is.na(x))})%>%
              unlist()
          }
          
          # Keep the relevant variables
          df<- df%>%
            select(any_of(keep_cols))
          
          # Add extracted data to a dataframe to output
          if(exists("all_tweets")){
            # Make sure variables match between new data and existing data
            df<- select(df, any_of(colnames(all_tweets)))
            # Make sure rownames are just rownumbers
            rownames(df)<- c(nrow(all_tweets)+1):c(nrow(all_tweets)+nrow(df))
            # Bind new data to existing data
            all_tweets<- data.table::rbindlist(list(all_tweets, df))
          } else {
            all_tweets<- df
          }
          
        }
        # Return output dataframe from tryCatch attempt
        return(all_tweets)
      })
    },
    error = function(cond){
      return(NULL)
    }
  )
  # Return output dataframe from read_tweet_json function
  return(output_df)
}


### STEP 2: RUN FUNCTION IN PARALLEL ACROSS MULTIPLE CORES ###


# Use three fewer cores than the available cores
n_cores<- detectCores() - 3

# Create a cluster of cores running R to run in parallel 
cl<- makeCluster(n_cores)

# Split files indexes into a set of n lists where n is n_cores. Each core will read the files in its given range.
file_ranges<- split(1:length(files), ceiling(seq_along(1:length(files))/20))

# Export the relevant R objects to the cluster
clusterExport(cl = cl, varlist = c("read_tweet_json", "file_ranges", "files", "keep_cols"))

# Apply the read_tweet_json function to each set of files, in parallel across the cluster of cores
results<- parLapply(cl, file_ranges, read_tweet_json)

# Stop the cluster of cores
stopCluster(cl)