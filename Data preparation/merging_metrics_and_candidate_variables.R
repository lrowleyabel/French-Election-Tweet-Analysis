#############################################
#                                           #
#  MERGING METRICS AND CANDIDATE VARIABLES  #
#                                           #
#############################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 08/01/23

# DESCRIPTION: This file takes the mterics of Twitter activity for each candidate and merges it with the
# relevant demographic and political data collected on each candidate by the TplF team.

library(dplyr)
library(readxl)
library(utils)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())

# Get directory to candidate datasets
candidate_dir<- choose.dir(caption = "Select directory containing working candidate datasets on University of Nottingham OneDrive (TplF - Working Dataset)")

# Read in the combined candidate dataset
candidate_df<- read_excel(paste0(candidate_dir, "/Substantive Data Combined Candidates 05-01-23.xlsx"))

# Get directory containing processed data
dataset_dir<- choose.dir("Select directory containing metrics data on University of Nottingham OneDrive, under Twitter Scrapes > Datasets")

# Read in the Tweet Metrics data
load(paste0(dataset_dir, "\\Overall Metric\\Overall Twitter Metrics.Rda"))

# Check all BIOIDs in the metrics data are in the candidate data
table(metrics_df$BIOID %in% candidate_df$BIOID)

# Create variable in candidate data indicating whether they have a Twitter account
candidate_df<- candidate_df%>%
  mutate(HAS_TWITTER_ACCOUNT = ifelse(BIOID %in% metrics_df$BIOID, 1, 0))

# Change BIOID variable in the metrics data from numeric to string
metrics_df<- metrics_df%>%
  mutate(BIOID = as.character(BIOID))

# Merge metrics and candidate data
df<- right_join(metrics_df, candidate_df, by = "BIOID")

# Save final dataset as Rda and CSV file
save(df, file = paste0(dataset_dir, "\\Analysis Dataset\\Analysis Dataset.Rda"))
write.csv(df, file = paste0(dataset_dir, "\\Analysis Dataset\\Analysis Dataset.csv"), row.names = F, fileEncoding = "UTF-8")
