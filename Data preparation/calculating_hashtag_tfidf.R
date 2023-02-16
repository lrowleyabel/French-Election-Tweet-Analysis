###############################
#                             #
#  CALCULATING HASHTAG TFIDF  #
#                             #
###############################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 16/02/23

# DESCRIPTION: This file takes the hashtags used by each candidate and creates a score representing how
# distinctive their hashtags are (the TF-IDF score) based on each hashtag's frequency in their Tweets compared
# to its frequency in the corpus of Tweets as a whole. It also creates a score representing how distinctive
# their hashtags are relative to those used by others in their party.


library(dplyr)
library(tidytext)
library(ggplot2)
library(utils)
library(readxl)
library(lubridate)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())


### STEP 1: SET UP DATA IN TIDY TEXT FORMAT ###


# Get directory containing processed data
dataset_dir<- choose.dir("Select directory containing metrics data on University of Nottingham OneDrive, under Twitter Scrapes > Datasets")

# Load in hashtag data
load(paste0(dataset_dir, "//Hashtag Data/Rda Formatted Data//Hashtag Data.Rda"))

# Parse date as a date
hashtag_df<- hashtag_df%>%
  mutate(date = parse_date_time(tweet_date, "a b d H:M:S z Y")%>%
           lubridate::date())

# Filter for only hashtags that appear in tweets posted within the 9 months prior to the election
hashtag_df<- hashtag_df%>%
  filter(date >= dmy("12/09/2021"))

# Select relevant variables
hashtag_df<- hashtag_df%>%
  select(BIOID, hashtags)


### STEP 2: CALCULATE MEAN OVERALL TF-IDF FOR EACH CANDIDATE ###


# This step calculates how distinct, on average, each candidate's hashtags were compared to the corpus of tweets as a whole

# Calculate hashtag frequency for each BIOID
htag_freq<- hashtag_df%>%
  count(BIOID, hashtags, sort = T)

# Calculate the overall TF-IDF score for each BIOID-hashtag combination
tfidf<- htag_freq%>%
  bind_tf_idf(hashtags, BIOID, n)

# Calculate the mean overall TF-IDF score for each BIOID
mean_tfidf<- tfidf%>%
  group_by(BIOID)%>%
  summarise(mean_tfidf = mean(tf_idf, na.rm = T))

# Plot the distribution of the mean overall TF-IDF scores
ggplot(mean_tfidf)+
  geom_histogram(aes(x = mean_tfidf), bins = 100, color = "white")


### STEP 3: CALCULATE THE MEAN WITHIN-PARTY TF-IDF FOR EACH CANDIDATE ###


# This step calculates how distinct, on average, each candidate's tweets were compared to the tweets of others from their party

# Get directory containing main candidate datasets on University of Nottingham OneDrive
candidate_dataset_dir<- choose.dir(caption = "Select directory containing main candidate datasets (TplF - Working Datasets)")

# Read in candidate dataset
candidate_df<- read_excel(paste0(candidate_dataset_dir, "//Substantive Data Combined Candidates 05-01-23.xlsx"))

# Select the variables indicating party/list name and whether the candidate is independent from the candidate dataset
candidate_df<- candidate_df%>%
  select(BIOID, LISTNAME_ORIGINAL, INDEPENDENT)

# Create collapsed party variable and drop unnecessary variables
candidate_df<- candidate_df%>%
  mutate(party_list = case_when(LISTNAME_ORIGINAL == "Ensemble" ~ "Ensemble",
                                LISTNAME_ORIGINAL == "Rassemblement National" ~ "Rassemblement National",
                                LISTNAME_ORIGINAL == "La Nouvelle Union populaire écologique et sociale" ~ "NUPES",
                                LISTNAME_ORIGINAL == "Les Républicains" ~ "Les Républicains",
                                LISTNAME_ORIGINAL == "Reconquête" ~ "Reconquête",
                                INDEPENDENT == 1 ~ "Independent",
                                T ~ "Other party"))%>%
  select(BIOID, party_list)

# Check recoded party variable
table(candidate_df$party_list)

# Join party data to hashtag data
candidate_df$BIOID<- as.integer(candidate_df$BIOID)
htag_freq<- left_join(htag_freq, candidate_df, by = "BIOID")

# For each main party group, calculate a TF-IDF score for each hashtag-candidate combination relative to others in their party
for (party in unique(htag_freq$party_list)){
  
  out_df<- htag_freq%>%
    filter(party_list == party)%>%
    bind_tf_idf(hashtags, BIOID, n)
  
  if(exists("party_tfidf")){
    party_tfidf<- rbind(party_tfidf, out_df)
  } else {
    party_tfidf<- out_df
  }
    
}

# Calculate the mean within-party TF-IDF score for each candidate

mean_party_tfidf<- party_tfidf%>%
  group_by(BIOID)%>%
  summarise(mean_party_tfidf = mean(tf_idf, na.rm = T))


### STEP 4: SAVE DATA ###


# Merge overall TF-IDF and within-party TF-IDF data
mean_tfidf<- left_join(mean_tfidf, mean_party_tfidf, by = "BIOID")

# Save the dataframe of mean TF-IDF scores
save(mean_tfidf, file = paste0(dataset_dir, "//Hashtag Data//Rda Formatted Data//Mean Hashtag TFIDF Scores.Rda"))

# Get the existing overall metrics data
load(paste0(dataset_dir, "\\Overall Metrics\\Overall Twitter Metrics.Rda"))

# Join the mean TF-IDF score to the existing metrics data
metrics_df<- left_join(metrics_df, mean_tfidf, by = "BIOID")

# Save the updated metrics data as Rda and CSV file
save(metrics_df, file = paste0(dataset_dir, "\\Overall Metrics\\Overall Twitter Metrics.Rda"))
write.csv(metrics_df, file = paste0(dataset_dir, "\\Overall Metrics\\Overall Twitter Metrics.csv"), row.names = F, fileEncoding = "UTF-8")
