###############################
#                             #
#  CALCULATING HASHTAG TFIDF  #
#                             #
###############################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 08/01/23

# DESCRIPTION: This file takes the hashtags used by each candidate and creates a score representing how
# distinctive their hashtags are (the TF-IDF score) based on each hashtag's frequency in their Tweets compared
# to its frequency in the corpus of Tweets as a whole.


library(dplyr)
library(tidytext)
library(ggplot2)
library(utils)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())

# Load in hashtag data
load("Hashtag Data.Rda")

# Select relevant variables
hashtag_df<- hashtag_df%>%
  select(BIOID, hashtags)

# Tokenise the hashtags (ie: create a dataframe withone row for each BIOID-hashtag combination that occurs)
hashtag_df<- hashtag_df%>%
  unnest_tokens(hashtag, hashtags)

# Calculate hashtag frequency for each BIOID
htag_freq<- hashtag_df%>%
  count(BIOID, hashtag, sort = T)

# Calculate overall hashtag frequency
total_htags<- htag_freq%>%
  group_by(BIOID)%>%
  summarise(total_htags = sum(n))

# Join total hashtag frequency to the dataframe of frequency per BIOID
htag_freq<- left_join(htag_freq, total_htags)

# Calculate the TF-IDF score for each BIOID-hashtag combination
tfidf<- htag_freq%>%
  bind_tf_idf(hashtag, BIOID, n)

# Calculate the mean TF-IDF score for each BIOID
mean_tfidf<- tfidf%>%
  group_by(BIOID)%>%
  summarise(mean_tfidf = mean(tf_idf, na.rm = T))

# Plot the distribution of the mean TF-IDF scores
ggplot(mean_tfidf)+
  geom_histogram(aes(x = mean_tfidf), bins = 100, color = "white")

# Save the dataframe of mean TF-IDF scores
save(mean_tfidf, file = "Mean TFIDF Scores.Rda")

# Get the existing overall metrics data
data_dir<- choose.dir("Select data directory on University of Nottingham OneDrive under Twitter Scrapes > Datasets")
load(paste0(data_dir, "\\Overall Metrics\\Overall Twitter Metrics.Rda"))

# Join the mean TF-IDF score to the existing metrics data
metrics_df<- left_join(metrics_df, mean_tfidf, by = "BIOID")

# Save the updated metrics data as Rda and CSV file
save(metrics_df, file = paste0(data_dir, "\\Overall Metrics\\Overall Twitter Metrics.Rda"))
write.csv(metrics_df, file = paste0(data_dir, "\\Overall Metrics\\Overall Twitter Metrics.csv"), row.names = F, fileEncoding = "UTF-8")
