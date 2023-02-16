###################################
#                                 #
#  CALCULATING ORIGINALITY SCORE  #
#                                 #
###################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 16/02/23

# DESCRIPTION: This file calculates a score indicating how original each candidate was in their tweeting behaviour.
# This is based on the proportion of their tweets that are their own, rather than retweets, and the mean TF-IDF score of their hashtags relative to others in their party.

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(utils)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())

# Get directory containing processed data
dataset_dir<- choose.dir("Select directory containing metrics data on University of Nottingham OneDrive, under Twitter Scrapes > Datasets")

# Load in metrics data
load(paste0(dataset_dir, "//Overall Metrics//Overall Twitter Metrics.Rda"))

# Look at distribution of variables to be used in originality score
ggplot(metrics_df)+
  geom_histogram(aes(x = prop_original), color = "white", bins = 100)

ggplot(metrics_df)+
  geom_histogram(aes(x = mean_party_tfidf), color = "white", bins = 100)

# Transform TFIDF variable so it is less skewed by taking log(1+x)
metrics_df<- metrics_df%>%
  mutate(log_mean_party_tfidf = log(mean_party_tfidf))

ggplot(metrics_df)+
  geom_histogram(aes(x = log_mean_party_tfidf), color = "white", bins = 100)

# Create dataframe containing variables for originality score
score_df<- metrics_df%>%
  select(BIOID, prop_original, log_mean_party_tfidf)

# Convert BIOID to string variable so it doesn't get picked up as a numeric variable to include in the composite score
score_df<- score_df%>%
  mutate(BIOID = as.character(BIOID))

# Rescale variables to between 0 and 1
rerange01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}

score_df<- score_df%>%
  mutate(across(where(is.numeric), ~rerange01(.x)))

# Check rescaling
score_df%>%
  summarise(across(where(is.numeric),
                   .fns = list(Min = ~min(.x, na.rm=T),
                               Max = ~max(.x, na.rm=T),
                               Mean = ~mean(.x, na.rm =T),
                               SD = ~sd(.x, na.rm=T)),
                   .names = "{.col}__{.fn}"))%>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_sep = "__")%>%
  mutate(Variable = str_replace_all(Variable, "_", " "))%>%
  mutate(across(c(Min, Max, Mean, SD), ~round(.x, 2)))

# Look at correlation of the two score variables
cor(score_df$prop_original, score_df$log_mean_party_tfidf, use = "complete")

ggplot(score_df)+
  geom_point(aes(x = prop_original, y = log_mean_party_tfidf))

# Create composite score using row means
score_df<-score_df%>%
  rowwise()%>%
  mutate(originality = mean(c(prop_original, log_mean_party_tfidf)))

# Look at composite score distribution
score_df%>%
  ggplot()+
  geom_histogram(aes(x = originality), color = "white")

# Convert BIOID back to numeric
score_df<- score_df%>%
  mutate(BIOID = as.numeric(BIOID))

# Add composite score to main df
metrics_df<- left_join(metrics_df, select(score_df, BIOID, originality))

# Save metrics data as Rda and CSV file
save(metrics_df, file = paste0(dataset_dir, "\\Overall Metrics\\Overall Twitter Metrics.Rda"))
write.csv(metrics_df, file = paste0(dataset_dir, "\\Overall Metrics\\Overall Twitter Metrics.csv"), row.names = F, fileEncoding = "UTF-8")
