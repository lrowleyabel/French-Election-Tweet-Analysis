###############################
#                             #
#  CALCULATING TWEET METRICS  #
#                             #
###############################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 07/01/23

# DESCRIPTION: This file takes the Tweet data and account data, links it to the relevant BIOIDs and creates a set of metrics
# summarising the candidate's Twitter activity across all their accounts.

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readxl)
library(utils)
library(fastDummies)
library(ggplot2)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())

# Select data directory on University of Nottingham OneDrive under Twitter Scrapes > Datasets
data_dir<- choose.dir(caption = "Select data directory on University of Nottingham OneDrive under Twitter Scrapes > Datasets")


### STEP 1: CREATE ACCOUNT METRICS ###


# Load account dataset
load(paste0(data_dir, "\\Account Data\\Rda Formatted Data\\Account Data.Rda"))

# Read in BIOID to Twitter ID Lookup
lookup<- read.csv("..\\Lookups\\BIOID_to_twitter_handle_lookup.csv", fileEncoding = "UTF-8")

# Check there are no scraped accounts who are not in our lookpup (allowing for differences in capitalisation)
table(str_to_lower(accounts_df$screen_name) %in% str_to_lower(lookup$Account_Handle))

# Create lowercase handle variable in both the accounts dataset and the lookup
accounts_df<- accounts_df%>%
  mutate(lower_screen_name = str_to_lower(screen_name))

lookup<- lookup%>%
  mutate(Lower_Account_Handle = str_to_lower(Account_Handle))

# Join BIOID to accounts dataset using lowercase handle
accounts_df<- left_join(accounts_df, select(lookup, BIOID, Lower_Account_Handle), by = c("lower_screen_name" = "Lower_Account_Handle"))

# Parse account creation date as a date
accounts_df<- accounts_df%>%
  mutate(date = parse_date_time(created_at, "a b d H:M:S z Y")%>%
           lubridate::date())

# Create variable indicating number of days before the campaign start (30-05-2022) that the account was created
campaign_start<- dmy("30-05-2022")

accounts_df<- accounts_df%>%
  mutate(account_age_days = as.double(campaign_start - date, units = "days"))

# Create variables indicating whether the account was created within the t months prior to the start of the campaign for t = 0 to t = 12
accounts_df<- accounts_df%>%
  mutate(account_age_months = ceiling(account_age_days/30))%>%
  mutate(recency = case_when(account_age_months > 12 ~ "created_over_a_year_ago",
                                        account_age_months < 1 ~ "created_after_campaign_start",
                                        is.numeric(account_age_months) ~ paste0("created_within_the_", account_age_months, "_months_prior_to_campgain")))%>%
  dummy_cols("recency")

# Create dataframe summarising account metrics for each BIOID 
account_metrics<- accounts_df%>%
  group_by(BIOID)%>%
  summarise(total_account = n(),
            total_followers = sum(followers_count),
            total_following = sum(friends_count),
            across(starts_with("recency_"), .fns = ~sum(as.numeric(.x))))

# Save account metrics as CSV file
write.csv(account_metrics, file = paste0(data_dir, "\\Account Data\\CSV Formatted Data\\Account Metrics.csv"), row.names = F, fileEncoding = "UTF-8")


### STEP 2: CREATE TWEET METRICS ###


# Load Tweet data
load(paste0(data_dir, "\\Tweet Data\\Rda Formatted Data\\Tweet Data.Rda"))



# Check for duplicated tweets
duplicated_tweets<- tweet_df[duplicated(tweet_df$id_str), "id_str"]

tweet_df%>%
  filter(id_str %in% duplicated_tweets$id_str)%>%
  View()

# Remove duplicate tweets
tweet_df<- tweet_df[!duplicated(tweet_df$id_str),]

# Remove the tweet text variable to make dataframe smaller
tweet_df<- tweet_df%>%
  select(-full_text)

# Create lowercase version of Twitter handle in Tweet data
tweet_df<- tweet_df%>%
  mutate(lower_user_screen_name = str_to_lower(user_screen_name))

# Join BIOID to tweet data based on Twitter handle, allowing for differences in capitalisation
tweet_df<- left_join(tweet_df, select(lookup, BIOID, Lower_Account_Handle), by = c("lower_user_screen_name" = "Lower_Account_Handle"))

# Parse tweet creation date as a date
tweet_df<- tweet_df%>%
  mutate(date = parse_date_time(created_at, "a b d H:M:S z Y")%>%
           lubridate::date())

# Create dataframe summarising tweet metrics for each BIOID
tweet_metrics<- tweet_df%>%
  group_by(BIOID)%>%
  summarise(total_tweets = n(),
            total_original_tweets = sum(!is_retweet),
            total_tweets_retweeted_by_candidate = sum(is_retweet),
            total_campaign_tweets = sum(date >= campaign_start),
            total_pre_campaign_tweets = sum(as.double(campaign_start - date, units = "days") <= 90),
            total_likes = sum(favorite_count),
            total_times_retweeted_by_others = sum(retweet_count))%>%
  mutate(prop_original = 100*(total_original_tweets/total_tweets),
         prop_retweets = 100*(total_tweets_retweeted_by_candidate/total_tweets),
         mean_likes = total_likes/total_tweets,
         mean_times_retweeted_by_others = total_times_retweeted_by_others/total_tweets)


# Save tweet metrics as CSV file
write.csv(tweet_metrics, file = paste0(data_dir, "\\Tweet Data\\CSV Formatted Data\\Tweet Metrics.csv"), row.names = F, fileEncoding = "UTF-8")

# Check there are the same BIOIDs in the account metrics and tweet metrics datafranes 
n_distinct(account_metrics$BIOID)
n_distinct(tweet_metrics$BIOID)
table(account_metrics$BIOID %in% tweet_metrics$BIOID)
table(tweet_metrics$BIOID %in% account_metrics$BIOID)

# Join account and tweet metrics by BIOID
metrics_df<- left_join(account_metrics, tweet_metrics, by = "BIOID")

# Plot distribution of the variables and test if logging is required for skewed variables
ggplot(metrics_df)+
  geom_histogram(aes(x = total_tweets), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = log(total_tweets)), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = prop_original), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = prop_retweets), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = mean_likes), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = log(mean_likes)), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = mean_times_retweeted_by_others), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = log(mean_times_retweeted_by_others)), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = total_campaign_tweets), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = log(total_campaign_tweets)), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = total_pre_campaign_tweets), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = log(total_pre_campaign_tweets)), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = total_followers), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = log(total_followers)), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = total_following), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = log(total_following)), color = "white")


# Transform heavily skewed variables by calculating log(x + 1)
metrics_df<- metrics_df%>%
  mutate(across(c(total_followers, total_following, total_tweets, total_campaign_tweets, total_pre_campaign_tweets, mean_likes, mean_times_retweeted_by_others), ~log(.x+1), .names = "log_{.col}"))

# Save metrics data as Rda and CSV file
save(metrics_df, file = paste0(data_dir, "\\Overall Metrics\\Overall Twitter Metrics.Rda"))
write.csv(metrics_df, file = paste0(data_dir, "\\Overall Metrics\\Overall Twitter Metrics.csv"), row.names = F, fileEncoding = "UTF-8")



