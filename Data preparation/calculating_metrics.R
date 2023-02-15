###############################
#                             #
#  CALCULATING TWEET METRICS  #
#                             #
###############################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 08/01/23

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

# Load Tweet data
load(paste0(data_dir, "\\Tweet Data\\Rda Formatted Data\\Tweet Data.Rda"))

# Parse tweet creation date as a date
tweet_df<- tweet_df%>%
  mutate(date = parse_date_time(created_at, "a b d H:M:S z Y")%>%
           lubridate::date())

# Get date of latest tweet for each account in the tweet dataset
latest_tweets<- tweet_df%>%
  group_by(user_id_str)%>%
  slice_max(order_by = date, n = 1, with_ties = F)%>%
  rename(latest_tweet_date = date)%>%
  select(user_id_str, latest_tweet_date)

# Join latest tweet dates to account data
accounts_df<- left_join(accounts_df, latest_tweets, by = c("id_str" = "user_id_str"))

# Filter account data to keep only accounts active in the 9 months prior to the election
accounts_df<- accounts_df%>%
  filter(latest_tweet_date >= dmy("12/09/2021"))

# Read in BIOID to Twitter ID Lookup
lookup<- read.csv("..\\Lookups\\BIOID_to_twitter_handle_lookup.csv", fileEncoding = "UTF-8")

# Check for any scraped accounts which are not in our lookpup (allowing for differences in capitalisation)
# These are accounts which were included in the original data collection but have subsequently been removed as they were not valid (generally because they represented an area, rather than an individual candidate)
accounts_df%>%
  filter(!str_to_lower(screen_name) %in% str_to_lower(lookup$Account_Handle))%>%
  View()

# Check for accounts linked to more than one BIOID in the lookup
lookup%>%
  group_by(Account_Handle)%>%
  summarise(n = n())%>%
  arrange(-n)%>%
  filter(n>1)

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

# Create variable indicating whether the account is a 'new' account, defined as being created within the 9 months prior to the election.
# Accounts created after 14th May 2022 are given a missing value for this variable, since looking for new accounts for some parties stopped at this point due to the data collection process.
accounts_df<- accounts_df%>%
  mutate(new_account = case_when(date > dmy("14/05/2022") ~ NA,
                                 date >= dmy("12/09/2021") ~ T,
                                 T ~ F))

# Create dataframe summarising account metrics for each BIOID 
account_metrics<- accounts_df%>%
  group_by(BIOID)%>%
  summarise(total_account = n(),
            total_followers = sum(followers_count),
            total_following = sum(friends_count),
            created_new_accounts = ifelse(sum(new_account, na.rm = T)>0,1,0))

# Save account metrics as CSV file
write.csv(account_metrics, file = paste0(data_dir, "\\Account Data\\CSV Formatted Data\\Account Metrics.csv"), row.names = F, fileEncoding = "UTF-8")


### STEP 2: CREATE TWEET METRICS ###


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

# Filter for only tweets in the 9 months prior to the elections
tweet_df<- tweet_df%>%
  filter(date >= dmy("12/09/2021"))

# Set campaign start date
campaign_start<- dmy("30/05/2022")

# Create dataframe summarising tweet metrics for each BIOID
tweet_metrics<- tweet_df%>%
  group_by(BIOID)%>%
  summarise(total_tweets = n(),
            total_original_tweets = sum(!is_retweet),
            total_tweets_retweeted_by_candidate = sum(is_retweet),
            total_likes = sum(favorite_count),
            total_times_retweeted_by_others = sum(retweet_count))%>%
  mutate(prop_original = 100*(total_original_tweets/total_tweets),
         prop_retweets = 100*(total_tweets_retweeted_by_candidate/total_tweets),
         mean_likes = total_likes/total_tweets,
         mean_times_retweeted_by_others = total_times_retweeted_by_others/total_tweets)


# Save tweet metrics as CSV file
write.csv(tweet_metrics, file = paste0(data_dir, "\\Tweet Data\\CSV Formatted Data\\Tweet Metrics.csv"), row.names = F, fileEncoding = "UTF-8")


### STEP 3: CREATE METRICS ON OVERALL ACTIVITY ###


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
  geom_histogram(aes(x = total_followers), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = log(total_followers)), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = total_following), color = "white")

ggplot(metrics_df)+
  geom_histogram(aes(x = log(total_following)), color = "white")

# Transform heavily skewed variables by calculating log(x + 1)
metrics_df<- metrics_df%>%
  mutate(across(c(total_followers, total_following, total_tweets, mean_likes, mean_times_retweeted_by_others), ~log(.x+1), .names = "log_{.col}"))

# Create composite 'dynamism' score to summarise overall Twitter activity
# Select variables to include in composite score
score_df<- metrics_df%>%
  select(BIOID, log_total_tweets, log_total_following, log_mean_likes, log_mean_times_retweeted_by_others, created_new_accounts)

# Conver BIOID to string variable so it doesn't get picked up as a numeric variable to include in the composite score
score_df<- score_df%>%
  mutate(BIOID = as.character(BIOID))

# Rescale variables to between 0 and 1
rerange01 <- function(x){(x-min(x))/(max(x)-min(x))}

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


# Look at Cronbach's alpha for continuous score variables
score_df%>%
  select(-BIOID, -created_new_accounts)%>%
  psych::alpha()

# Create composite score using row means
score_df<-score_df%>%
  rowwise()%>%
  mutate(dynamism = mean(log_total_tweets:created_new_accounts))

# Look at composite score distribution
score_df%>%
  ggplot()+
  geom_histogram(aes(x = dynamism), color = "white")

# Convert BIOID back to numeric
score_df<- score_df%>%
  mutate(BIOID = as.numeric(BIOID))

# Add composite score to main df
metrics_df<- left_join(metrics_df, select(score_df, BIOID, dynamism))

# Save metrics data as Rda and CSV file
save(metrics_df, file = paste0(data_dir, "\\Overall Metrics\\Overall Twitter Metrics.Rda"))
write.csv(metrics_df, file = paste0(data_dir, "\\Overall Metrics\\Overall Twitter Metrics.csv"), row.names = F, fileEncoding = "UTF-8")



