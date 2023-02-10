#####################################
#                                   #
#  EXPLORATORY TWEET TEXT ANALYSIS  #
#                                   #
#####################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 03/02/23

# DESCRIPTION: This file conducts some basic text analysis of the Tweet text

library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(ggthemr)
library(utils)
library(tidystopwords)
library(proustr)
library(hunspell)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())

# Select data directory on University of Nottingham OneDrive under Twitter Scrapes > Datasets
data_dir<- choose.dir(caption = "Select data directory on University of Nottingham OneDrive under Twitter Scrapes > Datasets")

# Load in tweet dataset which contains one observation for each tweet
load(paste0(data_dir, "//Tweet Data//Rda Formatted Data//Tweet Data.Rda"))

# Remove duplicate tweets
tweet_df<- tweet_df[!duplicated(tweet_df$id_str),]

# Read in the lookup to join the Twitter accounts to the relevant BIOIDs
lookup<- read.csv("..//..//Lookups//BIOID_to_twitter_handle_lookup.csv")

# Join BIOIDs to tweet dataframe
tweet_df<- left_join(tweet_df, lookup, by = c("user_screen_name" = "Account_Handle"))

# Parse tweet date variable to date format
tweet_df<- tweet_df%>%
  mutate(date = parse_date_time(created_at, "a b d H:M:S z Y")%>%
           lubridate::date())

# Filter tweets to just keep those from 90 days before the campaign period onwards
tweet_df<- tweet_df%>%
  filter(date >= dmy("30-05-2022")-90)

# Filter tweets to just keep original tweets
tweet_df<- tweet_df%>%
  filter(is_retweet==F)

# Get directory containing candidate data
candidate_data_dir<- choose.dir(caption = "Select directory on University of Nottingham OneDrive (TplF - Working Dataset)")

# Read in candidate data
candidate_df<- read_excel(paste0(candidate_data_dir, "//Substantive Data Combined Candidates 05-01-23.xlsx"))

# Convert BIOID variable in candidate dataframe to integer
candidate_df<- candidate_df%>%
  mutate(BIOID = as.integer(BIOID))

# Join tweet dataframe and candidate dataframe so that we have one account per row (with repeated candidates where necessary)
tweet_df<- left_join(tweet_df, candidate_df, by = "BIOID")

# Standardise capitalisation of variable names
colnames(tweet_df)<- str_to_lower(colnames(tweet_df))

# Keep only relevant variables
tweet_df<- tweet_df%>%
  select(id_str, full_text, is_retweet, is_quote_status, date, bioid, party_name_original, listname_original, independent, gender, birthdate)

# Create a variable indicating the main parties
tweet_df<- tweet_df%>%
  mutate(party_list = case_when(listname_original == "Ensemble" ~ "Ensemble",
                                listname_original == "Rassemblement National" ~ "Rassemblement National",
                                listname_original == "La Nouvelle Union populaire écologique et sociale" ~ "NUPES",
                                listname_original == "Les Républicains" ~ "Les Républicains",
                                listname_original == "Reconquête" ~ "Reconquête",
                                independent == 1 ~ "Independent",
                                T ~ "Other party"))




# Clean the text
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

tweet_df<- tweet_df%>%
  pr_normalize_punc(full_text)%>%
  mutate(clean_text = full_text%>%
           str_replace_all("(?<=[:alpha:])'(?=[:alpha:])", "e ")%>%
           str_remove_all(url_pattern)%>%
           pr_keep_only_alnum())

# Tokenize the tweet text
token_df<- tweet_df%>%
  unnest_tokens(term, clean_text)

# Remove stop words, retweet indicator and non-valid French words
stop_words<- generate_stoplist(language = "French")%>%
  append("pas")

token_df<- token_df%>%
  filter(!term %in% stop_words)%>%
  filter(term != "rt")%>%
  filter(hunspell_check(term, dict = dictionary("fr_FR")))

# Calculate term frequency
tf<- token_df%>%
  group_by(term)%>%
  summarise(n = n())

tf%>%
  slice_max(order_by = n, n = 10)


# Calculate term frequency by party
party_tf<- token_df%>%
  group_by(party_list, term)%>%
  summarise(n = n())%>%
  ungroup()

party_tf%>%
  group_by(party_list)%>%
  slice_max(order_by = n, n = 10)%>%
  print(., n = nrow(.))




# Set ggplot theme and colour palette for parties
ggtheme<- ggthemr("flat", spacing = 1.5, layout = "plain")

hiro<- met.brewer("Hiroshige")%>%
  as.character()

demuth<- met.brewer("Demuth")%>%
  as.character()

degas<- met.brewer("Degas")%>%
  as.character()

party_colours<- list("Other party" = degas[5],
                     "Ensemble" = hiro[3],
                     "Les Républicains" = hiro[9],
                     "NUPES" = hiro[1],
                     "Reconquête" = hiro[8],
                     "Rassemblement National" = hiro[6],
                     "Independent" = demuth[6])

party_fill_scale<- scale_fill_manual(name = "", values = party_colours)
party_colour_scale<- scale_color_manual(name = "", values = party_colours)

# Set standard plot width, height and margins
width_horizontal<- 4*1280
height_horizontal<- 4*720
margins_horizontal<- margin(t = 25, r = 20, b = 10, l = 5, unit = "pt")


party_tf%>%
  group_by(party_list)%>%
  slice_max(order_by = n, n = 10)%>%
    ggplot()+
      geom_col(aes(x = n, y =reorder_within(term, n, party_list), fill = party_list))+
      facet_wrap(~party_list, scales = "free_y")+
      scale_y_reordered()+
      party_fill_scale+
      labs(y = "")+
      theme(legend.position = "none")

tfidf<- party_tf%>%
  filter(n>=100)%>%
  filter(nchar(term)>1)%>%
  bind_tf_idf(term, party_list, n)

tfidf%>%
  group_by(party_list)%>%
  filter(term != "reconquête")%>%
  slice_max(order_by = tf_idf, n = 10)%>%
  ggplot()+
  geom_col(aes(x = tf_idf, y =reorder_within(term, tf_idf, party_list), fill = party_list))+
  facet_wrap(~party_list, scales = "free_y")+
  scale_y_reordered()+
  party_fill_scale+
  labs(y = "")+
  theme(legend.position = "none")

  tweet_df$party_list%>%
    table()

date_tf<- token_df%>%
    group_by(date, term)%>%
    summarise(n = n())%>%
    slice_max(order_by = n, n = 20)

library(streamgraph)

streamgraph(date_tf, term, n, date)


nupes_date_tf<- token_df%>%
  filter(party_list=="NUPES")%>%
  group_by(date, term)%>%
  summarise(n = n())%>%
  slice_max(order_by = n, n = 10)

streamgraph(nupes_date_tf, term, n, date)


ensemble_date_tf<- token_df%>%
  filter(party_list=="Ensemble")%>%
  group_by(date, term)%>%
  summarise(n = n())%>%
  slice_max(order_by = n, n = 10)

streamgraph(ensemble_date_tf, term, n, date)

