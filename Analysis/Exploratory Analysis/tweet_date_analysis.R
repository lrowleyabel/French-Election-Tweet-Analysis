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
library(lubridate)
library(readxl)
library(MetBrewer)
library(ggridges)

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

# Plot distribution of account creation dates overall
p<- ggplot(tweet_df)+
  geom_density(aes(x = date), color = "white", fill = met.brewer("Degas")[5], adjust = 0.25)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_y_continuous(labels = function(tick_label){format(tick_label, scientific = F)})+
  labs(title = "Distribution of Candidates' Tweet Dates", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Date", y = "Density")+
  theme(legend.position = "none",
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0),
        plot.margin = margins_horizontal)

p

ggsave(p, filename = "Plots\\tweet_date_distribution.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)


tweet_df<- tweet_df%>%
  mutate(party_list = factor(party_list, levels = c("Ensemble", "NUPES", "Rassemblement National", "Les Républicains", "Reconquête", "Independent", "Other party")))

# Plot distribution of account creation dates by party

p<- ggplot(tweet_df)+
  geom_density_ridges2(aes(x = date, y = party_list, fill = party_list, height = ..count..), stat = "density", trim = T, adjust = 0.1, scale = 3, size = 0.2, alpha = 0.9)+
  annotate(geom = "text", x = dmy("01-01-2007"), y = 9, label = "Note: height represents number of accounts created on that date", size = 2.5, hjust = 0)+
  party_fill_scale+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_y_discrete(expand = expand_scale(c(0.1,0.55)))+
  labs(title = "Distribution of Tweet Dates by Party", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Date", y = "")+
  theme(text = element_text(size = 9),
        legend.position = "none",
        axis.text = element_text(size = 7),
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.margin = margins_horizontal)

p

ggsave(p, filename = "Plots\\tweet_date_distribution_by_party.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)


# Plot distribution of account creation dates by party

p<-tweet_df%>%
  filter(date>=dmy("12/06/2021"))%>%
  ggplot()+
  geom_density_ridges2(aes(x = date, y = party_list, fill = party_list, height = ..count..), stat = "density", trim = T, adjust = 0.1, scale = 3, size = 0.2, alpha = 0.9)+
  party_fill_scale+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  scale_y_discrete(expand = expand_scale(c(0.1,0.55)))+
  labs(title = "Distribution of Tweet Dates by Party in Year Preceding Election", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Date", y = "")+
  theme(text = element_text(size = 9),
        legend.position = "none",
        axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.margin = margins_horizontal)

p

ggsave(p, filename = "Plots\\tweet_date_distribution_by_party_12_months.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)
