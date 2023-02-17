########################
#                      #
#  BIVARIATE ANALYSIS  #
#                      #
########################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 17/02/23

# DESCRIPTION: This file conducts some basic bivariate analysis of the data, looking at the relationship
# between the main dependent and independent variable

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemr)
library(MetBrewer)
library(utils)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())

# Get directory containing processed datasets on University of Nottingham OneDrive, under Twitter Scrapes > Datasets
data_dir<- choose.dir(caption = "Select directory containing processed datasets on University of Nottingham OneDrive, under Twitter Scrapes > Datasets")

# Read in analysis dataset
df<- read.csv(paste0(data_dir, "\\Analysis Dataset\\Analysis Dataset.csv"), fileEncoding = "UTF-8")

# Standardise the capitalisation of variable names
colnames(df)<- str_to_lower(colnames(df))


### STEP 1: ANALYSE PROPORTION ACTIVE BY PARTY ###


# Recode the listname to group minor parties as an "Other party" category and independents as their own category

df<- df%>%
  mutate(party_list = case_when(listname_original == "Ensemble" ~ "Ensemble",
                                listname_original == "Rassemblement National" ~ "Rassemblement National",
                                listname_original == "La Nouvelle Union populaire écologique et sociale" ~ "NUPES",
                                listname_original == "Les Républicains" ~ "Les Républicains",
                                listname_original == "Reconquête" ~ "Reconquête",
                                independent == 1 ~ "Independent",
                                T ~ "Other party"))

# Create binary indicator of Esnemble vs. non-Ensemble candidates
df<- df%>%
  mutate(ensemble = case_when(party_list == "Ensemble" ~ "Ensemble",
                              T ~ "Non-Ensemble"))

# Set ggtheme
ggtheme<- ggthemr("flat", spacing = 1.5, layout = "plain")

# Set color palette for party lists using MetBrewer package
unique(df$party_list)

hiro<- met.brewer("Hiroshige")%>%
  as.character()

demuth<- met.brewer("Demuth")%>%
  as.character()

degas<- met.brewer("Degas")%>%
  as.character()

monet<- met.brewer("Monet")%>%
  as.character()


party_colours<- list("Other party" = degas[5],
           "Ensemble" = hiro[3],
           "Les Républicains" = hiro[9],
           "NUPES" = hiro[1],
           "Reconquête" = hiro[6],
           "Rassemblement National" = monet[4],
           "Independent" = demuth[6])

party_fill_scale<- scale_fill_manual(name = "Party List", values = party_colours)
party_colour_scale<- scale_color_manual(name = "Party List", values = party_colours)

# Set colour palette for Ensemble vs Non-Ensemble
binary_ensemble_colours<- list("Ensemble" = hiro[3],
                              "Non-Ensemble" = degas[5])

binary_ensemble_fill_scale<- scale_fill_manual(name = "Ensemble vs Non-Ensemble", values = binary_ensemble_colours)
binary_ensemble_colour_scale<- scale_color_manual(name = "Ensemble vs Non-Enseble", values = binary_ensemble_colours)

# Set standard plot width, height and margins
width_horizontal<- 4*1280
height_horizontal<- 4*720
margins_horizontal<- margin(t = 25, r = 20, b = 10, l = 5, unit = "pt")

# Plot number of active candidates for each (grouped) party
p<- df%>%
  group_by(party_list)%>%
  summarise(n = sum(!is.na(total_followers)))%>%
  ggplot()+
    geom_bar(aes(x = n, y = reorder(party_list, n), fill = party_list), color = "#2c3e50", stat = "identity")+
    geom_text(aes(x = n - 150, y = reorder(party_list, n), label = n), color = "white", size = 3)+
    labs(title = "Total Number of Candidates Active on Twitter per Party", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Total active candidates", y = "")+
    party_fill_scale+
    theme(legend.position = "none",
          plot.title.position = "panel",
          plot.title = element_text(hjust = 0),
          plot.margin = margins_horizontal)

p
ggsave(p, filename = "Plots\\total_active_candidates_per_party_list.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)

# Plot proportion of active candidates per party
p<- df%>%
  group_by(party_list)%>%
  summarise(prop = 100*sum(!is.na(total_followers))/n())%>%
  ggplot()+
  geom_bar(aes(x = prop, y = reorder(party_list, prop), fill = party_list), color = "#2c3e50", stat = "identity")+
  geom_text(aes(x = prop - 10, y = reorder(party_list, prop), label = round(prop,0)), color = "white", size = 3)+
  labs(title = "Proportion of Candidates Active on Twitter per Party", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Proportion of candidates active (%)", y = "")+
  party_fill_scale+
  theme(legend.position = "none",
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0),
        plot.margin = margins_horizontal)

p
ggsave(p, filename = "Plots\\proportion_active_candidates_per_party_list.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)


# Re-scale all the dependent variables to between 0 and 100
rerange0_100 <- function(x){100*((x-min(x,na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)))}

df<- df%>%
  mutate(across(c(dynamism, originality, log_total_following, log_total_tweets, prop_original, log_mean_party_tfidf), ~rerange0_100(.x), .names = "scaled_{col}"))

df%>%
summarise(across(c(scaled_dynamism, scaled_originality, scaled_log_total_following, scaled_log_total_tweets, scaled_prop_original, scaled_log_mean_party_tfidf),
                 .fns = list(Min = ~min(.x, na.rm=T),
                             Max = ~max(.x, na.rm=T),
                             Mean = ~mean(.x, na.rm =T),
                             SD = ~sd(.x, na.rm=T)),
                 .names = "{.col}__{.fn}"))%>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_sep = "__")%>%
  mutate(Variable = str_replace_all(Variable, "_", " "))%>%
  mutate(across(c(Min, Max, Mean, SD), ~round(.x, 2)))


### STEP 2: VARIABLES RELATED TO DYNAMISM ###


# Plot overall dynamism by party list
p<- df%>%
  ggplot()+
    geom_boxplot(aes(x = scaled_dynamism, y = party_list, fill = party_list))+
    labs(title = "Distribution of Overall Dynamism by Party", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Overall dynamism score", y = "")+
    party_fill_scale+
    theme(legend.position = "none",
          plot.title.position = "panel",
          plot.title = element_text(hjust = 0),
          plot.margin = margins_horizontal)

p
ggsave(p, filename = "Plots\\dynamism_by_party_list.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)

# Test difference in dynamism for Ensemble and non-Ensemble candidates
t.test(scaled_dynamism ~ ensemble, data = df)

# Plot tweet volume by party list
p<- df%>%
  ggplot()+
  geom_boxplot(aes(x = scaled_log_total_tweets, y = party_list, fill = party_list))+
  labs(title = "Distribution of Tweet Volume by Party", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Scaled log of tweet volume", y = "", caption = "Note: tweet volume is logged and rescaled to between 0 and 100")+
  party_fill_scale+
  theme(legend.position = "none",
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0),
        plot.margin = margins_horizontal,
        plot.caption = element_text(hjust = 0.5, size = 7),
        plot.caption.position = "panel")

p
ggsave(p, filename = "Plots\\scaled_tweet_volume_by_party_list.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)

# Test difference in tweet volume for Ensemble and non-Ensemble candidates
t.test(scaled_log_total_tweets ~ ensemble, data = df)

# Plot total following by party list
p<- df%>%
  ggplot()+
  geom_boxplot(aes(x = scaled_log_total_following, y = party_list, fill = party_list))+
  labs(title = "Distribution of Following Numbers by Party", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Scaled log of number of accounts candidate follows", y = "", caption = "Note: number following is logged and rescaled to between 0 and 100")+
  party_fill_scale+
  theme(legend.position = "none",
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0),
        plot.margin = margins_horizontal,
        plot.caption = element_text(hjust = 0.5, size = 7),
        plot.caption.position = "panel")

p
ggsave(p, filename = "Plots\\scaled_following_by_party_list.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)

# Test difference in following numbers for Ensemble and non-Ensemble candidates
t.test(scaled_log_total_following ~ ensemble, data = df)


### STEP 3: VARIABLES RELATED TO ORIGINALITY ###


# Plot originality score by party list
p<- df%>%
  ggplot()+
  geom_boxplot(aes(x = scaled_originality, y = party_list, fill = party_list))+
  party_fill_scale+
  labs(title = "Distribution of Originality Scores by Party List", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Overall originality score", y = "")+
  theme(legend.position = "none",
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0),
        plot.margin = margins_horizontal)

p
ggsave(p, filename = "Plots\\originality_by_party_list.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)

# Test difference in mean TFIDF for Ensemble and non-Ensemble candidates
t.test(scaled_originality ~ ensemble, data = df)

# Plot hashtag distinctiveness by party list
p<- df%>%
  ggplot()+
  geom_boxplot(aes(x = scaled_prop_original, y = party_list, fill = party_list))+
  party_fill_scale+
  labs(title = "Distribution of Original Tweet Proportion by Party", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Proportion of Tweets that are original (%)", y = "")+
  theme(legend.position = "none",
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0),
        plot.margin = margins_horizontal)

p
ggsave(p, filename = "Plots\\original_proportion_by_party_list.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)

# Test difference in mean TFIDF for Ensemble and non-Ensemble candidates
t.test(scaled_prop_original ~ ensemble, data = df)

# Plot hashtag distinctiveness by party list
p<- df%>%
  ggplot()+
  geom_boxplot(aes(x = scaled_log_mean_party_tfidf, y = party_list, fill = party_list))+
  party_fill_scale+
  labs(title = "Distribution of Hashtag Distinctiveness by Party", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Scaled log of mean hashtag TF-IDF", y = "", caption = "Note: mean TF-IDF scores are logged and rescaled to between 0 and 100")+
  theme(legend.position = "none",
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0),
        plot.margin = margins_horizontal)

p
ggsave(p, filename = "Plots\\sclaed_mean_party_tfidf_by_party_list.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)

# Test difference in mean TFIDF for Ensemble and non-Ensemble candidates
t.test(scaled_log_mean_party_tfidf ~ ensemble, data = df)

