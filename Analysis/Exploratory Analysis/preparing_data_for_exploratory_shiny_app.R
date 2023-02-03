############################################
#                                          #
#  PREPARE DATA FOR EXPLORATORY SHINY APP  #
#                                          #
############################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 21/01/23

# DESCRIPTION: This file creates a smaller version of the analysis dataset which can be used in the
# exploratory Shiny app for looking at bivariate relationships.

library(dplyr)
library(stringr)
library(ggplot2)
library(utils)
library(lubridate)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())

# Get directory containing processed datasets on University of Nottingham OneDrive, under Twitter Scrapes > Datasets
data_dir<-choose.dir(caption = "Sekect directory containing processed datasets on University of Nottingham OneDrive, under Twitter Scrapes > Datasets")

# Read in analysis dataset
df<- read.csv(paste0(data_dir, "\\Analysis Dataset\\Analysis Dataset.csv"), fileEncoding = "UTF-8")

# Standardise the capitalisation of variable names
colnames(df)<- str_to_lower(colnames(df))

# Select relevant variables
df<- df%>%
  select(bioid:mean_party_tfidf, party_name_original, listname_original, independent, gender, birthdate, office1_round_percent, office2_round_percent)

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

# Recode gender with substantive categories
df<- df%>%
  mutate(gender = case_when(gender == 0 ~ "Male",
                            gender == 1 ~ "Female"))

# Create age variable
df<- df%>%
  mutate(age = interval(ymd(birthdate), dmy("12-06-2022"))%/%years(1)%>%
           floor())%>%
  select(-birthdate)

# Replace erroneous values for age and offce1_round_percent
df$age[df$age>100]<- NA
df$office1_round_percent[df$office1_round_percent>100]<- NA

# Create logged mean_tfidf and mean_party_tfidf variables
df<- df%>%
  mutate(log_mean_tfidf = log(mean_tfidf),
         log_mean_party_tfidf = log(mean_party_tfidf))

# Save data for shiny app
save(df, file = ".//Exploratory Visualisation App//app_data_ignore.Rda")


