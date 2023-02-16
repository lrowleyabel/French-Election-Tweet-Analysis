############################################
#                                          #
#  ADDING CIRCONSCRIPTION-LEVEL VARIABLES  #
#                                          #
############################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 16/02/23

# DESCRIPTION: This file adds circonscription-level variables to the main analysis dataset.

library(tidyverse)
library(utils)
library(readxl)
library(sf)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())

# Get directory containing processed data
dataset_dir<- choose.dir("Select directory containing metrics data on University of Nottingham OneDrive, under Twitter Scrapes > Datasets")

# Load in existing analysis dataset
load(paste0(dataset_dir, "//Analysis Dataset//Analysis Dataset.Rda"))

# Get directory containing circonscription-level data
circo_dir<- choose.dir("Select direcotry containing circonscription-level data")

# Read in presidential results for each circonscription
presid_df<- read_xlsx(paste0(circo_dir, "//Second Round Presidential Results by Circonscription.xlsx"))

# Convert department codes in presidential results data to the same codes as those in the main analysis dataset
presid_df<- presid_df%>%
  mutate(code_department = case_when(code_department == "2B" ~ "120",
                                     code_department == "2A" ~ "020",
                                     code_department == "ZA" ~ "971",
                                     code_department == "ZC" ~ "973",
                                     code_department == "ZD" ~ "974",
                                     code_department == "ZM" ~ "976",
                                     code_department == "ZX" ~ "977",
                                     code_department == "ZP" ~ "987",
                                     code_department == "ZN" ~ "988",
                                     code_department == "ZZ" ~ "999",
                                     code_department == "ZB" ~ "972",
                                     code_department == "ZS" ~ "975",
                                     code_department == "ZW" ~ "986",
                                     T ~ code_department))

# Create five-digit circo number by combining department code and circo code
presid_df<- presid_df%>%
  mutate(circonscription_code = paste0(str_pad(code_department, width = 3, side = "left", pad = "0"), code_circo))

# Calculate turnout and winner lead
presid_df<- presid_df%>%
  mutate(presidential_turnout = 100*votants/inscrits)%>%
  mutate(presidential_winner_lead = abs(pourcent_exprime_macron - pourcent_exprime_lepen))

# Select relevant variables from presidential results and rename variables into English
presid_df<- presid_df%>%
  select(circonscription_code, pourcent_exprime_macron, pourcent_exprime_lepen, presidential_winner_lead, presidential_turnout)%>%
  rename(macron_percent = pourcent_exprime_macron,
         lepen_percent = pourcent_exprime_lepen)

# Check all circos in the main analysis dataset are also in the presidential results dataset
df%>%
  filter(!OFFICE1_CONSTITUENCYLEVEL1NUMBER %in% presid_df$circonscription_code)%>%
  select(OFFICE1_CONSTITUENCYLEVEL1NUMBER, OFFICE1_CONSTITUENCYLEVEL1)%>%
  unique()%>%
  View()

# Join presidential results variables to main analysis dataframe
df<- left_join(df, presid_df, by = c("OFFICE1_CONSTITUENCYLEVEL1NUMBER" = "circonscription_code"))

# Read in 2017 legislative results
leg17_df<- read_xlsx(paste0(circo_dir, "//Second Round 2017 Legislative Results.xlsx"))

# Convert department codes used in 2017 legislative results to the same as those used in the main analysis dataset
leg17_df<- leg17_df%>%
  mutate(code_department = case_when(code_department == "2B" ~ "120",
                                   code_department == "2A" ~ "020",
                                   code_department == "ZA" ~ "971",
                                   code_department == "ZC" ~ "973",
                                   code_department == "ZD" ~ "974",
                                   code_department == "ZM" ~ "976",
                                   code_department == "ZX" ~ "977",
                                   code_department == "ZP" ~ "987",
                                   code_department == "ZN" ~ "988",
                                   code_department == "ZZ" ~ "999",
                                   code_department == "ZB" ~ "972",
                                   code_department == "ZS" ~ "975",
                                   code_department == "ZW" ~ "986",
                                   T ~ code_department))

# Create five-digit circo number by combining department code and circo code
leg17_df<- leg17_df%>%
  mutate(circonscription_code = paste0(str_pad(code_department, width = 3, side = "left", pad = "0"), str_pad(code_circonscription, width = 2, side = "left", pad = "0")))

# Calculate winner's lead
leg17_df<- leg17_df%>%
  rowwise()%>%
  mutate(winning_vote = max(pourcent_candidat_1, pourcent_candidat_2, pourcent_candidat_3, na.rm = T))%>%
  mutate(second_place_vote = c(pourcent_candidat_1, pourcent_candidat_2, pourcent_candidat_3)[-which.max(c(pourcent_candidat_1, pourcent_candidat_2, pourcent_candidat_3))]%>%
                                max(na.rm = T))%>%
  mutate(legislative_2017_winner_lead = case_when(is.finite(second_place_vote) ~ winning_vote - second_place_vote,
                                                  T ~ 100))

# Select relevant variables and join to main analysis dataset
leg17_df<- leg17_df%>%
  select(circonscription_code, legislative_2017_winner_lead)

df<- left_join(df, leg17_df, by = c("OFFICE1_CONSTITUENCYLEVEL1NUMBER" = "circonscription_code"))


# Read in circo boundaries
shp <- read_sf(paste0(circo_dir, "//Circonscription Boundaries//circonscriptions_legislatives_030522.shp"))

# Correct crossing shape edges
shp<- st_make_valid(shp)

# Change circo codes in the boundaries data to the same as those in the main analysis dataset
shp<- shp%>%
  mutate(id_circo = str_pad(id_circo, 5, "left", "0"))%>%
  mutate(id_circo = str_replace(id_circo, "02A", "020")%>%
           str_replace("02B", "120"))

shp%>%
  filter(! id_circo %in% df$OFFICE1_CONSTITUENCYLEVEL1NUMBER)


# Calculate centroids
shp$centroid<- st_centroid(shp$geometry)

# Calculate distance from Paris
shp$distance_from_paris<- st_distance(shp$centroid, shp$centroid[shp$id_circo=="07501"])%>%
  as.numeric()

# Select relevant variables and join to main dataset
shp<- shp%>%
  select(id_circo, distance_from_paris)%>%
  st_drop_geometry()

df<- left_join(df, shp, by = c("OFFICE1_CONSTITUENCYLEVEL1NUMBER" = "id_circo"))

# Save dataset as Rda and CSV file
save(df, file = paste0(dataset_dir, "\\Analysis Dataset\\Analysis Dataset.Rda"))
write.csv(df, file = paste0(dataset_dir, "\\Analysis Dataset\\Analysis Dataset.csv"), row.names = F, fileEncoding = "UTF-8")
