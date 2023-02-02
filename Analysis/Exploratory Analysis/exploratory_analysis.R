##########################
#                        #
#  EXPLORATORY ANALYSIS  #
#                        #
##########################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 08/01/23

# DESCRIPTION: This file conducts some basic exploratory analysis of the data, looking at the relationship
# between the main dependent and independent variables

library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemr)
library(MetBrewer)
library(utils)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())

# Get directory containing processed datasets on University of Nottingham OneDrive, under Twitter Scrapes > Datasets
data_dir<- choose.dir(caption = "Sekect directory containing processed datasets on University of Nottingham OneDrive, under Twitter Scrapes > Datasets")

# Read in analysis dataset
df<- read.csv(paste0(data_dir, "\\Analysis Dataset\\Analysis Dataset.csv"), fileEncoding = "UTF-8")

# Standardise the capitalisation of variable names
colnames(df)<- str_to_lower(colnames(df))

### STEP 1: ANALYSIS BY PARTY ###


# Recode the listname to group minor parties as an "Other party" category and independents as their own category

table(df$listname_original)%>%
  sort()

df<- df%>%
  mutate(party_list = case_when(listname_original == "Ensemble" ~ "Ensemble",
                                listname_original == "Rassemblement National" ~ "Rassemblement National",
                                listname_original == unique(df$listname_original)[4] ~ "NUPES",
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

party_colours<- list("Other party" = degas[5],
           "Ensemble" = hiro[3],
           "Les Républicains" = hiro[9],
           "NUPES" = hiro[1],
           "Reconquête" = hiro[8],
           "Rassemblement National" = hiro[6],
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

# Plot frequency of each summarised party
p<- df%>%
  group_by(party_list)%>%
  summarise(n = n())%>%
  ggplot()+
    geom_bar(aes(x = n, y = reorder(party_list, n)), fill = degas[5], color = "#2c3e50", stat = "identity")+
    geom_text(aes(x = n - 150, y = reorder(party_list, n), label = n), color = "white", size = 3)+
    labs(title = "Total Number of Candidates per Party List", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Total candidates", y = "")+
    theme(legend.position = "none",
          plot.title.position = "panel",
          plot.title = element_text(hjust = 0),
          plot.margin = margins_horizontal)

p
ggsave(p, filename = "Plots\\total_candidates_per_party_list.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)

# Plot overall dynamism by party list
p<- df%>%
  ggplot()+
    geom_boxplot(aes(x = dynamism, y = party_list), fill = degas[5])+
    labs(title = "Distribution of Overall Dynamism by Party List", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Overall dynamism score", y = "")+
    theme(legend.position = "none",
          plot.title.position = "panel",
          plot.title = element_text(hjust = 0),
          plot.margin = margins_horizontal)

p
ggsave(p, filename = "Plots\\dynamism_by_party_list.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)

# Test difference in dynamism for Ensemble and non-Ensemble candidates
t.test(dynamism ~ ensemble, data = df)


p<- df%>%
  ggplot()+
  geom_boxplot(aes(x = mean_tfidf, y = party_list), fill = degas[5])+
  scale_x_log10()+
  labs(title = "Distribution of Hashtag Distinctiveness by Party List", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Mean hashtag TF-IDF (log scale)", y = "")+
  theme(legend.position = "none",
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0),
        plot.margin = margins_horizontal)

p
ggsave(p, filename = "Plots\\mean_tfidf_by_party_list.png", width = width_horizontal, height = height_horizontal, units = "px", dpi = 700)

# Test difference in mean TFIDF for Ensemble and non-Ensemble candidates
t.test(mean_tfidf ~ ensemble, data = df)

df%>%
ggplot()+
  geom_boxplot(aes(x = mean_party_tfidf, y = party_list), fill = degas[5])+
  scale_x_log10()+
  labs(title = "Distribution of Hashtag Distinctiveness by Party List", subtitle = "Data from Digital Society Project and University of Nottingham", x = "Mean hashtag TF-IDF (log scale)", y = "")+
  theme(legend.position = "none",
        plot.title.position = "panel",
        plot.title = element_text(hjust = 0),
        plot.margin = margins_horizontal)



