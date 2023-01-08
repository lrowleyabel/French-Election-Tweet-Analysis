#############################################
#                                           #
#  CREATING BIOID TO TWITTER HANDLE LOOKUP  #
#                                           #
#############################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 07/01/23

# DESCRIPTION: This file takes the candidate dataset and creates a long-format lookup for BIOID to Twitter handle

library(dplyr)
library(tidyr)
library(readxl)
library(utils)

setwd(choose.dir(caption = "Selecting working directory"))
rm(list = ls())

# Get directory containing working candidate datasets on University of Nottingham OneDrive
data_dir<- choose.dir(caption = "Select directory containing working candidate datasets on University of Nottingham OneDrive")

# Read in all candidate data
df<- read_excel(paste0(data_dir, "\\Combined Candidates 05-01-23.xlsx"), col_types = "text")

# Create dataframe with variables with BIOID and Twitter handles
handles_df<- df%>%
  select(BIOID, matches("[A-Z]*TWIT[0-9]?[0-9]?$"))

# Pivot handles dataframe to long format and filter out NAs
lookup<- handles_df%>%
  pivot_longer(cols = matches("[A-Z]*TWIT[0-9]?[0-9]?$"), values_to = "Account_Handle", names_to = c("Account_Type"))%>%
  filter(Account_Handle != "NA")

# Create dataframe of OTWIT handles and types
otwit_types<- df%>%
  select( matches("OTWIT[0-9]?[0-9]?(_TYPE)?$"))

# Rearrange to only one OTWIT account per row
for (i in seq(1, 11, by = 2)){
  
  tmp_df<- otwit_types[,c(i, i+1)]
  colnames(tmp_df)<- c("OTWIT", "OTWIT_TYPE")
  if (exists("otwit_types_long")){
    otwit_types_long<- rbind(otwit_types_long, tmp_df)
  } else {
    otwit_types_long<- tmp_df
  }
  
}

# Filter out NAs from OTWIT types
otwit_types_long<- otwit_types_long%>%
  filter(OTWIT != "NA")

# Make sure there is only one instance of each OTWIT account
otwit_types_long<- otwit_types_long%>%
  group_by(OTWIT)%>%
  slice_max(order_by = row_number(), n = 1)

# Join OTWIT types data to lookup dataframe
lookup<- left_join(lookup, otwit_types_long, by = c("Account_Handle" = "OTWIT"))

# Append OTWIT account type to the Account_Type variable for OTWIT accounts
lookup<- lookup%>%
  mutate(Account_Type = case_when(Account_Type == "OTWIT" ~ paste0(Account_Type, "_TYPE_", OTWIT_TYPE),
                                  T ~ Account_Type))

# Remove OTWIT_TYPE variable and reorder remaining variables
lookup<- lookup%>%
  select(-OTWIT_TYPE)%>%
  select(BIOID, Account_Handle, Account_Type)

# Save to lookups directory as CSV file
write.csv(lookup, file = "..\\Lookups\\BIOID_to_twitter_handle_lookup.csv", row.names = F, fileEncoding = "UTF-8")
