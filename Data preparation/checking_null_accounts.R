library(rtweet)
library(dplyr)
library(utils)
library(stringr)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())

# Select directory containing twitter scrapes
dir<- choose.dir()

# Get subdirectories. There is one for each account.
subdirs<- list.dirs(dir)
subdirs<- subdirs[2:length(subdirs)]

# Create list of empty subdirectories (ie: accounts were nothing was scraped).
empty_subdirs<- c()

for (subdir in subdirs){
  
  n_files<- length(list.files(subdir))
  
  if (n_files == 0){
    empty_subdirs<- c(empty_subdirs, subdir)
  }
  
}

# Create dataframe of null accounts
null_accounts<- data.frame(Account_Handle = str_remove(empty_subdirs, coll(paste0(dir, "/"))))

# Write dataframe to CSV
write.csv(null_accounts, file = "null_accounts.csv", row.names = F, fileEncoding = "UTF-8")


# Get directory containing datasets on University of Nottingham OneDrive
data_dir<- choose.dir(caption = "Select dataset directory")

# Get existing account dataset
load(paste0(data_dir, "\\Account Data\\Rda Formatted Data\\Account Data.Rda"))

# Get list of variables in existing account dataset
extracted_variables<- colnames(accounts_df)

# Lookup null accounts using Twitter API
key = "A6kiu8A6Od6YtvGlTCC7Nq8wF"
secret = "gwF7ojd5KGcumKJFGbLuGXvbZNL9MiKRd8y3pvMd6F2Rfuq0hv"

accessToken = "1188784519832317953-ZBppI2XzBXeDsGeJ83PkTWUWAJyrOM"
accessSecret = "phfN8ElYDlFwCN3El9v8cT9yrtV9MXpDFvjmrwwe1HbKg"


token<- create_token(app = "refreshingTweepy", consumer_key = key, consumer_secret = secret, access_token = accessToken, access_secret = accessSecret)

user<- lookup_users(null_accounts$Account_Handle[4])

