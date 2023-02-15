############################################
#                                          #
#  ADDING CIRCONSCRIPTION-LEVEL VARIABLES  #
#                                          #
############################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 15/02/23

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

