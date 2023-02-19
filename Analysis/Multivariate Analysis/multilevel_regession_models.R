##################################
#                                #
#  MULTILEVEL REGRESSION MODELS  #
#                                #
##################################


# LAURENCE ROWLEY-ABEL, UNIVERSITY OF EDINBURGH
# UPDATED: 17/02/23

# DESCRIPTION: This file runs a series of multilevel regressions to test the relationship between party and the dependent variables
# while controlling for other relevant factors

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(utils)
library(lubridate)
library(lme4)
library(gtsummary)
library(broom.mixed)
library(parameters)

setwd(choose.dir(caption = "Select working directory"))
rm(list = ls())

# Get directory containing processed datasets on University of Nottingham OneDrive, under Twitter Scrapes > Datasets
data_dir<-choose.dir(caption = "Select directory containing processed datasets on University of Nottingham OneDrive, under Twitter Scrapes > Datasets")

# Read in analysis datasetg
df<- read.csv(paste0(data_dir, "\\Analysis Dataset\\Analysis Dataset.csv"), fileEncoding = "UTF-8")

# Standardise the capitalisation of variable names
colnames(df)<- str_to_lower(colnames(df))

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

# Recode and rename incumbency variable
df<- df%>%
  mutate(incumbent = case_when(office1_incumbent == 0 ~ "No",
                               office1_incumbent %in% c(1,2) ~ "Yes"))

# Rename count of past runs for legislative office and recode as a categorical variable
df<- df%>%
  rename(past_runs = office1_prior_natl_legis_count)%>%
  mutate(past_runs = as.factor(past_runs))
  

# Rename the variable indicating the circonscription
df<- df%>%
  rename(circo = office1_constituencylevel1)

# Create age variable
df<- df%>%
  mutate(age = interval(ymd(birthdate), dmy("12-06-2022"))%/%years(1)%>%
           floor())%>%
  select(-birthdate)

# Create variable indicating whether candidate is standing in an overseas territory
df<- df%>%
  mutate(overseas = ifelse(substr(office1_constituencylevel1number,1,1)=="9", "Yes", "No"))

# Replace erroneous values for age and offce1_round_percent
df$age[df$age>100]<- NA
df$office1_round_percent[df$office1_round_percent>100]<- NA

# Re-scale the continuous variables to have mean of 0 and SD of 1
df<- df%>%
  mutate(across(c(dynamism, originality, log_total_following, log_total_tweets, prop_original, log_mean_party_tfidf, age, presidential_winner_lead, legislative_2017_winner_lead, distance_from_paris, log_total_followers), ~scale(.x)[,1]))


df%>%
  summarise(across(c(dynamism, originality, log_total_following, log_total_tweets, prop_original, log_mean_party_tfidf, age, presidential_winner_lead, legislative_2017_winner_lead, distance_from_paris, log_total_followers),
                   .fns = list(Min = ~min(.x, na.rm=T),
                               Max = ~max(.x, na.rm=T),
                               Mean = ~mean(.x, na.rm =T),
                               SD = ~sd(.x, na.rm=T)),
                   .names = "{.col}__{.fn}"))%>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_sep = "__")%>%
  mutate(Variable = str_replace_all(Variable, "_", " "))%>%
  mutate(across(c(Min, Max, Mean, SD), ~round(.x, 2)))


# Run multilevel regression models for all of the continuous dependent variables, with candidates clustered within circos
m1<- lmer(dynamism ~ age + gender + party_list + incumbent + past_runs +  log_total_followers + presidential_winner_lead + legislative_2017_winner_lead + overseas + distance_from_paris + (1 | circo), data = df)

m2<- lmer(log_total_following ~ age + gender + party_list + incumbent + past_runs +  log_total_followers + presidential_winner_lead + legislative_2017_winner_lead + overseas + distance_from_paris + (1 | circo), data = df)

m3<- lmer(log_total_tweets ~ age + gender + party_list + incumbent + past_runs +  log_total_followers + presidential_winner_lead + legislative_2017_winner_lead + overseas + distance_from_paris + (1 | circo), data = df)

m4<- lmer(originality ~ age + gender + party_list + incumbent + past_runs +  log_total_followers + presidential_winner_lead + legislative_2017_winner_lead + overseas + distance_from_paris + (1 | circo), data = df)

m5<- lmer(prop_original ~ age + gender + party_list + incumbent + past_runs +  log_total_followers + presidential_winner_lead + legislative_2017_winner_lead + overseas + distance_from_paris + (1 | circo), data = df)

m6<- lmer(log_mean_party_tfidf ~ age + gender + party_list + incumbent + past_runs +  log_total_followers + presidential_winner_lead + legislative_2017_winner_lead + overseas + distance_from_paris + (1 | circo), data = df)

# Run a multilevel logistic regression for the created_new_accounts variable
m7<- glmer(created_new_accounts ~age + gender + party_list + incumbent + past_runs +  log_total_followers + presidential_winner_lead + legislative_2017_winner_lead + overseas + distance_from_paris + (1 | circo), data = df, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000)))

# Format model results into tables using the gtsummary package.

# Combine linear models for dynamism into a list
dynamism_linear_model_list<- list(m1, m2, m3)

# Specify a custom tidy function to use with lmer models
lmer_tidy<- function(m, exponentiate = F){
  tidied_model<- broom.mixed::tidy(m, effects = "fixed")  
  tidied_model$conf.int<- paste(parameters(m)$CI_low[1:nrow(tidied_model)], "-", parameters(m)$CI_high[1:nrow(tidied_model)])
  tidied_model$p.value<- parameters(m)$p[1:nrow(tidied_model)]
  if(exponentiate==T){
    tidied_model$estimate<- exp(tidied_model$estimate)
  }
  return(tidied_model)
}

# Produce gt tables for each linear model
dynamism_linear_tables_list<- lapply(dynamism_linear_model_list, function(m){tbl_regression(m,
                                                            label = list(age ~ "Age",
                                                                          gender ~ "Gender",
                                                                          party_list ~ "Party",
                                                                          incumbent ~ "Incumbent",
                                                                          past_runs ~ "Number of past runs",
                                                                          log_total_followers ~ "Log of total followers",
                                                                          presidential_winner_lead ~ "Lead of presidential winner in circo",
                                                                          legislative_2017_winner_lead ~ "Lead of 2017 legisltaive winner in circo",
                                                                          overseas ~ "Overseas circo",
                                                                          distance_from_paris ~ "Distance from Paris"),
                                                            tidy_fun = function(x, ...) lmer_tidy(x),
                                                            estimate_fun =  function(x, ...) round(x,3))%>%
                                              add_glance_table(include = c(nobs, logLik, AIC))})

# Produce a gt table for the logistic regression model
new_accounts_table<- tbl_regression(m7,
                                    label = list(age ~ "Age",
                                                 gender ~ "Gender",
                                                 party_list ~ "Party",
                                                 incumbent ~ "Incumbent",
                                                 past_runs ~ "Number of past runs",
                                                 log_total_followers ~ "Log of total followers",
                                                 presidential_winner_lead ~ "Lead of presidential winner in circo",
                                                 legislative_2017_winner_lead ~ "Lead of 2017 legisltaive winner in circo",
                                                 overseas ~ "Overseas circo",
                                                 distance_from_paris ~ "Distance from Paris"),
                                    exponentiate = T,
                                    tidy_fun = function(x, ...) lmer_tidy(x, exponentiate = T))%>%
                    add_glance_table(include = c(nobs, logLik, AIC))

# Combine all the gt tables for dynamism variables into one list
all_dynamism_tables<- list.append(tables_list, new_accounts_table)

# Merge the gt tables for dynasmism variables into one table and save as an html file
tbl_merge(all_dynamism_tables, tab_spanner = c("Dynamism", "Log total following", "Log total tweets", "Created new accounts"))%>%
  as_gt()%>%
  tab_footnote(footnote = html("<strong>Note: All continuous variables scaled such that mean = 0 and SD = 1</strong><br>
                              Models for dynamism, log of total following and log of total tweets are multilevel linear regressions<br>
                              Models for created new accounts is a multilevel logistic regression"))%>%
  tab_caption(md("**Multilevel Regressions of Dynamism Score and the Component Variables**"))%>%
  gtsave(filename = "dynamism_model_results.html")

# Combine all the models for the originality variables into one list
originality_models_list<- list(m4, m5, m6)

# Produce gt tables for each linear model
originality_tables_list<- lapply(originality_models_list, function(m){tbl_regression(m,
                                                                            label = list(age ~ "Age",
                                                                                         gender ~ "Gender",
                                                                                         party_list ~ "Party",
                                                                                         incumbent ~ "Incumbent",
                                                                                         past_runs ~ "Number of past runs",
                                                                                         log_total_followers ~ "Log of total followers",
                                                                                         presidential_winner_lead ~ "Lead of presidential winner in circo",
                                                                                         legislative_2017_winner_lead ~ "Lead of 2017 legisltaive winner in circo",
                                                                                         overseas ~ "Overseas circo",
                                                                                         distance_from_paris ~ "Distance from Paris"),
                                                                            tidy_fun = function(x, ...) lmer_tidy(x),
                                                                            estimate_fun =  function(x, ...) round(x,3))%>%
    add_glance_table(include = c(nobs, logLik, AIC))})

# Merge the gt tables for originality variables into one table and save as an html file
tbl_merge(originality_tables_list, tab_spanner = c("Originality", "Original tweet proportion", "Log of mean TFIDF"))%>%
  as_gt()%>%
  tab_footnote(footnote = html("<strong>Note: All continuous variables scaled such that mean = 0 and SD = 1</strong><br>
                              Models for originality, original tweet proportion and log of mean TFIDF are multilevel linear regressions"))%>%
  tab_caption(md("**Multilevel Regressions of Originality Score and the Component Variables**"))%>%
  gtsave(filename = "originality_model_results.html")

