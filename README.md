# French Election Tweet Analysis

This is code for a research project looking at the Twitter data of candidates in the 2022 French legislative elections. The research is being carried out with the University of Nottingham and [the Digital Society Project](http://digitalsocietyproject.org/).

There is no actual data in this repository. The code creates and analyses datasets, but they are stored on the University of Nottingham OneDrive.

**Workflow:**
- **Data preparation:**
   - Create dataset of Tweets: _getting_tweet_data_from_raw_json_files.R_
   - Create dataset of Twitter accounts: _getting_account_data_from_raw_json_files.R_
   - Create lookup joining BIOIDs to Twitter Handles: _creating_BIOID_to_twitter_handle_lookup.R_
   - Create dataset of Tweet metrics for each BIOIDs: _calculating_metrics.R_
- **Analysis:**
   - TBD
