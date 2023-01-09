# French Election Tweet Analysis

This is code for a research project looking at the Twitter data of candidates in the 2022 French legislative elections. The research is being carried out with the University of Nottingham and [the Digital Society Project](http://digitalsocietyproject.org/).

None of the main Twitter data is in this repository. The code creates and analyses datasets, but they are stored on the University of Nottingham OneDrive.

**Workflow:**
- **Data preparation:**
   - Create dataset of Tweets: _getting_tweet_data_from_raw_json_files.R_
   - Create dataset of Twitter accounts: _getting_account_data_from_raw_json_files.R_
   - Create lookup joining BIOIDs to Twitter Handles: _creating_BIOID_to_twitter_handle_lookup.R_
   - Create dataset of Twitter metrics for each BIOIDs: _calculating_metrics.R_
   - Extract hashtags used in Tweets: _extracting_hashtags.R_
   - Create TF-IDF metric for each candidate's hashtags: _calculating_hashtag_tfidf.R_
   - Merge Twitter metrics and demographic/political data on candidates: _merging_metrics_and_candidate_variables.R_
- **Analysis:**
   - Conduct basic exploratory analysis: _exploratory_analysis.R_


**Example of analysis output:**


![Example Plot](https://raw.githubusercontent.com/lrowleyabel/French-Election-Tweet-Analysis/main/Analysis/Exploratory%20Analysis/Plots/mean_tfidf_by_party_list.png)
