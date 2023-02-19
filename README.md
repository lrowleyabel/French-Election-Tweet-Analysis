# French Election Tweet Analysis

This is code for a research project looking at the Twitter data of candidates in the 2022 French legislative elections. The research is being carried out with the University of Nottingham and [the Digital Society Project](http://digitalsocietyproject.org/).

None of the main Twitter data is in this repository. The code creates and analyses datasets, but accesses them remotely.

**Workflow:**
- **Data preparation:**
   - Create dataset of Tweets: _getting_tweet_data_from_raw_json_files.R_
   - Create dataset of Twitter accounts: _getting_account_data_from_raw_json_files.R_
   - Create lookup joining BIOIDs to Twitter Handles: _creating_BIOID_to_twitter_handle_lookup.R_
   - Create dataset of Twitter metrics for each BIOIDs: _calculating_metrics.R_
   - Extract hashtags used in Tweets: _extracting_hashtags.R_
   - Create TF-IDF metric for each candidate's hashtags: _calculating_hashtag_tfidf.R_
   - Create composite score measuring originality of each candidate's Twitter behaviour: _calculating_originality_score.R_
   - Merge Twitter metrics and demographic/political data on candidates: _merging_metrics_and_candidate_variables.R_
   - Add circonscription-level variables: _adding_circonscription_level_variables.R_
- **Exploratory Analysis:**
   - Analyse tweet volumes over time: _tweet_date_analysis.R_
   - Analyse account creation dates: _account_recency_analysis.R_
   - Explore data with Shiny app: _Exploratory Visualisation App/app.R_
- **Bivariate Analysis:**
   - Conduct bivariate analysis looking at distribution of the dependent variables across parties: _bivariate_analysis.R_
- **Multivariate Analysis:**
   - Run multilevel regression models for each of the dependent variables: _multilevel_regression_models.R_
- ...


![example plot](https://raw.githubusercontent.com/lrowleyabel/French-Election-Tweet-Analysis/main/Analysis/Exploratory%20Analysis/Plots/account_creation_date_distribution_by_party.png)
