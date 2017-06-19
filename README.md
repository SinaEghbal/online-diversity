# online-diversity
## - Fig 1:
  * Number of posts/tweets (for twitter and reddit)
  * Number of links/time
  * Total domains/time
  * Link uniqueness (# of links/# of linked species)/time
  ### Number of comments and link uniqueness plots for reddit added

## - Fig 2:
  * Cumulative function attention for all crunchbase species (log)/time
  * Number of links to each hero (log)/time
  
## - Fig 3:
  * Overlapped HHI plots.
    - music_reddit vs. music_twitter

## - Fig 4:
  * Number of links/age
  * Active species/age
  * HHI/age -- The peak around the 37th month represents the time where the domains "youtu.be" and "amzn.com" have high market shares. This can be read in the top_names table within "reddit_cohort_analysis.dat".
  * Survival (V2; # of active domains born in the timespan/# of domains born in the timespan)/age

#### To generate the data tables for each figure, we can follow the below instructions:
- Fig 1:
  + scripts/extract_dataset_properties.r
  + scripts/plot_volume_time.r
- Fig 2:
  + scripts/join_data.r
  + scripts/analyse.r
  + scripts/plot.fig2a.r
  + scripts/draw.r //shortlist = TRUE

- Fig 3:
  + scripts/draw.r //shortlist = FALSE

- Fig 4:
  + scripts/temporal_analysis.r
