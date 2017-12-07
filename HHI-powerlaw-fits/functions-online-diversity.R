## these are common functions related to online diversity

#' Given a dataset name, this function loads it and put it into the common
#' format, ready to be used by analysis functions
load_dataset <- function(datasetname = "reddit") {
  print(sprintf("--> Loading dataset '%s' ...", datasetname))
  
  ####### dataset selection and loading
  ## Wikipedia dataset
  switch(datasetname,
         wikilinks = {
           load("data/binary_datasets/wikilinks-complete.dat") ; dataset <- wikilinks ; rm(wikilinks)
           outliers <- c()
         },
         reddit = {
           load("data/binary_datasets/reddit.dat") ; dataset <- reddit ; rm(reddit)
           outliers <- c()
         },
         twitter = {
           load("data/binary_datasets/twitter.dat") ; datasetname <- "twitter" ; dataset <- twitter ; rm(twitter) ; 
           ## TODO: recheck these with new version of dataset
           outliers <- c("X2011.10.13", "X2012.06.30", "X2012.12.27", "X2013.01.21", "X2013.01.19", "X2011.09.27") 
         },
         {
           stop(sprintf("Unknown dataset name '%s'.", datasetname))
         })
  
  ## remove domains that never appear (some bug in code? in dataset construction?)
  effectives <- rowSums(dataset[,-1]) > 0
  ## remove outliers
  dataset <- dataset[effectives, ! names(dataset) %in% outliers]
  
  return(dataset)  
}

#' Given a dataset, this function returns the series of dates corresponding to
#' the columns.
get_dates <- function(dataset) {
  ## extract the timeframes from attr names
  dt <- names(dataset)[-1]
  dt <- gsub(pattern = "X", replacement = "", x = dt)
  dt <- as.Date(dt, "%Y.%m.%d")
  
  return(dt)
}