## once the datasets are precomputed in the dataset folder, I analyze them here.
require("car")
require("moments")
require("poweRlaw")
require("scales")
require("DescTools")

source("HHI-powerlaw-fits/functions-online-diversity.R")

####### dataset selection and loading
# datasetname <- "wikilinks"
# datasetname <- "reddit"
datasetname <- "twitter"

# ## load dataset and extract the timeframes from attr names
# dataset <- load_dataset(datasetname = datasetname)
# dt <- get_dates(dataset = dataset)
# 
# ###### done loading -- start analysis
# 
# total <- colSums(dataset[, -1], na.rm = T)
# uniqueness <- colSums(dataset[, -1] > 0, na.rm = T) / total
# 
# perc_1000 <- apply(X = dataset[, -1], MARGIN = 2, FUN = function(x) {
#   pos <- order(x, decreasing = T)
#   pos <- pos[1:1000]
#   total_1000 <- sum(x[pos], na.rm = T)
#   return(total_1000 / sum(x, na.rm = T))
# })
# gc()
# 
# ## construct the datasetCopy, with only active domain (rest replaced by NA)
# datasetCopy <- dataset[, -1]
# for (i in 1:ncol(datasetCopy)) {
#   datasetCopy[datasetCopy[, i] == 0, i] <- NA
# }
# # datasetCopy[datasetCopy == 0] <- NA
# 
# ## distribution analysis - higly skewed distribution
# skew_series <- skewness(x = datasetCopy, na.rm = T)
# kurt_series <- kurtosis(x = datasetCopy, na.rm = T)
# 
# first_dens <- 1 ; last_dens <- length(dt) ; middle_dens <- round( mean(c(first_dens, last_dens)))
# if (datasetname == "reddit") {
#   dens_first <- density(x = na.omit(datasetCopy[, first_dens]) ) #, bw = "SJ" ## maybe SJ is better, but it takes a crazy amount of time for my dataset size
#   dens_middle <- density(x = na.omit(datasetCopy[, middle_dens])) #, bw = "SJ" ## maybe SJ is better, but it takes a crazy amount of time for my dataset size
#   dens_last <- density(x = na.omit(datasetCopy[, last_dens])) #, bw = "SJ" ## maybe SJ is better, but it takes a crazy amount of time for my dataset size
# } else {
#   dens_first <- density(x = na.omit(datasetCopy[, first_dens])) #, bw = "SJ" ## maybe SJ is better, but it takes a crazy amount of time for my dataset size
#   dens_middle <- density(x = na.omit(datasetCopy[, middle_dens])) #, bw = "SJ" ## maybe SJ is better, but it takes a crazy amount of time for my dataset size
#   dens_last <- density(x = na.omit(datasetCopy[, last_dens])) #, bw = "SJ" ## maybe SJ is better, but it takes a crazy amount of time for my dataset size
# }
# 
# ## fit a powerlaw for each timeframe
# if (!file.exists(sprintf("data/results-pl-fit-%s.dat", datasetname))) {
#   require(parallel)
#   .cl <- makeCluster(spec = min(detectCores(), length(names(dataset))))
#   results <- parSapply(cl = .cl, X = dataset[,-1], FUN = function(series) {
#     require("poweRlaw")
# 
#     series <- unlist(series)
#     my_pl <- displ$new(unlist(series[series > 0]))
#     est <- estimate_xmin(m = my_pl)
#     # my_pl$setXmin(est)
# 
#     return( c(est$pars, est$xmin, est$gof))
#   })
#   stopCluster(.cl)
# 
#   results <- data.frame(t(results))
#   names(results) <- c("alpha", "xmin", "gof")
#   save(results, file = sprintf("data/results-pl-fit-%s.dat", datasetname), compress = "bzip2")
# } else {
#   ## simply load from file the fitting results
#   load(file = sprintf("data/results-pl-fit-%s.dat", datasetname))
#   names(results) <- c("alpha", "xmin", "gof")
# }
# 
# HHI <- sapply(X = dataset[, -1], FUN = Herfindahl)
# Rosen <- sapply(X = dataset[, -1], FUN = Rosenbluth)
# 
# ############# save the data for plots, for Daniel to play with
# colMeansDatasetCopy <- colMeans(datasetCopy, na.rm = T)
# save(datasetname, uniqueness, dt, colMeansDatasetCopy, perc_1000, skew_series, kurt_series, dens_last, dens_first, dens_middle, results, first_dens, middle_dens, last_dens, HHI,
#      file = sprintf("data/2016-04-15-%s-total-diversity-decline.dat", datasetname),  compress = "bzip2")
## load with
load(file = sprintf("HHI-powerlaw-fits/data/2016-04-15-%s-total-diversity-decline.dat", datasetname))
############ done saving / loading

## the next line computes the mean and median for each percentile
pdf(file = sprintf("HHI-powerlaw-fits/2016-04-15-%s-total-diversity-decline.pdf", datasetname), width = 6, height = 6)
val_to_plot <- uniqueness
# plot(val_to_plot, type = "l", xaxt = "n", 
#      main = sprintf("%s: link uniqueness", datasetname), xlab = "Time", ylab = "Uniqueness" )
# axis(side = 1, at = 1:length(dt), labels = rep(x = "", times = length(dt)), las = 2)
x <- barplot(height = val_to_plot, names.arg = rep(x = "", times = length(dt)), las = 2,
             main = sprintf("%s: link uniqueness", datasetname), xlab = "", ylab = "Uniqueness" )
## for 45 degrees labels
xs <- x + 0.7
# ys <- min(val_to_plot, na.rm = T) * 0.65 #-1.25
ys <- -0.004
labs <- format(dt, "%m-%Y")
idx <- 1:length(dt)
if (datasetname != "wikilinks") {
  idx <- seq(from = 1, to = length(dt), length.out = 20)
  xs <- xs + 2
  ys <- -0.01
}
text(cex=1, x=xs[idx], y=ys, labs[idx], xpd=TRUE, srt=45, pos=2)
###### plot 2
par(mar = c(6, 4, 4, 2) + 0.1)
plot(colMeansDatasetCopy, type = "l", las = 2, xaxt = "n",
     main = sprintf("%s: mean #links per domain", datasetname), xlab = "", ylab = "mean #links" )
axis(side = 1, at = idx, labels = labs[idx], las = 2)
plot(perc_1000, type = "l", las = 2, xaxt = "n",
     main = sprintf("%s: percentage of top 1000 domains", datasetname), xlab = "", ylab = "#links (%)" )
axis(side = 1, at = idx, labels = labs[idx], las = 2)
plot(skew_series, type = "l", las = 2, xaxt = "n", lwd = 3, cex.main = 1.75, cex.axis=1.25, las = 1, cex.lab = 1.25,
     main = sprintf("%s: Sample skewness over time", datasetname), xlab = "", ylab = "Sample skewness" )
axis(side = 1, at = idx, labels = labs[idx], las = 2)
plot(kurt_series, type = "l", las = 2, xaxt = "n", lwd = 3, cex.main = 1.75, cex.axis=1.25, las = 1, cex.lab = 1.25,
     main = sprintf("%s: kurtosis over time", datasetname), xlab = "", ylab = "Kurtosis" )
axis(side = 1, at = idx, labels = labs[idx], las = 2)

###### 3 densities plot
plot(dens_last, log = "xy", main = sprintf("%s: (log/log) Domain frequency density", datasetname),
     xlab = "Domain frequency", ylab = "Density")
lines(dens_middle, col = "blue")
lines(dens_first, col = "red")
legend("bottomleft", legend = c(labs[first_dens], labs[middle_dens], labs[last_dens]), 
       col = c("red", "blue", "black"), lty = c(1, 1, 1), lwd = c(2, 2, 2), bty = "n")

##### powerlaw distribs fits
plot(results$alpha, type = "l", xaxt = "n", lwd = 3, cex.main = 1.75, cex.axis=1.25, las = 1, cex.lab = 1.25,
     main = sprintf("%s: Power-law exponent over time", datasetname), 
     xlab = "", ylab = "Power-law exponent")
axis(side = 1, at = idx, labels = labs[idx], las = 2)
plot(results$xmin, type = "l", xaxt = "n", 
     main = sprintf("%s: Xmin of domain freq. distr. over time", datasetname), 
     xlab = "", ylab = "Xmin")
axis(side = 1, at = idx, labels = labs[idx], las = 2)
plot(results$gof, type = "l", xaxt = "n", 
     main = sprintf("%s: goodness of fit for domain freq. distr. over time", datasetname), 
     xlab = "", ylab = "Xmin")
axis(side = 1, at = idx, labels = labs[idx], las = 2)

## HHI over time
plot(HHI, type = "l", las = 2, xaxt = "n",
     main = sprintf("%s: HHI / Simpson's index", datasetname), xlab = "", ylab = "HHI" )
axis(side = 1, at = idx, labels = labs[idx], las = 2)
dev.off()
