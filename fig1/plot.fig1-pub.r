#!/usr/local/bin/Rscript

library(lubridate)
library(png)

source("../utils.R")

mylabel <- function(txt) label(txt, -0.15, 1.2, xpd=NA, cex=1.5)
as.date1 <- function(txt) {ymd(sprintf("%s-01", names(txt)))}
as.date2 <- function(txt) {ymd(names(txt))}

cols <- c("#0084b4", "#ff4500")

load ('data/posts-tweets.dat')
posts [1, '2011-12'] = NA
posts [1, '2015-03'] = NA

pdf ('fig1-pub.pdf', height=3, width=9)
par(mfrow=c(1,3), oma=c(4,2,0,1), mar=c(2,5,4,0))

plot(as.date1(posts [1, ]),posts [1, ], type = 'l', ylab = '', xlab = '', yaxt = 'n', col = cols[1], log = "y",
			ylim = c (1e4, 1e9), lty = "dashed", lwd=2)
points (as.date1(posts [2, ]),  posts [2, ], type = 'l',  col = cols[2], lty = "dashed", lwd=2)
add_axis_log10(2)

fn_list <- load (sprintf ('data/ds_properties.dat'))
# # Get rid of abnormal data
domains ['twitter', '2011-12-15'] = NA
domains ['twitter', '2011-10-15'] = NA
links ['twitter', '2015-03-15'] = NA
links ['twitter', '2011-09-15'] = NA

points (as.date2(links ['reddit',]), links ['reddit',], col = cols[2], type = 'l', lwd=2)
points (as.date2(links ['twitter',]), links ['twitter',], col = cols[1], type = 'l', lwd=2)

x <- 0.05
y <- 0.9
rasterImage_percent(readPNG("../assets/twitter.png"), x + c(0, 0.05), y + c(0, 0.05))
text(percent_x(x + 0.03), percent_y(y + 0.02), "Twitter", pos=4)
rasterImage_percent(readPNG("../assets/reddit.png"), x + c(0, 0.05), y - 0.075 + c(0, 0.06))
text(percent_x(x + 0.03), percent_y(y - 0.075 + 0.02), "Reddit", pos=4)
mtext("Number (per month)", 2, cex=0.9, line=3)

mylabel("A - Number of posts & links")

plot (as.date2(domains ['reddit', ]), domains ['reddit', ], log = "y", col = cols[2], yaxt = 'n', ylim = c (1e3, 1e6), ylab = '', xlab = '', type = 'l', lwd=2)
points (as.date2(domains ['twitter', ]), domains ['twitter', ], col = cols[1], type = 'l', lwd=2)
add_axis_log10(2)
mtext("Year", 1, cex=1.2, line=4)
# mtext("Number (per month)", 2, cex=0.9, line=3)
mylabel("B - Number of active domains")

plot (as.date2(uniqueness ['reddit', ]), uniqueness ['reddit', ], col = cols[2], las=1, ylim = c (0, 0.8), ylab = '', xlab = '', type = 'l', lwd=2)
points(as.date2(uniqueness ['twitter', ]),uniqueness ['twitter', ], col = cols[1],  type = 'l', lwd=2)
mtext("Domains per link", 2, cex=0.9, line=3)
mylabel("C - Uniqueness of links")


dev.off()
