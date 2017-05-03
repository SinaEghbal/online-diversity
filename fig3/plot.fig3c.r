#!/usr/bin/Rscript
DATE_AS_COLNAME <- 'X%Y.%m.%d'

# An ugly piece of code.

plot.HHI <- function (table_name, color) {
	table <- get (table_name)
	# get the first date column in each table
	begin.index <- grep (pattern = 'X((0-9)*\\-*)*', x = colnames (table)) [1]
	end.index <- ncol (table)
	competitors <- table ['competitors', begin.index:end.index]
	dates <- as.Date (colnames (table)[begin.index:end.index], DATE_AS_COLNAME)
	values <- table ["HHI", begin.index : end.index]
	plot (dates, values, type = 'l', xlab = 'Time', ylab = 'Herfindahl-Hirschman Index', ylim = c (0, 1), col = color)
	x <- seq (1, length (values))# which (!is.na (links ['reddit', ]))# which (!is.na (links ['reddit',]))
	fitted_HHI <- lm (unlist (values, use.names = FALSE)~poly(x, 2, raw=TRUE))
	points (dates, predict (fitted_HHI, data.frame (x = x)), col = color, pch = '.')
	axis (3, at = values [seq (1,length (values), by = 10)], labels = competitors [seq (1, length (values), by = 10)])
	# mtext (paste ("HHI: ", table_name, sep = ''))
}

load ('data/analysis_unified_date.dat')

pdf ('music_HHI_overlapped.pdf')
plot.HHI ('music_twitter', 'red')
par (new = TRUE)
plot.HHI ('music_reddit', 'blue')
mtext (paste ("HHI: music_reddit, music_twitter", sep = ''))
legend ("topright", legend = c ('music_reddit', 'music_twitter'), cex = 0.9, col = c ('blue', 'red'), lty = 1, bg = 'transparent')
dev.off ()