#!/usr/bin/Rscript

source("../utils.R")

library(lubridate)

load ('data/heros_merged.dat')

# build summary dataset from
datasets <- c ('twitter', 'reddit')
data <- list()

for (ds in datasets) {
	tables <- ls (pattern = ds)
	rows <- data.frame ()
	for (table in tables) {
		current_table <- get (table)
		rows <- rbind (rows, current_table 
					   [1 : which (rownames (current_table) == 'total_links') - 1, ])
	}
	column <- as.Date (colnames (current_table), 'X%Y.%m.%d')
	begin.index <- which (!is.na (column))[1]
	end.index <- ncol (current_table)
	colnames (rows) [begin.index:end.index] <- format (column) [begin.index:end.index]
	rows <- rows [, c (1, begin.index:end.index)]
	tbl <- data.frame(t (rows [,2:ncol (rows)]))
	names(tbl) <- unlist(rows['Company.Name'])
	tbl[tbl==0] <- 0.001

	#remove spaces from front of some names
	i <- substr(names(tbl), 1,1)==" "
  names(tbl)[i] <- substring(names(tbl)[i], 2)

	ylim <- ymd(c("2006-01-01", "2016-07-01"))
	date <-  ymd(rownames(tbl))
	i <- date >= ylim[1] & date < ylim[2]
	data[[ds]] <- tbl[i,]
}

# Load grouping variables
competitors <- read.csv("data/hero_competitors.csv",
		stringsAsFactors = FALSE, check.names=FALSE)[, c("Company","Website","Year","Category")]
groups <- split(competitors, competitors[["Category"]])

# sanity check -- check names in datasets match
all(sort(unique(names(data[[1]]), names(data[[2]]))) %in% competitors[["Company"]])

# colors for each group
colors <- col.lots(length(names(groups)))
names(colors) <- names(groups)

# do for each dataset
for (ds in datasets) {
	df <- data[[ds]]
	pdf (sprintf('fig2c-%s.pdf', ds), height = 6, width=8)

	par(mar=c(5,5,1,6))

	x <- ymd(rownames(df))

	plot(x, rep(1, length(x)), type='n', log="y",
					 xlab = '', xaxs="i",
					 yaxt = 'n',  ylim=c(1, 1E7), xlim= ymd(c("2006-01-01","2016-06-01")), ylab = '')
	add_axis_log10(2)

	for(v in names(groups)){
		g <- groups[[v]]
		f <- g[["Company"]] %in% names(df)
		y <- rowSums(df[,g[["Company"]][f]])
		r <- lines_rolling_mean(x, y, n=3, col = colors[v], lwd=2)

		text(x=ymd("2016-06-01"), y=r$y[length(r$y)], labels = v, col = colors[v], las=1, cex=0.6, pos=4, xpd=NA)
	}

	mtext(sprintf("Links (per month)", ds), 2, cex=1.2, line=3)
	mtext("Year", 1, cex=1.2, line=3)
	dev.off ()
}

