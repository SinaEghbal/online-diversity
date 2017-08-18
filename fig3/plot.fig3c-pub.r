#!/usr/local/bin/Rscript

library(png)
library(lubridate)

source("../utils.R")

mylabel <- function(txt) label(txt, -0.25, 1.2, xpd=NA, cex=1.4)
cols <- c("#0084b4", "#ff4500")

load ('data/analysis_unified_date.dat')

plot.HHI <- function (table_name, color, add=FALSE) {
	table <- get (table_name)
	# get the first date column in each table
	begin.index <- grep (pattern = 'X((0-9)*\\-*)*', x = colnames (table)) [1]
	end.index <- ncol (table)
	dates <- ymd(gsub("X", "", colnames (table)[begin.index:end.index]))
	values <- table ["HHI", begin.index : end.index]

	if(!add)
		plot (dates, values, type = 'n', xlab = '', ylab = '', las=1,  ylim = c (0, 1),
					xlim= ymd(c("2006-01-01","2016-06-01")), xaxs="i", yaxs="i",)

	lines (dates, values, type = 'l', col = color)
}

plot.top.n.players <- function (top_n_players, table_name) {
	companies <- top_n_players [, 'Company.Name']
	companies <- gsub(" (F)", "", companies, fixed=TRUE)
	companies <- gsub(" (S)", "", companies, fixed=TRUE)

	index <- names(companies)
	colors <-  get_colors(index)

	y <- as.data.frame(t(top_n_players [, 2: ncol (top_n_players)]), stringsAsFactors = FALSE)
	for(i in seq_len(ncol(y))) {
		y[[i]] <- as.numeric(y[[i]])
	}
	y[y==0] <- 0.8

	x <- ymd(rownames(y))
	plot(x, rep(1, length(x)), type='n', log="y",
					 xlab = '', xaxs="i",
					 yaxt = 'n',  ylim=c(1, max(y)*2), xlim= ymd(c("2006-01-01","2016-06-01")), ylab = '')
	add_axis_log10(2)

	for(v in names(colors)){
		if(companies[v] != "rest"){
			r <- lines_rolling_mean(x, y[[v]], n=3, col = colors[v], lwd=1)
			text(x=ymd("2016-06-01"), y=r$y[length(r$y)], labels = companies[v], col = colors[v], las=1, cex=0.6, pos=4, xpd=NA)
		}
	}
}

pdf ('fig3-pub.pdf', height=8, width=10)
par(mfcol=c(3,2), oma=c(4,4,1,10), mar=c(2,4,4,2))

#pdf ('music_HHI_overlapped.pdf')
plot.HHI ('music_twitter', cols[1])
plot.HHI ('music_reddit', cols[2], TRUE)
mylabel("A - Music streaming")

x <- 0.75
y <- 0.9
rasterImage_percent(readPNG("../assets/twitter.png"), x + c(0, 0.05), y + c(0, 0.05))
text(percent_x(x + 0.03), percent_y(y + 0.02), "Twitter", pos=4)
rasterImage_percent(readPNG("../assets/reddit.png"), x + c(0, 0.05), y - 0.075 + c(0, 0.06))
text(percent_x(x + 0.03), percent_y(y - 0.075 + 0.02), "Reddit", pos=4)


plot.HHI ('file_sharing_reddit', cols[2])
plot.HHI ('file_sharing_twitter', cols[1],  TRUE)
mylabel("B - File sharing")
mtext ("Herfindahl-Hirschman Index", 2, line=4, cex=.8)

plot.HHI ('hospitality_reddit', cols[2])
plot.HHI ('hospitality_twitter', cols[1],  TRUE)
mylabel("C - Hospitality")
mtext ("Year", 1, line=4, cex=.8)

load ('data/top.n.players_music.dat')
plot.top.n.players (top_n_players, 'music_reddit')

load ('data/top.n.players_file_sharing.dat')
plot.top.n.players (top_n_players, 'file_sharing_reddit')
mtext("Links (per month)", 2, line=4, cex=.8)

load ('data/top.n.players_hospitality.dat')
plot.top.n.players (top_n_players, 'hospitality_reddit')


mtext ("Year", 1, line=4, cex=.8)

dev.off()
