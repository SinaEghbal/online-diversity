#!/usr/bin/Rscript
DATE_AS_COLNAME <- 'X%Y.%m.%d'

# We use this function to get random colors for our top n players
# Hacky and crappy
# TODO : To be rewritten
check_colors <- function (top_n_players) {
	if (exists ('chosen_colors')) {
		new_players <- setdiff (c (top_n_players, 'rest'), names (chosen_colors))
		colors_needed <- length (new_players)
		if (colors_needed == 0) return (NA)
		used_colors <- unlist (chosen_colors, use.names = FALSE)
		new_colors <- unique (sample (x = colors (), size = length (colors_needed)))
		
		while (length (intersect (new_colors, used_colors)) != 0 || length (new_colors) != colors_needed) {
			new_colors = unique (sample (x = colors (), size = colors_needed))
		}
		chosen_colors [new_players] = new_colors
		assign ('chosen_colors', chosen_colors, globalenv())
	} else {
		chosen_colors <<- list ()
		chosen_colors ['rest'] = sample (x = colors (), size = 1)
		assign ('chosen_colors', chosen_colors, globalenv())
		check_colors (top_n_players)
	}
	# assign ('chosen_colors', chosen_colors, globalenv())
}

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

plot.top.n.players <- function (top_n_players, table_name) {
	columns <- colnames (top_n_players)

	check_colors (top_n_players [, 'Company.Name'])
	matplot(y = t (top_n_players [, 2: ncol (top_n_players)]), type = c("b"), pch=19, col = unlist (chosen_colors
			[c (top_n_players [, 'Company.Name'], 'rest')], use.names = FALSE), xaxt = 'n', xlab = NA,
			ylab = 'links')

	axis (1, las = 2, at = seq (1, length (columns), by = 8), xlab = 'Dates', ylab = 'links',
		labels = columns [seq (2, length (columns), by = 8)], cex.axis = 0.7)
	legend ("topleft", legend = top_n_players [, 1], col = unlist (chosen_colors [c (top_n_players [, 'Company.Name'], 'rest')], use.names = FALSE), pch=19)
	mtext (paste ('Top ', 7, ' players', ': ', table_name, sep = ''))
	mtext(line = -1, cex = 0.5, text = 'Specified (S), Found (F)')
}

load ('data/analysis_unified_date.dat')

pdf ('music_HHI_overlapped.pdf')
plot.HHI ('music_twitter', 'red')
par (new = TRUE)
plot.HHI ('music_reddit', 'blue')
mtext (paste ("HHI: music_reddit, music_twitter", sep = ''))
legend ("topright", legend = c ('music_reddit', 'music_twitter'), cex = 0.9, col = c ('blue', 'red'), lty = 1, bg = 'transparent')
dev.off ()

pdf ('file_sharing_HHI_overlapped.pdf')
plot.HHI ('file_sharing_reddit', 'blue')
par (new = TRUE)
plot.HHI ('file_sharing_twitter', 'red')
mtext (paste ("HHI: file_sharing_reddit, file_sharing_twitter", sep = ''))
legend ("topright", legend = c ('file_sharing_reddit', 'file_sharing_twitter'),
	cex = 0.9, col = c ('blue', 'red'), lty = 1, bg = 'transparent')
dev.off ()

pdf ('hospitality_HHI_overlapped.pdf')
plot.HHI ('hospitality_reddit', 'blue')
par (new = TRUE)
plot.HHI ('hospitality_twitter', 'red')
mtext (paste ("HHI: hospitality_reddit, hospitality_twitter", sep = ''))
legend ("topright", legend = c ('file_sharing_reddit', 'file_sharing_twitter'),
	cex = 0.9, col = c ('blue', 'red'), lty = 1, bg = 'transparent')
dev.off ()

pdf ('hospitality_reddit.pdf')
load ('data/top.n.players_hospitality.dat')
plot.top.n.players (top_n_players, 'hospitality_reddit')
dev.off ()


pdf ('file_sharing_reddit.pdf')
load ('data/top.n.players_file_sharing.dat')
plot.top.n.players (top_n_players, 'file_sharing_reddit')
dev.off ()

pdf ('music_reddit.pdf')
load ('data/top.n.players_music.dat')
plot.top.n.players (top_n_players, 'music_reddit')
dev.off ()