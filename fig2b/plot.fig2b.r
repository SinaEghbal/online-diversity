#!/usr/bin/Rscript

rm (list = ls ())

DATE_AS_COLNAME <- 'X%Y.%m.%d'
load ('data/heros_merged.dat')

datasets <- c ('twitter', 'reddit', 'wikilinks')

# pdf ('plots/plot.top.players.growth.pdf')

for (ds in datasets) {
	pdf (sprintf ('fig2b-%s.pdf', ds))
	tables <- ls (pattern = ds)
	current_dataaset <- data.frame ()
	rows <- data.frame ()
	for (table in tables) {
		current_table <- get (table)
		# last_row <- which (rownames (current_table) == 'total_links') - 1
		rows <- rbind (rows, current_table [, ])
	}
	# ds_table <- rows
	column <- as.Date (colnames (current_table), DATE_AS_COLNAME)
	begin.index <- which (!is.na (column))[1]
	end.index <- ncol (current_table)
	
	# order <- rownames (sort (rows [, begin.index:end.index]))
	colnames (rows) [begin.index:end.index] <- format (column) [begin.index:end.index]
	rows <- rows [, c (1, begin.index:end.index)]
	# rows [, 2:ncol (rows)] = log (rows [, 2:ncol (rows)])
	# rows [rows == -Inf] = 0
	
	# rows [rows == -Inf] = 0
	colors <- unique (rainbow (end.index - begin.index + 1))
	tbl<- t (t (rows [,2:ncol (rows)]))
	tbl [which (tbl == 0)] = tbl [which (tbl == 0)] + 0.0001
	bp <- barplot (tbl, xaxt = 'n', main = sprintf ('Functions growth - %s', ds)
				   , ylab = '# of links (log)', col = colors, log = 'y')
	legend ("topleft", legend = unlist (rows ['Company.Name']), cex = 0.4, col = colors, lty = 1, bg = 'transparent')
	axis (side = 1, at = bp [seq (1, end.index - begin.index + 1, by = 3)],
		  labels = column [seq (begin.index, end.index, by = 3)], las = 2, cex.axis = 0.7)
	dev.off ()
}
# dev.off ()
