#!/bin/bash/Rscript

load ('data/cumulative_attention.dat')

#!/usr/bin/Rscript

rm (list = ls ())

DATE_AS_COLNAME <- '%Y-%m-%d'
load ('data/cumulative_attention.dat')

datasets <- c ('twitter', 'reddit', 'wikilinks')

for (ds in datasets) {
	pdf (sprintf ('fig2a-%s.pdf', ds))
	current_table <- get (ds)
	column <- as.Date (colnames (current_table), DATE_AS_COLNAME)
	begin.index <- which (!is.na (column))[1]
	end.index <- ncol (current_table)
	
	colnames (current_table) [begin.index:end.index] <- format (column) [begin.index:end.index]
	current_table <- current_table [, c (1, begin.index:end.index)]
	
	colors <- unique (rainbow (nrow (current_table) + 1))
	tbl<- t (t (current_table))
	tbl [which (tbl == 0)] = tbl [which (tbl == 0)] + 0.0001
	bp <- barplot (tbl, xaxt = 'n', main = sprintf ('Cumulative attention - %s', ds)
				   , ylab = '# of links (log)', col = colors, log = 'y')
	legend ("topleft", legend = unlist (rownames (current_table)), cex = 0.4, col = colors, lty = 1, bg = 'transparent')
	axis (side = 1, at = bp [seq (1, end.index - begin.index + 1, by = 3)],
		  labels = column [seq (begin.index, end.index, by = 3)], las = 2, cex.axis = 0.7)
	dev.off ()
}