#!/bin/bash/Rscript

load ('data/cumulative_attention.dat')

#!/usr/bin/Rscript

rm (list = ls ())

DATE_AS_COLNAME <- '%Y-%m-%d'
load ('data/cumulative_attention.dat')

datasets <- c ('twitter', 'reddit', 'wikilinks')

for (ds in datasets) {
	pdf (sprintf ('fig2a-%s.pdf', ds))
	# tables <- ls (pattern = ds)
	# rows <- data.frame ()
	# for (table in tables) {
	# 	current_table <- get (table)
	# 	rows <- rbind (rows, current_table [, ])
	# }
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

# variables <- new.env ()
# source ('scripts/environment_variables.r', variables)
# 
# max <- - Inf
# for (ds in paste ('data/', c ('twitter', 'wikilinks', 'reddit'), '_analysed.dat', sep = '')) {
# 	# if (own_y && ds != dataset) next
# 	objs <- load (ds)
# 	for (fn in objs) {
# 		current_fn <- get (fn)
# 		indices <- variables$get_date_indexes (current_fn)
# 		current_max <- max (current_fn ['total_links', indices ['begin']:indices ['end']])
# 		if (current_max > max) max = current_max
# 	}
# }
# DATE_AS_COLNAME <- 'X%Y.%m.%d'
# 
# twitter_dates <- as.Date (colnames (edtech_twitter), DATE_AS_COLNAME)
# twitter_dates = twitter_dates [- (which (is.na (twitter_dates)))]
# reddit_dates <- as.Date (colnames (edtech_reddit), DATE_AS_COLNAME)
# reddit_dates = reddit_dates [- (which (is.na (reddit_dates)))]
# wikilinks_dates <- as.Date (colnames (edtech_wikilinks), DATE_AS_COLNAME)
# wikilinks_dates = wikilinks_dates [- (which (is.na (wikilinks_dates)))]
# 
# timespan <- sort (unique (c (twitter_dates, reddit_dates, wikilinks_dates)))
# 
# expand_fun <- function (table) {
# 	# table <- get (func_name)
# 	dates <- as.Date (colnames (table), variables$DATE_AS_COLNAME)
# 	index <- which (!is.na (dates))[1]
# 	dates = dates [which (!is.na (dates))]
# 	nrows <- nrow (table)
# 	# Create a big table with all the date columns
# 	new_cols <- format (timespan [which (!timespan %in% dates)], DATE_AS_COLNAME)
# 	new_cols_na <- matrix(nrow = nrow (table), ncol = length (new_cols))
# 	colnames (new_cols_na) <- new_cols
# 	table = cbind (table, new_cols_na)
# 	columns_in_order <- order (colnames (table))
# 	return (table [, columns_in_order])
# }
# 
# datasets <- c ('twitter', 'wikilinks', 'reddit')
# # plot.function.attention <- function (dataset) {
# for (ds in datasets) {
# 	# own_y <- if (missing (own_y)) FALSE else own_y
# 	dataset <- sprintf ('data/%s_analysed.dat', ds)
# 	new.objects <- load (dataset)
# 	links <- list ()
# 	
# 	name <- ifelse (length (grep (pattern = 'twitter', x = dataset)) != 0,'twitter',
# 					ifelse (length (grep (pattern = 'reddit', x = dataset)) != 0, 'reddit',
# 							ifelse (length (grep (pattern = 'wiki', x = dataset)) != 0, 'wiki', NA)))
# 	
# 	for (func_name in new.objects) {
# 		current_fun <- get (func_name)
# 		# get the starting index and the last index for each function
# 		begin.index <- grep (pattern = 'X((0-9)*\\-*)*', x = colnames (current_fun)) [1]
# 		end.index <- ncol (current_fun)
# 		links [[gsub (pattern = '_.*', x = func_name, replacement = '')]] <- current_fun ['total_links', begin.index : end.index]
# 	}
# 	# get the column names and convert them to Date
# 	# date_columns <- gsub (pattern = '_.*', x = func_name, replacement = '')
# 	# column_names <- as.Date (colnames (links [[gsub (pattern = '_.*', x = func_name, replacement = '')]]), 'X%Y.%m.%d')
# 	functions.matrix <- do.call (rbind, links)
# 	
# 	functions.matrix = expand_fun (functions.matrix)
# 	colnames (functions.matrix) [1:ncol (functions.matrix)] <- format (as.Date (colnames (functions.matrix) [1:ncol (functions.matrix)],
# 																				DATE_AS_COLNAME))
# 	
# 	assign (x = ds, value = functions.matrix, envir = globalenv())
# 	variables$check_colors (names (links))
# 	
# 	attention_file <- file ('data/function.attention.dat', 'wb')
# 	save (file = attention_file, chosen_colors, functions.matrix)
# 	
# 	bp <- barplot (sapply (functions.matrix, as.numeric), beside = TRUE, legend = (c (rownames (functions.matrix))),
# 				   args.legend = list (x = 'topleft'), col = unlist (chosen_colors [names (links)], use.names = FALSE),# palette (rainbow (nrow (functions.matrix))), 
# 				   ylab = 'Number of links', border = NA, xaxt = 'n', ylim = c (0, max))
# 	# draw_lines_and_NAs (sapply (functions.matrix, as.numeric), x = bp[1,], colors = unlist (chosen_colors [names (links)], use.names = FALSE),
# 	# 					range = c (0, max))
# 	axis (1, at = bp[1,seq (1, length (bp[1,]), by = 10)] , labels = colnames (functions.matrix) [seq (1, ncol (functions.matrix), by = 10)], las = 2, cex.axis = 0.7)
# 	# axis (1, at = bp + co, labels = colnames (functions.matrix) [seq (1, ncol (functions.matrix), by = 10)], las = 2)
# 	mtext (paste ("Function attention - ", name, sep = ''))
# }
# 
# cumulative_attention_file <- file ('data/cumulative_attention.dat', 'wb')
# save (file = cumulative_attention_file, twitter, reddit, wikilinks)
# close (cumulative_attention_file)