#!/usr/bin/Rscript

fn_list <- load (sprintf ('data/ds_properties.dat'))

# Let's get rid of some of the abnormalities
# active ['twitter', '2011-12-15'] = NA
# active ['twitter', '2011-10-15'] = NA
# sum ['twitter', '2015-03-15'] = NA
# sum ['twitter', '2011-09-15'] = NA

# posts [1, '2011-12'] = NA
# posts [1, '2015-03'] = NA

pdf ('fig1-b.pdf')
max_sum <- max (sapply (sum, as.numeric), na.rm = TRUE)
plot (unlist (sum ['reddit',]), col = 'red', xaxt = 'n', ylim = c (0, max_sum), ylab = '# of links',
	  main = 'Links by time', xlab = 'Date', type = 'l')
par (new = TRUE)
plot (unlist (sum ['wikilinks',]), col = 'blue', xaxt = 'n', ylim = c (0, max_sum), ylab = '', xlab = '', type = 'l')
par (new = TRUE)
plot (unlist (sum ['twitter',]), col = 'green', xaxt = 'n', ylim = c (0, max_sum), ylab = '', xlab = '', type = 'l')
par (new = TRUE)
axis (side = 1, at = seq (1, length (sum), by = 8), labels = colnames (sum)[seq (1, length (sum), by = 8)], las = 2, cex.axis = 0.7)
legend("topleft",
	   legend=c ("Reddit", "Wiki", "Twitter"), lty= c (1,1,1),
	   col = c ("red", "blue", 'green'))
dev.off ()

pdf ('active_species.pdf')
max_active <- max (sapply (active, as.numeric), na.rm = TRUE)
plot (unlist (active ['reddit', ]), col = 'red', xaxt = 'n', ylim = c (0, max_active), ylab = 'active species',
	  main = 'Active species/time', xlab = 'Date', type = 'l')
par (new = TRUE)
plot (unlist (active ['wikilinks', ]), col = 'blue', xaxt = 'n', ylim = c (0, max_active), ylab = '', xlab = '', type = 'l')
par (new = TRUE)
plot (unlist (active ['twitter', ]), col = 'green', xaxt = 'n', ylim = c (0, max_active), ylab = '', xlab = '', type = 'l')
par (new = TRUE)
axis (side = 1, at = seq (1, length (active), by = 8), labels = colnames (active)[seq (1, length (active), by = 8)], las = 2, cex.axis = 0.7)
legend("topleft",
	   legend=c ("Reddit", "Wiki", "Twitter"), lty= c (1,1,1),
	   col = c ("red", "blue", 'green'))
dev.off ()

pdf ('fig1-d.pdf')
max_uniqueness <- max (sapply (uniqueness, as.numeric), na.rm = TRUE)
plot (unlist (uniqueness ['reddit', ]), col = 'red', xaxt = 'n', ylim = c (0, max_uniqueness), ylab = 'Link uniqueness',
	main = 'Link Uniqueness (# of active domains/# of links)', xlab = 'Date', type = 'l')
par (new = TRUE)
plot (unlist (uniqueness ['wikilinks', ]), col = 'blue', xaxt = 'n', ylim = c (0, max_uniqueness), ylab = '', xlab = '', type = 'l')
par (new = TRUE)
plot (unlist (uniqueness ['twitter', ]), col = 'green', xaxt = 'n', ylim = c (0, max_uniqueness), ylab = '', xlab = '', type = 'l')
par (new = TRUE)
axis (side = 1, at = seq (1, length (uniqueness), by = 8), labels = colnames (uniqueness)[seq (1, length (uniqueness), by = 8)], las = 2, cex.axis = 0.7)
legend("topleft", legend=c ("Reddit", "Wiki", "Twitter"), lty= c (1,1,1),
	   col = c ("red", "blue", 'green'))
dev.off ()


load ('data/posts-tweets.dat')
pdf ('fig1-a.pdf')
max_y <- max (posts, na.rm = TRUE)
plot (posts [1, ], type = 'l', ylab = '# of Posts-Tweets', xlab = '', xaxt = 'n', col = 'blue', ylim = c (0, max_y + 5))
par (new = TRUE)
plot (posts [2, ], type = 'l', ylim = c (0, max_y + 5), col = 'red', ylab = '', yaxt = 'n', xaxt = 'n', xlab = '')
axis (1, at = seq (1,ncol (posts), by = 5), labels = colnames (posts)[seq (1,ncol (posts), by = 5)], las = 2, xlab = '', cex.axis = 0.7)
legend("topleft", legend = c ('Tweets', 'Reddit posts'), lty= c (1, 1),
	   col = c ('blue', 'red'))
mtext ('Posts - Tweets/Time')
par (new = FALSE)
dev.off ()