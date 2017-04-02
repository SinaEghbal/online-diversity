fn_list <- load (sprintf ('data/ds_properties.dat'))

pdf ('links.pdf')
max_sum <- max (sapply (sum, as.numeric), na.rm = TRUE)
plot (unlist (sum ['reddit',]), col = 'red', xaxt = 'n', ylim = c (0, max_sum), ylab = '# of links',
	  main = 'Links by time', xlab = 'Date')
par (new = TRUE)
plot (unlist (sum ['wikilinks',]), col = 'blue', xaxt = 'n', ylim = c (0, max_sum), ylab = '', xlab = '')
par (new = TRUE)
plot (unlist (sum ['twitter',]), col = 'green', xaxt = 'n', ylim = c (0, max_sum), ylab = '', xlab = '')
par (new = TRUE)
axis (side = 1, at = seq (1, length (sum), by = 8), labels = colnames (sum)[seq (1, length (sum), by = 8)], las = 2, cex.axis = 0.7)
legend("topleft",
	   legend=c ("Reddit", "Wiki", "Twitter"), lty= c (1,1,1),
	   col = c ("red", "blue", 'green'))
dev.off ()

pdf ('active_species.pdf')
max_active <- max (sapply (active, as.numeric), na.rm = TRUE)
plot (unlist (active ['reddit', ]), col = 'red', xaxt = 'n', ylim = c (0, max_active), ylab = 'active species',
	  main = 'Active species/time', xlab = 'Date')
par (new = TRUE)
plot (unlist (active ['wikilinks', ]), col = 'blue', xaxt = 'n', ylim = c (0, max_active), ylab = '', xlab = '')
par (new = TRUE)
plot (unlist (active ['twitter', ]), col = 'green', xaxt = 'n', ylim = c (0, max_active), ylab = '', xlab = '')
par (new = TRUE)
axis (side = 1, at = seq (1, length (active), by = 8), labels = colnames (active)[seq (1, length (active), by = 8)], las = 2, cex.axis = 0.7)
legend("topleft",
	   legend=c ("Reddit", "Wiki", "Twitter"), lty= c (1,1,1),
	   col = c ("red", "blue", 'green'))
dev.off ()

pdf ('uniqueness.pdf')
max_uniqueness <- max (sapply (uniqueness, as.numeric), na.rm = TRUE)
plot (unlist (uniqueness ['reddit', ]), col = 'red', xaxt = 'n', ylim = c (0, max_uniqueness), ylab = 'Link uniqueness',
	main = 'Link Uniqueness', xlab = 'Date')
par (new = TRUE)
plot (unlist (uniqueness ['wikilinks', ]), col = 'blue', xaxt = 'n', ylim = c (0, max_uniqueness), ylab = '', xlab = '')
par (new = TRUE)
plot (unlist (uniqueness ['twitter', ]), col = 'green', xaxt = 'n', ylim = c (0, max_uniqueness), ylab = '', xlab = '')
par (new = TRUE)
axis (side = 1, at = seq (1, length (uniqueness), by = 8), labels = colnames (uniqueness)[seq (1, length (uniqueness), by = 8)], las = 2, cex.axis = 0.7)
legend("topleft", legend=c ("Reddit", "Wiki", "Twitter"), lty= c (1,1,1),
	   col = c ("red", "blue", 'green'))
dev.off ()