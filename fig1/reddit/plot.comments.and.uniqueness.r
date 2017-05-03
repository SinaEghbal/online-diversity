#!/usr/bin/Rscript
rm (list = ls ())
load ('data/reddit-volume.dat')

pdf ('comments.pdf')
max_y <- max (as.numeric (reddit['total.comments',]))
plot (reddit['total.comments',], type = 'l', xaxt = 'n', xlab = rep ('', ncol (reddit)), ylab = 'comments', col = 'blue',
	  ylim = c (0, max_y + 5))
par (new = TRUE)
plot (reddit ['comments.with.links', ], type = 'l', xaxt = 'n', yaxt = 'n', xlab = rep ('', ncol (reddit)), ylab = '', col = 'red', ylim = c (0, max_y + 5))

axis (1, at = seq (1,ncol (reddit), by = 5), labels = colnames (reddit)[seq (1,ncol (reddit), by = 5)], las = 2, xlab = '', cex.axis = 0.7)
legend("topleft", legend = c ('total comments', 'comments with links'), lty= c (1, 1),
	   col = c ('blue', 'red'))
mtext ('# of Comments/Time - reddit')
dev.off ()

pdf ('uniqueness.pdf')
par (new = FALSE)
max_y <- as.numeric (max (as.numeric (reddit ['uniqueness.of.links', ], na.rm = TRUE)))
# min_y <- as.numeric (min (as.numeric (reddit ['uniqueness.of.links', ], na.rm = TRUE)))
plot (reddit ['uniqueness.of.links', ], type = 'l', ylab = 'Links uniqueness (%)', xlab = '', xaxt = 'n', col = 'blue', ylim = c (0, max_y + 5))
par (new = TRUE)
# plot (reddit ['posts.with.links',], type = 'l', ylim = c (0, max_y + 5), col = 'red', ylab = '', yaxt = 'n', xaxt = 'n', xlab = '')
axis (1, at = seq (1,ncol (reddit), by = 5), labels = colnames (reddit)[seq (1,ncol (reddit), by = 5)], las = 2, xlab = '', cex.axis = 0.7)
legend("topleft", legend = c ('Uniquness (%)'), lty= c (1),
	   col = c ('blue'))
mtext ('Link uniqueness/Time - reddit')
par (new = FALSE)
dev.off ()


x_reddit <- seq (1, length (reddit ['posts.with.links', ]))# which (!is.na (links ['reddit', ]))# which (!is.na (links ['reddit',]))
fit_linked_posts_reddit <- lm (unlist (reddit ['posts.with.links', ], use.names = FALSE)~poly(x_reddit,2,raw=TRUE))

pdf ('reddit-posts-w-links.pdf')
columns <- colnames (reddit)
plot (reddit ['posts.with.links',], type = 'l', xaxt = 'n', col = 'red', ylab = "% of posts w links", xlab = '')
points (x_reddit, predict (fit_linked_posts_reddit, data.frame (x = x_reddit)), col = 'red', pch = '.')
axis (1, at = seq (1, length (columns), 5), labels = columns [seq (1, length (columns), 5)], las = 2, cex = 0.6)
legend("topleft", legend = c ('# of posts with links', 'prediction model'), lty= c (1, 3),
	   col = c ('red', 'red'))

mtext ('# of posts with links - reddit')
dev.off ()