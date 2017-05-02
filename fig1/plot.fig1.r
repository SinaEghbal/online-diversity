#!/usr/bin/Rscript

fn_list <- load (sprintf ('data/ds_properties.dat'))

# Let's get rid of some of the abnormalities
# Manually, or anything beyond stdev*2?!

active ['twitter', '2011-12-15'] = NA
active ['twitter', '2011-10-15'] = NA
sum ['twitter', '2015-03-15'] = NA
sum ['twitter', '2011-09-15'] = NA

# Get rid of abnormal data
active ['wikilinks', colnames (active) < 2010] = NA
sum ['wikilinks', colnames (active) < 2010] = NA
uniqueness ['wikilinks', colnames (active) < 2010] = NA

# Let's fit the data to a quadratic curve
x_reddit <- seq (1, length (sum ['reddit', ]))# which (!is.na (sum ['reddit', ]))# which (!is.na (sum ['reddit',]))
fit_sum_reddit <- lm (unlist (sum ['reddit',], use.names = FALSE)~poly(x_reddit,2,raw=TRUE))
x_wiki <- seq (1, length (sum ['wikilinks', ])) #which (!is.na (sum ['wikilinks',]))
fit_sum_wikilinks <- lm (unlist (sum ['wikilinks',], use.names = FALSE)~poly(x_wiki,2,raw=TRUE))
x_twitter <- seq (1, length (sum ['twitter', ]))# which (!is.na (sum ['twitter',]))
fit_sum_twitter <- lm (unlist (sum ['twitter',], use.names = FALSE)~poly(x_twitter,2,raw=TRUE))

pdf ('fig1-b.pdf')
max_sum <- max (sapply (sum, as.numeric), na.rm = TRUE)
plot (unlist (sum ['reddit',]), col = 'red', xaxt = 'n', ylim = c (0, max_sum), ylab = '# of links',
	  main = 'Links by time', xlab = 'Date', type = 'l')
# print (length (x_reddit))
# print (length (predict (fit_sum_reddit, data.frame (x = x_reddit))))
points (x_reddit, predict (fit_sum_reddit, data.frame (x = x_reddit)), col = 'red', pch = '.')
par (new = TRUE)
plot (unlist (sum ['wikilinks',]), col = 'blue', xaxt = 'n', ylim = c (0, max_sum), ylab = '', xlab = '', type = 'l')
points (x_wiki, predict (fit_sum_wikilinks, data.frame (x = x_wiki)), col = 'blue', pch = '.')
par (new = TRUE)
plot (unlist (sum ['twitter',]), col = 'green', xaxt = 'n', ylim = c (0, max_sum), ylab = '', xlab = '', type = 'l')
points (x_twitter, predict (fit_sum_twitter, data.frame (x = x_twitter)), col = 'green', pch = '.')
par (new = TRUE)
axis (side = 1, at = seq (1, length (sum), by = 8), labels = colnames (sum)[seq (1, length (sum), by = 8)], las = 2, cex.axis = 0.7)
legend("topleft",
	   legend=c ("Reddit", "Wiki", "Twitter"), lty= c (1,1,1),
	   col = c ("red", "blue", 'green'))
dev.off ()


# Let's fit the data to a quadratic curve
x_reddit <- seq (1, length (active ['reddit', ]))# which (!is.na (sum ['reddit', ]))# which (!is.na (sum ['reddit',]))
fit_active_reddit <- lm (unlist (active ['reddit',], use.names = FALSE)~poly(x_reddit,2,raw=TRUE))
x_wiki <- seq (1, length (active ['wikilinks', ])) #which (!is.na (sum ['wikilinks',]))
fit_active_wikilinks <- lm (unlist (active ['wikilinks',], use.names = FALSE)~poly(x_wiki,2,raw=TRUE))
x_twitter <- seq (1, length (active ['twitter', ]))# which (!is.na (sum ['twitter',]))
fit_active_twitter <- lm (unlist (active ['twitter',], use.names = FALSE)~poly(x_twitter,2,raw=TRUE))

pdf ('active_species.pdf')
max_active <- max (sapply (active, as.numeric), na.rm = TRUE)
plot (unlist (active ['reddit', ]), col = 'red', xaxt = 'n', ylim = c (0, max_active), ylab = 'active species',
	  main = 'Active species/time', xlab = 'Date', type = 'l')
points (x_reddit, predict (fit_active_reddit, data.frame (x = x_reddit)), col = 'red', pch = '.')
par (new = TRUE)
plot (unlist (active ['wikilinks', ]), col = 'blue', xaxt = 'n', ylim = c (0, max_active), ylab = '', xlab = '', type = 'l')
points (x_wiki, predict (fit_active_wikilinks, data.frame (x = x_wiki)), col = 'blue', pch = '.')
par (new = TRUE)
plot (unlist (active ['twitter', ]), col = 'green', xaxt = 'n', ylim = c (0, max_active), ylab = '', xlab = '', type = 'l')
points (x_twitter, predict (fit_active_twitter, data.frame (x = x_twitter)), col = 'green', pch = '.')
par (new = TRUE)
axis (side = 1, at = seq (1, length (active), by = 8), labels = colnames (active)[seq (1, length (active), by = 8)], las = 2, cex.axis = 0.7)
legend("topleft",
	   legend=c ("Reddit", "Wiki", "Twitter"), lty= c (1,1,1),
	   col = c ("red", "blue", 'green'))
dev.off ()

# Let's fit the data to a quadratic curve
x_reddit <- seq (1, length (uniqueness ['reddit', ]))# which (!is.na (sum ['reddit', ]))# which (!is.na (sum ['reddit',]))
fit_uniqueness_reddit <- lm (unlist (uniqueness ['reddit',], use.names = FALSE)~poly(x_reddit,2,raw=TRUE))
x_wiki <- seq (1, length (uniqueness ['wikilinks', ])) #which (!is.na (sum ['wikilinks',]))
fit_uniqueness_wikilinks <- lm (unlist (uniqueness ['wikilinks',], use.names = FALSE)~poly(x_wiki,2,raw=TRUE))
x_twitter <- seq (1, length (uniqueness ['twitter', ]))# which (!is.na (sum ['twitter',]))
fit_uniqueness_twitter <- lm (unlist (uniqueness ['twitter',], use.names = FALSE)~poly(x_twitter,2,raw=TRUE))

pdf ('fig1-d.pdf')
max_uniqueness <- max (sapply (uniqueness, as.numeric), na.rm = TRUE)
plot (unlist (uniqueness ['reddit', ]), col = 'red', xaxt = 'n', ylim = c (0, max_uniqueness), ylab = 'Link uniqueness',
	main = 'Link Uniqueness (# of active domains/# of links)', xlab = 'Date', type = 'l')
points (x_reddit, predict (fit_uniqueness_reddit, data.frame (x = x_reddit)), col = 'red', pch = '.')

par (new = TRUE)
plot (unlist (uniqueness ['wikilinks', ]), col = 'blue', xaxt = 'n', ylim = c (0, max_uniqueness), ylab = '', xlab = '', type = 'l')
points (x_wiki, predict (fit_uniqueness_wikilinks, data.frame (x = x_wiki)), col = 'blue', pch = '.')

par (new = TRUE)
plot (unlist (uniqueness ['twitter', ]), col = 'green', xaxt = 'n', ylim = c (0, max_uniqueness), ylab = '', xlab = '', type = 'l')
points (x_twitter, predict (fit_uniqueness_twitter, data.frame (x = x_twitter)), col = 'green', pch = '.')

par (new = TRUE)
axis (side = 1, at = seq (1, length (uniqueness), by = 8), labels = colnames (uniqueness)[seq (1, length (uniqueness), by = 8)], las = 2, cex.axis = 0.7)
legend("topleft", legend=c ("Reddit", "Wiki", "Twitter"), lty= c (1,1,1),
	   col = c ("red", "blue", 'green'))
dev.off ()
rm (list = ls ())

load ('data/posts-tweets.dat')
posts [1, '2011-12'] = NA
posts [1, '2015-03'] = NA

x_reddit <- seq (1, length (posts ['reddit', ]))# which (!is.na (sum ['reddit', ]))# which (!is.na (sum ['reddit',]))
fit_posts_reddit <- lm (unlist (posts ['reddit',], use.names = FALSE)~poly(x_reddit,2,raw=TRUE))

x_twitter <- seq (1, length (posts ['twitter', ]))# which (!is.na (sum ['twitter',]))
fit_posts_twitter <- lm (unlist (posts ['twitter',], use.names = FALSE)~poly(x_twitter,2,raw=TRUE))

pdf ('fig1-a.pdf')
max_y <- max (posts, na.rm = TRUE)
plot (posts [1, ], type = 'l', ylab = '# of Posts-Tweets', xlab = '', xaxt = 'n', col = 'blue', ylim = c (0, max_y + 5))
points (x_reddit, predict (fit_posts_twitter, data.frame (x = x_reddit)), col = 'blue', pch = '.')
par (new = TRUE)
plot (posts [2, ], type = 'l', ylim = c (0, max_y + 5), col = 'red', ylab = '', yaxt = 'n', xaxt = 'n', xlab = '')
points (x_twitter, predict (fit_posts_reddit, data.frame (x = x_twitter)), col = 'red', pch = '.')

axis (1, at = seq (1,ncol (posts), by = 5), labels = colnames (posts)[seq (1,ncol (posts), by = 5)], las = 2, xlab = '', cex.axis = 0.7)
legend("topleft", legend = c ('Tweets', 'Reddit posts'), lty= c (1, 1),
	   col = c ('blue', 'red'))
mtext ('Posts - Tweets/Time')
par (new = FALSE)
dev.off ()