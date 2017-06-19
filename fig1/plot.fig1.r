#!/usr/bin/Rscript

fn_list <- load (sprintf ('data/ds_properties.dat'))

# Let's get rid of some of the abnormalities
# Manually, or anything beyond stdev*2?!

domains ['twitter', '2011-12-15'] = NA
domains ['twitter', '2011-10-15'] = NA
links ['twitter', '2015-03-15'] = NA
links ['twitter', '2011-09-15'] = NA

# # Get rid of abnormal data
# domains ['wikilinks', colnames (domains) < 2010] = NA
# links ['wikilinks', colnames (domains) < 2010] = NA
# uniqueness ['wikilinks', colnames (domains) < 2010] = NA

# Let's fit the data to a quadratic curve
x_reddit <- seq (1, length (links ['reddit', ]))# which (!is.na (links ['reddit', ]))# which (!is.na (links ['reddit',]))
fit_links_reddit <- lm (unlist (links ['reddit',], use.names = FALSE)~poly(x_reddit,2,raw=TRUE))
# x_wiki <- seq (1, length (links ['wikilinks', ])) #which (!is.na (links ['wikilinks',]))
# fit_links_wikilinks <- lm (unlist (links ['wikilinks',], use.names = FALSE)~poly(x_wiki,2,raw=TRUE))
x_twitter <- seq (1, length (links ['twitter', ]))# which (!is.na (links ['twitter',]))
fit_links_twitter <- lm (unlist (links ['twitter',], use.names = FALSE)~poly(x_twitter,2,raw=TRUE))

pdf ('fig1-b.pdf')
max_links <- max (sapply (links, as.numeric), na.rm = TRUE)
plot (unlist (links ['reddit',]), col = 'red', xaxt = 'n', ylim = c (0, max_links), ylab = '# of links',
	  main = 'Links by time', xlab = 'Date', type = 'l')
# print (length (x_reddit))
# print (length (predict (fit_links_reddit, data.frame (x = x_reddit))))
points (x_reddit, predict (fit_links_reddit, data.frame (x = x_reddit)), col = 'red', pch = '.')
par (new = TRUE)
# plot (unlist (links ['wikilinks',]), col = 'blue', xaxt = 'n', ylim = c (0, max_links), ylab = '', xlab = '', type = 'l')
# points (x_wiki, predict (fit_links_wikilinks, data.frame (x = x_wiki)), col = 'blue', pch = '.')
# par (new = TRUE)
plot (unlist (links ['twitter',]), col = 'green', xaxt = 'n', ylim = c (0, max_links), ylab = '', xlab = '', type = 'l')
points (x_twitter, predict (fit_links_twitter, data.frame (x = x_twitter)), col = 'green', pch = '.')
par (new = TRUE)
axis (side = 1, at = seq (1, length (links), by = 8), labels = colnames (links)[seq (1, length (links), by = 8)], las = 2, cex.axis = 0.7)
legend("topleft",
	   legend=c ("Reddit", "Twitter"), lty= c (1,1,1),
	   col = c ("red", 'green'))
dev.off ()


# Let's fit the data to a quadratic curve
x_reddit <- seq (1, length (domains ['reddit', ]))# which (!is.na (links ['reddit', ]))# which (!is.na (links ['reddit',]))
fit_domains_reddit <- lm (unlist (domains ['reddit',], use.names = FALSE)~poly(x_reddit,2,raw=TRUE))
# x_wiki <- seq (1, length (domains ['wikilinks', ])) #which (!is.na (links ['wikilinks',]))
# fit_domains_wikilinks <- lm (unlist (domains ['wikilinks',], use.names = FALSE)~poly(x_wiki,2,raw=TRUE))
x_twitter <- seq (1, length (domains ['twitter', ]))# which (!is.na (links ['twitter',]))
fit_domains_twitter <- lm (unlist (domains ['twitter',], use.names = FALSE)~poly(x_twitter,2,raw=TRUE))

pdf ('fig1-c.pdf')
max_domains <- max (sapply (domains, as.numeric), na.rm = TRUE)
plot (unlist (domains ['reddit', ]), col = 'red', xaxt = 'n', ylim = c (0, max_domains), ylab = '# of domains',
	  main = 'Active domains species/time', xlab = 'Date', type = 'l')
points (x_reddit, predict (fit_domains_reddit, data.frame (x = x_reddit)), col = 'red', pch = '.')
par (new = TRUE)
# plot (unlist (domains ['wikilinks', ]), col = 'blue', xaxt = 'n', ylim = c (0, max_domains), ylab = '', xlab = '', type = 'l')
# points (x_wiki, predict (fit_domains_wikilinks, data.frame (x = x_wiki)), col = 'blue', pch = '.')
# par (new = TRUE)
plot (unlist (domains ['twitter', ]), col = 'green', xaxt = 'n', ylim = c (0, max_domains), ylab = '', xlab = '', type = 'l')
points (x_twitter, predict (fit_domains_twitter, data.frame (x = x_twitter)), col = 'green', pch = '.')
par (new = TRUE)
axis (side = 1, at = seq (1, length (domains), by = 8), labels = colnames (domains)[seq (1, length (domains), by = 8)], las = 2, cex.axis = 0.7)
legend("topleft",
	   legend=c ("Reddit", "Twitter"), lty= c (1,1,1),
	   col = c ("red", 'green'))
dev.off ()

# Let's fit the data to a quadratic curve
x_reddit <- seq (1, length (uniqueness ['reddit', ]))# which (!is.na (links ['reddit', ]))# which (!is.na (links ['reddit',]))
fit_uniqueness_reddit <- lm (unlist (uniqueness ['reddit',], use.names = FALSE)~poly(x_reddit,2,raw=TRUE))
# x_wiki <- seq (1, length (uniqueness ['wikilinks', ])) #which (!is.na (links ['wikilinks',]))
# fit_uniqueness_wikilinks <- lm (unlist (uniqueness ['wikilinks',], use.names = FALSE)~poly(x_wiki,2,raw=TRUE))
x_twitter <- seq (1, length (uniqueness ['twitter', ]))# which (!is.na (links ['twitter',]))
fit_uniqueness_twitter <- lm (unlist (uniqueness ['twitter',], use.names = FALSE)~poly(x_twitter,2,raw=TRUE))

pdf ('fig1-d.pdf')
max_uniqueness <- max (sapply (uniqueness, as.numeric), na.rm = TRUE)
plot (unlist (uniqueness ['reddit', ]), col = 'red', xaxt = 'n', ylim = c (0, max_uniqueness), ylab = 'Link uniqueness',
	main = 'Link Uniqueness (# of domains domains/# of links)', xlab = 'Date', type = 'l')
points (x_reddit, predict (fit_uniqueness_reddit, data.frame (x = x_reddit)), col = 'red', pch = '.')

# par (new = TRUE)
# plot (unlist (uniqueness ['wikilinks', ]), col = 'blue', xaxt = 'n', ylim = c (0, max_uniqueness), ylab = '', xlab = '', type = 'l')
# points (x_wiki, predict (fit_uniqueness_wikilinks, data.frame (x = x_wiki)), col = 'blue', pch = '.')

par (new = TRUE)
plot (unlist (uniqueness ['twitter', ]), col = 'green', xaxt = 'n', ylim = c (0, max_uniqueness), ylab = '', xlab = '', type = 'l')
points (x_twitter, predict (fit_uniqueness_twitter, data.frame (x = x_twitter)), col = 'green', pch = '.')

par (new = TRUE)
axis (side = 1, at = seq (1, length (uniqueness), by = 8), labels = colnames (uniqueness)[seq (1, length (uniqueness), by = 8)], las = 2, cex.axis = 0.7)
legend("topleft", legend=c ("Reddit", "Twitter"), lty= c (1,1,1),
	   col = c ("red", 'green'))
dev.off ()

load ('data/posts-tweets.dat')
posts [1, '2011-12'] = NA
posts [1, '2015-03'] = NA

x_reddit <- seq (1, length (posts ['reddit', ]))# which (!is.na (links ['reddit', ]))# which (!is.na (links ['reddit',]))
fit_posts_reddit <- lm (unlist (posts ['reddit',], use.names = FALSE)~poly(x_reddit,2,raw=TRUE))

x_twitter <- seq (1, length (posts ['twitter', ]))# which (!is.na (links ['twitter',]))
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

## and now for fig1-e: ratio of links to posts (ratio of linked posts)
posts <- as.data.frame(posts)

## now, posts seems to be shorter than links (because of Wikipedia having two more data points at the end). Align them
which_links <- format(as.Date(names(links)), "%Y-%m") %in% names(posts) 
## and compute the link ratio
link_ratio <- links[c('twitter', 'reddit'), which_links] / posts

pdf ('fig1-e.pdf')

matplot(t(link_ratio), type = "l", col = c('blue', 'red'), lty = c(1, 1), xlab = '', xaxt = 'n', ylab = "Linked percentage" )
axis (1, at = seq (1, ncol(link_ratio), by = 5), labels = colnames (link_ratio)[seq (1,ncol (link_ratio), by = 5)], las = 2, xlab = '', cex.axis = 0.7)
legend("topleft", legend = c ('Tweets', 'Reddit posts'), lty= c (1, 1),
       col = c ('blue', 'red'), bty = "n")
mtext ('Percentage of linked posts/tweets')

dev.off ()

