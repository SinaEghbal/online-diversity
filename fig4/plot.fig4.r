#!/usr/bin/Rscript

ds <- c ('reddit', 'twitter', 'wikilinks')

for (ds_name in ds) {
	fn_list <- load (sprintf ('data/%s_cohort_analysis.dat', ds_name))

	max_hhi_x <- max (HHI_clusters, na.rm = TRUE)
	max_links_x <- max (links_clusters, na.rm = TRUE)
	max_surivival_x <- max (survival_clusters, na.rm = TRUE)
	max_active_species <- max (active_species_clusters, na.rm = TRUE)
	max_surivival_x_2 <- max (survival_clusters2, na.rm = TRUE)

	colors <- unique (rainbow (nrow (HHI_clusters)))

	pdf (sprintf ('fig4_a_%s.pdf',ds_name))
	for (row in rownames (HHI_clusters)) {
		plot (HHI_clusters [row, ], col = colors [which (rownames (HHI_clusters) == row)]
			  ,xlab = '', ylab = '', xaxt = 'n', ylim = c (0, max_hhi_x), type = 'l')
		par (new = TRUE)
	}
	axis (side = 1, at = seq (1, ncol (HHI_clusters), by = 4),
		  labels = colnames (HHI_clusters)[seq (1, ncol (HHI_clusters), by = 4)], las = 2, cex.axis = 0.7)
	
	legend("topleft", legend = rownames (HHI_clusters), lty= rep (nrow (HHI_clusters), 1),
		   col = colors [seq (1, nrow (HHI_clusters))])
	mtext (side = 2, text = 'HHI', line = 2.0)
	mtext (side = 1, text = 'Age (month)', line = 3.5)
	mtext (sprintf ('HHI over age - %s', ds_name))
	
	par (new = FALSE)
	dev.off ()

	pdf (sprintf ('fig4_b_%s.pdf', ds_name))
	for (row in rownames (links_clusters)) {
		plot (links_clusters [row, ], col = colors [which (rownames (links_clusters) == row)]
			  ,xlab = '', ylab = '', xaxt = 'n', ylim = c (0, max_links_x), type = 'l')
		par (new = TRUE)
	}
	axis (side = 1, at = seq (1, ncol (links_clusters), by = 4),
		  labels = colnames (links_clusters)[seq (1, ncol (links_clusters), by = 4)], las = 2, cex.axis = 0.7)
	
	legend("topleft", legend = rownames (links_clusters), lty= rep (nrow (links_clusters), 1),
		   col = colors [seq (1, nrow (links_clusters))])
	mtext (side = 2, text = 'Links', line = 2.0)
	mtext (side = 1, text = 'Age (month)', line = 3.5)
	mtext (sprintf ('Links over age - %s', ds_name))
	par (new = FALSE)
	dev.off ()

	# Here we have looked further and created a death date for the species based on the last time they were linked
	pdf (sprintf ('fig4_b(1)_%s.pdf', ds_name))
	for (row in rownames (survival_clusters)) {
		plot (survival_clusters [row, ], col = colors [which (rownames (survival_clusters) == row)]
			  ,xlab = '', ylab = '', xaxt = 'n', ylim = c (0, max_surivival_x), type = 'l')
		par (new = TRUE)
	}
	axis (side = 1, at = seq (1, ncol (survival_clusters), by = 4),
		  labels = colnames (survival_clusters)[seq (1, ncol (survival_clusters), by = 4)], las = 2, cex.axis = 0.7)
	
	legend("topleft", legend = rownames (survival_clusters), lty= rep (nrow (survival_clusters), 1),
		   col = colors [seq (1, nrow (survival_clusters))])
	mtext (side = 2, text = 'Survival rate', line = 2.0)
	mtext (side = 1, text = 'Age (month)', line = 3.5)
	mtext (sprintf ('Survival over age - %s', ds_name))
	par (new = FALSE)
	dev.off ()

	pdf (sprintf ('fig4_active_species_%s.pdf', ds_name))
	for (row in rownames (active_species_clusters)) {
		plot (active_species_clusters [row, ], col = colors [which (rownames (active_species_clusters) == row)],
			  xlab = '', ylab = '', xaxt = 'n', ylim = c (0, max_active_species), type = 'l')
		par (new = TRUE)
	}
	
	axis (side = 1, at = seq (1, ncol (active_species_clusters), by = 4),
		  labels = colnames (active_species_clusters)[seq (1, ncol (active_species_clusters), by = 4)], las = 2, cex.axis = 0.7)
	
	legend("topleft", legend = rownames (active_species_clusters), lty= rep (nrow (active_species_clusters), 1),
		   col = colors [seq (1, nrow (active_species_clusters))])
	mtext (side = 2, text = 'Active species', line = 2.0)
	mtext (side = 1, text = 'Age (month)', line = 3.5)
	mtext (sprintf ('Active species over age - %s', ds_name))
	par (new = FALSE)
	dev.off ()

	# No death date assumed. Based on current activities 
	pdf (sprintf ('fig4_b(2)_%s.pdf',ds_name))
	for (row in rownames (survival_clusters2)) {
		plot (survival_clusters2 [row, ], col = colors [which (rownames (survival_clusters2) == row)]
			  ,xlab = '', ylab = '', xaxt = 'n', ylim = c (0, max_surivival_x_2), type = 'l')
		par (new = TRUE)
	}
	axis (side = 1, at = seq (1, ncol (survival_clusters2), by = 4),
		  labels = colnames (survival_clusters2)[seq (1, ncol (survival_clusters2), by = 4)], las = 2, cex.axis = 0.7)
	
	legend("topleft", legend = rownames (survival_clusters2), lty= rep (nrow (survival_clusters2), 1),
		   col = colors [seq (1, nrow (survival_clusters2))])
	mtext (side = 2, text = 'Survival rate (V2.0)', line = 2.0)
	mtext (side = 1, text = 'Age (month)', line = 3.5)
	mtext (sprintf ('Survival (V2.0 - active/domains born in timespan) over age - %s', ds_name))
	par (new = FALSE)
	
	dev.off ()
	rm (list = fn_list)
}