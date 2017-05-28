.PHONY: all fig1 fig2 fig3 fig4

all:
	make fig1
	make fig2
	make fig3
	make fig4

fig1:
	cd fig1 && Rscript plot.fig1.r && cd reddit && Rscript plot.comments.and.uniqueness.r

fig2:
	cd fig2 && Rscript plot.fig2.r

fig3:
	cd fig3 && Rscript plot.fig3c.r

fig4:
	cd fig4	&& Rscript plot.fig4.r
