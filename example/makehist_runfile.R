#!/usr/bin/Rscript

library(ggplot2)

# parameters for data extraction and plot naming
outname <- "example"
binwidth <- 1
we_iter <- 100
bf_iter <- 1

# my data tools
source("../WE_utils.R")

# load WE data
we_data1 <- iter_data("we_example.h5", iter=we_iter)
wehist1 <- weighted_hist(we_data1$pcoord, we_data1$weight, binwidth=binwidth, int_data=TRUE)

# load BF data
bf_data1 <- iter_data("bf_example.h5", iter=1)
bfhist1 <- weighted_hist(bf_data1$pcoord, bf_data1$weight, binwidth=binwidth, int_data=TRUE)

# merge data
wehist1$type <- 'WE'
bfhist1$type <- 'BF'
histcompare <- rbind(wehist1, bfhist1)


# basic histogram plot object
h <- ggplot() + 
	geom_point(
		data=histcompare,
		aes(
			x=pcoord,
			y=probability,
			colour=type,
			shape=type
		),
	alpha=0.8
	) +
    geom_segment(
    	aes(
    		x=min(histcompare$pcoord),
    		xend=max(histcompare$pcoord),
    		y=1/172,
    		yend=1/172
    	)
    )


# pretty normal histogram plot
hfancy <- h +
	theme_publication() + 
	theme(
		legend.position = c(0.2,0.5)
	) + 
	ggtitle("Equivalent to 172 BF Runs") +
	ylab("Probability") + 
	xlab("Dimers on Membrane") + 
	scale_x_continuous(
		breaks=seq(0,250,50),
		limits = c(0,250)
	) +
	geom_text(
		aes(
			min(histcompare$pcoord)*2/3,
			min(bfhist1$probability)*3/2,
			label="BF Limit"
		),
		family="Times",
		size=4
	) +
	scale_shape_manual(values=c(15,16,17))
	

# save pretty normal hist to pdf
fname_nolog <- paste(outname, "iter", we_iter, "binwidth", binwidth,"nolog.pdf", sep="_")
ggsave(hfancy, file=fname_nolog, width=4, height=3.25, units = "in")


# pretty log-scaled histogram plot
hfancylog <- hfancy + 
	theme(legend.position = c(0.2,0.2)) +
	scale_y_continuous(
		trans = 'log10',
		breaks = logspace(-8,0,5),
		labels = trans_format('log10', math_format(10^.x)),
		limits = c(1e-8,1)
	)


# save pretty log-hist to pdf
fname_log <- paste(outname, "iter", we_iter, "binwidth", binwidth,"log.pdf", sep="_")
ggsave(hfancylog, file=fname_log, width=4, height=3.25, units = "in")
