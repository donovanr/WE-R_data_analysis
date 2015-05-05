#!/usr/bin/Rscript

######################################################
# three functions i find useful for WE data analysis #
#     iter_data: extract data from a WE h5 file      #
#     weighted_hist: make a histogam from that data  #
#     theme_publication: defaults for ggplo2 plots   #
######################################################


# get all the pcoords and weights from a desired iteration of a WE simulation
# args:
#     h5_file = the hdf5 file that WE wrote out for your simulation
#     iter = the iteration that you're interested in (defaluts to last iteration)
# output:
#     data frame containing pcoords and weights for each segment in the specified iteration
iter_data <- function(h5_file, iter=num_iters, pcoord_dim=1){
	
	# necessary library
	require(rhdf5)

	# find number of iterations in simulation
	summary_data <- h5read(h5_file, "/summary", bit64conversion='double')
	num_iters <- dim(summary_data)[1] - 2 # -2 for zero indexing and dummy extra iteration
	
	# set paths to data in h5 file: -1 is for 0 indexing
	pcoord_path <- sprintf("/iterations/iter_%08d/pcoord", iter)
	segindex_path <- sprintf("/iterations/iter_%08d/seg_index", iter)
	
	# extract pcoords
	pcoord_alldata <- h5read(h5_file, pcoord_path, bit64conversion='double')
	this_iter_pcoord_dims <- dim(pcoord_alldata)
	# added the data.frame wrapper to make 1-d pcoord stuff work -- need to test with 2-d
	pcoords <- data.frame(pcoord_alldata[ ,this_iter_pcoord_dims[2], ]) # grab last timepoint in iteration)
	
	# extract weights
	seg_alldata <- h5read(h5_file, segindex_path, bit64conversion='double')
	weights <- seg_alldata[ ,1]
	
	# make dataframe
	
	# this works for 1-d, need to test with 2-d (swapped array indices -- see commented if/else below)
	#iterdata <- data.frame(pcoord=pcoords[,pcoord_dim],weight=weights)
	
	if (pcoord_dim == 1) {
        iterdata <- data.frame(pcoord=pcoords[,pcoord_dim],weight=weights)
    } else {
	    iterdata <- data.frame(pcoord=pcoords[pcoord_dim,],weight=weights)
    } 
    return(iterdata)

}

coord_iter_data <- function(h5_file, coord_index, iter=num_iters){
	
	# necessary library
	require(rhdf5)

	# find number of iterations in simulation
	summary_data <- h5read(h5_file, "/summary", bit64conversion='double')
	num_iters <- dim(summary_data)[1] - 2 # -2 for zero indexing and dummy extra iteration
	
	# set paths to data in h5 file: -1 is for 0 indexing
	coord_path <- sprintf("/iterations/iter_%08d/auxdata/coord", iter)
	segindex_path <- sprintf("/iterations/iter_%08d/seg_index", iter)
	
	# extract coords
	coord_alldata <- h5read(h5_file, coord_path, bit64conversion='double')
	this_iter_coord_dims <- dim(coord_alldata)
	# added the data.frame wrapper to make 1-d coord stuff work -- need to test with 2-d
	coords <- data.frame(coord_alldata[1,this_iter_coord_dims[2],coord_index,]) # grab last timepoint in iteration)
	
	# extract weights
	seg_alldata <- h5read(h5_file, segindex_path, bit64conversion='double')
	weights <- seg_alldata[ ,1]
	
	# make dataframe
	
	# this works for 1-d, need to test with 2-d (swapped array indices -- see commented if/else below)
	#iterdata <- data.frame(coord=coords[,coord_dim],weight=weights)
	
    iterdata <- data.frame(coord=coords[,1],weight=weights)

    return(iterdata)

}

# generate histogram data from wighted inputs
weighted_hist <- function(my_list, weights, binwidth=1, int_data=FALSE){
	
	require(plotrix)
	
	# construct bin edges
	my_pcoord_max <- max(my_list)
	my_pcoord_min <- min(my_list)
	# 2*binwidth added at end for extra empty bin and to make extra sure we don't miss anything 
	my_bin_min <- as.integer(binwidth * floor(my_pcoord_min/binwidth)) - 2*binwidth
	my_bin_max <- as.integer(binwidth * ceiling(my_pcoord_max/binwidth)) + 2*binwidth
	my_bin_edges <- seq(my_bin_min,my_bin_max,binwidth)
	
	# if the pcoord data take integer values, center bins on ints
	# not sure how useful this is if binwdth != 1
	if(int_data) {my_bin_edges <- my_bin_edges - 0.5}
	
	# create weighted histogram
	wh <- weighted.hist(my_list, weights, breaks=my_bin_edges, plot=FALSE)
	
	# convert to data frame and clean out bins with zero weight
	# divide by binwidth to maintain normalization of PDF
	df <- data.frame(pcoord = wh$mids, probability = wh$density/binwidth)
	df <- df[df[,2]!=0,]
	return(df)
}

# generate un-normlized histogram data from wighted inputs
weighted_hist_nonorm <- function(my_list, weights, binwidth=1, int_data=FALSE){
	
	require(plotrix)
	
	# construct bin edges
	my_pcoord_max <- max(my_list)
	my_pcoord_min <- min(my_list)
	# 2*binwidth added at end for extra empty bin and to make extra sure we don't miss anything 
	my_bin_min <- as.integer(binwidth * floor(my_pcoord_min/binwidth)) - 2*binwidth
	my_bin_max <- as.integer(binwidth * ceiling(my_pcoord_max/binwidth)) + 2*binwidth
	my_bin_edges <- seq(my_bin_min,my_bin_max,binwidth)
	
	# if the pcoord data take integer values, center bins on ints
	# not sure how useful this is if binwdth != 1
	if(int_data) {my_bin_edges <- my_bin_edges - 0.5}
	
	# create weighted histogram
	wh <- weighted.hist(my_list, weights, breaks=my_bin_edges, plot=FALSE)
	
	# convert to data frame and clean out bins with zero weight
	# divide by binwidth to maintain normalization of PDF
	df <- data.frame(pcoord = wh$mids, probability = wh$density/binwidth)
	df <- df[df[,2]!=0,]
	return(df)
}

# generate un-normlized histogram data from wighted inputs
weighted_hist_nonorm_nodrop <- function(my_list, weights, binwidth=1, int_data=FALSE){
	
	require(plotrix)
	
	# construct bin edges
	my_pcoord_max <- max(my_list)
	my_pcoord_min <- min(my_list)
	# 2*binwidth added at end for extra empty bin and to make extra sure we don't miss anything 
	my_bin_min <- as.integer(binwidth * floor(my_pcoord_min/binwidth)) - 2*binwidth
	my_bin_max <- as.integer(binwidth * ceiling(my_pcoord_max/binwidth)) + 2*binwidth
	my_bin_edges <- seq(my_bin_min,my_bin_max,binwidth)
	
	# if the pcoord data take integer values, center bins on ints
	# not sure how useful this is if binwdth != 1
	if(int_data) {my_bin_edges <- my_bin_edges - 0.5}
	
	# create weighted histogram
	wh <- weighted.hist(my_list, weights, breaks=my_bin_edges, plot=FALSE)
	
	# convert to data frame and clean out bins with zero weight
	# divide by binwidth to maintain normalization of PDF
	df <- data.frame(pcoord = wh$mids, probability = wh$counts)
	return(df)
}


# defaults for the plots i like to make using ggplot2
# basically, white background, times font, bold titles
# save at 4in x 3in for good font size to plot size ration

theme_publication <- function() {
	
	require(ggplot2)
	require(grid)
	require(scales)
	require(matlab)
	require(reshape)
	
	theme_bw(
		base_size=12,
		base_family="Times"
	) + 
	theme(
		plot.title = element_text(face="bold", size=12),
		axis.title.x = element_text(face="bold"),
		axis.title.y = element_text(face="bold"),
		legend.title = element_blank(),
		legend.key = element_blank(),
		legend.text = element_text(size=8),
		legend.key.size = unit(4, "mm"),
		legend.background = element_rect(fill="gray95"),
		legend.margin = unit(1, "in")
	)
}

partitiondf <- function(dataframe, partitionlist, seed=NULL) {
    
    # dataframe will get partitioned into sets of difference rows.
    # partitionlist is a list of the different size partions you want,
    # ie partitionlist <- c(10,20,30) will give you three partions,
    # of size 10, 20, 30 respectively.
     
    if(sum(partitionlist) > nrow(dataframe)) 
    stop("Error: not enough data to partition")

    if (!is.null(seed)) set.seed(seed)
    
    thepartition <- vector("list", length(partitionlist))
    
    unsampled <- 1:nrow(dataframe)
    for (i in 1:length(partitionlist) ) {
        thepartition[[i]] <- sample(unsampled, partitionlist[i])
        unsampled <- unsampled[-thepartition[[i]]]
    }
    thepartition
}
