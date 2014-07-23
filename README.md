# How to use

This repo contains the tools I use for data analysis and making plots in R of weighted ensemble simulations.

The  file *WE_utils.R* contains three R functions that

1. extract data from an hdf5 file
2. make a weighted histogram of theta data
3. set up defaults for plotting that data with ggplot2

If you have R installed on your system, you should be able to download the files in this repo and run the *makehist_runfile.R* included in the *example* directory by entering `source("makehist_runfile.R")` in the R prompt.  You may have to install some things, since these functions make use of the following R packages:

- rhdf5
- plotrix
- ggplot2
- grid
- scales
- matlab
- reshape
	
Adding packages is easy, though -- for example, to add the ggplot2 package, just type `install.packages("ggplot2")` in the R prompt and it should work.

The *makehist_runfile.R* file should run using the provided weighted ensemble and brute-force hdf5 files.  The Brute-force data is in fact a weighted ensemble simulation of only one iteration, but with 100 sub time points recorded in that iteration.  To use these functions on your own data, you may have to change the iteration and/or other parameters appropriately.
