# The FastDM-Plot Package 

This is a collection of R scripts that can be used to compose input for the fast-dm software (http://www.psychologie.uni-heidelberg.de/ae/meth/fast-dm/#fd) and plot the results. fast-dm can be used to define a diffusion model for reaction times and accuracy data.
One case of application could be the visualization of the model fit or a plot of the actual distribution of the emprirical or model values.


# How to get started 

To install the package 
Make sure you have 'httr' and 'curl' packages installed
Type the following to directly in the R console to install the package: 
```
install.packages("devtools")
devtools::install_github("s4rify/fastdm-plot", subdir = "FastDMPlot")
```


# How the package works

The package is written for Windows systems only at the moment. Since Voss et al. also included executables for Linux systems, the scripts will also run on Linux with a few changes in the code.
The package assumes a folder structure like given in the git repository.
- one folder which contains the scripts and functions
- one folder which contains the rawdata as .csv files
- one folder which contains the FastDM executables for some processing steps (those are called *.dummy at the moment, replace those by the actual *.exe files which you can download on the above mentioned website)

# A small example

Assume that you have a csv file from a reaction time experiment and you want to use the diffusion model to analyze the reaction time distribution. 
You can use this package to assess the model fit and visualize cumulative density function plots and probability function plots.
You can use the 
```
composeFastDMinput.R
```
script to convert your csv file into the correct input format for the fast-dm executables.
After using the fast-dm software by Voss et al. you will end up with a log file which contains all the estimated and fixed model parameter for a subject. This log file can be handed to the 

```
composeCDFparameters.R
```
script which takes a log file as input and returns composes the system call to plot-cdf.exe. This is especially helpful if you want to compute the CDF values for several subjects at once.
Plot-cdf.exe will return the CDF values which can be plotted with

```
calculateCDFparamsForPlot.R
```
This script returns objects that contain model and empirical values that can be plotted together with
```
composeCDFplot.R
```

You can see an example of the combined CDF plots on the poster (Poster.pdf).

There is an additional feature which produces a quantile probability function plot (after Ratcliff and Tuerlinckx et al.). Scripts and functions for these plots can be found in the "additionalScripts" folder in the git repository. This is not part of the R package. The functions and scripts are very well documented.

# Copyright and License

This package is licensed under the Creative Commons CC0 License (https://creativecommons.org/publicdomain/zero/1.0/).
