# Main Script for DiffModel fitting and calculation

# The workflow of the diffusion model process is as follows:
# 1 fast-dm.exe <- needs  : .csv files, .ctl file,.dll file (library)
#               -> returns: a .log file which contains all the estimated
#                           parameters per subject
# 2 plot-cdf.exe <- needs  : the .log file
#                -> returns: an .lst file for every subject
# 3 my Rfunctions<- need   : the .log files, the.lst files, optional: parameters
#                 -> return: a plot of the model fit


SUBJ <- 1

# This function expects a working directory with a DiffModel folder in it.
# Here, the .lst files from plot-cdf.exe are created, imported
# from the file system and returned as a list
# The parameters are the decision bias z and the
# motor constant t0, that are used by plot-cdf.exe
# for creating th cdf parameters per subject
lst.light <- composeCDFparameters(z=0.5, t=0.65, light = TRUE)
lst.dark  <- composeCDFparameters(z=0.5, t=0.4, light = FALSE)

# This function calculates the necessary
# empirical cdf parameters from the raw data (data=data)
# and prepares the model parameters, so that they are
# in the same form, divided by correct and false
# answers.
# It does not return all the data objects, but
# puts them in the Global Environment
calculateCDFParamsForPlot(model.cdf = lst.light)
calculateCDFParamsForPlot(model.cdf = lst.dark)

# This function uses the calculated parameters for
# the empirical and the model CDFunction and creates
# one plot per suject. It clears every existing plot
# from the environment if clearPlots is TRUE
composeCDFplot(SUBJ, light=FALSE, correct.responses = FALSE, clearplots = FALSE)
composeCDFplot(SUBJ, light=TRUE, correct.responses = FALSE, clearplots = FALSE)



# Use this loop to get an overview of all subjects
# in one plot, this is mainly useful for adjustments
# of the parameters.
par(mfrow=c(3,4))
for (i in c(1,3,4,5,6,7,9,12,13,14,15,18)){
  composeCDFplot(i, correct.responses = TRUE, clearplots = FALSE)
}


# notes:
# the lst files for light and dark ARE indeed different
# but the plots are the same
