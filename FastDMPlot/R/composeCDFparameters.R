composeCDFparameters <- function(z, t, d, light=TRUE){
  # Composes the parameters for the plot-cdf.exe call.
  # The .log file in the file system is used to compose the
  # system call to plot-cdf.exe. This program needs all the
  # estimated and set parameters for the creation of a .lst
  # file for each subject. This .lst file contains all the
  # cdf parameters for correct and false responses.
  #
  # Args:
  #   Optional model parameters used to create the .lst files:
  #   z: The decision bias. Shoould be set to 0.5 if the responses
  #      need the same amount of information.
  #   t: The motor constant t0. This is a linear additive
  #      that can be used to shift the model parameters.
  #   d: Differences in speed of response execution. Should be left
  #      free if both answers are given in the same manner.
  #
  # Returns:
  #   The list which contains the cdf parameters for every subject.

  if(light){
    fn <- "/light.log"

  } else {
    fn <- "/nolight.log"
  }
  cdfparams.new <- paste( fn, sep="")

  path <- "./FastDMSoftware"
  estimatedParams.diffmodel <- read.table(paste(path, cdfparams.new, sep=""), header = TRUE)

  # decision bias
  if(missing(z)){
    z.param <- 0.5
  } else {
    z.param <- z
  }
  # motor component of response time
  if(missing(t)){
    t.param <- estimatedParams.diffmodel$t0
  } else {
    t.param <- t
  }
  # differences in speed of response execution
  if(missing(d)){
    d.param <- estimatedParams.diffmodel$d
  } else {
    d.param <- d
  }

  # those are always estimated by fast-dm.exe
  # threshold separation
  a.param <- estimatedParams.diffmodel$a
  # drift rate
  v.param <- estimatedParams.diffmodel$v

  # create vector
  outputname <- vector(mode="character", length = length(a.param))


  for (i in seq_along(1:dim(estimatedParams.diffmodel)[1])){
    outputname[i] <- paste(path, "/", i, ".lst", sep = "")
  }

  input <- paste(file.path(path, "plot-cdf.exe"),
                 "-a", a.param ,
                 "-z", z.param,
                 "-v", v.param,
                 "-t", t.param,
                 "-d", d.param,
                 "-o", outputname)

  # system call with parameters. output is in wd
  for (comm in input){
    system(comm, invisible = FALSE, intern = TRUE, wait = TRUE)
  }

  lst.list <- list()
  for (r in seq_along(1:dim(estimatedParams.diffmodel)[1])){
    lst.list[[r]] <- read.table(paste(path,"/" ,r, ".lst" , sep=""), header = TRUE)
  }

  return(lst.list)
}
