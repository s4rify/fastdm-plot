composePDFparameters <- function(){
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
  
  logparams.path <- "./diffusionmodel_cdf/allcorrSubj.log"
  path.to.exe <- "./pdfparams"
  estimatedParams.diffmodel <- read.table(logparams.path , header = TRUE)
  
  # decision bias
  z.param <- 0.5
  # motor component of response time
  t.param <- estimatedParams.diffmodel$t0
  # differences in speed of response execution
  d.param <- estimatedParams.diffmodel$d
  # threshold separation
  a.param <- estimatedParams.diffmodel$a
  # drift rate
  v.param <- estimatedParams.diffmodel$v
  
  # create vector
  outputname <- vector(mode="character", length = length(a))
  
  
  for (i in seq_along(1:dim(estimatedParams.diffmodel)[1])){
    outputname[i] <- paste("pdf", i, ".dat", sep = "")
  }
  
  input <- paste(file.path(path.to.exe, "plot-density.exe"),
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
  
  dat.list <- list()
  for (r in seq_along(1:dim(estimatedParams.diffmodel)[1])){
    dat.list[[r]] <- read.table(paste("./pdfparams/pdf" ,r, ".dat" , sep=""), header = TRUE)
  }
  
  
  ## empirical pdf values
  for (i in 1:18){
    
    empirical_pdf_corr[[i]]   <- density(RT_corr[[i]])
    empirical_pdf_incorr[[i]] <- density(RT_incorr[[i]])
  }
  # declare all objects global so that they are in the global environment
  assign("empirical_pdf_corr",       empirical_pdf_corr,       envir = .GlobalEnv)
  assign("empirical_pdf_incorr",     empirical_pdf_incorr,     envir = .GlobalEnv)

  return(dat.list)
}


