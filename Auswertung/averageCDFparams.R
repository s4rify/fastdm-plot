# This function averages all cdf values across the subjects.
# After Schmitz, Voss (2012): "Decomposing Task-Switching Costs
# with the Diffusion Model". 
# In Appendix A they describe the averaging of the parameters
# across subjects
averageAcrossSubjects <- function(cdf_params, subjects){
  averaged_cdfs <- vector(mode = "numeric", length = length(subjects))
  
  # take each predicted cdf value for all subjects and divide by N
  for (i in seq_along(subjects)){
    averaged_cdfs[i] <- mean(cdf_params[[i]][ ,2])
  }
  return(averaged_cdfs)
}

avs <- averageAcrossSubjects(cdf_params = cdf_params, subjects = subjects)