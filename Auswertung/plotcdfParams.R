# plots

colarray <- colors()
#colarray <- sample(colors(), length(params), replace = FALSE)

subjects <- c(1:18)

# close all plots
dev.off()
for ( j in seq_along(1:length(subjects))) {
  plot(cdf_params[[j]], type = 'l', main = "Cumulative Distribution Functions of all Subjects",
       xlab = "Reaction Times (x)", ylab = "Probability X <= x")
  par(new = TRUE, xaxt = "n", yaxt ="n", col = colarray[j], lwd=2)
  
}
# call legend only once because otherwise the legends will be overdrawn each time!
legend('topleft', legend=subjects , lty=1, col=colarray, bty="n", text.col = "black")

## interpretation of params: from Voss et al 2015 (p.12)
RT_incorr <- list()
RT_corr <- list()
RTcdf_incorr.model <- list()
RTcdf_corr.model <- list()
RTpdf_corr.model <- list()
RTpdf_incorr.model <- list()
empirical_cdf_corr <- list()
empirical_cdf_incorr <- list()
empirical_pdf_corr <- list()
empirical_pdf_incorr <- list()



for (s in seq_along(subjects)){
  # model CDF values for correct and false responses for every subject
  #RTcdf_incorr.model[[s]] <- cdf_params[[s]][which(cdf_params[[s]][1] <= 0), ]
  #RTcdf_corr.model[[s]] <-cdf_params[[s]][which(cdf_params[[s]][1] > 0 ), ]
  
  # empirical cdf values for correct and false response
  RT_corr[[s]] <- data[[s]]$reactionTime_PT[which(data[[s]]$correct == 1)]
  RT_incorr[[s]] <- data[[s]]$reactionTime_PT[which(data[[s]]$correct == 0)]
  # problem: ecdf returns a function (closure)
  #empirical_cdf_corr[[s]] <- ecdf(RT_corr[[s]])
  #empirical_cdf_incorr[[s]] <- ecdf(RT_incorr[[s]])
  
  # model PDF values for correct and false responses
  RTpdf_incorr.model[[s]] <- pdf_params[[s]][ ,3]
  RTpdf_corr.model[[s]] <- pdf_params[[s]][ ,2]
  # empirical PDF values for correct and false responses
  empirical_pdf_corr[[s]] <- density(RT_corr[[s]])
  empirical_pdf_incorr[[s]] <- density(RT_incorr[[s]])
  
}  


# clear all plots
dev.off()

par(mfrow=c(3,6))
for (p in seq_along(subjects)){
  
  
  plot(RTpdf_corr.model[[5]], col = "green", xlab="", ylab="", xlim =c(0,5))
  points(empirical_pdf_corr[[5]], col = "red")
  
  
}



