composePDFplot <- function(subject.to.plot, clearplots=FALSE){
  # Composes the plot for the Cumulative Density Function values of
  # the Diffusion Model and the empirical values.
  #
  # Args:
  #   subject.to.plot: The subject for which the plot should be created.
  #                    This is a mandatory argument.
  #   boolean clearplot: This determines whether all plots should be cleared
  #                      from the global env. and whether plot preferences
  #                      are reset to default.
  # Returns:
  #   No return value, a plot is being shown.
  
  
  # clear all plots if there are any open and sets
  # subdivision of plots back to default
  n <- subject.to.plot
  if(dev.size()[1] > 0 && clearplots){
    dev.off()
    par(mfrow = c(1,1))
    cat("Cleared previous plots and reset plot prefs.\n")
  }
  
  correct.rate <- length(data[[n]]$reactionTime_PT[data[[n]]$correct==1]) / dim(data[[n]])[1]
  
  model.color <- "lightseagreen"
  emp.color <- "magenta4"
  pch=16 # small solid circle
  plot(pdfvalues.model[[n]], 
       col = model.color, 
       type="p",
       pch = pch,
       xlab="Reaction Time [s]", 
       ylab="P",
       xlim=c(min(pdfvalues.model[[n]]), 3), 
       main=paste("Subject",n, "(Correctness:", ceiling(correct.rate*1000)/10, "%)")
  )

  points(x = RT_corr[[n]], 
         y = empirical_pdf_corr[[n]](RT_corr[[n]]), 
         pch = pch, 
         col = emp.color)
  legend("bottomright", 
         legend=c("Model", "Empirical") , 
         lty=1, 
         col=c(model.color, emp.color), 
         bty="n", 
         text.col = "black")
  
}