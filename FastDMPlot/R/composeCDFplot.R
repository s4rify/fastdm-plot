composeCDFplot <- function(subject.to.plot,light=TRUE, correct.responses = TRUE, clearplots=FALSE){
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

  if(light){
    cond <- "Light"
    if(correct.responses){
      dataset.model <- RTcdf_corr.model
      dataset.emp.x <- RT_corrLIGHT
      dataset.emp.y <- empirical_cdf_corrLIGHT
    } else {
      dataset.model <- RTcdf_incorr.model
      dataset.emp.x <- RT_incorrLIGHT
      dataset.emp.y <- empirical_cdf_incorrLIGHT
    }
  } else{
    cond <- "Dark"
    if(correct.responses){
      dataset.model <- RTcdf_corr.model
      dataset.emp.x <- RT_corrDARK
      dataset.emp.y <- empirical_cdf_corrDARK
    } else {
      dataset.model <- RTcdf_incorr.model
      dataset.emp.x <- RT_incorrDARK
      dataset.emp.y <- empirical_cdf_incorrDARK
    }
  }
  n <- subject.to.plot



  # clear all plots if there are any open and sets
  # subdivision of plots back to default
  if(dev.size()[1] > 0 && clearplots){
    dev.off()
  #  par(mfrow = c(1,1))
    cat("Cleared previous plots and reset plot prefs.\n")
  }

  correct.rate <- length(dat[[n]]$reactionTime_PT[dat[[n]]$correct==1]) / dim(dat[[n]])[1]

  model.color <- "lightseagreen"
  emp.color <- "magenta4"
  pch=16 # small solid circle
  par( mar=c(5.1,5.1,4.1,4.1))
  plot(dataset.model[[n]],
       col = model.color,
       cex.main = 2.8,
       cex.axis = 2,
       cex.lab = 2,
       bty = 'n',
       type="p",
       pch = pch,
       xlab="Reaction Time [s]",
       ylab="P",
       xlim=c(min(dataset.model[[n]]), 2),
       main=paste("Light, Incorrect Responses")
       #main=paste("Subject",n, "(Correctness:", ceiling(correct.rate*1000)/10, "%)")
       )
  points(x = dataset.emp.x[[n]],
         y = dataset.emp.y[[n]](dataset.emp.x[[n]]),
         pch = pch,
         col = emp.color)
  legend("bottomright",
         legend=c("Model    ", "Empirical    ") ,
        # lty=1,
         col=c(model.color, emp.color),
         #bty="o",
         lwd=2,
         cex = 1.3,
         text.col = "black",
         text.font = 2 )
 # text(1.7, 0.3, paste("Condition:", cond, "\n Correct responses:", correct.responses), cex = 2)


}
