QP.plot <- function(dat, quantiles = seq.int(.1, .9, .2), subject.title,  ...) {
  # Prduces two Quantile Probability Function PLots (Ratcliff 2004).
  # In those plots, the quantiles of the  reaction time distribution
  # are plotted against probabilities for the responses. In one plot only the
  # correct responses are plotted, in the other plot only the
  # incorrect responses are plotted.
  #
  #
  # Args:
  #   dat: the preprocessed raw data from one subject
  #   quantiles: a vector determining which quantiles are used
  #   plot args: optional plot arguments for matplot
  #
  # Returns:
  #   probability: the probabilities for every response time
  #   quartiles: the quartiles of the distribtion
  
  # omit all missing values
  dat <- dat[complete.cases(dat), ]
  # make sure that there is a condition
  if (is.null(dat$condition)) { 
    dat$condition <- 1
    condition <- 1
  } else {
    condition <- unique(dat$condition)
  }
 
  # pre-allocate space for matrices
  freq  <- matrix(data=0, nrow=length(quantiles), ncol=2*length(condition))
  quant <- matrix(data=0, nrow=length(quantiles), ncol=2*length(condition))
  
  # In this loop, the reaction times corresponding to correct and incorrect are filled
  # into quant at alternating indices.
  # The odd indices contain the rts for correct responses, the even indices therefore contain
  # the rts for the  incorrect responses.
  for (j in 1:length(condition)) {
    idx.corr <- 2*j - 1
    
    quant[ , idx.corr  ] <- quantile(subset(dat$rt, (dat$condition == condition[j])&(dat$correct == 1)), probs = quantiles)
    quant[ , idx.corr+1] <- quantile(subset(dat$rt, (dat$condition == condition[j])&(dat$correct == 0)), probs = quantiles)    
    
    # temp contains the relative frequencies of correct and incorrect answers per condition.
    # Again, the odd indices contain the answers for correct
    # and the even indices the answers for incorrect (by inverting the temp index)
    temp <- mean(subset(dat$correct,  dat$condition == condition[j]))
    freq[ , idx.corr  ] <- rep(temp,   length(quantiles))
    freq[ , idx.corr+1] <- rep(1-temp, length(quantiles))
  }
  
  # this object contains all the optional parameters that are 
  # handed over to QP.plot and hands them over to matplot() when do.call() is used
  matplot.args <- list(...)
  
  if (!('ylab' %in% matplot.args)) {
    matplot.args$ylab <- 'RT [sec]'
  }
  if (!('xlab' %in% matplot.args)) {
    matplot.args$xlab <- 'Probability'
  }
  # make sure that two plots can be plotted side by side
  par(mfrow=1:2)
  
  # This function indexes the freq and quant matrix in the correct manner so
  # that with offset 1 a plot for correct answers is plotted
  # and with offset 2 a plot for incorrect answwers is plotted.
  # Additionally, a title can be set for each subplot
  composeQPFplot <- function(offset, title) {
    idx.all.incorr <- seq.int(offset,dim(freq)[2],2)
    order.incorr <- order( freq[1, idx.all.incorr] )
    # data
    matplot.args$x <- t(freq [ , idx.all.incorr[order.incorr] ])
    matplot.args$y <- t(quant[ , idx.all.incorr[order.incorr] ])
    # additional plot parameter
    matplot.args$main = paste(subject.title, title)
    matplot.args$type = 'b'
    matplot.args$ylim = c(0.5,1.8)
    matplot.args$bty="l"
    do.call(matplot, matplot.args)
  }
  
  # call the function so that the left subplot shows
  # all the content for incorrect and the right
  # subplot shows everything for correct
  composeQPFplot(2,"(incorrect)")
  composeQPFplot(1,"(correct)")
  
  # the return value of QP.plot in case the values are needed.
  return(list(probability =  freq, quantile = quant))
}

##
##
## Main Method

# which dataset
dat <- data[[15]]
dat$condition <- dat$fading_function
dat$rt <- dat$reactionTime_PT

QP.plot(dat, subject.title="Subject 15")

