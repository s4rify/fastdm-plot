QP.plot <- function(dat, conditions = NULL, quantiles = seq.int(.1, .9, .2), subject.title,  ...) {
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
  if (is.null(conditions)) {
    if (is.null(dat$condition)) {
      dat$condition <- 1
      condition <- 1
    } else {
      condition <- unique(dat$condition)
    }
  } else {
    condition <- conditions
  }
  # Adjust condition to parameter: if light, take only
  # those rts and correct responses that have been recorded
  # when light was presented and vice versa.
  # if(light){
  #   condition <- condition[-1] # all but first value
  # } else {
  #   condition <- condition[1]  # only first value
  # }

  # pre-allocate space for matrices
  freq  <- matrix(data=0, nrow=length(quantiles), ncol=2*length(condition))
  quant <- matrix(data=0, nrow=length(quantiles), ncol=2*length(condition))


  # In this loop, the reaction times corresponding to correct and incorrect are filled
  # into quant at alternating indices.
  # The odd indices contain the rts for correct responses, the even indices contain
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

  # Set min and max values for both y-axes in the plots
  ymax <- max(quant, na.rm = TRUE)
  ymin <- min(quant, na.rm = TRUE)

  # this object contains all the optional parameters that are
  # handed over to QP.plot and hands them over to matplot() when do.call() is used
  matplot.args <- list(...)

  # make sure that two plots can be plotted side by side
  par(mfrow=1:2, mar=c(5.1,6,4.1,4.1), mgp = c(4, 1, 0), xpd = TRUE)

  # This function indexes the freq and quant matrix in the correct manner so
  # that with offset 1 a plot for correct answers is plotted
  # and with offset 2 a plot for incorrect answwers is plotted.
  # Additionally, a title can be set for each subplot
  composeQPFplot <- function(offset, title) {

    # create color ramp
    colfunc <- colorRampPalette(c( "turquoise3", "purple4"))

    # a sequence starting from 1 or 2 until length(freq)
    idx <- seq.int(offset,dim(freq)[2],2)

    # contains the ordered frequencies beginning at offset
    ordering <- order( freq[offset, idx] )

    # datapoints
    matplot.args$x <- t(freq [ , idx[ordering] ])
    matplot.args$y <- t(quant[ , idx[ordering] ])


    # additional plot parameter
    matplot.args$main <- paste(title)

    # color set 2
    matplot.args$col <- colfunc(5)
    # logarithmic y axis
    matplot.args$log <- "y"
    matplot.args$bty <-"l"
    matplot.args$pch <- 16
    matplot.args$lwd <- 2
    matplot.args$cex <- 2
    matplot.args$cex.main <- 1.5
    matplot.args$cex.axis <- 1.5
    matplot.args$cex.lab <- 1.5
    matplot.args$ylab <- 'Log( RT [sec] )'
    # horizontal y axis values
    matplot.args$las <- 1
    # Set ylim to make space for Label of condition
    matplot.args$ylim <- c(ymin-0.05, 1)
    if(offset == 1){
      matplot.args$xlim <- c(0.4, 0.7)
    }  else {
      matplot.args$xlim <- c(0.3, 0.6)
    }

    if (!('xlab' %in% matplot.args)) {
      matplot.args$xlab <- ifelse(offset==1,'Correctness Frequency', 'Incorrectness Frequency')
    }


     do.call(matplot, matplot.args)

     i <-1
     for (c in condition[ordering]){
       cond.text <- condition.to.label(c)
       text(freq[1, idx[ordering[i]]], ymin-0.05, labels = cond.text, font = 2)
       i <- i+1
     }
  }

  condition.to.label <- function(c){
    if(c == -1){
      return("DARK")
    } else if (c == 0) {
      return("L0")
    } else if (c == 800){
      return("L800")
    } else {
      return("error")
    }
  }

  # create a color ramp
  colfunc <- colorRampPalette(c( "turquoise3", "purple4"))


  symbols <- c("1","3","5","7","9")
  matpoints.args <- list ()
  matpoints.args$col <- colfunc(5)
  matpoints.args$x <- model.neg.x
  matpoints.args$y <- model.neg.y
  matpoints.args$pch <- symbols
  matpoints.args$lwd <- 2
  matpoints.args$cex <- 2
  matpoints.args$cex.main <- 2
  matpoints.args$cex.axis <- 1
  matpoints.args$cex.lab <- 1
  matpoints.args$type <- "b"

  # call the function so that the left subplot shows
  # all the content for incorrect and the right
  # subplot shows everything for correct
  composeQPFplot(2,"Incorrect Responses")
  do.call(matpoints, matpoints.args)

  legend(x=0.5, y=0.5,
         legend = c("Model", "Empirical   "),
         col = c("turquoise3", "gray40" ),
         lty = c(4, 0),
         pch = c(-1, 19),
         text.col = "black",
         cex = 1,
         lwd = 2)

  matpoints.args$x <- model.pos.x
  matpoints.args$y <- model.pos.y

  composeQPFplot(1,"Correct Responses")
  do.call(matpoints, matpoints.args)


  # the return value of QP.plot in case the values are needed.
  return(list(probability =  freq, quantile = quant))
}


##
## Main Method
##
# which dataset
subject.number <- 1

dat <- dat[[subject.number]]
dat$condition <- dat$fading_function
dat$rt <- dat$reactionTime_PT

selected.conditions <- NULL #unique(dat$condition)[1]

# Here we want to change the input so that only the RTs are used
# for the corresponding condition
QP.plot(dat, conditions=selected.conditions, subject.title=paste("Subject",subject.number))

