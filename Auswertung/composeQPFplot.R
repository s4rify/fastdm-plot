
# This function takes the model values of the reaction time and the cumulative probability and
# produces a QPF plot like in Voss and Tuerlincks paper.


# These are the model data for correct and incorrect responses, taken from the output
# of the CDFplot.exe function and pre-processed in QPplotModel.r.
model.neg.y <- t(matrix(c(sort(value.rt.dark.neg), value.rt.0.neg, value.rt.800.neg), nrow = 5, ncol = 3))
model.neg.x <- t(matrix(c(rep(plateau.dark.neg, 5), rep(plateau.neg.cP.0, 5), rep(plateau.neg.cP.800, 5)), nrow = 5, ncol = 3))

model.pos.y <- t(matrix(c(value.rt.dark.pos, value.rt.0.pos, value.rt.800.pos), nrow = 5, ncol = 3))
model.pos.x <- t(matrix(c(rep(1-plateau.dark.neg,5), rep(1-plateau.neg.cP.0, 5), rep(1-plateau.neg.cP.800, 5)), nrow = 5, ncol =3))


# make sure that two plots can be plotted side by side
par(mfrow=1:2, mar=c(5.1,5.1,4.1,4.1))


composeQPFplotModel <- function(x,y, correct, title) {

  matplot.args <- list() 
  
  # datapoints are the parameters
  matplot.args$y <- y
  matplot.args$x <- x
  
  # additional plot parameter
  matplot.args$main <- title
  # why are there no lines.
  matplot.args$type <- "b"
  matplot.args$col <- c("darkolivegreen3", "lightseagreen", "dodgerblue3", "darkmagenta", "midnightblue")
  matplot.args$bty <-"l"
  matplot.args$pch <- sapply(as.integer(quant*10), toString)
  matplot.args$ylab <- "RT[sec]"
  if (!('xlab' %in% matplot.args)) {
    matplot.args$xlab <- ifelse(correct,'Correctness Frequency', 'Incorrectness Frequency')
  }
  matplot.args$lwd <- 2
  matplot.args$cex <- 2
  matplot.args$cex.main <- 2
  matplot.args$cex.axis <- 2
  matplot.args$cex.lab <- 2
  # Set ylim to make spce for Label of condition
  matplot.args$ylim <- c(0.4,1)
  
  do.call(matplot, matplot.args)
  
  
  #axis(4)
}

composeQPFplotModel(x = model.neg.x, y = model.neg.y, correct = FALSE, title = "Subject 3 (incorrect)")
composeQPFplotModel(x = model.pos.x, y = model.pos.y, correct = TRUE, title = "Subject 3 (correct)")
