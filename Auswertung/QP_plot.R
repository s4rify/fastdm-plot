# set working directory
setwd("./csvfiles")
# read data file
dat = read.table(file = 'test.csv', header = TRUE, sep = ',');

# load function
QP.plot = function(dat, quantiles = seq.int(.1, .9, .2), ...) {
  dat = dat[complete.cases(dat), ] # omit all missings
  if (is.null(dat$condition)) { # if there is no condition there is one condition
    dat$condition = 1
    uniq = 1
  } else {
    uniq = unique(dat$condition)
  }
  uniq
  # empty matrices
  prob = matrix(0, length(quantiles), 2*length(uniq))
  quant = matrix(0, length(quantiles), 2*length(uniq))
  for (j in 1:length(uniq)) { 
    # get quantiles 0,1
    quant[,2*(j-1)+1] = quantile(subset(dat$rt, (dat$condition == uniq[j])&(dat$correct == 1)), probs = quantiles) # hier stond: quantile(subset(dat$reactionTime_PT, (dat$condition == uniq[j])&(dat$correct == 1), probs = quantiles)) 
    quant[,2*(j-1)+2] = quantile(subset(dat$rt, (dat$condition == uniq[j])&(dat$correct == 0)), probs = quantiles)    
    temp=mean(subset(dat$correct,  dat$condition == uniq[j]))
    prob[,2*(j-1)+1] = rep(temp,length(quantiles))
    prob[,2*(j-1)+2] = rep(1-temp,length(quantiles))
  }
  # some plotting defaults
  dots = list(...)
  if (!('pch' %in% dots)) {
    #dots$pch = 1:length(uniq)
  } 
  if (!('ylab' %in% dots)) {
    dots$ylab = 'rt'
  }
  if (!('xlab' %in% dots)) {
    dots$xlab = 'probability'
  }
  idx=order(prob[1,])
  dots$x = t(prob[,idx])
  dots$y = t(quant[,idx])
  # do plot
  do.call(matplot, dots)
  return(list(probability = prob, quantile = quant))
}

# define a condition variable to get the results for multiple plots
dat$condition = dat$fading_function
dat$rt=dat$reactionTime_PT

# use function
QP.plot(dat, type = 'b')
# default plot arguments still work
QP.plot(dat, type = 'b', xlim = c(0, 1), bty = 'l', las = 1, ylab = 'rt')



