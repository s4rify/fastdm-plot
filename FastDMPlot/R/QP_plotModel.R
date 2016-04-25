# This script composes the model parameter for the qpf plot.
# It assumes several data fields to be accessable from the global environment
# and creates several fata fields itself.
# It assumes that the working directory is a subfolder of "Auswertung" and
# expects the lst files in the given folder below.
#
# lst.dark: This is a data field  which contains the output of plot-cdf.exe
#           for the condition dark and is at the moment produced by the function
#           composeCDFparameters.R.
# lst.0/800: This is a data field which is an output of plot-cdf.exe and which 
#            is created manually at the moment with one call to plot-cdf in the console.
#



########## global variables ##########
subj  <- 3
quant <- c(.1, .3, .5, .7, .9) 
path  <- "/Auswertung/DiffModel/"
#>plot-cdf.exe -a 00.65 -z 0.5 -v -0.3432 -t 0.65 -o "s3LIGHT800t065.lst"
file.0   <- "/LIGHT0/s3LIGHT0t065.lst" 
file.800 <- "/LIGHT800/s3LIGHT800t065.lst"
#file.dark <- "/NOLIGHT/s3DARKt065.lst"
s3.lst.t047 <-read.table(paste(getwd(), "/Auswertung/DiffModel/NOLIGHT/tests3/s3NOLIGHT_t047.lst" , sep=""), header = TRUE)


########### input  #################
lst.0   <- read.table(paste(getwd(), path, file.0,   sep=""), header = TRUE)
lst.800 <- read.table(paste(getwd(), path, file.800, sep=""), header = TRUE)
#lst.dark.3 <-read.table(paste(getwd(), path, file.dark, sep=""), header = TRUE)
lst.dark.3 <- s3.lst.t047
model.rt.0 <- lst.0[1]
model.cP.0 <- lst.0[2]

#lst.dark.3 <- lst.dark[[subj]] # lst dark contains all the cdf parameters for all subjects
model.dark.rt <- lst.dark.3[1]
model.dark.cP <- lst.dark.3[2]

model.rt.800 <- lst.800[1]
model.cP.800 <- lst.800[2]

############## find the plateaus from the cdf output #######################
# biggest that is smaller 0 and the smallest that is bigger than 0 is PLATEAU
plateau.neg.cP.0   <- max(model.cP.0[[1]][which(model.rt.0 <= 0)])
plateau.pos.cP.0   <- min(model.cP.0[[1]][which(model.rt.0 >= 0)])
plateau.neg.cP.800 <- max(model.cP.800[[1]][which(model.rt.800 <= 0)])
plateau.pos.cP.800 <- min(model.cP.800[[1]][which(model.rt.800 >= 0)])
plateau.dark.neg <- max(model.dark.cP[[1]][which(model.dark.rt[[1]] <= 0)])
plateau.dark.pos <- min(model.dark.cP[[1]][which(model.dark.rt[[1]] >= 0)])


# vectors that contain the rt value corresponding to a given quantile value
value.rt.0.neg   <- vector(mode = "numeric", length = length(quant))
value.rt.800.neg <- vector(mode = "numeric", length = length(quant))
value.rt.0.pos   <- vector(mode = "numeric", length = length(quant))
value.rt.800.pos <- vector(mode = "numeric", length = length(quant))
value.rt.dark.pos <- vector(mode = "numeric", length = length(quant))
value.rt.dark.neg <- vector(mode = "numeric", length = length(quant))


# vectors that contain the scaled quantiles for the negative and the positive rts
# the quantiles are transformed from the normal [0,1]-quantiles to the
# scaled quantiles of the CDF plot
q.neg.0   <- vector(mode = "numeric", length = length(quant))
q.neg.800 <- vector(mode = "numeric", length = length(quant))
q.pos.0   <- vector(mode = "numeric", length = length(quant))
q.pos.800 <- vector(mode = "numeric", length = length(quant))
q.pos.dark <- vector(mode = "numeric", length = length(quant))
q.neg.dark <- vector(mode = "numeric", length = length(quant))


for (q in 1: length(quant)){

    # these are the scaled quantiles for the negative responses
    q.neg.0[q]   <- plateau.neg.cP.0   * (1-quant[q])
    q.neg.800[q] <- plateau.neg.cP.800 * (1-quant[q])

    # or:
    q.pos.0[q]   <- quant[q] * (1 - plateau.pos.cP.0)   + plateau.pos.cP.0
    q.pos.800[q] <- quant[q] * (1 - plateau.pos.cP.800) + plateau.pos.cP.800
    
    q.pos.dark[q] <- quant[q] * (1- plateau.dark.pos) + plateau.dark.pos
    q.neg.dark[q] <- quant[q] * (1- plateau.dark.neg) + plateau.dark.neg
    
    # find the rt values corresponding to the scaled quantiles
    value.rt.0.neg[q]   <- abs( max( model.rt.0  [[1]][which(model.cP.0   <= q.neg.0[q])] ) )
    value.rt.800.neg[q] <- abs( max( model.rt.800[[1]][which(model.cP.800 <= q.neg.800[q])] ) )
    
    value.rt.0.pos[q]   <- max( model.rt.0 [[1]] [which(model.cP.0   <= q.pos.0[q])] )
    value.rt.800.pos[q] <- max( model.rt.800[[1]][which(model.cP.800 <= q.pos.800[q])])
    
    value.rt.dark.pos[q] <-  max( model.dark.rt[[1]] [which(model.dark.cP <= q.pos.dark[q])] )
    value.rt.dark.neg[q] <- abs( max( model.dark.rt[[1]][which(model.dark.cP <= q.neg.dark[q])] ) )
}

