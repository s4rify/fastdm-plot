########################################################################################
#
# read in data
#
########################################################################################

setwd("./csvfiles/")


# List of file names
all_files <- list.files(path=".", pattern = "*.csv")

# list of all data
data <- list()
for (i in seq_along(all_files)) {
  data[[i]] <- read.csv(file = all_files[i], header=TRUE, sep=",")
}


########################################################################################
#
# data structures for analysis
#
########################################################################################

################
## This is the vector that contains the means of the reaction times to the primary task
## of all 18 subjects. The calculation of the mean in the loop is conditional on the fact that an LED
## has been presented because we want to compare the reaction times of the primary
## task to the reaction times of the secondary task and the sample sizes should be identical
mean_rt_PT <- vector(mode="numeric", length=length(all_files))

###############
## This vector contains the means of the reaction times to the secondary task of all subjects.
## The calculation of the mean in the loop is conditional on the fact that an LED
## has been presented because we want to compare the reaction times of the primary
## task to the reaction times of the secondary task and the sample sizes should be identical
mean_rt_ST <- vector(mode="numeric", length=length(all_files))

###############
## This vector contains the means of the reaction times to the primary task of all subjects when
## sound was played during the condition.
## The calculation of the mean in the loop is conditional on the fact that sound has been played.
mean_rt_PT_with_sound <- vector(mode = "numeric", length = length(all_files))

###############
## This vector contains the means of the reaction times to the secondary task of all subjects when
## sound was played during the condition.
## The calculation of the mean in the loop is conditional on the fact that sound has been played and 
## that an LED was shown.
mean_rt_ST_with_sound <- vector(mode = "numeric", length = length(all_files))


###############
## This vector contains the means of the reaction times to the primary task of all subjects when
## they answered correctly. These means are later on compared to the means to the same task when
## the answer was wrong
mean_RT_PT_when_correct <- vector(mode = "numeric", length = length(all_files))

###############
## This vector contains the means of the reaction times to the primary task of all subjects when
## they answered incorrectly
mean_RT_PT_when_incorrect <- vector(mode = "numeric", length = length(all_files))


###############
## This vector contains the number of how many correct answers were given by each subject
how_many_correct <- vector(mode = "numeric", length = length(all_files))



###############
## This vector contains the means of the reaction times to the primary task of all subjects when
## only sound was played and there was no additional LED presented
mean_RT_PT_with_sound_only <- vector(mode = "numeric", length = length(all_files))

###############
## This vector contains the means of the reaction times to the primary task of all subjects when
## only sound was played and there was no additional LED presented
mean_RT_PT_without_sound_only <- vector(mode = "numeric", length = length(all_files))



########################################################################################
#
# prepare data for plots
#
########################################################################################

for (i in seq_along(all_files)) {
  # remove all NaN values
  lines_to_rem <- which(grepl(NaN, data[[i]]$correct))
  if (length(lines_to_rem) != 0){
    data[[i]] <- data[[i]][-lines_to_rem, ]
  }
  
  ## means of reaction times  of all subjects
  is_led <- data[[i]]$reaction_time_ST > 0
  # conditional means of the reaction times of all subjects
  mean_rt_PT[i] <- mean(data[[i]]$reactionTime_PT[is_led])
  # conditional reaction times for secondary task
  mean_rt_ST[i] <- mean(data[[i]]$reaction_time_ST[is_led])
  
  # means of reaction times of all subjects when sound was played
  has_sound <- data[[i]]$sound == 0
  mean_rt_PT_with_sound[i] <- mean(data[[i]]$reactionTime_PT[has_sound])
  # for ST: sound && ST>0
  mean_rt_ST_with_sound[i] <- mean(data[[i]]$reaction_time_ST[has_sound & is_led])
  
  ## correct/incorrect (only for primary task)
  is_correct <- data[[i]]$correct == 1
  how_many_correct[i] <- length(which(is_correct))
  mean_RT_PT_when_correct[i] <- mean(data[[i]]$reactionTime_PT[is_correct])
  mean_RT_PT_when_incorrect[i] <- mean(data[[i]]$reactionTime_PT[!is_correct])

  ## only primary task with and without sound
  is_secondary_task <- data[[i]]$reaction_time_ST == 0
  mean_RT_PT_with_sound_only[i] <- mean(data[[i]]$reactionTime_PT[!is_secondary_task & has_sound])
  mean_RT_PT_without_sound_only[i] <- mean(data[[i]]$reactionTime_PT[!is_secondary_task & !has_sound])
}


########################################################################################
#
#
# plots
#
#
########################################################################################
# 2 figures side by side
par(mfrow=c(1,2))
plot(mean_rt_PT)
plot(mean_rt_ST)
par(mfrow=c(1,1))



#################
##
## comparison of reaction times to primary task and secondary task
##
##
leg <- legend("topright", c("RT to LED", "RT to letters"), lty=1, col=c("red", "blue"), cex = 0.75)
plot(mean_rt_ST,type='p',col="red", 
     axes=TRUE, 
     ylab="Reaction Times [s]", xlab="Subjects", 
     ylim=range(c(min_y, max_y)), xlim=range(c(min_x, max_x)),
     pch=4)
par(new=TRUE)
points(mean_rt_PT, col="blue", pch=4)

# boxplot of reaction times
boxplot.default(mean_rt_PT, mean_rt_ST, xlab="mean reaction time to circle of letters (1) and to LEDs (2)", ylab="reaction time [s]")

#############
## 
##
## conditions with sound compared to no sound
##
##
# Add extra space to plot area; change clipping to figure
par(mar=c(5.1, 4.1, 8.1, 2.1), xpd=TRUE)

min_y <- 0
max_y <- max(max(mean_rt_PT_with_sound), max(mean_rt_ST_with_sound),max(mean_rt_PT), max(mean_rt_ST))
min_x <- 0
max_x <- length(all_files) + 1

plot(mean_rt_PT_with_sound,type='p',col="blue", axes=TRUE, 
     ylab="Reaction Times [s]", xlab="Subjects",
     ylim=range(c(min_y, max_y)), xlim=range(c(min_x, max_x)),
     pch=15)
par(new=TRUE)
points(mean_rt_ST_with_sound, col="red", pch=15)
par(new=TRUE)
points(mean_rt_PT, col="blue", pch=0)
par(new=TRUE)
points(mean_rt_ST, col="red",pch=0)

leg <- legend(0,2.8, c("RT: LED WITH sound", "RT: letters WITH sound"),
              col=c("red", "blue"), pch=15, inset=c(-0.2,0))
leg1 <- legend(12,2.8, c("RT: LED WITHOUT sound", "RT: letters WITHOUT sound"),
               col=c("red", "blue"), pch=0,inset=c(-0.2,1))

############
## 
## comparison of reaction times to primary task when answered correctly vs incorrectly
##
##
library(calibrate)
par(mfrow=c(1,2))
plot(mean_RT_PT_when_correct,type='p',col="blue", axes=TRUE, 
     ylab="Reaction Times [s]", xlab="Subjects",
     ylim=range(c(min_y, max_y)), xlim=range(c(min_x, max_x)),
     pch=3)
par(new=TRUE)
points(mean_RT_PT_when_incorrect,col="red")
par(new=TRUE)
leg <- legend("topright", c("RT when correct", "RT when incorrect"), 
              lty=1, col=c("blue", "red"), cex = 0.75)

plot(how_many_correct, pch =20)
textxy(data[[1]]$number, how_many_correct, data[[1]]$number)

#################
##
## comparison of reaction times to primary task only; with and without sound
##
##

min_y <- 0
max_y <- max(max(mean_rt_PT_with_sound), max(mean_rt_ST_with_sound),max(mean_rt_PT), max(mean_rt_ST))
min_x <- 0
max_x <- length(all_files) + 1
plot(mean_RT_PT_with_sound_only,type='p',col="red", 
     axes=TRUE, 
     ylab="Reaction Times [s]", xlab="Subjects", 
     ylim=range(c(min_y, max_y)), xlim=range(c(min_x, max_x)),
     pch=4)
par(new=TRUE)
points(mean_RT_PT_without_sound_only, col="blue", pch=4)
leg <- legend("topright", c("RT to PT with sound", "RT to PT without sound"), lty=1, col=c("red", "blue"), cex = 0.75)

# boxplot of reaction times
boxplot.default(mean_rt_PT, mean_rt_ST, xlab="mean reaction time to circle of letters (1) and to LEDs (2)", ylab="reaction time [s]")

#################
## 
## Confidence Intervals of reaction times
## 
##
library(plotrix)
plotCI(data[[1]]$reactionTime_PT, ui = 2, li = 0)