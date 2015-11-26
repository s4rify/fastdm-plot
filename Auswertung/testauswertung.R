########################################################################################
#
# read in data
#
########################################################################################

## This script expects to find a subfolder containing the .csv files relative to the working directory
#csv_path <- "./csvfiles"
csv_path <- "."

anova_output_path <- "./ANOVA_results"


# List of file names
all_files <- list.files(path = csv_path, pattern = "*.csv")

# store all files in a dataframe called data
data <- list()
for (i in seq_along(all_files)) {
  data[[i]] <- read.csv(file = file.path(csv_path, all_files[i]), header=TRUE, sep=",")
}


########################################################################################
#
# prepare data for analysis
#
########################################################################################
###############
## AA vector that stores the numer of trials in which a timeout occured.
##
sum_timeouts <- vector(mode = 'numeric', length = length(all_files))

for (i in seq_along(all_files)) {
  # remove all NaN values
  lines_to_rem <- which(grepl(NaN, data[[i]]$correct))
  if (length(lines_to_rem) != 0){
    data[[i]] <- data[[i]][-lines_to_rem, ]
  }
  # count how many timeouts occured
  sum_timeouts[i] <- length(lines_to_rem)
  
  # add subject row to identify subjects in the analysis
  data[[i]]$subject <- i
}

## overall sum of trials with timeouts
total_timeouts <- sum(sum_timeouts)

## merge all datasets together into one dataframe for the analysis
library(plyr)
DATA <- rbind.fill(data)



########################################################################################
#
# Analysis
#
########################################################################################

###########
##
## Within subject ANOVA with error term
##
##

# is it ok to have one EV and one error term in one anova??
# 1.0.1. change levels for fading factor
# 1.1. look for interdependencies of factors!
# 3. do tukey test 
# 4. plot results

## get fields for analysis
rtPT <- DATA$reactionTime_PT
rtST <- DATA$reaction_time_ST
corr <- DATA$correct

## transform variables into factors for the ANOVa
fading_factor <- factor(DATA$fading_function) 
angle_factor <- factor (DATA$LEDangle)
sound_factor <- factor(DATA$sound)

############
## ANOVA 
an1 <-aov(rtPT ~  fading_factor + Error(DATA$subject/fading_factor))
an1.1 <- aov(rtPT ~ angle_factor + Error(DATA$subject/angle_factor))
an1.2 <- aov(rtPT ~ sound_factor + Error(DATA$subject/sound_factor))
aov.ex4=aov(Recall~(Task*Valence)+Error(Subject/(Task*Valence)),data.ex4 )

an2 <- aov(rtST ~ fading_factor + angle_factor + sound_factor)
an3 <- aov(corr ~ fading_factor + angle_factor + sound_factor)

#> aov.out = aov(price ~ store + Error(subject/store), data=groceries2)

##
## provide summary as output
## 
summary_an1 <- summary(aov(rtPT ~  fading_factor + angle_factor + sound_factor))
summary_an2 <- summary(aov(rtST ~ fading_factor + angle_factor + sound_factor))
summary_an3 <- summary(aov(corr ~ fading_factor + angle_factor + sound_factor))


###########
##
## Confidence Intervals
##
##

# 1. compute confidence intervals
# 2. plot them plus error bars


###########
##
## Plot results
##
##





##
## also save results in file
##
capture.output(summary_an1, file = file.path(anova_output_path, "summary_RT_PT.txt"))
capture.output(summary_an2, file = file.path(anova_output_path, "summary_RT_ST.txt"))
capture.output(summary_an3, file = file.path(anova_output_path, "summary_corr.txt"))
capture.output(an1, file = file.path(anova_output_path, "anova_RT_PT.txt"))
capture.output(an2, file = file.path(anova_output_path, "anova_RT_ST.txt"))
capture.output(an3, file = file.path(anova_output_path, "anova_corr.txt"))

###################
##
## Cohen's D
##

library(lsr)
effect_size_reaction_time <- cohensD(mean_rt_ST, mean_rt_PT)
effect_size_correct_incorrect <- cohensD(mean_RT_PT_when_incorrect, mean_RT_PT_when_correct)
effect_size_sound <- cohensD(mean_rt_ST_with_sound, mean_rt_PT_with_sound)


