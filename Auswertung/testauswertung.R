########################################################################################
#
# read in data
#
########################################################################################

## This script expects to find a subfolder containing the .csv files relative to the working directory
#csv_path <- "./csvfiles"
csv_path <- "./csvfiles"

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

## transform variables into factors for the ANOVa
rtPT <- DATA$reactionTime_PT
rtST <- DATA$reaction_time_ST
corr <- DATA$correct
fading_factor <- factor(DATA$fading_function) 
angle_factor <- factor (DATA$LEDangle)
levels(angle_factor) <- c("periphery", "no_light", "central", "periphery")
# angle is only looked at with two possibilities IF light was presented: central and periphery
sound_factor <- factor(DATA$sound)
# subject must be a factor for a within subject anova
subject_factor <- factor(DATA$subject)

#############
##
## estimate a simple linear model
##
library(arm)
linear_model <- lm( rtPT ~  fading_factor*angle_factor*sound_factor)
coef(linear_model)

############
## ANOVA 
## three way within subject anova
## appearently this model is not appripriate bc we get "Error() is singular" warning. So we need a another model
an1 <- aov(   rtPT ~  fading_factor*angle_factor*sound_factor + Error(DATA$subject/(fading_factor*angle_factor*sound_factor))   )
an3 <- anova(linear_model)

############
##
## provide summary as output
## 
anova_summary <- summary(an1)

############
## find out that our model is BS and fit a linear mixed model instead
##
##
# We use the lmer function with the familiar formula interface, 
# but now group level variables are specified using a special syntax:
#  (1|subject) tells lmer to fit a linear model with a varying-intercept group effect using the variable subject  .
library(lme4)
lmm <- lmer(rtPT ~ DATA$fading_function*DATA$LEDangle *DATA$sound + (1| DATA$subject ), data=DATA)
lmm1 <- lmer(rtPT ~ fading_factor + angle_factor + sound_factor + (1|subject_factor), data=DATA)


###########
## 
## Tukey Test
##
library(lmtest)
t_test <- coeftest(linear_model)
tukey_contrasts <- summary(glht(lmm1, linfct=mcp(fading_factor="Tukey")), test = adjusted(type = "bonferroni"))


###########
##
## Confidence Intervals
##
##
CI_linear <- confint(linear_model)
CI_linear_mixed <- confint(lmm)

###########
##
## Plot results
##
##
library(arm)
coefplot(linear_model)

## qq plot for random effects
library(sjPlot)
sjp.glmer(lmm, type = "re.qq")

sjp.glmer(lmm1, 
          facet.grid = FALSE, 
          sort = "sort.all")

# plot probability curves for each covariate
# grouped by random intercepts
sjp.glmer(lmm1,
          type = "ri.pc",
          facet.grid = FALSE)

## plot random effects
sjp.lmer(lmm1)


###########
##
## Find a model using the AIC!
##
library(stats4)
fitted_model <- step(linear_model)

## plot
coefplot(fitted_model)
t_test1 <- coeftest(fitted_model)


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


