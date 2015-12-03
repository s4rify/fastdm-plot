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
  
  # add trialidentifier to data
  data[[i]]$trialnumber <-seq.int(nrow(data[[i]]))
  

}

## overall sum of trials with timeouts
total_timeouts <- sum(sum_timeouts)

## merge all datasets together into one dataframe for the analysis
library(plyr)
DATA <- rbind.fill(data)


########
##
## Data Set diagnostics
##
timeout_rate <- (total_timeouts / 5067) *100
error_rate <- (length(which(DATA$correct==0)) / 5067) *100

########################################################################################
#
# Analysis
#
########################################################################################

## check for normal distribution of response variable
library(car)
qqp(DATA$reactionTime_PT, "norm") # quantile comparison plot
#response is exponentially distributed
qqp(DATA$reactionTime_PT, "exp")

## normalize reaction times through transformation
normalized_RTPT <- log(DATA$reactionTime_PT)
# confirm through plot
qqnorm(normalized_RTPT) # nice!

############
## a linear mixed model with random effects, taken from paper Baayen et al
## right handed side: random effects (slope | intercept)
##
library(nlme)
priming_model <- lmer(normalized_RTPT ~ fading_factor + (1|DATA$trialnumber) + (1 + fading_factor|DATA$subject), data = DATA)

##############
##
## Model Diagnistics
##

# check residuals for normal distribution
residuals_priming <- resid(priming_model)
hist(residuals_priming, n = 40) # nice!


###########
## 
## Tukey Test
##
# for a linear model
library(lmtest)
t_test <- coeftest(linear_model)

# for a linear mixed model
library(multcomp)
summary(glht(priming_model, linfct=mcp(fading_factor="Tukey")))
tukey_contrasts <- summary(glht(priming_model, linfct=mcp(fading_factor="Tukey")), test = adjusted(type = "bonferroni"))




###########
##
## Confidence Intervals
##
##
CI_linear <- confint(linear_model)
CI_linear_mixed <- confint(lmm)


###############################################################################################################
##
##
## Assuming the data is normally distibuted and a simple linear model would fit the data, continue here!
##
##
###############################################################################################################
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

###############################################################################################################
##
##
## Assuming the data is normally distibuted and a linear mixed model fits the data, continue here!
##
##
###############################################################################################################

############
##
## find out that our model is BS and fit a linear mixed model instead
##

library(lme4)
lmm <- lmer(rtPT ~ fading_factor + angle_factor +  sound_factor + (1| DATA$trialnumber ), data=DATA)
lmm1 <- lmer(rtPT ~ fading_factor + angle_factor + sound_factor + (1|subject_factor) + (1|DATA$trialnumber), data=DATA)


##########
##
## some plots
##
library(arm)
coefplot(linear_model)

## qq plot for random effects
library(sjPlot)
sjp.glmer(GHQ, type = "re.qq")

sjp.glmer(GHQ, 
          facet.grid = FALSE, 
          sort = "sort.all")

# plot probability curves for each covariate
# grouped by random intercepts
sjp.glmer(lmm1,
          type = "ri.pc",
          facet.grid = FALSE)

## plot random effects
sjp.lmer(GHQ)

sjp.glmer(lmm,
          type = "ri.pc",
          show.se = TRUE)


###############
##
## CI plot
##
plot(lsmeans(priming_model))

