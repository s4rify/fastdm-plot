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
## A vector that stores the numer of trials in which a timeout occured.
##
sum_timeouts <- vector(mode = 'numeric', length = length(all_files))

##########
## A vector that stores the error rates per subject
##
subj_error_rates <- vector(mode = "numeric", length = length(all_files))


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
  
  # length of data
  lengthdata <- dim(data[[i]])[1] # ~ 280
  # calculate error rate per subject
  subj_error_rates[i] <- length(which(data[[i]]$correct == 0))/ lengthdata *100

}

## overall sum of trials with timeouts
total_timeouts <- sum(sum_timeouts)

## merge all datasets together into one dataframe for the analysis
library(plyr)
DATA <- rbind.fill(data)
lengthDATA <- dim(DATA)[1] #5067
########
##
## Data Set diagnostics
##
timeout_rate <- (total_timeouts / lengthDATA) *100
error_rate <- (length(which(DATA$correct==0)) / lengthDATA) *100

## look at data frame, describe() gives a nice summary
library(psych)
describe(DATA)

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
# ST fading factor
valid_fading_factor <- DATA$fading_function[rt_ST_valid]
fading_factor_ST <- factor(valid_fading_factor)
# ST trialnumbers
valid_trialnumbers <- DATA$trialnumber[rt_ST_valid]
trialnumber_ST <- factor(valid_trialnumbers)
# ST subject 
valid_subjects <- DATA$subject[rt_ST_valid]



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
library(lme4)
priming_model <- lmer(normalized_RTPT ~ fading_factor + (1|DATA$trialnumber) + (1 + fading_factor|DATA$subject), data = DATA)
priming_model_ST <- lmer(rt_ST_valid ~ fading_factor_ST + (1|valid_trialnumbers) + (1 + valid_fading_factor | valid_subjects), data = DATA)

###########
##
## A logistic regression for the binary response variable correctness
## 
logit_regression <-  glm(DATA$correct ~ fading_factor + angle_factor + sound_factor, data = DATA, family = "binomial")
summary(logit_regression)
results$logistic_regression <- summary(logit_regression)



##############
##
## Model Diagnistics
##

# check residuals for normal distribution
residuals_priming_PT <- resid(priming_model)
hist(residuals_priming, n = 40) # nice!

residuals_priming_ST <- resid(priming_model_ST)
hist(residuals_priming_ST, n = 40) #nice

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
results$tukey <- summary(glht(priming_model, linfct=mcp(fading_factor="Tukey")), test = adjusted(type = "bonferroni"))


###########
##
## Friedmann Test
##
results = NULL
friedmandata <- data.frame(normalized_RTPT, fading_factor, DATA$trialnumber)
results$friedman <- friedman.test(normalized_RTPT ~ fading_factor| trialnumber, data = friedmandata )
## results in an error: An unreplicated complete block design has 
# exactly 1 observation for each combination of the two grouping factors. The above clearly has 2 observations with "HC, Sad". So Friedman's test does not apply.


###########
##
## T-test
## 
results$t_test_sound <- t.test(normalized_RTPT ~ sound_factor)

###########
##
## Non-paramteric Tests
##
# Wilcoxon
results$wilcoxon <- wilcox.test(norm_rt_numeric, DATA$fading_function) # where y and x are numeric

# Kruskal Wallis Test One Way Anova by Ranks 
results$kruskal_wallis <- kruskal.test(norm_rt_numeric~fading_factor) # where y1 is numeric and A is a factor

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
## Or: work with normalized data (log transform)
##
##
###############################################################################################################
simple_lm_model <- lm(normalized_RTPT ~ fading_factor * angle_factor, data = DATA)
results$simple_lm_model <- summary(simple_lm_model)
hist(resid(simple_lm_model), n =100) # naja

coef(simple_lm_model)
results$anova_simple_lm_model <- aov(simple_lm_model)



##########
##
## some plots
##
library(arm)
coefplot(simple_lm_model)

## qq plot for random effects
library(sjPlot)
sjp.glmer(simple_lm_model, type = "re.qq")

sjp.glmer(priming_model, 
          facet.grid = FALSE, 
          sort = "sort.all")

# plot probability curves for each covariate
# grouped by random intercepts
sjp.glmer(simple_lm_model,
          type = "ri.pc",
          facet.grid = FALSE)

## plot random effects
sjp.lmer(GHQ)

sjp.glmer(priming_model,
          type = "ri.pc",
          show.se = TRUE)


###############
##
## Dynamite Plot: means plus error bars
##
library(sciplot)
bp <-with(data=DATA, bargraph.CI(x.factor=DATA$subject, group=fading_factor, response=normalized_RTPT,
                                                              lc=FALSE, xlab="Subjects",
                                                              legend=TRUE, x.leg=3.3, cex.leg=1.3, cex.names=1.5, cex.lab = 1.5,
                                                              ci.fun=function(x) {c(mean(x) - 1.96*se(x), mean(x) + 1.96*se(x))}))
save(bp, file = paste(filepath, "dynamiteplot", sep="/"))

