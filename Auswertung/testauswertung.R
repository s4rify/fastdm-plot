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


soundidx_withsound <- which(DATA$sound == 0)
soundidx_nosound <- which(DATA$sound == -99999) 


fadingidx_800 <- which(DATA$fading_function == 800)
fadingidx_0 <- which(DATA$fading_function == 0)
fadingidx_nolight <- which(DATA$fading_function == -1)

angleidx_nolight <- which(DATA$LEDangle == -1)
angleidx_rightside <- which(DATA$LEDangle == 45)
angleidx_leftside <- which(DATA$LEDangle == -45)
angleidx_central <- which(DATA$LEDangle == 0)
angleidx_peripher <- union(angleidx_leftside, angleidx_rightside) # for friedman

## Deskriptive Daten (Durchschnitt, StAbw., Median und Quartile) 
DATA[soundidx_nosound, ]$reactionTime_PT #reaction_time_ST, correct
DATA[soundidx_withsound, ]$reactionTime_PT #reaction_time_ST correct
DATA[fadingidx_nolight, ]$reactionTime_PT #reaction_time_ST correct
DATA[fadingidx_0, ]$reactionTime_PT #reaction_time_ST correct
DATA[fadingidx_800, ]$reactionTime_PT #reaction_time_ST correct
DATA[angleidx_leftside, ]$reactionTime_PT #reaction_time_ST correct
DATA[angleidx_rightside, ]$reactionTime_PT #reaction_time_ST correct
DATA[angleidx_nolight, ]$reactionTime_PT #reaction_time_ST correct
DATA[angleidx_central, ]$reactionTime_PT

## now combine
# angle = left
rtPTleft_fading0_nosound <- DATA[intersect(intersect(angleidx_leftside, fadingidx_0), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
rtPTleft_fading800_nosound <- DATA[intersect(intersect(angleidx_leftside, fadingidx_800), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
rtPTleft_fading0_withsound <- DATA[intersect(intersect(angleidx_leftside, fadingidx_0), soundidx_withsound), ]$reactionTime_PT #reaction_time_ST correct

# angle = right
rtPTright_fading0_nosound <- DATA[intersect(intersect(angleidx_rightside, fadingidx_0), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
rtPTright_fading800_nosound <- DATA[intersect(intersect(angleidx_rightside, fadingidx_800), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
rtPTright_fading0_withsound <- DATA[intersect(intersect(angleidx_rightside, fadingidx_0), soundidx_withsound), ]$reactionTime_PT #reaction_time_ST correct

# angle = no light
rtPTonly_nosound <- DATA[intersect(intersect(angleidx_nolight, fadingidx_nolight), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
rtPTonly_withsound <- DATA[intersect(intersect(angleidx_nolight, fadingidx_nolight), soundidx_withsound), ]$reactionTime_PT #reaction_time_ST correct

# angle = central
rtPTcentral_fading0_nosound <- DATA[intersect(intersect(angleidx_central, fadingidx_0), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
rtPTcentral_fading800_nosound <- DATA[intersect(intersect(angleidx_central, fadingidx_800), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
rtPTcentral_fading0_withsound <- DATA[intersect(intersect(angleidx_central, fadingidx_0), soundidx_withsound), ]$reactionTime_PT #reaction_time_ST correct

## for Friedman I also need angle = peripher as a combined group 
rtPTperipher_fading800_nosound <- DATA[intersect(intersect(angleidx_peripher, fadingidx_800), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
rtPTperipher_fading0_nosound <- DATA[intersect(intersect(angleidx_peripher, fadingidx_0), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
rtPTperipher_fading0_withsound <- DATA[intersect(intersect(angleidx_peripher, fadingidx_0), soundidx_withsound), ]$reactionTime_PT #reaction_time_ST correct


###########
##
## Friedmann Test
##

# change dataframe to matrix in data bc friedman is a bitch
# groups = combinations of factors --> eg. combo1: fading=800, angle=left, sound=NO
group_CoN <- DATA[intersect(intersect(angleidx_central, fadingidx_0), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
groupC_800N <- DATA[intersect(intersect(angleidx_central, fadingidx_800), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
group_C0Y <- DATA[intersect(intersect(angleidx_central, fadingidx_0), soundidx_withsound), ]$reactionTime_PT #reaction_time_ST correct
groupP_800N <- DATA[intersect(intersect(angleidx_peripher, fadingidx_800), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
group_P0N <- DATA[intersect(intersect(angleidx_peripher, fadingidx_0), soundidx_nosound), ]$reactionTime_PT #reaction_time_ST correct
group_P0Y <- DATA[intersect(intersect(angleidx_peripher, fadingidx_0), soundidx_withsound), ]$reactionTime_PT #reaction_time_ST correct

#
# alpha = 0.05
# df = #groups-1 = 6-1
# ChiSquared value with df=11 and alpha=0.05 => 1,145
# H0 = There is no difference between the groups
# H1 = There is a difference between the groups.
# => Reject H0 if ChiSquared > 1,145

## Friedman for reaction time on PT
friedman_input <- cbind(group_P0Y[1:280], group_P0N[1:280], groupP_800N[1:280], group_C0Y[1:280], groupC_800N[1:280], group_CoN[1:280])
friedman.test(friedman_input)
## Friedman for reaction time on ST



#######
##
## Ranked ANOVA
## 
rankedPT <- rank(DATA$reactionTime_PT)
ranked_priming_model <- lmer(rankedPT ~ fading_factor + (1|DATA$trialnumber) + (1 + fading_factor|DATA$subject), data = DATA)
results$rankedAnova <- anova(ranked_priming_model)

results$simple_ranked_anova <- aov(rankedPT ~ fading_factor + angle_factor + sound_factor)


#########
##
## Cochran's Q Test
##
##
library(coin)
symmetry_test(DATA$correct ~ fading_factor , data = DATA, teststat = "quad")


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
trial_factor <- factor(DATA$trialnumber)
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
results$priming_model <- summary(priming_model)
results$anova_priming <- anova(priming_model)
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

