# load script
setwd("~/Documents/Master/WS15-16/HiwiJobAndreas/rprojects/Auswertung")
source("testauswertung.R") # load and execute a script of R commands
#######
##
## Ranked ANOVA
## 
rankedPT <- rank(DATA$reactionTime_PT)
ranked_priming_model <- lmer(rankedPT ~ fading_factor + (1|DATA$trialnumber) + (1 + fading_factor|DATA$subject), data = DATA)
results$rankedAnova <- anova(ranked_priming_model)

results$simple_ranked_anova <- aov(rankedPT ~ fading_factor + angle_factor + sound_factor)

ranked_trial <- rank(trial_factor)
ranked_fading <- rank(fading_factor)
ranked_angle <- rank(angle_factor)
ranked_PT <- rank(DATA$reactionTime_PT)

ranked_simpleLm <- lm(ranked_PT ~ ranked_fading * ranked_angle)
results$ranked_simple_lm <- summary(ranked_simpleLm)

ranked_simpleLm_with_trials <- lm(ranked_PT ~ ranked_fading * ranked_angle * ranked_trial)
results$ranked_simple_lm_with_trials <- summary(ranked_simpleLm)

rANOVA_with_trials <- aov(ranked_simpleLm_with_trials)
rANOVA <- aov(ranked_simpleLm)
results$rANOVA <- summary(rANOVA)
results$rANOVA_with_trials <- summary(rANOVA_with_trials)

#########
##
## Cochran's Q Test
##
##
load("RVAideMemoire")
formula_cochran <- response ~ factor | group
cochran.qtest(formula_cochran, DATA, alpha = 0.05, p.method = "fdr")

# example
response <- c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1)
# gl(n=number of levels,k=number of replications, length=n*k, labels=optional vector of labels, ordered=False)
fact <- gl(3,1,30,labels=LETTERS[1:3])
block <- gl(10,3,labels=letters[1:10])
cochran.qtest(response~fact|block)


## Friedman for reaction time on PT
friedman_input <- cbind(group_P0Y[1:280], group_P0N[1:280], groupP_800N[1:280], group_C0Y[1:280], groupC_800N[1:280], group_CoN[1:280])
friedman.test(friedman_input)
## Friedman for reaction time on ST
