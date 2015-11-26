#### FUNCTIONS ####
plotByCondition <- function(formula, variable, groupvar, data, binomialdata=F){
  library(nlme)
  library(multcomp)
  library(ggplot2)
  library(reshape2)
  
  # 1. test for normal distribution
  data.split = split(data, f=data[,groupvar])
  result = lapply(data.split, FUN = function(x) {
    shapiro.test(as.numeric(x[,variable]))
  })
  
  result = as.data.frame(do.call("rbind", result))
  result$data.name = NULL
  result$normal = (result$p.value > .05)
  print(result)
  rm(data.split)
  
  # 2. non-normal distribution
  if (any(!result$normal)){
    if(binomialdata == T){
      library(coin)
      #print(symmetry_test(as.formula(paste(Answer ~ factor(Software) | factor(Participant))), data = data, teststat = "quad"))
      print(symmetry_test(as.formula(paste(variable," ~ factor(",groupvar,") | factor(participant)", sep="")), data = data, teststat = "quad"))
    } else { # not binomial
      print("Non-Normal distribution: Using Quade")
      x = quade.test(acast(data, as.formula(paste("participant~", groupvar, sep="")), value.var=variable))
      print(sprintf("QUADE with condition only (using ranks): p=%g, F=%g", x$p.value, x$statistic))
      if (x$p.value<=.1){
        print(x)
        print(pairwise.wilcox.test(as.numeric(data[,variable]), data[,groupvar],  p.adjust.method = "hommel", paired=T, exact=F))
        print(pairwise.wilcox.test(as.numeric(data[,variable]), data[,groupvar],  p.adjust.method = "fdr", paired=T, exact=F))
        print(pairwise.wilcox.test(as.numeric(data[,variable]), data[,groupvar],  p.adjust.method = "hochberg", paired=T, exact=F))
        print(pairwise.wilcox.test(as.numeric(data[,variable]), data[,groupvar],  p.adjust.method = "holm", paired=T, exact=F))
      } else {
        print(sprintf("ANOVA (on ranks) with condition only: p=%g, X^2 = %g )", lme_x_anova$"p-value"[2], lme_x_anova$"F-value"[2]))
        if (lme_x_anova$"p-value"[2] <= .1){
          print(summary(glht(lme_x, linfct=mcp(variable = "Tukey")), test = adjusted(type = "holm")))
        }
        else {
          print("Not significant, thus no posthoc analysis.")
        }
      }
    }
  }
  
  result.lme = lme(formula, data=data, random = ~1|participant)
  result.anova = anova(result.lme)
  print(result.anova)
  summary( glht(result.lme, linfct=mcp(condition="Tukey")))
  # Use 95% confidence interval instead of SEM
  tgc <- summarySE(data, measurevar=variable, groupvars=c(groupvar))
  pd <- position_dodge(0.9) # move them .05 to the left and right
  ggplot(tgc, aes(x=as.name(groupvar), y=as.name(variable))) + 
    geom_errorbar(aes(ymin=as.name(variable)-ci, ymax=as.name(variable)+ci), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd) 
  tgc
}

getCleanData <- function(data){
  library(reshape2)
  data_wide <- dcast(data, participant + left_v + init_front + init_closing + assumedTTC + orderPerP ~ condition, value.var="braked")
  data_wide = data_wide[complete.cases(data_wide),]
  data_long <- melt(data_wide,
                    # ID variables - all the variables to keep but not split apart on
                    id.vars=c("participant","assumedTTC", "left_v", "init_front", "init_closing", "orderPerP"),
                    # The source columns
                    measure.vars=c("Baseline", "Constant", "Adapting" ),
                    # Name of the destination column that will identify the original
                    # column that the measurement came from
                    variable.name="condition",
                    value.name="braked"
  )
  data_long$braked=NULL
  merge(data_long, data, by=c("participant","assumedTTC", "left_v", "init_front", "init_closing", "orderPerP", "condition"))
}

plotMeans = function(tgc, measurement, group){
  ## plotting
  library(ggplot2)
  
  pd <- position_dodge(0.9) # move them .05 to the left and right
  tgc$measurement = tgc[,measurement]
  plotted = ggplot(tgc, aes(x=condition, y=measurement)) + 
    geom_errorbar(aes(ymin=measurement-ci, ymax=measurement+ci), width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd) +
    ylab(label = measurement)
  rm( pd)
  
  plotted
}

# taken from http://yatani.jp/teaching/doku.php?id=hcistats:cochran
reportBinomialWithin <- function(measurement, data) {
  formula = as.formula(paste(measurement, " ~ condition ", sep=""))
  formula2 = as.formula(paste(measurement, " ~ condition | participant", sep=""))
  
  result = c()
  
  d = getCleanData(data)
  library(lattice)
  #densityplot(as.numeric(d[,measurement]), na.rm=T)  # plot(density(...))
  #boxplot(formula, d)
  # binomial data -> Cochran's Q test
  library(coin)
  result$test = symmetry_test(formula2, data = d, teststat = "quad")
  #--> significant, chi-squared = 12.2658, df = 2, p-value = 0.00217
  
  ## post-hoc (McNemar's test)
  tests = list(mcnemar.test(d[d$condition=="Baseline",measurement], d[d$condition=="Constant",measurement]))
  tests = append(tests,list(mcnemar.test(d[d$condition=="Baseline",measurement], d[d$condition=="Adapting",measurement])))
  tests = append(tests,list(mcnemar.test(d[d$condition=="Constant",measurement], d[d$condition=="Adapting",measurement])))
  names(tests) = c("Baseline-Constant", "Baseline-Adapting", "Constant-Adapting")
  result$posthoc = tests
  
  p = lapply(tests, FUN = function(x) { x$p.value})
  p = p.adjust(p, method="bonferroni")
  result$bonferroni = p
  rm(p)
  
  phi = unlist(lapply(tests, FUN = function(x) { as.numeric(x$statistic[[1]])}))
  phi = sqrt(phi/(nrow(d[d$condition=="Constant",])*2))
  result$phis =  phi
  rm(phi)
  
  ## plotting
  # Use 95% confidence interval instead of SEM
  tgc <- summarySE(d, measurevar=measurement, groupvars=c("condition"))
  result$descriptive = tgc
  
  result$plot = plotMeans(tgc, measurement, "condition")
  rm(tgc)
  
  rm (tests, formula, formula2)
  
  result
}

## run non-normal tests (friedman + wilcoxon)
reportNonNormalData <- function(measurement, data) {
  result = NULL
  
  data_wide <- dcast(data, participant ~ condition, value.var=measurement)
  data_matrix <- as.matrix(data_wide[,c("BL", "NonAdapt", "Adapt")])
  result$friedman = friedman.test(data_matrix)
  d = melt(as.data.frame(data_matrix))
  names(d) = c("condition", measurement)
  rm(data_matrix)
  
  library(coin)
  ## post-hoc (Wilcoxon Signed-rank test)
  tests = list(wilcoxsign_test(formula = BL ~ NonAdapt, distribution="exact", data=data_wide, zero.method = "Wilcoxon"))
  tests = append(tests,list(wilcoxsign_test(formula = BL ~ Adapt, distribution="exact", data=data_wide, zero.method = "Wilcoxon")))
  tests = append(tests,list(wilcoxsign_test(formula = NonAdapt ~ Adapt, distribution="exact", data=data_wide, zero.method = "Wilcoxon")))
  names(tests) = c("BL-NonAdapt", "BL-Adapt", "NonAdapt-Adapt")
  result$posthoc = tests
  rm(data_wide)
  
  p = lapply(tests, FUN = function(x) { pvalue(x)})
  p = p.adjust(p, method="bonferroni")
  result$bonferroni = p
  rm(p)
  
  r = unlist(lapply(tests, FUN = function(x) { as.numeric(statistic(x))}))
  r = sqrt(r/(nrow(d[d$condition=="NonAdapt",])*2))
  result$rs =  r
  rm(r)

  
  ## plotting
  library(ggplot2)
  # Use 95% confidence interval instead of SEM
  tgc <- summarySE(d, measurevar=measurement, groupvars=c("condition"))
  result$descriptive = tgc
  
  result$plot = plotMeans(tgc, measurement, "condition")
  rm(tgc)
  
  result
}

reportNormalData <- function(measurement, data) {
  
  result = c()
  
  d=getCleanData(data)
  d$measurement = d[,measurement]
  formula = as.formula(paste(measurement, " ~ condition ", sep=""))
  
  
  # 1. Homogeneity of variances (p>.05 = variances are equal (good))
  library(car)
  result$homogeneity = leveneTest(formula, d)
  
  # 2.`Mauchly's Test for Sphericity` (p>.05 = Sphericity ok (good))
  library(ez)
  options(contrasts=c("contr.sum", "contr.poly"))
  result$sphericity = ezANOVA(data=d, dv=.(measurement), wid=.(participant), within=.(condition), type=3, within_full = c(condition, left_v, init_front, init_closing, orderPerP))
  
  library(nlme)
  options(contrasts=c("contr.sum","contr.poly"))
  lme_x = lm(formula, data=clean_data, random = ~1|participant)
  x = anova(lme_x)
  result$anova = x
  
  f.value = x$"F-value"[2]
  p.value = x$"p-value"[2]
  rm(x)
  
  #library(MBESS)
  #ci.pvaf(F.value=f.value, df.1=df1, df.2=df2, N=nrow(d))
  library(Hmisc)
  
  result$ttests = with(d, pairwise.t.test(measurement, condition, p.adjust.method="bonferroni", paired=T))
  
  data_wide <- dcast(d, participant+ left_v + init_front + init_closing + orderPerP ~ condition , value.var=measurement)
  result$pearson = rcorr(as.matrix (data_wide[, c("Baseline", "Constant", "Adapting")]), type="pearson")
  
  tgc <- summarySE(d, measurevar=measurement, groupvars=c("condition"))
  result$descriptive = tgc
  
  result$plot = plotMeans(tgc, measurement, "condition")
  rm(tgc)
  
  result
}

###############
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}
## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}


#### READ QUESTIONAIRE DATA FROM CSV ####
questiondata <- read.csv("AmbiCar2015_Responses_formatiert.csv", header=TRUE, sep = ";")
questiondata$participant = questiondata$p
questiondata$p = NULL
questiondata$condition = factor(questiondata$condition, levels=c("None", "Nonadapt", "Adapt"), labels = c("BL", "NonAdapt", "Adapt"))
questiondata$RightDecision = factor(as.character(questiondata$RightDecision), levels=c("Lehne vollständig ab", "Lehne ab", "Neutral", "Stimme zu", "Stimme vollständig zu"))
questiondata$Distracted = factor(as.character(questiondata$Distracted), levels=c("Lehne vollständig ab", "Lehne ab", "Neutral", "Stimme zu", "Stimme vollständig zu"))
questiondata$DecisionDifficult = factor(as.character(questiondata$DecisionDifficult), levels=c("Lehne vollständig ab", "Lehne ab", "Neutral", "Stimme zu", "Stimme vollständig zu"))
questiondata$SatisfiedWithPerformance = factor(as.character(questiondata$SatisfiedWithPerformance), levels=c("Lehne vollständig ab", "Lehne ab", "Neutral", "Stimme zu", "Stimme vollständig zu"))

#### READ SIMULATOR DATA FROM CSV####
memory.limit(size=4095)
dirs=list.dirs(path=".")#,pattern = "*.csv")
#dbWriteTable(conn=db,name="DATA_RAW",value=data,row.names=FALSE,header=false)


#length(data$V1)
col_names=read.table("p0/data.1.csv",nrows=21) # Namen der Spalten auslesen
raw_data = NULL
for (j in 2:length(dirs)){
  
  files=list.files(path=dirs[j],pattern = "*.csv")

  for(i in 1:length(files)){
    
    dir_file = paste (dirs[j],files[i],sep="/")
    data = read.csv(dir_file,skip=21,header=FALSE) # 21 Zeilen überspringen (Header), Daten dann ohne Überschrift
    names(data)=gsub("Spalte[0-9]+=","",col_names$V1)
    
    if (is.null(raw_data)){
      raw_data = data
    } else {
      raw_data = rbind(raw_data, data)
      raw_data = raw_data[raw_data$training == 0,] # already remove training data (or else will run into memory problems)
    }
  }
  print(j)
}

rm(i, j, dir_file, dirs, files, data, col_names)

save(raw_data, file="raw_data.rDa")

#### PREPARE FOR ANALYSIS ####
leftBorder = -1.35

## now we can do stuff :)
load("raw_data.rDa")

## add markers for every new block:
x = raw_data[2:length(raw_data$closing_visible),]$closing_visible != raw_data[1:(length(raw_data$closing_visible)-1),]$closing_visible
x = x | raw_data[2:length(raw_data$participant),]$participant != raw_data[1:(length(raw_data$participant)-1),]$participant
x = x | raw_data[2:length(raw_data$condition),]$condition != raw_data[1:(length(raw_data$condition)-1),]$condition
raw_data$firstMeasure = c(T, x)
raw_data$lastMeasure = c(x, T)
rm(x)

# filter relevant data (no training, other cars visible)
relevant_data = raw_data[raw_data$training == 0 & raw_data$closing_visible == 1, ]
relevant_data$training = NULL
relevant_data$closing_visible = NULL
relevant_data$participant = factor(relevant_data$participant)
relevant_data$condition = factor(relevant_data$condition, levels = c(1,2,3), labels = c("BL", "NonAdapt", "Adapt"))
relevant_data$lane = factor(relevant_data$lane, levels = c(4,5), labels = c("left", "right"))
# raw_data not needed anymore: 
rm(raw_data)

## add ID to blocks:
relevant_data = transform(relevant_data, run_id = ave(firstMeasure, FUN = cumsum))
relevant_data$run_id = factor(relevant_data$run_id )

## add info about overtaking
relevant_data$changing_lane = relevant_data$lane == "left" | relevant_data$lane_pos < leftBorder # sometimes they did not change far enough
relevant_data$braking =  relevant_data$ego_brake > .001 & relevant_data$ego_speed < 34 # sometimes they brake if they are much too fast, but still overtake afterwards
{
  relevant_data.split = split (relevant_data[,c("Messzeitpunkt", "run_id", "changing_lane", "braking")], f = relevant_data$run_id)
  # get start time
  relevant_data.split = lapply(relevant_data.split, FUN = function(x) {
      x = transform(x, started_lanechange = ave(changing_lane, FUN = cumsum))
      x$started_lanechange = x$started_lanechange > 0
      y = x[2:length(x$started_lanechange),]$started_lanechange != x[1:(length(x$started_lanechange)-1),]$started_lanechange
      x$started_lanechange = c(F, y)
      x$changed_lane = any(x$changing_lane)
      
      x = transform(x, started_braking = ave(braking, FUN = cumsum))
      x$started_braking = x$started_braking > 0
      y = x[2:length(x$started_braking),]$started_braking != x[1:(length(x$started_braking)-1),]$started_braking
      x$started_braking = c(F, y)
      x$braked = any(x$braking)
      
      x[,c("Messzeitpunkt", "run_id", "started_lanechange", "started_braking", "changed_lane", "braked")]
    })
  relevant_data.startDecisions = do.call("rbind", relevant_data.split)
  relevant_data = merge(relevant_data, relevant_data.startDecisions, by=c("Messzeitpunkt", "run_id"))
  rm(relevant_data.split, relevant_data.startDecisions)
  
  relevant_data$started_lanechange = relevant_data$started_lanechange > 0
  relevant_data$started_braking = relevant_data$started_braking > 0
  
  relevant_data = relevant_data[with(relevant_data, order(run_id, Messzeitpunkt)), ]
}
relevant_data$startReaction = relevant_data$started_lanechange | relevant_data$started_braking


# for some readon left_x is saved as character...
# see: View(raw_data[is.na(as.numeric(raw_data$left_X)),] ) #<-- seems to contain some NAs when car was not visible...
relevant_data$left_X = as.numeric( relevant_data$left_X )

# adding distance in m
relevant_data$distm_closing = relevant_data$ego_X - relevant_data$closing_X
relevant_data$distm_front = relevant_data$front_X - relevant_data$ego_X
relevant_data$distm_left = relevant_data$left_X - relevant_data$ego_X
relevant_data$distm_left_front = relevant_data$front_X - relevant_data$left_X 

# adding distance in seconds
relevant_data$dists_closing = relevant_data$distm_closing * 3.6 / 138 # related to speed of closing car
relevant_data$dists_front = relevant_data$distm_front / relevant_data$ego_speed # related to speed of ego car
relevant_data$dists_left = relevant_data$distm_left /  relevant_data$ego_speed # related to speed of ego car

# adding TTCs
relevant_data$ttc_closing = relevant_data$distm_closing  / ((138/3.6) - relevant_data$ego_speed) # 138/3.6 = closing speed
relevant_data$ttc_front = relevant_data$distm_front  / (relevant_data$ego_speed - (90/3.6)) # 90/3.6 = front speed
relevant_data$ttc_left = relevant_data$distm_left / (relevant_data$left_v - relevant_data$ego_speed) 

# distance to front violated if right
relevant_data$violation_front = relevant_data$dists_front < 2 & relevant_data$lane == 'right' # so far -> never
# distance to rear violated if left (or very close to)
relevant_data$violation_closing = relevant_data$dists_closing < 2 & (relevant_data$lane == 'left' | relevant_data$lane_pos < -1) # so far -> never
# distance to front left violated if left (or very close to)
relevant_data$violation_left = relevant_data$dists_left < 2 & (relevant_data$lane == 'left' | relevant_data$lane_pos < leftBorder) # so far -> never
# any violation
relevant_data$violation_any = relevant_data$violation_front | relevant_data$violation_closing | relevant_data$violation_left 


### validation
x = aggregate(cbind(braked, changed_lane, abs(lane_pos)) ~ run_id, relevant_data, max)
x$validation = (x$braked != x$changed_lane) & (x$braked == 1 | x$changed_lane == 1) & x$V3 < 2.5
sum((x$braked == x$changed_lane) & (x$changed_lane == 1))  # -> 1 --> 1 trials is both (start to change lane and brake)
sum((x$braked == x$changed_lane) & (!x$changed_lane == 1))  # -> 0 --> 0 trials is nothing (not steer nor brake)
summary(x)
wrong_data = relevant_data[relevant_data$run_id %in% x[!x$validation,]$run_id,c("ego_brake", "braked", "run_id", "lane_pos", "changed_lane", "ego_speed", "participant", "order")]
rm(x)

# all data prepared for analysis
save(relevant_data, file="relevant_data.rDa")


### remove bad measures (15 bad measures (14 because of bad measurement for lane_pos, 1 because of bad meneuver))
"%nin%" <- function(x,table) match(x,table, nomatch = 0) == 0 
relevant_data = relevant_data[relevant_data$run_id %nin% wrong_data$run_id, ]
summary(relevant_data)

### trying to find an alternative for deciding via lane_pos
#summary(relevant_data[relevant_data$ego_steer < -.05 & relevant_data$changed_lane == F , c("participant", "order")])
#x = aggregate(cbind(ego_steer,ego_accelerate)  ~ order + participant + run_id, relevant_data[relevant_data$ego_accelerate > .08 | relevant_data$ego_steer > .05,], FUN = max )
#y = aggregate(cbind(ego_steer,ego_accelerate)  ~ order + participant + run_id, relevant_data[(relevant_data$ego_accelerate > .08 | relevant_data$ego_steer > .05 ) & relevant_data$changed_lane == F,], FUN = max )
#z = relevant_data[relevant_data$run_id %in% y$run_id,]

#### Analysis (summarizing) ####
load(file="relevant_data.rDa")

## create summary
summarized_data = relevant_data[relevant_data$firstMeasure==T,c("run_id", "Messzeitpunkt")]
summarized_data$full_time = (relevant_data[relevant_data$lastMeasure==T,c("Messzeitpunkt")] - relevant_data[relevant_data$firstMeasure==T,c("Messzeitpunkt")])/1000
x = aggregate(cbind(Messzeitpunkt, startReaction) ~ run_id, relevant_data[relevant_data$startReaction == T,], FUN = min)
x = merge(x, relevant_data[relevant_data$startReaction==T,], c("run_id", "Messzeitpunkt", "startReaction"))
y = relevant_data[relevant_data$firstMeasure==T,c("Messzeitpunkt", "run_id")]
z = merge (x,y, "run_id")
z$reaction_time = (z$Messzeitpunkt.x - z$Messzeitpunkt.y) / 1000
z$Messzeitpunkt.x = NULL
z$Messzeitpunkt.y = NULL
summarized_data = merge(summarized_data, z, c("run_id"))
rm(x,y,z)

# add initial time to decision
summarized_data$time_to_decision = round((summarized_data$init_front-(2*100/3))/(100/3-25),digits=2)

# add overall accelaration(pedal)...
x = aggregate(ego_accelerate ~ run_id , relevant_data, FUN = sum)
summarized_data$ego_accelerate = NULL
summarized_data = merge(summarized_data, x, c("run_id"))
rm(x)

# add overall braking(pedal)...
x = aggregate(ego_brake ~ run_id , relevant_data, FUN = sum)
summarized_data$ego_brake = NULL
summarized_data = merge(summarized_data, x, c("run_id"))
rm(x)

# add overall steering(wheel)...
x = aggregate(ego_steer ~ run_id , relevant_data, FUN = sd)
summarized_data$ego_steer = NULL
summarized_data = merge(summarized_data, x, c("run_id"))
rm(x)

# add number of frames with violations - front (each frame represents 17ms)
x = aggregate(violation_front ~ run_id, relevant_data, FUN=sum)
summarized_data = merge(summarized_data, x, by="run_id", all = T, suffixes = c("", ".fr"))
summarized_data$violation_front.fr = summarized_data$violation_front.fr * 17 / 1000
summarized_data$violation_front_no = summarized_data$violation_front.fr > 0

# add number of frames with violations - left (each frame represents 17ms)
x = aggregate(violation_left  ~ run_id, relevant_data, FUN=sum)
summarized_data = merge(summarized_data, x, by="run_id" ,all = T, suffixes = c("", ".fr"))
summarized_data$violation_left.fr = summarized_data$violation_left.fr * 17 / 1000
summarized_data$violation_left_no = summarized_data$violation_left.fr > 0

# add number of frames with violations - rear (each frame represents 17ms)
x = aggregate(violation_closing  ~ run_id, relevant_data, FUN=sum)
summarized_data = merge(summarized_data, x, by="run_id" ,all = T, suffixes = c("", ".fr"))
summarized_data$violation_closing.fr = summarized_data$violation_closing.fr * 17 / 1000
summarized_data$violation_closing_no = summarized_data$violation_closing.fr > 0

# add number of frames with violations - any (each frame represents 17ms)
x = aggregate(violation_any  ~ run_id, relevant_data, FUN=sum)
summarized_data = merge(summarized_data, x, by="run_id" ,all = T, suffixes = c("", ".fr"))
summarized_data$violation_any.fr = summarized_data$violation_any.fr * 17 / 1000
summarized_data$violation_any_no = summarized_data$violation_any.fr > 0
rm(x)

# add questionaire data
summarized_data = merge (summarized_data, questiondata, c("participant", "condition"))

# summarize distance to closing car as factor
summarized_data$dist_closing_fac = factor(round(summarized_data$distm_closing, digits=0))

# add violation_frontrear
summarized_data$violation_frontrear = summarized_data$violation_front | summarized_data$violation_closing 

# add assumed ttc:
summarized_data$assumedTTC = round(-((summarized_data$init_front-(2*100/3))/(100/3-25)*(5)+summarized_data$init_closing)/(5),digits=1)

# add order:
summarized_data$orderPerP = NULL
summarized_data$splitFac = factor(paste(summarized_data$participant , summarized_data$condition , summarized_data$left_v , summarized_data$init_front , summarized_data$init_closing))
splittedfoo = split(summarized_data, f="splitFac")
splittedfoo = lapply(splittedfoo, FUN = function(x) {
  x = transform(x, orderPerP = ave(splitFac==splitFac, splitFac, FUN = cumsum))
})
summarized_data = unsplit(splittedfoo, f="splitFac")
summarized_data$splitFac = NULL
rm(splittedfoo)

#summary
summary(summarized_data)
save(summarized_data, file="summarized_data.rDA")
rm(relevant_data, questiondata)



### ANALYSIS II ####
load(file="summarized_data.rDA")

clean_data = summarized_data[summarized_data$changed_lane == F | summarized_data$distm_left >= 0,] # no crazy overtaking maneuvers
levels(clean_data$condition) = c("Baseline", "Constant", "Adapting")
clean_data = getCleanData(clean_data)

summary(clean_data[clean_data$braked==F, ]$dists_left)
summary(clean_data[clean_data$braked==F, ]$distm_left)
# Min. 1st Qu.   Median    Mean 3rd Qu.    Max. 
# 0.07349 0.47270 0.69780 0.76610 0.99290 2.19200 --> s
# 2.615   16.360  23.910  26.230  33.980  72.880  --> m
 
summary(clean_data[clean_data$braked==F, ]$dists_closing)
summary(clean_data[clean_data$braked==F, ]$distm_closing)

summary(clean_data$dists_front)
summary(clean_data$distm_front)


## violations raw (binomial) ####
overtaking_data = clean_data[clean_data$changed_lane==T, ]
overtaking_data = getCleanData(overtaking_data)

braking_data = clean_data[clean_data$braked==T, ]
braking_data = getCleanData(braking_data)

splitBySpeed = split(overtaking_data, f=as.factor(overtaking_data$left_v))
resultlist = lapply (splitBySpeed, FUN = function(x) {
  reportNormalData("dists_left", x) 
})

sink("dists_left.txt")
resultlist
sink()

splitBySpeed = split(clean_data, f=as.factor(clean_data$assumedTTC))
resultlist = lapply (splitBySpeed, FUN = function(x) {
  reportNormalData("dists_front", x) 
})

sink("dists_front_ttc.txt")
resultlist
sink()

splitBySpeed = split(clean_data, f=as.factor(clean_data$assumedTTC))
resultlist = lapply (splitBySpeed, FUN = function(x) {
  reportBinomialWithin("violation_left", x) 
})

sink("violation_left_ttc.txt")
resultlist
sink()



reportBinomialWithin("violation_front", clean_data)     # --> significant (Adapt < NonAdapt)
reportBinomialWithin("violation_front", ttd1600_data)     # --> significant (Adapt < NonAdapt)
reportBinomialWithin("violation_left", clean_data)      # --> not significant
reportBinomialWithin("violation_closing", clean_data)   # --> significant (Adapt < others)

x = reportBinomialWithin("violation_closing", overtaking_data)   # --> significant (Adapt < others)
x$plot + ylim(0,1) + ylab("Ratio of violations of safety gaps to the rear car") + xlab("Condition")
dev.copy2pdf(file="vio_rear.pdf",width=3,height=4.5)

x = reportBinomialWithin("violation_frontrear", clean_data) # --> significant Adapt less than both others
x$plot + ylim(0,1) + ylab("Ratio of violations of safety gaps to front or rear car") + xlab("Condition")
dev.copy2pdf(file="vio_frontrear.pdf",width=3,height=4.5)

x = reportBinomialWithin("violation_any", clean_data)       # --> significant (both less)
x$plot + ylim(0,1) + ylab("Ratio of violations of safety gaps to any other car") + xlab("Condition")
dev.copy2pdf(file="vio_any.pdf",width=3,height=4.5)

## behavioural ####
# full time
x = reportNormalData("reaction_time", clean_data)     # Significant (Adapt < Both)
x$plot + ylim(8,9.5) +ylab("Time to decision (s)") + xlab("Condition")
dev.copy2pdf(file="ttd.pdf",width=3,height=4.5)

reportNormalData("reaction_time", overtaking_data)     # Significant (Adapt < Both)
reportNormalData("reaction_time", braking_data)     # Significant (Adapt < Both)
#reportNormalData("ego_accelerate", clean_data)    # Significant, but Sphericity violated (nonadapt < baseline)
#reportNormalData("ego_brake", clean_data)         # not significant
x = reportNormalData("dists_front", clean_data)       # significant (Adapt > Both)
x$plot + ylim(0.5,2.5) +ylab("Distance to front car (s)") + xlab("Condition")
dev.copy2pdf(file="dist_front.pdf",width=3,height=4.5)

# overtaking / braking
reportNormalData("reaction_time", overtaking_data)  # significant ttests, not anova (Adapt < BL < nonAdapt) -> but graphically not a big difference to without filter
reportNormalData("reaction_time", clean_data[clean_data$braked==T,])        # significant ttests & anova (BL > both)
reportNormalData("dists_front", overtaking_data)    # significant ttests, not anova (Adapt > BL > NonAdapt)
reportNormalData("dists_front", overtaking_data)    # significant ttests, not anova (Adapt > BL > NonAdapt)
reportNormalData("dists_front", clean_data[clean_data$braked==T,])          # significant ttests & anova (BL < both)
x = reportNormalData("dists_closing", overtaking_data)  # significant ttests, not anova (Adapt > both)
x$plot + ylim(0.5,2.5) +ylab("Distance to rear car (s)") + xlab("Condition")
dev.copy2pdf(file="dist_rear.pdf",width=3,height=4.5)

x = reportNormalData("dists_left",  overtaking_data)    # significant ttests, not anova (NonAdapt > Both)
x$plot + ylim(0.5,2.5) +ylab("Distance to left car (s)") + xlab("Condition")
# NonAdapt is bigger for left distances and reaction time --> took more time to decide
dev.copy2pdf(file="dist_left.pdf",width=3,height=4.5)

## questionaire ####
library(reshape2)
short_table = aggregate(cbind(violation_front_no,violation_left_no, violation_closing_no, violation_any_no) ~ condition + participant + SatisfiedWithPerformance + DecisionDifficult + Distracted +RightDecision + glasses + gender + age + kmPerYear + licenceSince, summarized_data , sum)
reportNonNormalData("violation_any_no", short_table)
short_table$satisfaction = as.numeric(short_table$SatisfiedWithPerformance) 
reportNonNormalData("satisfaction", (short_table))         # --> not significant (around 4)
short_table$difficulty = as.numeric(short_table$DecisionDifficult) 
reportNonNormalData("difficulty", (short_table))                   # --> not significant (around 2)
short_table$rightDec = as.numeric(short_table$RightDecision)
reportNonNormalData("rightDec", (short_table))                     # --> not significant (around 4)

short_table$distraction = as.numeric(short_table$Distracted)
data_wide <- dcast(short_table, participant ~ condition, value.var="distraction")
data_matrix <- as.matrix(data_wide[,c( "NonAdapt", "Adapt")])
friedman.test(data_matrix) # --> not significant (around 2.2)
rm(data_wide,data_matrix, short_table)

#### plots ####

boxplot(dists_front ~ condition + braked.x, clean_data)
boxplot(dists_closing ~ condition + braked.x, clean_data)
boxplot(dists_left ~ condition + braked.x, clean_data)

ggplot(clean_data, aes(x=time_to_decision, y=as.numeric(violation_frontrear), color=condition))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess")+
  xlab("Time to Decision")+
  ylab("Probability of violating distance to front or rear car")+
  theme_bw()

ggplot(clean_data[clean_data$changed_lane==T,], aes(x=distm_closing, y=as.numeric(violation_front), color=condition))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess")+
  xlab("Time to Decision")+
  ylab("Probability of violating distance to front or rear car")+
  theme_bw()

ggplot(clean_data, aes(x=time_to_decision, y=as.numeric(violation_any), color=condition))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess")+
  xlab("Time to Decision")+
  ylab("Probability of violating distance to any car")+
  theme_bw()

ggplot(clean_data, aes(x=time_to_decision, y=as.numeric(violation_closing), color=condition))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess")+
  xlab("Time to Decision")+
  ylab("Probability of violating distance to rear car")+
  theme_bw()

ggplot(clean_data, aes(x=dists_closing, y=as.numeric(changed_lane), color=condition))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess")+
  xlab("Distance to rear car (seconds)")+
  ylab("Probability of changing lane")+
  theme_bw()


ggplot(clean_data, aes(x=dists_left, y=as.numeric(changed_lane), color=condition))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess")+
  xlab("Distance to left car (seconds)")+
  ylab("Probability of changing lane")+
  theme_bw()


ggplot(clean_data, aes(x=distm_closing, y=as.numeric(violation_any), color=condition))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess")+
  xlab("Distance to rear car (meter)")+
  ylab("Probability of violating safety gap to any car")+
  theme_bw()

ggplot(clean_data, aes(x=dists_front, y=as.numeric(braked), color=condition))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess")+
  xlab("Distance to rear car (meter)")+
  ylab("Probability of violating safety gap to front car")+
  theme_bw()


## cool :D
ggplot(clean_data, aes(x=time_to_decision, y=as.numeric(reaction_time), color=condition))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess")+
  xlab("Assumed Time to Decision")+
  ylab("Time until Maneuvre")+
  geom_abline(intercept = 0)+
  theme_bw()


ggplot(clean_data, aes(x=assumedTTC, y=as.numeric(ego_accelerate), color=condition))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess")+
  xlab("Distance to the closing car")+
  ylab("Summed accelaration in maneuver")+
  theme_bw()

# also okish...
ggplot(clean_data, aes(x=time_to_decision, y=as.numeric(dists_front), color=condition))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess")+
  xlab("Distance to the closing car")+
  ylab("Summed accelaration in maneuver")+
  theme_bw()


ggplot(clean_data[clean_data$braked==T, ], aes(x=time_to_decision, y=as.numeric(reaction_time), color=condition))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess")+
  xlab("Assumed Time to Decision")+
  ylab("Time until Maneuvre")+
  theme_bw()


#### linear models ####





#from Fei:
# par(mfrow=c(3,3))
# lapply(data2.split, FUN=function(x) {
#   interaction.plot(x$ttc,x$behind_vdiff, x$certainty, type="b", col=c(1:3), 
#                    leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
#                    xlab="TTC", ylab="Mean Certainty Scores")
#   
#   
#   
# })


interaction.plot(summarized_data$time_to_decision,summarized_data$condition, summarized_data$violation_frontrear, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="time_to_decision", ylab="violation_frontrear", ylim=c(0,1))



interaction.plot(summarized_data$assumedTTC,summarized_data$condition, summarized_data$violation_frontrear, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="assumedTTC", ylab="violation_frontrear", ylim=c(0,1))


interaction.plot(cut(summarized_data[summarized_data$dists_closing < 2,]$dists_closing, breaks = c(0,.75,1,1.25,1.5,1.75,2.0)),summarized_data[summarized_data$dists_closing < 2,]$condition, summarized_data[summarized_data$dists_closing < 2,]$changed_lane, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="dists_closing", ylab="changed_lane", ylim=c(0,1))


interaction.plot(cut(summarized_data[summarized_data$dists_left < 2,]$dists_left, breaks = c(0,.75,1,1.25,1.5,1.75,2.0)),summarized_data[summarized_data$dists_left < 2,]$condition, summarized_data[summarized_data$dists_left < 2,]$changed_lane, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="dists_left", ylab="changed_lane", ylim=c(0,1))


# only overtakings
overtakings = summarized_data[summarized_data$changed_lane==T,]
interaction.plot(overtakings$time_to_decision,overtakings$condition, overtakings$violation_frontrear, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="time_to_decision", ylab="violation_frontrear", ylim=c(0,1))

interaction.plot(overtakings$time_to_decision,overtakings$condition, overtakings$violation_any, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="time_to_decision", ylab="violation_any", ylim=c(0,1))


interaction.plot(overtakings$time_to_decision,overtakings$condition, overtakings$dists_closing, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="time_to_decision", ylab="dists_closing")
interaction.plot(overtakings$assumedTTC,overtakings$condition, overtakings$dists_closing, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="assumedTTC", ylab="dists_closing")

summarized_data$realTTC = summarized_data$distm_closing / 5
interaction.plot(overtakings$assumedTTC,overtakings$condition, overtakings$braked, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="assumedTTC", ylab="braked", ylim=c(0,.1))

interaction.plot(overtakings$time_to_decision,overtakings$condition, overtakings$braked, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="time_to_decision", ylab="braked", ylim=c(0,.1))


interaction.plot(overtakings$time_to_decision,overtakings$condition, overtakings$dists_front, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="time_to_decision", ylab="dists_front")

rm(overtakings)


### ANOVA

#see http://www.uni-kiel.de/psychologie/rexrepos/posts/anovaMixed.html#multiple-comparisons-using-glht-from-package-multcomp
library(nlme)
library(multcomp)
library(ggplot2)
library(mgcv)
ggplot(summarized_data, aes(distm_closing, as.numeric(violation_frontrear), colour= condition)) + stat_smooth() 
#ggplot(summarized_data, aes(distm_closing, as.numeric(reaction_time), colour= condition)) + stat_smooth() 
#ggplot(summarized_data, aes(distm_closing, as.numeric(full_time), colour= condition)) + stat_smooth() 
#ggplot(summarized_data, aes(distm_closing, as.numeric(full_time), colour= condition)) + stat_smooth() 
#ggplot(summarized_data, aes(distm_closing, as.numeric(dists_front), colour= condition)) + stat_smooth() 
#ggplot(summarized_data, aes(distm_closing, as.numeric(dists_closing), colour= condition)) + stat_smooth() 
#ggplot(summarized_data, aes(distm_closing, as.numeric(dists_left), colour= condition)) + stat_smooth() 
ggplot(summarized_data, aes(as.numeric(time_to_decision), as.numeric(braked), colour= condition)) + stat_smooth() 
ggplot(summarized_data, aes(as.numeric(time_to_decision), as.numeric(changed_lane), colour= condition)) + stat_smooth() 
ggplot(summarized_data, aes(as.numeric(time_to_decision), as.numeric(violation_frontrear), colour= condition)) + stat_smooth() 
ggplot(summarized_data, aes(as.numeric(time_to_decision), as.numeric(violation_any), colour= condition)) + stat_smooth() 

#ggplot(summarized_data[summarized_data$time_to_decision>2.5,], aes(as.numeric(time_to_decision), as.numeric(violation_frontrear), colour= condition)) + stat_smooth() 
