# Load in data from Matlab
# setwd("C:/Users/FPF/Desktop/fef_ready/data/mainstudy/")
setwd("//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/script/mainstudy/")
# data <- read.csv(file="C:/Users/FPF/Desktop/fef_ready/data/mainstudy/ERP_data_for_R_with_LeftRight.csv", header=F, sep=",", dec=".")
data <- read.csv(file="//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/data/mainstudy/ERP_data_for_R_with_LeftRight.csv", header=F, sep=",", dec=".")
data <- as.data.frame(data)

####################################################################################
# define column order:
# subject's indices:
# 01st: Subject;
# variables:
# 02nd: Early; % mean amplitude [C3 Cz C4]
# 03rd: Late; % mean amplitude [C3 Cz C4]
# 04th: LateMinusEarly; % mean amplitude [C3 Cz C4]
# factors:
# 05th: Lateralization % C3: 0; Cz: 1; C4: 2;
# 06th: LeftRight % Left: 0; Right: 1;
# 07th: SimpleComplex % Simple: 0; Complex: 1;
# 08th: SeatedWalking % Seated: 0; Walking: 1;
# 09th: SeqCorrect % percentage sequence correctness
# 10th: SeqDuration % mean duration of sequences
# 11th: IntDuration % mean duration of responses
####################################################################################

# transform data to ANOVA factors
colnames(data) <- c("Subject", "Early", "Late", "LateMinusEarly",
                    "Lateralization", "LeftRight", "SimpleComplex", "SeatedWalking",
                    "SeqCorrect", "SeqDuration", "IntDuration", "RP_Complete")

data$Lateralization[data$Lateralization == 0] <- "C3"
data$Lateralization[data$Lateralization == 1] <- "Cz"
data$Lateralization[data$Lateralization == 2] <- "C4"

data$LeftRight[data$LeftRight == 0] <- "Left"
data$LeftRight[data$LeftRight == 1] <- "Right"

data$SimpleComplex[data$SimpleComplex == 0] <- "Simple"
data$SimpleComplex[data$SimpleComplex == 1] <- "Complex"

data$SeatedWalking[data$SeatedWalking == 0] <- "Seated"
data$SeatedWalking[data$SeatedWalking == 1] <- "Walking"

####################################################################################
# ERP analysis for the factors Lateralization and LeftRight
####################################################################################

# ANOVA for Early and Lateralization, LeftRight
library("ez", lib.loc="~/R/win-library/3.1")
ANOVA_Early <- ezANOVA(data
                       , Early
                       , Subject
                       , within = .(Lateralization,LeftRight)
                       , type = 2
                       , detailed = F
                       , within_full = .(SimpleComplex,SeatedWalking)
)
# capture.output(ANOVA_Early,file="C:/Users/FPF/Desktop/fef_ready/results/ANOVA_Early_Lateralization_LeftRight.xls")
capture.output(ANOVA_Early,file="//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/results/ANOVA_Early_Lateralization_LeftRight.xls")

meanLateralization <- tapply(data$Early, data$Lateralization, mean)
seLateralization <- (tapply(data$Early, data$Lateralization, sd))/sqrt(16)
CIearlyLateralization <- 1.96*seLateralization

meanLeftRight <- tapply(data$Early, data$LeftRight, mean)
seLeftRight <- (tapply(data$Early, data$LeftRight, sd))/sqrt(16)
CIearlyLeftRight <- 1.96*seLeftRight

# ANOVA for Late and Lateralization, LeftRight
library("ez", lib.loc="~/R/win-library/3.1")
ANOVA_Late <- ezANOVA(data
                       , Late
                       , Subject
                       , within = .(Lateralization,LeftRight)
                       , type = 2
                       , detailed = F
                       , within_full = .(SimpleComplex,SeatedWalking)
)
# capture.output(ANOVA_Late,file="C:/Users/FPF/Desktop/fef_ready/results/ANOVA_Late_Lateralization_LeftRight.xls")
capture.output(ANOVA_Late,file="//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/results/ANOVA_Late_Lateralization_LeftRight.xls")

meanLateralization <- tapply(data$Late, data$Lateralization, mean)
seLateralization <- (tapply(data$Late, data$Lateralization, sd))/sqrt(16)
CIlateLateralization <- 1.96*seLateralization

meanLeftRight <- tapply(data$Late, data$LeftRight, mean)
seLeftRight <- (tapply(data$Late, data$LeftRight, sd))/sqrt(16)
CIlateLeftRight <- 1.96*seLeftRight

# ANOVA for Complete and Lateralization, LeftRight
library("ez", lib.loc="~/R/win-library/3.1")
ANOVA_Complete <- ezANOVA(data
                      , RP_Complete
                      , Subject
                      , within = .(Lateralization,LeftRight)
                      , type = 2
                      , detailed = F
                      , within_full = .(SimpleComplex,SeatedWalking)
)
# capture.output(ANOVA_Complete,file="C:/Users/FPF/Desktop/fef_ready/results/ANOVA_Late_Lateralization_Complete.xls")
capture.output(ANOVA_Complete,file="//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/results/ANOVA_Late_Lateralization_Complete.xls")

meanLateralization <- tapply(data$RP_Complete, data$Lateralization, mean)
seLateralization <- (tapply(data$RP_Complete, data$Lateralization, sd))/sqrt(16)
CIRP_CompleteLateralization <- 1.96*seLateralization

meanLeftRight <- tapply(data$RP_Complete, data$LeftRight, mean)
seLeftRight <- (tapply(data$RP_Complete, data$LeftRight, sd))/sqrt(16)
CIRP_CompleteLeftRight <- 1.96*seLeftRight


####################################################################################
# Taking the mean for LeftRight - rejecting the factor
####################################################################################

# Load in data from Matlab
# setwd("C:/Users/FPF/Desktop/fef_ready/data/mainstudy/")
setwd("//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/script/mainstudy/")
# data <- read.csv(file="C:/Users/FPF/Desktop/fef_ready/data/mainstudy/ERP_data_for_R_without_LeftRight.csv", header=F, sep=",", dec=".")
data <- read.csv(file="//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/data/mainstudy/ERP_data_for_R_without_LeftRight.csv", header=F, sep=",", dec=".")
data <- as.data.frame(data)

####################################################################################
# define column order:
# subject's indices:
# 01st: Subject;
# variables:
# 02nd: Early; % mean amplitude [IPSI CENTRAL CONTRA]
# 03rd: Late; % mean amplitude [IPSI CENTRAL CONTRA]
# 04th: LateMinusEarly; % mean amplitude [IPSI CENTRAL CONTRA]
# factors:
# 05th: Lateralization % IPSI: 0; CENTRAL: 1; CONTRA: 2;
# 06th: SimpleComplex % Simple: 0; Complex: 1;
# 07th: SeatedWalking % Seated: 0; Walking: 1;
# 08th: SeqCorrect % percentage sequence correctness
# 09th: SeqDuration % mean duration of sequences
# 10th: IntDuration % mean duration of responses
####################################################################################

# transform data to ANOVA factors
colnames(data) <- c("Subject", "Early", "Late", "LateMinusEarly",
                    "Lateralization", "SimpleComplex", "SeatedWalking",
                    "SeqCorrect", "SeqDuration", "IntDuration")

data$Lateralization[data$Lateralization == 0] <- "IPSI"
data$Lateralization[data$Lateralization == 1] <- "CENTRAL"
data$Lateralization[data$Lateralization == 2] <- "CONTRA"

data$SimpleComplex[data$SimpleComplex == 0] <- "Simple"
data$SimpleComplex[data$SimpleComplex == 1] <- "Complex"

data$SeatedWalking[data$SeatedWalking == 0] <- "Seated"
data$SeatedWalking[data$SeatedWalking == 1] <- "Walking"

####################################################################################
# Creating subsets of data for further analyses
####################################################################################

Simple <- subset(data, SimpleComplex == "Simple")
Complex <- subset(data, SimpleComplex == "Complex")

ComplexIPSI <- subset(Complex, Lateralization == "IPSI")
ComplexCENTRAL <- subset(Complex, Lateralization == "CENTRAL")
ComplexCONTRA <- subset(Complex, Lateralization == "CONTRA")

SimpleSeated <- subset(Simple, SeatedWalking == "Seated")
SimpleWalking <- subset(Simple, SeatedWalking == "Walking")

ComplexSeated <- subset(Complex, SeatedWalking == "Seated")
ComplexWalking <- subset(Complex, SeatedWalking == "Walking")

SimpleSeatedIPSI <- subset(SimpleSeated, Lateralization == "IPSI")
SimpleSeatedCENTRAL <- subset(SimpleSeated, Lateralization == "CENTRAL")
SimpleSeatedCONTRA <- subset(SimpleSeated, Lateralization == "CONTRA")

SimpleWalkingIPSI <- subset(SimpleWalking, Lateralization == "IPSI")
SimpleWalkingCENTRAL <- subset(SimpleWalking, Lateralization == "CENTRAL")
SimpleWalkingCONTRA <- subset(SimpleWalking, Lateralization == "CONTRA")

ComplexSeatedIPSI <- subset(ComplexSeated, Lateralization == "IPSI")
ComplexSeatedCENTRAL <- subset(ComplexSeated, Lateralization == "CENTRAL")
ComplexSeatedCONTRA <- subset(ComplexSeated, Lateralization == "CONTRA")

ComplexWalkingIPSI <- subset(ComplexWalking, Lateralization == "IPSI")
ComplexWalkingCENTRAL <- subset(ComplexWalking, Lateralization == "CENTRAL")
ComplexWalkingCONTRA <- subset(ComplexWalking, Lateralization == "CONTRA")

####################################################################################
# Analyses of SeqCorrect, SeqDuration and IntDuration
####################################################################################

## Sequence Correctness
t.test(ComplexSeatedCONTRA$SeqCorrect,ComplexWalkingCONTRA$SeqCorrect, var.equal=TRUE, paired=TRUE)

mean(ComplexSeatedCONTRA$SeqCorrect)
mean(ComplexWalkingCONTRA$SeqCorrect)
1.96*sd(ComplexSeatedCONTRA$SeqCorrect)/sqrt(16)
1.96*sd(ComplexWalkingCONTRA$SeqCorrect)/sqrt(16)

## Sequence Duration
t.test(ComplexSeatedCONTRA$SeqDuration,ComplexWalkingCONTRA$SeqDuration, var.equal=TRUE, paired=TRUE)

mean(ComplexSeatedCONTRA$SeqDuration)
mean(ComplexWalkingCONTRA$SeqDuration)
1.96*sd(ComplexSeatedCONTRA$SeqDuration)/sqrt(16)
1.96*sd(ComplexWalkingCONTRA$SeqDuration)/sqrt(16)

## Response Duration
tapply(data$IntDuration, list(data$SeatedWalking, data$SimpleComplex), mean)

# ANOVA
library("ez", lib.loc="~/R/win-library/3.1")
ANOVA_ResponseTimes <- ezANOVA(data
        , IntDuration
        , Subject
        , within = .(SimpleComplex,SeatedWalking)
        , type = 2
        , detailed = F
)
# capture.output(ANOVA_ResponseTimes,file="C:/Users/FPF/Desktop/fef_ready/results/ANOVA_ResponseTimes.xls")
capture.output(ANOVA_ResponseTimes,file="//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/results/ANOVA_ResponseTimes.xls")

mean <- tapply(data$IntDuration, data$SimpleComplex, mean)
se <- (tapply(data$IntDuration, data$SimpleComplex, sd))/sqrt(16)
CI <- 1.96*se

mean <- tapply(data$IntDuration, data$SeatedWalking, mean)
se <- (tapply(data$IntDuration, data$SeatedWalking, sd))/sqrt(16)
CI <- 1.96*se

# post-hoc-analysis
t.test(SimpleSeatedCONTRA$IntDuration,SimpleWalkingCONTRA$IntDuration, var.equal=TRUE, paired=TRUE)
t.test(ComplexSeatedCONTRA$IntDuration,ComplexWalkingCONTRA$IntDuration, var.equal=TRUE, paired=TRUE)

# write.csv(as.matrix(ANOVA_ResponseTimes), file = "C:/Users/FPF/Desktop/fef_ready/results/ANOVA_ResponseTimes.csv", na = "")

####################################################################################
# Early without Lateralization and LeftRight
####################################################################################

CONTRA <- subset(data, Lateralization == "CONTRA")

# ANOVA
library("ez", lib.loc="~/R/win-library/3.1")
ANOVA_Early <- ezANOVA(CONTRA
                       , Early
                       , Subject
                       , within = .(SimpleComplex,SeatedWalking)
                       , type = 2
                       , detailed = F
)
# capture.output(ANOVA_Early,file="C:/Users/FPF/Desktop/fef_ready/results/ANOVA_Early_SimpleComplex_SeatedWalking.xls")
capture.output(ANOVA_Early,file="//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/results/ANOVA_Early_SimpleComplex_SeatedWalking.xls")

mean <- tapply(CONTRA$Early, CONTRA$SimpleComplex, mean)
se <- (tapply(CONTRA$Early, CONTRA$SimpleComplex, sd))/sqrt(16)
CI <- 1.96*se

mean <- tapply(CONTRA$Early, CONTRA$SeatedWalking, mean)
se <- (tapply(CONTRA$Early, CONTRA$SeatedWalking, sd))/sqrt(16)
CI <- 1.96*se

# post-hoc-analysis
t.test(SimpleSeatedCONTRA$Early,SimpleWalkingCONTRA$Early, var.equal=TRUE, paired=TRUE)
t.test(ComplexSeatedCONTRA$Early,ComplexWalkingCONTRA$Early, var.equal=TRUE, paired=TRUE)

####################################################################################
# Late without Lateralization and LeftRight
####################################################################################

CONTRA <- subset(data, Lateralization == "CONTRA")

# ANOVA
library("ez", lib.loc="~/R/win-library/3.1")
ANOVA_Late <- ezANOVA(CONTRA
                      , Late
                      , Subject
                      , within = .(SimpleComplex,SeatedWalking)
                      , type = 2
                      , detailed = F
)
# capture.output(ANOVA_Late,file="C:/Users/FPF/Desktop/fef_ready/results/ANOVA_Late_SimpleComplex_SeatedWalking.xls")
capture.output(ANOVA_Late,file="//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/results/ANOVA_Late_SimpleComplex_SeatedWalking.xls")

mean <- tapply(CONTRA$Late, CONTRA$SimpleComplex, mean)
se <- (tapply(CONTRA$Late, CONTRA$SimpleComplex, sd))/sqrt(16)
CI <- 1.96*se

mean <- tapply(CONTRA$Late, CONTRA$SeatedWalking, mean)
se <- (tapply(CONTRA$Late, CONTRA$SeatedWalking, sd))/sqrt(16)
CI <- 1.96*se

# post-hoc-analysis
t.test(SimpleSeatedCONTRA$Late,SimpleWalkingCONTRA$Late, var.equal=TRUE, paired=TRUE)
t.test(ComplexSeatedCONTRA$Late,ComplexWalkingCONTRA$Late, var.equal=TRUE, paired=TRUE)

####################################################################################
# LateMinusEarly without Lateralization and LeftRight
####################################################################################

# ANOVA
library("ez", lib.loc="~/R/win-library/3.1")
ANOVA_LateMinusEarly <- ezANOVA(CONTRA
                                , LateMinusEarly
                                , Subject
                                , within = .(SimpleComplex,SeatedWalking)
                                , type = 2
                                , detailed = F
)
# capture.output(ANOVA_LateMinusEarly,file="C:/Users/FPF/Desktop/fef_ready/results/ANOVA_LateMinusEarly_SimpleComplex_SeatedWalking.xls")
capture.output(ANOVA_LateMinusEarly,file="//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/results/ANOVA_LateMinusEarly_SimpleComplex_SeatedWalking.xls")

tapply(CONTRA$LateMinusEarly, CONTRA$SimpleComplex, mean)
tapply(CONTRA$LateMinusEarly, CONTRA$SeatedWalking, mean)

####################################################################################
# Correlations for Complex and SeqDuration Early
####################################################################################

# IPSI
cor(ComplexIPSI$SeqDuration,ComplexIPSI$Early)
boxplot(ComplexIPSI$SeqDuration,ComplexIPSI$Early)

plot(ComplexIPSI$SeqDuration,ComplexIPSI$Early)
LateMinusEarly_lmfit_IPSI = lm(ComplexIPSI$Early ~ ComplexIPSI$SeqDuration)
summary(LateMinusEarly_lmfit_IPSI)
abline(lm(ComplexIPSI$Early ~ ComplexIPSI$SeqDuration), col="blue")

# CENTRAL
cor(ComplexCENTRAL$SeqDuration,ComplexCENTRAL$Early)
boxplot(ComplexCENTRAL$SeqDuration,ComplexCENTRAL$Early)

plot(ComplexCENTRAL$SeqDuration,ComplexCENTRAL$Early)
LateMinusEarly_lmfit_CENTRAL = lm(ComplexCENTRAL$Early ~ ComplexCENTRAL$SeqDuration)
summary(LateMinusEarly_lmfit_CENTRAL)
abline(lm(ComplexCENTRAL$Early ~ ComplexCENTRAL$SeqDuration), col="blue")

# CONTRA
cor(ComplexCONTRA$SeqDuration,ComplexCONTRA$Early)
boxplot(ComplexSeatedCONTRA$SeqDuration,ComplexSeatedCONTRA$Early)

plot(ComplexCONTRA$SeqDuration,ComplexCONTRA$Early)
LateMinusEarly_lmfit_CONTRA = lm(ComplexCONTRA$Early ~ ComplexCONTRA$SeqDuration)
summary(LateMinusEarly_lmfit_CONTRA)
abline(lm(ComplexCONTRA$Early ~ ComplexCONTRA$SeqDuration), col="blue")

####################################################################################
# Correlations for Complex and SeqDuration Late
####################################################################################

# IPSI
cor(ComplexIPSI$SeqDuration,ComplexIPSI$Late)
boxplot(ComplexIPSI$SeqDuration,ComplexIPSI$Late)

plot(ComplexIPSI$SeqDuration,ComplexIPSI$Late)
LateMinusLate_lmfit_IPSI = lm(ComplexIPSI$Late ~ ComplexIPSI$SeqDuration)
summary(LateMinusLate_lmfit_IPSI)
abline(lm(ComplexIPSI$Late ~ ComplexIPSI$SeqDuration), col="blue")

# CENTRAL
cor(ComplexCENTRAL$SeqDuration,ComplexCENTRAL$Late)
boxplot(ComplexCENTRAL$SeqDuration,ComplexCENTRAL$Late)

plot(ComplexCENTRAL$SeqDuration,ComplexCENTRAL$Late)
LateMinusLate_lmfit_CENTRAL = lm(ComplexCENTRAL$Late ~ ComplexCENTRAL$SeqDuration)
summary(LateMinusLate_lmfit_CENTRAL)
abline(lm(ComplexCENTRAL$Late ~ ComplexCENTRAL$SeqDuration), col="blue")

# CONTRA
cor(ComplexCONTRA$SeqDuration,ComplexCONTRA$Late)
boxplot(ComplexSeatedCONTRA$SeqDuration,ComplexSeatedCONTRA$Late)

plot(ComplexCONTRA$SeqDuration,ComplexCONTRA$Late)
LateMinusLate_lmfit_CONTRA = lm(ComplexCONTRA$Late ~ ComplexCONTRA$SeqDuration)
summary(LateMinusLate_lmfit_CONTRA)
abline(lm(ComplexCONTRA$Late ~ ComplexCONTRA$SeqDuration), col="blue")

## optional outlier analysis for the correlations
#
#NegativcriteriaIPSI <- mean(ComplexSeatedIPSI$SeqDuration) - 2.5*sd(ComplexSeatedIPSI$SeqDuration)
#PositivcriteriaIPSI <- mean(ComplexSeatedIPSI$SeqDuration) + 2.5*sd(ComplexSeatedIPSI$SeqDuration)
#ComplexSeatedIPSI <- subset(ComplexSeatedIPSI,ComplexSeatedIPSI$SeqDuration > NegativcriteriaIPSI & ComplexSeatedIPSI$SeqDuration < PositivcriteriaIPSI)
#
#NegativcriteriaCENTRAL <- mean(ComplexSeatedCENTRAL$SeqDuration) - 2.5*sd(ComplexSeatedCENTRAL$SeqDuration)/4
#PositivcriteriaCENTRAL <- mean(ComplexSeatedCENTRAL$SeqDuration) + 2.5*sd(ComplexSeatedCENTRAL$SeqDuration)/4
#ComplexSeatedCENTRAL <- subset(ComplexSeatedCENTRAL,ComplexSeatedCENTRAL$SeqDuration > NegativcriteriaCENTRAL & ComplexSeatedCENTRAL$SeqDuration < PositivcriteriaCENTRAL)
#
#NegativcriteriaCONTRA <- mean(ComplexSeatedCONTRA$SeqDuration) - 2.5*sd(ComplexSeatedCONTRA$SeqDuration)/4
#PositivcriteriaCONTRA <- mean(ComplexSeatedCONTRA$SeqDuration) + 2.5*sd(ComplexSeatedCONTRA$SeqDuration)/4
#ComplexSeatedCONTRA <- subset(ComplexSeatedCONTRA,ComplexSeatedCONTRA$SeqDuration > NegativcriteriaCONTRA & ComplexSeatedCONTRA$SeqDuration < PositivcriteriaCONTRA)

####################################################################################
# CorrelationIRTandLATE&Early
####################################################################################

# CONTRA

CONTRA <- subset(data, Lateralization == "CONTRA")

cor(CONTRA$IntDuration,CONTRA$Early)
boxplot(CONTRA$IntDuration,CONTRA$Early)

cor(CONTRA$IntDuration,CONTRA$Late)
boxplot(CONTRA$IntDuration,CONTRA$Late)

CONTRASeatedSimple <- subset(CONTRA, SeatedWalking == "Seated" & SimpleComplex == "Simple")
CONTRAWalkingSimple  <- subset(CONTRA, SeatedWalking == "Walking" & SimpleComplex == "Simple")
CONTRASeatedComplex  <- subset(CONTRA, SeatedWalking == "Seated" & SimpleComplex == "Complex")
CONTRAWalkingComplex  <- subset(CONTRA, SeatedWalking == "Walking" & SimpleComplex == "Complex")

CONTRASeatedSimple$Colour = "chocolate1"
CONTRAWalkingSimple$Colour = "red3"
CONTRASeatedComplex$Colour = "royalblue1"
CONTRAWalkingComplex$Colour = "blue2"

CONTRA <- rbind(CONTRASeatedSimple, CONTRAWalkingSimple, CONTRASeatedComplex, CONTRAWalkingComplex)

##start the graphics device driver and save the figure as eps
#postscript(file="//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/figures/mainstudy/Correlation_IRT_Late.eps",
#           paper="special",
#           width=4,
#           height=4,
#           horizontal=F)

par(mfrow=c(1,2))

# Early
plot(CONTRA$IntDuration,CONTRA$Early,bg=CONTRA$Colour, pch=21, cex=1,
     main="contralateral, early RP accross IRT",
     xlab="IRT (sec)",
     ylab = expression(paste("early RP amplitude ( ", mu, "V)")),
     xlim = c(2,12), ylim = c(5,-10))
Early_lmfit_CONTRA_IntDuration = lm(CONTRA$Early ~ CONTRA$IntDuration)
summary(Early_lmfit_CONTRA_IntDuration)
abline(lm(CONTRA$Early ~ CONTRA$IntDuration), col="black", lwd=4)

# add legend
legend(x="topleft",bty="n", legend=c("Simple Seated", "Simple Walking", "Complex Seated", "Complex Walking"),
       fill=c("chocolate1","red3","royalblue1","blue2", bg=CONTRA$Colour, pch=21))

# alternative models
Early_lmfit_CONTRA_IntDuration_1 = lm(CONTRA$Early ~ CONTRA$IntDuration + CONTRA$SimpleComplex)
summary(Early_lmfit_CONTRA_IntDuration_1)
Early_lmfit_CONTRA_IntDuration_2 = lm(CONTRA$Early ~ CONTRA$IntDuration + CONTRA$SeatedWalking)
summary(Early_lmfit_CONTRA_IntDuration_2)
Early_lmfit_CONTRA_IntDuration_3 = lm(CONTRA$Early ~ CONTRA$IntDuration + CONTRA$SimpleComplex + CONTRA$SeatedWalking)
summary(Early_lmfit_CONTRA_IntDuration_3)
Early_lmfit_CONTRA_IntDuration_4 = lm(CONTRA$Early ~ CONTRA$IntDuration + CONTRA$SimpleComplex*CONTRA$SeatedWalking)
summary(Early_lmfit_CONTRA_IntDuration_4)

anova(Early_lmfit_CONTRA_IntDuration, Early_lmfit_CONTRA_IntDuration_1, test="F")
anova(Early_lmfit_CONTRA_IntDuration, Early_lmfit_CONTRA_IntDuration_2, test="F")
anova(Early_lmfit_CONTRA_IntDuration, Early_lmfit_CONTRA_IntDuration_3, test="F")
anova(Early_lmfit_CONTRA_IntDuration, Early_lmfit_CONTRA_IntDuration_4, test="F")

# Late
plot(CONTRA$IntDuration,CONTRA$Late,bg=CONTRA$Colour, pch=21, cex=1,
     main="contralateral, late RP accross IRT",
     xlab="IRT (sec)",
     ylab = expression(paste("late RP amplitude ( ", mu, "V)")),
     xlim = c(2,12), ylim = c(5,-10))
Late_lmfit_CONTRA_IntDuration = lm(CONTRA$Late ~ CONTRA$IntDuration)
summary(Late_lmfit_CONTRA_IntDuration)
abline(lm(CONTRA$Late ~ CONTRA$IntDuration), col="black", lwd=4)

# add legend
legend(x="topleft",bty="n", legend=c("Simple Seated", "Simple Walking", "Complex Seated", "Complex Walking"),
       fill=c("chocolate1","red3","royalblue1","blue2", bg=CONTRA$Colour, pch=21))

# alternative models
Late_lmfit_CONTRA_IntDuration_1 = lm(CONTRA$Late ~ CONTRA$IntDuration + CONTRA$SimpleComplex)
summary(Late_lmfit_CONTRA_IntDuration_1)
Late_lmfit_CONTRA_IntDuration_2 = lm(CONTRA$Late ~ CONTRA$IntDuration + CONTRA$SeatedWalking)
summary(Late_lmfit_CONTRA_IntDuration_2)
Late_lmfit_CONTRA_IntDuration_3 = lm(CONTRA$Late ~ CONTRA$IntDuration + CONTRA$SimpleComplex + CONTRA$SeatedWalking)
summary(Late_lmfit_CONTRA_IntDuration_3)
Late_lmfit_CONTRA_IntDuration_4 = lm(CONTRA$Late ~ CONTRA$IntDuration + CONTRA$SimpleComplex*CONTRA$SeatedWalking)
summary(Late_lmfit_CONTRA_IntDuration_4)

anova(Late_lmfit_CONTRA_IntDuration, Late_lmfit_CONTRA_IntDuration_1, test="F")
anova(Late_lmfit_CONTRA_IntDuration, Late_lmfit_CONTRA_IntDuration_2, test="F")
anova(Late_lmfit_CONTRA_IntDuration, Late_lmfit_CONTRA_IntDuration_3, test="F")
anova(Late_lmfit_CONTRA_IntDuration, Late_lmfit_CONTRA_IntDuration_4, test="F")

#abline(lm(CONTRASeatedSimple$Late ~ CONTRASeatedSimple$IntDuration), col="chocolate1", lwd=2)
#abline(lm(CONTRAWalkingSimple$Late ~ CONTRAWalkingSimple$IntDuration), col="red3", lwd=2)
#abline(lm(CONTRASeatedComplex$Late ~ CONTRASeatedComplex$IntDuration), col="royalblue1", lwd=2)
#abline(lm(CONTRAWalkingComplex$Late ~ CONTRAWalkingComplex$IntDuration), col="blue2", lwd=2)

#abline(lm(CONTRASeatedSimple$Late ~ CONTRASeatedSimple$IntDuration), col="chocolate1", lwd=2)
#abline(lm(CONTRAWalkingSimple$Late ~ CONTRAWalkingSimple$IntDuration), col="red3", lwd=2)
#abline(lm(CONTRASeatedComplex$Late ~ CONTRASeatedComplex$IntDuration), col="royalblue1", lwd=2)
#abline(lm(CONTRAWalkingComplex$Late ~ CONTRAWalkingComplex$IntDuration), col="blue2", lwd=2)

## end device
#dev.off()

####################################################################################
# LabTimes
####################################################################################

# Load in data from Matlab
# setwd("C:/Users/FPF/Desktop/fef_ready/data/mainstudy/")
setwd("//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/script/mainstudy/")
# labtimes <- read.csv(file="C:/Users/FPF/Desktop/fef_ready/data/mainstudy/labtimes.csv", header=F, sep=",", dec=".")
labtimes <- read.csv(file="//daten.uni-oldenburg.de/home/leza6765/Desktop/fef_ready/data/mainstudy/labtimes.csv", header=F, sep=",", dec=".")
labtimes <- as.data.frame(labtimes)

colnames(labtimes) <- c("WalkingSimple", "WalkingComplex")
t.test(labtimes$WalkingSimple,labtimes$WalkingComplex, var.equal=TRUE, paired=TRUE)

mean(labtimes$WalkingSimple)
mean(labtimes$WalkingComplex)
1.96*sd(labtimes$WalkingSimple)/sqrt(16)
1.96*sd(labtimes$WalkingComplex)/sqrt(16)