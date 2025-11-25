#####################################################
## RAT PUP WEIGHT - Heterogeneous Errors
##
## Source: Pinheiro, JC and Bates, DM (2000). 
##         Mixed Effects Models in S and S-PLUS
#####################################################

rm(list=ls())
setwd("C:/Users/Owner/Desktop/Online_Training/SHORT/Datasets/RATPUP")
library(asreml)

# Reading data
LITTER<-read.table("RATPUP.txt",header=TRUE)
head(LITTER)

# Some Exploratory Data Analysis - EDA
table(LITTER$TREATMENT,LITTER$LITTER)
hist(LITTER$WEIGHT)

# Defining factors
LITTER$SEX<-as.factor(LITTER$SEX)
LITTER$LITTER<-as.factor(LITTER$LITTER)
LITTER$TREATMENT<-as.factor(LITTER$TREATMENT)
str(LITTER)

# One-way Classification Model
model.L<-asreml(fixed=WEIGHT~LSIZE+TREATMENT+SEX+TREATMENT:SEX,
                 random=~LITTER,
                 data=LITTER)
plot(model.L)
summary(model.L)$varcomp
wald.asreml(model.L,denDF='numeric')$Wald
predict(model.L,classify='TREATMENT')$pvals
predict(model.L,classify='SEX')$pvals
predict(model.L,classify='TREATMENT:SEX')$pvals
summary(model.L)$varcomp

# Detecting and eliminating an outlier
res<-residuals(model.L)
which.max(res)
which.min(res)
LITTER[66,]
LITTER$WEIGHT[66]<-NA

# One-way Classification Model with heterog. errors
LITTER<-LITTER[order(LITTER$TREATMENT),]
model.H<-asreml(fixed=WEIGHT~LSIZE+TREATMENT+SEX+TREATMENT:SEX,
                random=~LITTER,
                residual=~dsum(~units|TREATMENT),	
                data=LITTER)
plot(model.H)
summary(model.H)$varcomp
wald.asreml(model.H,denDF='numeric')$Wald    # ask for incremental
predict(model.H,classify='TREATMENT')$pvals
predict(model.H,classify='SEX')$pvals
predict(model.H,classify='TREATMENT:SEX')$pvals
summary(model.H)$varcomp

# Looking at BLUEs and BLUPs
(BLUE<-summary(model.H,coef=TRUE)$coef.fixed)
(BLUP<-summary(model.H,coef=TRUE)$coef.random)

# Comparing models with a LRT
lrt(model.H,model.L,boundary=FALSE)