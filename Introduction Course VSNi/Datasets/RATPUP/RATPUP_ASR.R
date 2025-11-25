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
aggregate(WEIGHT~TREATMENT,FUN=mean,data=LITTER)
aggregate(WEIGHT~TREATMENT,FUN=sd,data=LITTER)

# Defining factors
LITTER$SEX<-as.factor(LITTER$SEX)
LITTER$LITTER<-as.factor(LITTER$LITTER)
LITTER$TREATMENT<-as.factor(LITTER$TREATMENT)
str(LITTER)

# Fit LMM
model.L<-asreml(fixed=WEIGHT~LSIZE+TREATMENT+SEX+TREATMENT:SEX,
                random=~LITTER,
                data=LITTER)
plot(model.L)

# Dealing residuals
res<-resid(model.L)
which.min(res)
LITTER$WEIGHT[66]<-NA

# Fit LMM - homocedasticity
model.L<-asreml(fixed=WEIGHT~LSIZE+TREATMENT+SEX+TREATMENT:SEX,
                random=~LITTER,
                data=LITTER)
plot(model.L)
summary(model.L)$varcomp
predict(model.L,classify='TREATMENT')$pvals
wald.asreml(model.L,denDF='algebraic',ssType = 'conditional')

# Fit LMM - heteroscedasticity
model.H<-asreml(fixed=WEIGHT~LSIZE+TREATMENT+SEX+TREATMENT:SEX,
                random=~LITTER,
                residual=~dsum(~units|TREATMENT),
                data=LITTER)
plot(model.H)
summary(model.H)$varcomp
predict(model.H,classify='TREATMENT')$pvals
wald.asreml(model.H,denDF='algebraic',ssType = 'conditional')

# Comparing models using LRT
lrt.asreml(model.L,model.H,boundary=FALSE)

