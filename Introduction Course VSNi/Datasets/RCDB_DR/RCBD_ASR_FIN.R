#####################################################
## DOCTORS IN A RCB - Random effect
##
## Source: NCSS 2019 Statistical Software (2019). 
##         NCSS LLC. Kaysville, Utah, USA
#####################################################

rm(list=ls())
setwd("C:/Users/Owner/Desktop/Online_Training/SHORT/Datasets/RCDB_DR")

# Loading libraries
library(asreml)

# asreml(fixed=y~<fixed effects>,
#        random=~<random effects>.
#        residual=~<error structure>,
#        data=mydata)

# Reading data
RCBD<-read.table("RCBD_DR.txt",header=TRUE)
head(RCBD)

# Some EDA
boxplot(Response~Trt,RCBD)
table(RCBD$Trt,RCBD$Doctor)
hist(RCBD$Response)

# Defining factors
RCBD$Doctor<-as.factor(RCBD$Doctor)
RCBD$Trt<-as.factor(RCBD$Trt)
str(RCBD)

# Fitting model with FIXED effect of Doctor (random blocks) 
modF<-asreml(fixed=Response~Trt+Doctor,
             data=RCBD)
plot(modF)
summary(modF)$varcomp
wald.asreml(modF,denDF='algebraic',ssType='conditional')
(predF<-predict(modF,classify='Trt')$pvals)

# Fitting model with RANDOM effect of Doctor (random blocks) 
modR<-asreml(fixed=Response~Trt,
             random=~Doctor,
             data=RCBD)
summary(modR)$varcomp
plot(modR)
wald.asreml(modR,denDF='algebraic',ssType='conditional')
(predR<-predict(modR,classify='Trt')$pvals)
(sed<-predict(modR,classify='Trt',sed=TRUE)$sed)

# Fixed (BLUE) and Random (BLUP) effects
(BLUE<-summary(modR,coef=TRUE)$coef.fixed)
(BLUP<-summary(modR,coef=TRUE)$coef.random)

