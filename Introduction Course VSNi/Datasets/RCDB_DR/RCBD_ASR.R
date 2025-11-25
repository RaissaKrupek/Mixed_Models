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

# Fit LM - Block is fix
modF<-asreml(fixed=Response~Trt+Doctor,
             data=RCBD)
plot(modF)  
summary(modF)$varcomp
wald.asreml(modF,denDF='algebraic',ssType='conditional')
predict(modF,classify='Trt')

# Fit LMM - Block is random
modR<-asreml(fixed=Response~Trt,
             random=~Doctor,
             data=RCBD)
plot(modR)  
summary(modR)$varcomp
wald.asreml(modR,denDF='algebraic',ssType='conditional')
predict(modR,classify='Trt')

# Look at BLUE and BLUP
BLUP<-summary(modR,coef=TRUE)$coef.random
BLUE<-summary(modR,coef=TRUE)$coef.fixed

