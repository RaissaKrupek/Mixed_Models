#####################################################
## LUNG - Correlated Measurements (Errors) 
##
## Source: McClave, JT and Sincich, T. (2012). 
##         Statistics.,12th Edition. Prentice Hall
#####################################################

rm(list=ls())
setwd("C:/Users/Owner/Desktop/Online_Training/SHORT/Datasets/LUNG")
library(asreml)

# Reading data
LUNG<-read.table("LUNG.TXT",header=TRUE)
head(LUNG)
View(LUNG)

# Some EDA
boxplot(RESP~TIME,LUNG)
aggregate(RESP~TIME,FUN=mean,data=LUNG)

# Defining factors
LUNG$RAT<-as.factor(LUNG$RAT)
LUNG$TIME<-as.factor(LUNG$TIME)
str(LUNG)

# Model with simple RAT effect - compound symmetry
model.CS<-asreml(fixed=RESP~TIME,
                random=~RAT,
                residual=~idv(units),
                data=LUNG)  
summary(model.CS)$varcomp
wald(model.CS,denDF='algebraic',ssType='conditional')$Wald
plot(model.CS) 
predict(model.CS,classify='TIME')
vpredict(model.CS,r2~V1/(V1+V2))

# Model with correlated errors & no RAT effect
LUNG<-LUNG[order(LUNG$RAT),]  # Need to sort this
model.CORV<-asreml(fixed=RESP~TIME,
                 residual=~id(RAT):corv(TIME),
                 data=LUNG)  
summary(model.CORV)$varcomp

# A model generic model with corh
model.CORH<-asreml(fixed=RESP~TIME,
                   residual=~id(RAT):corh(TIME),
                   data=LUNG)  
summary(model.CORH)$varcomp
plot(model.CORH)
wald(model.CORH,denDF='algebraic',ssType='conditional')$Wald
predict(model.CORH,classify='TIME')

# Comparing models (select smallest value)
summary(model.CORV)$loglik
summary(model.CORH)$loglik
lrt.asreml(model.CORV, model.CORH)
summary(model.CORV)$aic
summary(model.CORH)$aic
summary(model.CORV)$bic
summary(model.CORH)$bic

