L###############################
## Parental Model
## SALMON_UK
###############################

rm(list=ls()) 
setwd("C:/Users/sgeza/OneDrive/Desktop/Workshops ASReml-R/ASReml_ESALQ/Material/Session04")
library(asreml)
library(nadiv)

# Reading Phenotypic data
FISH<-read.table("SALMON_UK.txt",h=TRUE,na.strings='NA')
head(FISH)

# Generating Factors
FISH$FISHID<-as.factor(FISH$FISHID)
FISH$FAMILY<-as.factor(FISH$FAMILY)
FISH$SIRE<-as.factor(FISH$SIRE)
FISH$DAM<-as.factor(FISH$DAM)
FISH$SEX<-as.factor(FISH$SEX)
str(FISH)

# EDA
hist(FISH$LICE)
summary(FISH$LICE)
table(FISH$DAM,FISH$SIRE)

################################
# Reading and looking at pedigree

pedind <- read.table("PEDIND.txt",h=T)
head(pedind)
tail(pedind)

library(nadiv)
APED<-makeA(pedind[,1:3])
dim(APED)
APED[1:10,1:10]
APED[100:115,100:115]

APEDINV <- solve(as.matrix(APED))
APEDINV[1:10,1:10]

# A inverse in ASReml
ainv<-ainverse(pedind)
head(ainv)

#####################
# Fit animal model

hist(FISH$LICE)
FISH$LNLICE<-100*log(FISH$LICE+10)

indiv<-asreml(fixed=LNLICE~SEX,
              random=~FISHID+FAMILY,
              residual=~idv(units),
              data=FISH)

indiv<-asreml(fixed=LNLICE~SEX,
             random=~vm(FISHID,ainv)+FAMILY,
             residual=~idv(units),
             data=FISH)
indiv<-update.asreml(indiv)
plot(indiv)
summary(indiv)$varcomp

indiv<-asreml(fixed=LNLICE~SEX,
              random=~vm(FISHID,ainv),
              residual=~idv(units),
              data=FISH)
indiv<-update.asreml(indiv)
plot(indiv)
summary(indiv)$varcomp

(h2<-vpredict(indiv,h2~V1/(V1+V2)))
BLUP<-summary(indiv,coef=TRUE)$coef.random
head(BLUP,10)
tail(BLUP,10)
View(BLUP)

EBV<-data.frame(BLUP)
head(EBV)
EBV$PEV<-EBV[,2]^2
EBV$reliab<-1-EBV$PEV/summary(indiv)$varcomp[1,1]
View(EBV)

pred.indiv<-predict(indiv,classify="FISHID",sed=TRUE)$pvals
pred.indiv$Tpredicted<-exp(pred.indiv$predicted.value/100)-10
head(pred.indiv)
View(pred.indiv)

# Comparing with parental Model
# Overlay Sire-Dam
progSD<-asreml(fixed=LNLICE~SEX,
               random=~SIRE+and(DAM),
               equate.levels=c('SIRE','DAM'), 
               residual=~idv(units),
               data=FISH)
summary(progSD)$varcomp
(h2<-vpredict(progSD,h2~4*V1/(2*V1+V2)))
BLUPP <- summary(progSD, coef=TRUE)$coef.random
head(BLUPP)
