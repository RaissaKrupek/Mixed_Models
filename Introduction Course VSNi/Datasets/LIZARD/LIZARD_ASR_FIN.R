##############################################
## LIZARD - Animal Model 
##
## Source: Katrina et. al. (2016).          
##         BMC Evolutionary Biology 16:179  
##############################################

rm(list=ls()) 
setwd("C:/Users/Owner/Desktop/Online_Training/SHORT/Datasets/LIZARD")
library(asreml)

# Reading Phenotypic data
LIZARD<-read.table("LIZARD.TXT",h=TRUE,na.strings='NA')
head(LIZARD)

# EDA
summary(LIZARD)
hist(LIZARD$P_YELLOW)
boxplot(LIZARD$P_YELLOW)
table(LIZARD$SEX,LIZARD$COHORT)

# Defining Factors
LIZARD$INDIV<-as.factor(LIZARD$INDIV)
LIZARD$COHORT<-as.factor(LIZARD$COHORT)
LIZARD$SEX<-as.factor(LIZARD$SEX)
str(LIZARD)

# Obtaining relationships from pedigree - A matrix
library(nadiv)
pedind<-LIZARD[,c(2:4)]
head(pedind)
APED<-makeA(pedind)
APED[1:10,1:10]
APED[40:50,40:50]

# Obtaining inverse of relationships from pedigree - A matrix
ainv<-ainverse(pedind)
head(ainv)

# Fitting Animal Model
mod.ind<-asreml(fixed=P_YELLOW~COHORT+SEX,
              random=~vm(INDIV,ainv),
              residual=~idv(units),
              data=LIZARD)
mod.ind<-update.asreml(mod.ind)
plot(mod.ind)
summary(mod.ind)$varcomp

# Transformation Proportion - z = ln[(P + c)/(100 - P + c)]
LIZARD$logitPY<-log((LIZARD$P_YELLOW+1)/(100-LIZARD$P_YELLOW+1))
hist(LIZARD$P_YELLOW)
hist(LIZARD$logitPY)

mod.indT<-asreml(fixed=logitPY~COHORT+SEX,
                random=~vm(INDIV,ainv),
                residual=~idv(units),
                data=LIZARD)
mod.indT<-update.asreml(mod.indT)
plot(mod.indT)
summary(mod.indT)$varcomp

# Calculating heritability
vpredict(mod.indT,h2~V1/(V1+V2))

# Obtaining BLUP & Predictions
BLUP<-as.data.frame(summary(mod.indT,coef=TRUE)$coef.random)
head(BLUP)
BLUP$Mean<-mean(LIZARD$logitP_YELLOW)
BLUP$Preds<-BLUP$Mean+BLUP$solution

# Back-transformation: P = [(100 + c)*exp(z) - c]/[1 + exp(z)]
BLUP$BT_Preds<-((100+1)*exp(BLUP$Preds)-1)/(1+exp(BLUP$Preds))
head(BLUP)
View(BLUP)



