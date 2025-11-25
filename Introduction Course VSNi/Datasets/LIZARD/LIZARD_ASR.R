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
