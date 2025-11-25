###############################
## Alfalfa Experiment 
###############################

rm(list=ls())  # Removes all variables in memory
setwd("C:/Users/sgeza/OneDrive/Desktop/Workshops ASReml-R/ASReml_ESALQ/Material/Session01")

alfalfa<-read.table("ALFALFA.TXT",header=TRUE,na.string='*')
head(alfalfa)  
summary(alfalfa)
str(alfalfa)

# Creating factors
alfalfa$Variety<-as.factor(alfalfa$Variety)
alfalfa$Block<-as.factor(alfalfa$Block)
str(alfalfa)

# EDA - exploratory data analysis
hist(alfalfa$Resp)
boxplot(alfalfa$Resp~alfalfa$Block)
table(alfalfa$Variety)
table(alfalfa$Variety,alfalfa$Block)

# Analysis using ASReml
library(asreml)

# asreml(fixed=y~<fixed effects>,
#        random=~<random effects>,
#        residual=<error structure>,
#        data=<mydata>)

# Variety Random
# y = mu + Block + Variety + e
