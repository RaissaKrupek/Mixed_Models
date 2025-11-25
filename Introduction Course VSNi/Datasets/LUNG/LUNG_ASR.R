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

