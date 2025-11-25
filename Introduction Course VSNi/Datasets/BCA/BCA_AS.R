##################################################
## BCA - Generalized Linear Mixed Model 
##
## Source: Welham et. al. (2016). Statistical           
##         Methods in Biology: Design and Analysis
##         of Experiments and Regression. CRC
##################################################

rm(list=ls()) 
setwd("C:/Users/Owner/Desktop/Online_Training/SHORT/Datasets/BCA")
library(asreml)

# Reading dataset
BCA<-read.table("BCA.TXT",header=TRUE)
head(BCA)

# Defining Factors
BCA$BLOCK<-as.factor(BCA$BLOCK)
BCA$VARIETY<-as.factor(BCA$VARIETY)
BCA$FUNGICIDE<-as.factor(BCA$FUNGICIDE)
BCA$BCA<-as.factor(BCA$BCA)
str(BCA)

# Additional Elements
BCA$TRT<-paste(BCA$VARIETY,BCA$FUNGICIDE,BCA$BCA,sep='-')
BCA$TRT<-as.factor(BCA$TRT)
BCA$PROP<-BCA$DISEASED/BCA$EMERGED

# EDA: Exploratory Data Analysis
summary(BCA$PROP)
hist(BCA$PROP)
table(BCA$TRT,BCA$BLOCK)
boxplot(PROP~TRT,data=BCA)
aggregate(PROP~TRT,FUN=mean,data=BCA)

