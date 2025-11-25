#####################################################
## SEMICONDUCTOR - Hierarchichal Model
##
## Source: Littell, RC et al. (2006).
##         SAS for Linear Mixed Models. 2nd Edition.
#####################################################

rm(list=ls()) 
setwd("C:/Users/Owner/Desktop/Online_Training/SHORT/Datasets/SEMICOND")
library(asreml)

# Reading Data
SEMIC<-read.table("SEMICOND.TXT",header=TRUE)
head(SEMIC)

# Some EDA
hist(SEMIC$THICK)
table(SEMIC$LOT,SEMIC$WAFER)
table(SEMIC$LOT,SEMIC$SOURCE)
boxplot(THICK~SOURCE,data=SEMIC)

# Defining Factors
SEMIC$SOURCE<-as.factor(SEMIC$SOURCE)
SEMIC$LOT<-as.factor(SEMIC$LOT)
SEMIC$WAFER<-as.factor(SEMIC$WAFER)
SEMIC$SITE<-as.factor(SEMIC$SITE)
str(SEMIC)
