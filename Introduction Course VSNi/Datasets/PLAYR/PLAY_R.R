#####################################################
## DOCTORS IN A RCB - Exploring Data - Linear Model
##
## Source: NCSS 2019 Statistical Software (2019). 
##         NCSS LLC. Kaysville, Utah, USA
#####################################################

rm(list=ls())
setwd("~/Documents/Mixed_Models/Introduction Course VSNi/Datasets/PLAYR")

# Read file
RCBD<-read.table('RCBD_DR.txt',header=TRUE)
head(RCBD)
View(RCBD)

# Some EDA
summary(RCBD)
hist(RCBD$Response)
boxplot(Response~Trt,data=RCBD)
table(RCBD$Doctor,RCBD$Trt)

# Fit Linear Model

# Define Factor
str(RCBD)
RCBD$Doctor<-as.factor(RCBD$Doctor)
RCBD$Trt<-as.factor(RCBD$Trt)
str(RCBD)

# My model: y = mu + Doctor + Trt + e
model0 <-lm(Response~Doctor+Trt, data=RCBD)
ls(model0)  
summary(model0)
plot(model0)
anova(model0)

# Means and LSmeans
aggregate(Response~Trt,FUN=mean,data=RCBD)
aggregate(Response~Trt,FUN=sd,data=RCBD)

library(emmeans)
emmeans(model0,~Trt)
emmeans(model0,pairwise~Trt,adjust='bon')
