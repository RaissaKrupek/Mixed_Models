#####################################################
## DOCTORS IN A RCB - Exploring Data - Linear Model
##
## Source: NCSS 2019 Statistical Software (2019). 
##         NCSS LLC. Kaysville, Utah, USA
#####################################################

rm(list=ls())
setwd("~/Documents/Mixed_Models/Introduction Course VSNi/Datasets/PLAYR")

# Reading data
RCBD<-read.table("RCBD_DR.txt",header=TRUE)
head(RCBD)
View(RCBD)
str(RCBD)

# Some Exploratory Data Analysis - EDA
summary(RCBD$Trt)
hist(RCBD$Response)
table(RCBD$Trt,RCBD$Doctor)
plot(RCBD$Patient,RCBD$Response, col=RCBD$Trt)
boxplot(Response~Trt,data=RCBD)
aggregate(Response~Trt,FUN=mean,data=RCBD)
aggregate(Response~Trt,FUN=sd,data=RCBD)

# Fitting a linear model (LM)

# Defining factors
RCBD$Doctor<-as.factor(RCBD$Doctor)
RCBD$Trt<-as.factor(RCBD$Trt)
str(RCBD)

# Response = mu + Trt + Doctor + e
model0<-lm(Response~1+Trt+Doctor,data=RCBD)
plot(model0)
ls(model0)
summary(model0)
anova(model0)

# Obtaining Treatment Means (LSMeans)
library(emmeans)
emmeans(model0,~Trt)  # Medias minimos cuadrados
emmeans(model0,pairwise~Trt,adjust='bon')  # Comparaciones multiples - Bonferroni
emmeans(model0,pairwise~Trt,adjust='none')  # Comparaciones multiples - LSD


