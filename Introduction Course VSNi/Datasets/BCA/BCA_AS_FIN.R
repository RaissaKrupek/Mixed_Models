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

# Base-Model for Disease/Emergence - Block Fixed
m.baseF<-asreml(fixed=DISEASED~BLOCK+TRT,
               family=asr_binomial(link='logit',dispersion=1,total=EMERGED),
               data=BCA)
plot(m.baseF)
summary(m.baseF)$varcomp
wald.asreml(m.baseF,denDF='numeric',ssType='incremental')
predict(m.baseF,classify='TRT')

# Base-Model for Disease/Emergence - Block Random
m.baseR<-asreml(fixed=DISEASED~TRT,
                random=~BLOCK,
                family=asr_binomial(link='logit',dispersion=1,total=EMERGED),
                data=BCA)
plot(m.baseR)
summary(m.baseR)$varcomp
wald.asreml(m.baseR,denDF='numeric',ssType='incremental')
predict(m.baseR,classify='TRT')

# Final Model full factors
m.full<-asreml(fixed=DISEASED~VARIETY*FUNGICIDE*BCA,
               random=~BLOCK,
               family=asr_binomial(link='logit',dispersion=1,total=EMERGED),
               data=BCA)
summary(m.full)$varcomp
wald.asreml(m.full,denDF='numeric',ssType='incremental')

predict(m.full,classify='VARIETY')
predict(m.full,classify='VARIETY:FUNGICIDE:BCA')

