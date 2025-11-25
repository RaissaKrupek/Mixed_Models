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

# Model with Source 
mod.s<-asreml(fixed=THICK~SOURCE,
         random=~LOT+LOT:WAFER,
         data=SEMIC)
plot(mod.s)
summary(mod.s)$varcomp
wald.asreml(mod.s,denDF='default')$Wald
predict.asreml(mod.s,classify='SOURCE')$pvals

# Calculating Prop Variability
(var.comp<-data.frame(summary(mod.s)$varcomp))
var.comp$prop.var<-100*var.comp$component/sum(var.comp$component)
var.comp