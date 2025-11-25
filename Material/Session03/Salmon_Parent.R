###############################
## Parental Model
## SALMON_UK
###############################

rm(list=ls()) 
setwd("C:/Users/sgeza/OneDrive/Desktop/Workshops ASReml-R/ASReml_ESALQ/Material/Session03")
library(asreml)

# Reading Phenotypic data
FISH<-read.table("SALMON_UK.txt",h=TRUE,na.strings='NA')
head(FISH)

# Generating Factors
FISH$FISHID<-as.factor(FISH$FISHID)
FISH$FAMILY<-as.factor(FISH$FAMILY)
FISH$SIRE<-as.factor(FISH$SIRE)
FISH$DAM<-as.factor(FISH$DAM)
FISH$SEX<-as.factor(FISH$SEX)
str(FISH)

# EDA
hist(FISH$LICE)
summary(FISH$LICE)
table(FISH$DAM,FISH$SIRE)

# Transformation Sea Lice
hist(FISH$LICE)
FISH$LNLICE<-100*log(FISH$LICE+10)
hist(FISH$LNLICE)

# Confounding Factors
asreml.options(ai.sing=TRUE)
prog<-asreml(fixed=LNLICE~SEX,
              random=~SIRE+DAM+FAMILY,
              residual=~idv(units),
              data=FISH)
summary(prog)$varcomp

# A bit better
prog<-asreml(fixed=LNLICE~SEX,
             random=~SIRE+DAM,
             residual=~idv(units),
             data=FISH)
plot(prog)
summary(prog)$varcomp
(h2<-vpredict(prog,h2~2*(V1+V2)/(V1+V2+V3)))
BLUP1<-summary(prog,coef=TRUE)$coef.random
View(BLUP1)

# Overlay of m & f
progo<-asreml(fixed=LNLICE~SEX,
             random=~SIRE+and(DAM),
             residual=~idv(units),
             equate.levels=c('SIRE','DAM'),
             data=FISH)
plot(progo)
summary(progo)$varcomp
(h2<-vpredict(progo,h2~4*V1/(V1+V2)))
BLUPo<-summary(progo,coef=TRUE)$coef.random
View(BLUPo)

# Reliability
BLUP0 <- as.data.frame(BLUPo)
head(BLUP0)
vc <- summary(progo)$varcomp
vc[1,1]
BLUP0$rel <- 1-BLUP0$std.error^2/vc[1,1]
View(BLUP0)

# Final predictions on natural/original scale (depend on SEX)
preds <- predict(progo, classify='SIRE')$pvals
head(preds)
preds <- as.data.frame(preds)
preds$BT <- exp(preds$predicted.value/100)-10
View(preds)
