###############################
## Variance Structure
## leafarea experiment
###############################

rm(list=ls()) 
setwd("C:/Users/sgeza/OneDrive/Desktop/Workshops ASReml-R/ASReml_ESALQ/Material/Session05")
library(asreml)

# Leafarea Example
leafarea<-read.table("LEAFAREA.txt", h=T)
head(leafarea)
leafarea$Block<-as.factor(leafarea$Block)
leafarea$Pot<-as.factor(leafarea$Pot)
leafarea$Variety<-as.factor(leafarea$Variety)
leafarea$Disease<-as.factor(leafarea$Disease)
str(leafarea)

#####################################################
# Traditional ANOVA - All fixed

# y = mu + Block + [ Variety + Disease + Variety:Disease ] + e
model0<-asreml(fixed=Leafarea~Block+Disease+Variety+Variety:Disease,
               data=leafarea)
plot(model0)
wald(model0,denDF='default',ssType='conditional') # Type3 SS
summary(model0)$varcomp

pred.model0<-predict(model0,classify="Variety:Disease",sed=TRUE)
head(pred.model0$pvals)

aggregate(Leafarea~Disease,FUN=mean,data=leafarea)
aggregate(Leafarea~Disease,FUN=sd,data=leafarea)

######################################################
# 2-way ANOVA with heterogeneous errors

# residual=~dsum(~idv(units)|Disease)
head(leafarea)
leafarea<-leafarea[order(leafarea$Disease),]  # ordering by disease
model0H<-asreml(fixed=Leafarea~Block+Disease+Variety+Variety:Disease,
                residual=~dsum(~units|Disease),
                data=leafarea)
plot(model0H)
wald(model0H,denDF='default',ssType='conditional') # Type3 SS

summary(model0)$varcomp
summary(model0H)$varcomp

pred.model0H<-predict(model0H,classify="Variety:Disease",sed=TRUE)
head(pred.model0H$pvals)

# Comparing homogenoous to heterogeneous model
# H0 : s2_1 = s2_2 
lrt.asreml(model0H,model0,boundary=FALSE)

######################################################
# Model Variety as random

# Traditional model + Interaction
modelI<-asreml(fixed=Leafarea~Block+Disease,
                random=~Variety+Variety:Disease,
                residual=~dsum(~units|Disease),
                data=leafarea)
summary(modelI)$varcomp
vpredict(modelI,rB~V1/(V1+V2))
BLUP<-summary(modelI,coef=TRUE)$coef.random
BLUP
View(BLUP)

preds <- predict(modelI, classify='Variety:Disease')$pvals
View(preds)

# Direct product model - Nested with corv
modelN<-asreml(fixed=Leafarea~Block+Disease,
               random=~id(Variety):corv(Disease),
               residual=~dsum(~units|Disease),
               data=leafarea)
summary(modelN)$varcomp
BLUP<-summary(modelN,coef=TRUE)$coef.random
BLUP

# Direct product model - Nested with corgh
modelNG<-asreml(fixed=Leafarea~Block+Disease,
               random=~id(Variety):corh(Disease),
               residual=~dsum(~units|Disease),
               data=leafarea)
summary(modelNG)$varcomp
lrt.asreml(modelN,modelNG,boundary=FALSE)

BLUP<-summary(modelNG,coef=TRUE)$coef.random
BLUP

vpredict(modelNG,h2_D~V2/(V2+V4))
vpredict(modelNG,h2_H~V3/(V3+V5))
vpredict(modelNG,h2m~(V2+V3)/(V2+V4+V3+V5))

preds<-predict(modelNG,classify='Variety:Disease')$pvals
View(preds)
