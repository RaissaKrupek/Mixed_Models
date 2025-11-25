###############################
## Sire Model (Female) 
## Data Open Polinization
###############################

rm(list=ls()) 
setwd("C:/Users/sgeza/OneDrive/Desktop/Workshops ASReml-R/ASReml_ESALQ/Material/Session02")
library(asreml)

# Open Pollinated Example
openp<-read.table("OPENPOL.txt", h=T)
head(openp)
openp$REP<-as.factor(openp$REP)
openp$FEMALE<-as.factor(openp$FEMALE)
openp$PLOT<-as.factor(openp$PLOT)
openp$TYPE<-as.factor(openp$TYPE)
str(openp)

# Some EDA
plot(openp$DBH,openp$HT)
boxplot(openp$HT)
hist(openp$HT)
table(openp$TYPE,openp$FEMALE)
table(openp$REP,openp$FEMALE)
View(openp)

# Fitting Sire/Female model
# y = mu + REP + REP:PLOT + FEMALE + e 
mf <- asreml(fixed=HT~REP,
             random=~FEMALE+PLOT,
             data=openp)
plot(mf)
summary(mf)$varcomp
vpredict(mf, h2~4*V1/(V1+V2+V3))
predict(mf, classify='FEMALE')$pvals

BLUP <- summary(mf, coef=TRUE)$coef.random
View(BLUP)
levels(openp$FEMALE)
length(levels(openp$FEMALE))
levels(openp$TYPE)

# Incorporate the TYPE - at()
mfat <- asreml(fixed=HT~REP+at(TYPE,'Control'):FEMALE,
             random=~at(TYPE,'Test'):FEMALE+PLOT,
             data=openp)
summary(mfat)$varcomp
preds <- predict(mfat, classify='TYPE:FEMALE')$pvals
View(preds)

vpredict(mfat,h2~4*V1/(V1+V2+V3))
BLUP <- summary(mfat,coef=TRUE)$coef.random
View(BLUP)

wald.asreml(mf, denDF='numeric', ssType='conditional')
wald.asreml(mfat, denDF='numeric', ssType='conditional')

preds <- predict.asreml(mfat, classify='TYPE:FEMALE')$pvals
preds

levels(openp$TYPE)
preds <- predict.asreml(mfat, classify='TYPE:FEMALE', levels=list(TYPE="Test"))$pvals
preds
