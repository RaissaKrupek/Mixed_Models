#################################
## Sire Model (Female) Bivariate
#################################

rm(list=ls()) 
setwd("C:/Users/sgeza/OneDrive/Desktop/Workshops ASReml-R/ASReml_ESALQ/Material/Session06")
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
cor(openp$DBH,openp$HT, use='complete.obs') # 0.674

##################################
# Univariate female models separating controls
# y = REP + PLOT + at(TYPE):FEMALE

# Trait 1
openfc.1<-asreml(fixed=HT~REP+at(TYPE,'Control'):FEMALE,
               random=~at(TYPE,'Test'):FEMALE+PLOT,data=openp)
summary(openfc.1)$varcomp
plot(openfc.1)
(h2<-vpredict(openfc.1,h2~4*V1/(V1+V2+V3)))

# Trait 2
openfc.2<-asreml(fixed=DBH~REP+at(TYPE,'Control'):FEMALE,
                 random=~at(TYPE,'Test'):FEMALE+REP:PLOT,data=openp)
summary(openfc.2)$varcomp
plot(openfc.2)
(h2<-vpredict(openfc.2,h2~4*V1/(V1+V2+V3)))

#########################
## Bivariate Analysis ###

initF<-c(0.67,0.175,0.457)
initR<-c(0.67,1.019,7.27)
openb<-asreml(fixed=cbind(HT,DBH)~trait+trait:REP+at(TYPE,'Control'):trait:FEMALE,
                 random=~at(TYPE,'Test'):corgh(trait,init=initF):id(FEMALE)+
                            diag(trait):id(PLOT), # diag or corgh?
                 residual=~units:corgh(trait,init=initR),
                 data=openp)
openb<-update.asreml(openb)
summary(openb)$varcomp
plot(openb)
(r2<-vpredict(openb,r2~V1))
vpredict(openb,h2HT~4*V2/(V2+V4+V8))
vpredict(openb,h2DBH~4*V3/(V3+V5+V9))

BLUP.bv<-summary(openb,coef=TRUE)$coef.random
head(BLUP.bv)
View(BLUP.bv)

BLUP.1<-summary(openfc.1,coef=TRUE)$coef.random
head(BLUP.1)
BLUP.2<-summary(openfc.2,coef=TRUE)$coef.random
head(BLUP.2)
