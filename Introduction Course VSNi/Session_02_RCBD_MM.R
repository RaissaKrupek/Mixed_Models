# Session 02 - RCBD with Mixed Models

# Link: Full description of the asreml() function from the ASReml-R library. Available here: https://asreml.kb.vsni.co.uk/knowledge-base/asreml-object/

setwd("~/Documents/Mixed_Models/Introduction Course VSNi/Datasets/RCDB_DR")

library(asreml)
asreml.license.activate()
# DGAF-AADG-BJDC-CGIF
detach("package:asreml")

# asreml(fixed=y~<fixed effects>,
#        random=~<random effects>.
#        residual=~<error structure>,
#        data=mydata)

#No residuo eh onde podemos ter uma estrutura de correlacao complicada, o que nao eh este caso

# Reading data
RCBD<-read.table("RCBD_DR.txt",header=TRUE)
head(RCBD)

# Some EDA
boxplot(Response~Trt,RCBD) # Espera-se que o trat C seja significativamente diferente dos demais
table(RCBD$Trt,RCBD$Doctor) # Experimento balanceado
hist(RCBD$Response) # Nao tao longe da normalidade padrao 

# Defining factors
RCBD$Doctor<-as.factor(RCBD$Doctor)
RCBD$Trt<-as.factor(RCBD$Trt)
str(RCBD)

# Fit Linear Model - Block is fix
modF<-asreml(fixed=Response~Trt+Doctor,
             data=RCBD)
plot(modF)  
summary(modF)$varcomp # Nesse caso teremos apenas 1, que eh o do residuo
wald.asreml(modF,denDF='algebraic',ssType='conditional') # No ASReml equivale a anova
#Temos diferença significatva entre tratamentos, que eh o que queremos
predict(modF,classify='Trt')
# Para predicoes das Least Square Means. O comando "classify" fala qual nivel de qual fator queremos nossas medias 
# Como resultado, nos dará as LSmeans e erros associados a cada tratamento 


# Fit Linear Mixed Model - Block is random
modR<-asreml(fixed=Response~Trt,
             random=~Doctor,
             data=RCBD)
# Eh possível notar que ha mais iterações 
plot(modR)  
summary(modR)$varcomp
wald.asreml(modR,denDF='algebraic',ssType='conditional')
predict(modR,classify='Trt')
# Temos um Standard Error maior associado aos tratamentos em comparaçao ao anterior,
# pois leva em conta a variabilidade extra adocionada ao modelo.


# BLUE and BLUP
BLUP<-summary(modR,coef=TRUE)$coef.random
BLUE<-summary(modR,coef=TRUE)$coef.fixed # O primeiro eh definido a 0
