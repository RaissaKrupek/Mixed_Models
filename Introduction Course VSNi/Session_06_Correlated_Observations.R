# Session 06 - ACCOUNTING FOR CORRELATED OBSERVATIONS IN A LINEAR MIXED MODEL 

# Document: Guide for Selecting the Covariance Structure in Mixed Model Analysis (C. Kincaid).
# details with emphasis on SAS on (co)variance structures and how to select the best variance structure.
# available here: https://support.sas.com/resources/papers/proceedings/proceedings/sugi30/198-30.pdf

# Video: Repeated Measures in ASReml
# Another example in ASReml-R4 with complex correlated observations: https://youtu.be/BwvTFZDejXI

# Aqui, as observacoes nao sao independentes, o que eh muito diferente dos modelos lineares, 
# os quais assumem independencia de observacoes e, portanto, residuos. No caso dos modelos mistos,
# modelamos essa correlacao considerando efeitos aleatorios, ou modelando algumas estruturas de
# covariancia de erros especificas, associadas com as dependencias observadas nos dados.

# Description of the Experiment:
# Long distance runners have indicated that moderate exposure to ozone increases lung capacity.
# A study exposed a total of 12 rats to ozone (2 parts per million) over a period of 30 days.
# The lung capacity (in ml) of each subject was determined at the beginning of the study and after 30 days of exposure.
# Observations are not independent as there are two measurements (before and after) per rat and this needs to be considered in the model.
# Temos 2observacoes por rato(antes e depois) e estas terao correlacao -> rats com efeito aleatorio

# Objective
# To evaluate if there is a change on lung capacity after ozone exposure on rats. 
# Avaliar se ha significant difference de antes para depois, e para isso devemos considerar as estruturas de correlacao dos dados -> Muito afetada 
# pelos graus de liberdade, pois nao temos as 24 observacoes sendo independentes


rm(list=ls())
setwd("~/Documents/Mixed_Models/Introduction Course VSNi/Datasets/LUNG")
library(asreml)

# Reading data
LUNG<-read.table("LUNG.TXT",header=TRUE)
head(LUNG)
View(LUNG)

# Some EDA
boxplot(RESP~TIME,LUNG)
aggregate(RESP~TIME,FUN=mean,data=LUNG)

# Defining factors
LUNG$RAT<-as.factor(LUNG$RAT)
LUNG$TIME<-as.factor(LUNG$TIME)
str(LUNG)

# Model with simple RAT effect - compound symmetry
model.CS<-asreml(fixed=RESP~TIME,
                 random=~RAT,
                 residual=~idv(units),
                 data=LUNG)  
summary(model.CS)$varcomp
wald(model.CS,denDF='algebraic',ssType='conditional')$Wald
plot(model.CS) 
predict(model.CS,classify='TIME')
vpredict(model.CS,r2~V1/(V1+V2))



# Model with correlated errors & no RAT effect - PRECISO ENTENDER MELHOR ESSA ESTRUTURA
# Usa-se a estrutura CORV = assume correlaao entre 2 observacoes e ha uma variancia comum 

LUNG<-LUNG[order(LUNG$RAT),]  # Reorganiza o data frame LUNG para que as linhas fiquem agrupadas por RAT, importante porque a estrutura de covariância corv(TIME) espera que as observações de cada unidade experimental estejam em ordem.

model.CORV<-asreml(fixed=RESP~TIME,
                   residual=~id(RAT):corv(TIME), #Produto cronocker
                   data=LUNG) 
                   ## id(RAT) → define blocos independentes de erros residuais para cada rato (RAT).
                   ## corv(TIME) → dentro de cada rato, as observações em tempos diferentes têm variância comum e correlação estimada entre si.
                   ## Ou seja, a matriz de covariância residual para cada rato terá mesma variância em todos os tempos e uma única correlação entre quaisquer dois tempos.
summary(model.CORV)$varcomp


# A model generic model with corh
model.CORH<-asreml(fixed=RESP~TIME,
                   residual=~id(RAT):corh(TIME), #Produto cronocker
                   data=LUNG)  
summary(model.CORH)$varcomp
plot(model.CORH)
wald(model.CORH,denDF='algebraic',ssType='conditional')$Wald
predict(model.CORH,classify='TIME')



# Comparing models (select smallest value)
summary(model.CORV)$loglik
summary(model.CORH)$loglik
lrt.asreml(model.CORV, model.CORH)
summary(model.CORV)$aic
summary(model.CORH)$aic
summary(model.CORV)$bic
summary(model.CORH)$bic
