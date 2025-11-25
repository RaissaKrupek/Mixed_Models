# Session 04 - Fitting a Multilevel Model for Hierarchical Structure

# Link: Multilevel Models: An Introduction and FAQs
# A webpage from the Center for Multilevel Modelling with several pieces of information
# Available online: http://www.bristol.ac.uk/cmm/learning/multilevel-models/

# Video: Multilevel Models: Introducing Multilevel Modeling  
# Explanation of multilevel models in the social science context
# Available online: https://www.youtube.com/watch?v=YLkXP3Edd80

# Description of the experiment:
# The measurements correspond to thickness (espessura) of the oxide layer (camada) on silicon wafers.
# The wafers originate from 3 randomly selected wafers from 8 different lots, where each has 2 wafers in total. 
# After the layer of oxide was deposited, the layer's thickness was assessed at 3 randomly selected sites within each wafer.
# There are also a treatment corresponding to two material sources (fontes) to be evaluated.
# This is a hierarchical model where the factors of lot and wafer within lot should be considered random effects.
#LOTS > WAFERS > SITES (seria a observacao - 1 associado a cada wafer) 
#
# Objectives:
# The objective of this analysis is to estimate the variance components to determine assignable causes for the observed variability.
# In addition, the two different sources of material available need to be compared statistically.


rm(list=ls()) 
setwd("~/Documents/Mixed_Models/Introduction Course VSNi/Datasets/SEMICOND")
library(asreml)

# Reading Data
library(readr)
SEMIC<-read.table("SEMICOND.TXT",header=TRUE)
head(SEMIC)

# Some EDA
hist(SEMIC$THICK)
table(SEMIC$LOT,SEMIC$WAFER) # experimento balanceado
table(SEMIC$LOT,SEMIC$SOURCE) # 4 primeiros lotes referentes ao primeiro sabor, e os 4 outros referentes ao segundo sabor
boxplot(THICK~SOURCE,data=SEMIC)

# Defining Factors
SEMIC$SOURCE<-as.factor(SEMIC$SOURCE)
SEMIC$LOT<-as.factor(SEMIC$LOT)
SEMIC$WAFER<-as.factor(SEMIC$WAFER)
SEMIC$SITE<-as.factor(SEMIC$SITE)
str(SEMIC)

# Model with Source 
mod.s<-asreml(fixed=THICK~SOURCE,
              random=~LOT+LOT:WAFER, #colocar a estrutura hierarquica em forma decrescente e aninhada
              data=SEMIC)

## pq Site nao foi incorporado ao modelo?
plot(mod.s)
summary(mod.s)$varcomp
wald.asreml(mod.s,denDF='default')$Wald # wald eh como uma anova para testar nossas hipoteses - nessse caso, nao ha diferenca significativa entre sources
predict.asreml(mod.s,classify='SOURCE')$pvals # valores preditos para source

# Calculating Prop Variability
(var.comp<-data.frame(summary(mod.s)$varcomp))
var.comp$prop.var<-100*var.comp$component/sum(var.comp$component) # Proporcao da variancia em relacao ao total
var.comp

# Podemos ver que LOTS sao mto mais diferentes que SITES (observacoes), por exemplo -> pode mudar como fazemos nossas amostras 
# mais focado em ter uma maior representacao de LOTS do que das observacoes (SITES) dentro dos Wafers