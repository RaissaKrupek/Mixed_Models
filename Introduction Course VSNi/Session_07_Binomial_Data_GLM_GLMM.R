# Session 07: Modelling Binomial Data with GLM and GLMM

# Aqui as observacoes/residuos nao seguirao uma Distribuicao Normal, mas uma Binomial
# Generalized Mixed Model (GMM), também chamado GLMM – Generalized Linear Mixed Model, é uma extensão dos Modelos Mistos Lineares (LMM) que permite modelar respostas que não seguem distribuição normal (ex.: binária, contagem, Poisson, binomial, gama...).

# Document: Understanding Logistic Regression Analysis (Sperandei, 2014)
# Available here: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3936971/pdf/biochem-24-1-12-4.pdf

# Link: Other specifications of distributions (from the exponential family) for ASReml-R
# Available here: https://asreml.kb.vsni.co.uk/knowledge-base/asr_families/

# Description of the experiment:
# A study was established to evaluate the effect of variety, fungicide and the application of a biological control agent (BCA) on onions.
# For each treatment combination of variety, fungicide and BCA the number of diseased plants out of the total plants emerged was recorded.
# As the data has a binomial distribution, it is required to fit a generalized linear model (GLM) with a logit link to this data.
 
# Objective
# To assess the effect of variety, fungicide and a biological control agent on the incidence of white rot on onions to make recommendations.

rm(list=ls()) 
setwd("~/Documents/Mixed_Models/Introduction Course VSNi/Datasets/BCA")
library(asreml)

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

