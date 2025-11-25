# Session 03 - Modeling Heterogenous Error Structures

# E-Book: Chapter 6: Multilevel Model with Heterogeneous Variance for Examining Inter- and Intra-individual Variability Tutorial that evaluates a linear mixed model with heterogeneous variances using nlme().
# Available here: https://quantdev.ssri.psu.edu/sites/qdev/files/ILD_Ch06_2017_MLMwithHeterogeneousVariance.html

rm(list=ls())
setwd("~/Documents/Mixed_Models/Introduction Course VSNi/Datasets/RATPUP")
library(asreml)

# Reading data
LITTER<-read.table("RATPUP.TXT",header=TRUE)
head(LITTER)

# Some Exploratory Data Analysis - EDA
table(LITTER$TREATMENT,LITTER$LITTER) 
## Pode-se notar que ha um número diferente de observacoes em cada ninhada, causando heterogeneidade da variancia dos residuos
hist(LITTER$WEIGHT)
aggregate(WEIGHT~TREATMENT,FUN=mean,data=LITTER)
aggregate(WEIGHT~TREATMENT,FUN=sd,data=LITTER)

# Defining factors
LITTER$SEX<-as.factor(LITTER$SEX)
LITTER$LITTER<-as.factor(LITTER$LITTER)
LITTER$TREATMENT<-as.factor(LITTER$TREATMENT)
str(LITTER)

# Fit LMM
model.L<-asreml(fixed=WEIGHT~LSIZE+TREATMENT+SEX+TREATMENT:SEX,
                random=~LITTER,
                data=LITTER)
plot(model.L) #Pode ser observado um outlier
summary(model.L)$varcomp
wald.asreml(model.L,denDF='numeric')$Wald #Nao ha efeito de interacao 
predict(model.L,classify='TREATMENT')$pvals
predict(model.L,classify='SEX')$pvals
predict(model.L,classify='TREATMENT:SEX')$pvals

# Dealing residuals - detecting and eliminating outliers
res<-resid(model.L)
which.min(res)
LITTER[66,]
LITTER$WEIGHT[66]<-NA

# Fit LMM - homocedasticity
model.L<-asreml(fixed=WEIGHT~LSIZE+TREATMENT+SEX+TREATMENT:SEX,
                random=~LITTER,
                data=LITTER)
## Após a retirada do outlier, a verossimilhanca melhorou
plot(model.L)
summary(model.L)$varcomp
predict(model.L,classify='TREATMENT')$pvals
wald.asreml(model.L,denDF='algebraic',ssType = 'conditional')

# Fit LMM - heteroscedasticity
model.H<-asreml(fixed=WEIGHT~LSIZE+TREATMENT+SEX+TREATMENT:SEX,
                random=~LITTER,
                residual=~dsum(~units|TREATMENT), #Especifica como vao ser os termos dos residuos: 
                # Direct Sum is going to take all my units, and the units represents all my observations, 
                # and we are separating it into block diagonals by a group factor, which is the treatment
                data=LITTER)
plot(model.H)
summary(model.H)$varcomp
## Como pedido na definicao do modelo, acima, foi gerado um componente de variancia para cada um dos tratamentos
predict(model.H,classify='TREATMENT')$pvals
predict(model.H,classify='SEX')$pvals
predict(model.H,classify='TREATMENT:SEX')$pvals
wald.asreml(model.H,denDF='algebraic',ssType = 'conditional')
## A maior diferenca esta aqui na ANOVA, pq os tratamentos tem diferentes variancas dos residuos - pequena diferenca nesse caso

# Comparing models using LRT
lrt.asreml(model.L,model.H,boundary=FALSE) #De acordo com o p-valor, ha diferenca significativa entre os modelos


# Explicacao para o tratamento da heterocedasticidade: residual=~dsum(~units|TREATMENT)
# [~units]: representa o erro residual de cada observação.[TREATMENT]: separa as observações por níveis de tratamento. [dsum()]: constrói uma soma direta (block diagonal) da matriz residual.
# Se você tem 3 tratamentos e 30 observações, ele divide a matriz residual em 3 blocos, cada bloco com as observações daquele tratamento.
# Cada bloco tem sua variância residual própria, ou seja, não assume que todos os tratamentos compartilham a mesma variância do erro.
# Se você usasse apenas residual = ~ units, o ASReml assumiria homocedasticidade (mesma variância para todos os tratamentos).
# Ao usar dsum(~units | TREATMENT), você está modelando explicitamente a heterocedasticidade, permitindo que cada tratamento tenha sua própria variância residual.
# Dessa forma, Os BLUPs dos efeitos aleatórios (ex: LITTER) ficam mais precisos; A estimação de variâncias e herdabilidade fica mais confiável; Evita subestimar ou superestimar a variabilidade do erro em certos tratamentos.

