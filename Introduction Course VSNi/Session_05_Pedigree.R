# Session 05 - Incorporating Pedigree Information in a Genetic Model

# Video: Genetic Relationships in ASReml
# Example of pedigree in ASReml-R4: https://www.youtube.com/watch?v=HcJd2m4hRIY&t=49s

# Description of the experiment:
# A group of 98 tawny dragon lizards (40 parents and 58 offsprings) were studied to evaluate the level of inheritance of their colour expression on their throats.
# The proportion of orange, yellow and grey was measured on their throats, and there is additional information on cohort and sex that is relevant for this analysis.
# In addition, pedigree information (sire and dam) is available for 58 offspring individuals.
# The analysis should be done by fitting an Animal Model that incorporates pedigree.

# Objective:
# Determine the level of genetic control of colour expression by fitting an Animal Model. -> calcularemos algumas herdabilidades e estimar alguns efeitos geneticos (BLUES)

# yijk = μ + αi + βj + aijk + ϵijk
#αi = fixed effect of the i-th cohort level
#βj = fixed effect of the j-th sex level
#aijk = random effect (efeito aditivo) of the ijk-th individual, with aijk ∼N(0,σ2 A), that is associated with a relationship matrix A -> faz com que cada individuo esteja relacionado com outro


rm(list=ls()) 
setwd("~/Documents/Mixed_Models/Introduction Course VSNi/Datasets/LIZARD")
library(asreml)

# Reading Phenotypic data
LIZARD<-read.table("LIZARD.TXT",h=TRUE,na.strings='NA')
head(LIZARD)

## Na base de dados temos na primeira parte a matriz de relacionamento, e depois informacoes referentes aos individuos
## Pode-se observar 3 variaveis resposta (3 ultimas colunas)

# EDA
summary(LIZARD) # Para estatisticas referentes as variaveis resposta
hist(LIZARD$P_YELLOW)
boxplot(LIZARD$P_YELLOW)
table(LIZARD$SEX,LIZARD$COHORT) #Nao balanceado

# Defining Factors
LIZARD$INDIV<-as.factor(LIZARD$INDIV)
LIZARD$COHORT<-as.factor(LIZARD$COHORT)
LIZARD$SEX<-as.factor(LIZARD$SEX)
str(LIZARD)

# Obtaining relationships from pedigree - A matrix
# install.packages("nadiv") auxilia na construção de matrizes de parentesco (A, G, H) e na decomposição de variâncias aditivas em componentes não-aditivos

library(nadiv)
pedind<-LIZARD[,c(2:4)] #Aqui retiramos da base completa as colunas da matrix de relacionamento
head(pedind)

APED<-makeA(pedind) # Para construcao de matriz de parentesco aditiva.
APED[1:10,1:10] # Como os primeiros sao os pais, nao temos infos de parentesco
APED[40:50,40:50] # Como os ultimos sao individuos com infos dos pais, temos correlacoes 


# Obtaining inverse of relationships from pedigree - A matrix
ainv<-ainverse(pedind)
head(ainv)

# Fitting Animal Model
mod.ind<-asreml(fixed=P_YELLOW~COHORT+SEX,
                random=~vm(INDIV,ainv), # Variance Model - Como is individuos nao sao independentes, usamos a estrutura vm(Variavel, Matrix de Relacionamento Inversa) - a matrix A entra inversa no ASReml
                residual=~idv(units), # Aqui usa-se a estrutura idv() pois as variaveis sao independentes, mas tbm nao teria necessidade de usar, pois eh o defaut do ASReml 
                data=LIZARD)

mod.ind<-update.asreml(mod.ind) #Para ter iteracoes adicionais
plot(mod.ind) 
#Como se tratade proporcao, a maioria das observacoes estarao entre 0 e 1, entao podemos ter alguns problemas com a distribuicao -> Transformacao 
summary(mod.ind)$varcomp

# LOG Transformation Proportion - z = ln[(P + c)/(100 - P + c)]
LIZARD$logitPY<-log((LIZARD$P_YELLOW+1)/(100-LIZARD$P_YELLOW+1)) #Escolhemos c=1
hist(LIZARD$P_YELLOW)
hist(LIZARD$logitPY)

# Modelo Pos Transformacao 
mod.indT<-asreml(fixed=logitPY~COHORT+SEX,
                 random=~vm(INDIV,ainv),
                 residual=~idv(units),
                 data=LIZARD)
mod.indT<-update.asreml(mod.indT)
plot(mod.indT)
summary(mod.indT)$varcomp

# Calculating heritability
vpredict(mod.indT,h2~V1/(V1+V2))

# Obtaining BLUP & Predictions
BLUP<-as.data.frame(summary(mod.indT,coef=TRUE)$coef.random)
head(BLUP)
BLUP$Mean<-mean(LIZARD$logitP_YELLOW)
BLUP$Preds<-BLUP$Mean+BLUP$solution

# Back-transformation: P = [(100 + c)*exp(z) - c]/[1 + exp(z)]
BLUP$BT_Preds<-((100+1)*exp(BLUP$Preds)-1)/(1+exp(BLUP$Preds))
head(BLUP)
View(BLUP)

