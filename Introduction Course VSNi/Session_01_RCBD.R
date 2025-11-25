# Session 01 - RCBD

# E-Book: Exploratory Data Analysis with R (Pend, 2016). Essentials of exploratory techniques for summarizing and understanding data with R. Available here: https://bookdown.org/rdpeng/exdata/ 

# Teremos 3 tratamentos avaliados com 14 pacientes cada. Para o estudo, um total 
# 14 Doctors foram considerados, cada um tendo 3 pacientes com todos os 3 tratamentos
# randomizados. -> Doctors são os blocos.

setwd("~/Documents/Mixed_Models/Introduction Course VSNi/Datasets/PLAYR")
# Com isso, chegamos até a pasta.

RCBD <- read.table("RCBD_DR.txt", header = T)
# Com o comando Header=T, a primeira linha se torna o título
head(RCBD)

# EDA
summary(RCBD)
str(RCBD)
hist(RCBD$Response)
boxplot(RCBD$Response~RCBD$Trt, data=RCBD) # Podemos ver grande diferenca do trat C para com os demais
table(RCBD$Doctor,RCBD$Trt) # Com essa tabela podemos ver se cada bloco possui todos os tratamentos, checando se eh balanceado

# Defining Factors
str(RCBD)
RCBD$Doctor <- as.factor(RCBD$Doctor)
RCBD$Trt <- as.factor(RCBD$Trt)

# Model: y = mu + doctor + treat + e
model0 <- lm(Response~Doctor+Trt, data=RCBD) # Neste caso, todos os efeitos sao fixos -> BLUES
summary(model0)
plot(model0)
anova(model0) # Ha diferenca significativa dos tratamentos, portanto ha efeito de tratamento a 5%.
# Mas quais tratamentos sao diferentes e quais sao as medias?

# Means and LSmeans: enquanto as means sao apenas raw means, sem o o erro 
# estandardizado associado a elas, o LSmean representa o modelo 

aggregate(Response~Trt,FUN=mean,data=RCBD)
aggregate(Response~Trt,FUN=sd,data=RCBD) # esses erros nao estao associados ao modelo e portanto as coisas nao estao sendo corrigidas pelo efeito de bloco

install.packages("emmeans")
library(emmeans)
emmeans(model0,~Trt) # Real media entre os diferentes ambientes
# Nesse caso, as means nao sao diferente das raw means pq o experimento eh balanceado
# Entretanto, os desvios sao bem diferentes 

emmeans(model0,pairwise~Trt,adjust='bon') #Para comparacoes multiplas
# A diferenca mostrada entre os contrastes se alinha bem com o boxplot feito inicialmente