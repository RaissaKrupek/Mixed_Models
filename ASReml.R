library(asreml)
asreml.license.activate()
# DGAF-AADG-BJDC-CGIF
detach("package:asreml")

library(ggplot2)
library(tidyverse)
library(car)

# LMM for a randomized complete block design - Wheat varieties
d001 <- read.table(here::here("Data_Exemples", "BESAG_ELBATAN.txt"), header = TRUE)

d001$yield <- log(d001$yield) #transformacao logaritmica dos dados


head(d001)
summary(d001)
str(d001)

d001$gen <- as.factor(d001$gen)
d001$col <- as.factor(d001$col)
# d001$row <- as.factor(d001$row) -> colocar linha como fator?

asr001 <- asreml(
  fixed = yield ~ col,
  random = ~gen,
  residual = ~units,
  na.action = na.method("include"),
  data = d001)

## units is the residual structure (standard in asreml to denote individual-level residuals).
## "include" significa que as observações com NA não serão removidas do modelo, o que é importante se você quiser estimar efeitos mesmo com dados faltantes

summary(asr001) 

#Plots
plot(asr001)

shapiro.test(residuals(asr001))

leveneTest(residuals(asr001), d001$col)

#AIC E BIC
summary(asr001)$aic
summary(asr001)$bic

# Evaluate the statistical significance of fixed effect of block: Wald Test
wald(asr001, denDF = 'numeric')$Wald #nao entendi o denDF

# variance components:
summary(asr001)$varcomp

# Estimate the heritability:
vpredict(asr001, H2 ~ V1/(V1+V2))

#BLUP
BLUP <- summary(asr001, coef = TRUE)$coef.random
head(BLUP)

#BLUE
BLUE <- summary(asr001, coef = TRUE)$coef.fixed
head(BLUE)

# FORMAS MATRICIAIS
X <- model.matrix(~ col, data = d001)

Z <- model.matrix(~ gen - 1, data = d001)
##O -1 remove a coluna do intercepto que o R insere automaticamente.

vc <- summary(asr001)$varcomp
var_gen <- vc["gen", "component"]
G <- diag(var_gen, nlevels(d001$gen))

## "residual = ~units" implica que os erros são independentes com variância constante, entao R=Iσ²
var_res <- vc["units!R", "component"]
R <- diag(var_res, nrow(d001))

dim(X)
dim(Z)
dim(G)
dim(R)

V = (Z %*% G %*% t(Z)) + R

y = d001$yield

# Estimando os BLUE's e BLUP's pela forma matricial 

b = (solve(t(X) %*% solve(V) %*% X)) %*% (t(X) %*% solve(V) %*% y)

u = G %*% t(Z) %*% solve(V) %*% (y - (X %*% b))

list(BLUE = b, BLUP = u)


#-------------------------------------------------------------------------------

# Utilizando mesma base de dados mas ajustando outro modelo
# Aqui adiciona-se a covariavel row -> col and row need to be set as (co)variates.

d001$gen <- as.factor(d001$gen)
d001$col <- as.factor(d001$col)
d001$row <- as.factor(d001$row)

asr002 <- asreml(
  fixed = yield ~ row + col,
  random = ~ gen,
  residual = ~units,
  na.action = na.method("include"),
  data = d001
)

head(d001)
summary(d001)
str(d001)

summary(asr002, coef = T)

wald(asr002, denDF = 'numeric')$Wald

plot(asr002)

shapiro.test(residuals(asr002))

summary(asr002, coef = TRUE)$coef.fixed

vpredict(asr002, H2 ~ V1/(V1+V2))

BLUE <- summary(asr002, coef=T)$coef.fixed

BLUP <- summary(asr002, coef=T)$coef.random

# FORMAS MATRICIAIS
X <- model.matrix(~ row + col, data=d001)

Z <- model.matrix(~gen -1, data=d001)

vc2 <- summary(asr002)$varcomp
var_gen2 <- vc2["gen", "component"]
G <- diag(var_gen2, nlevels(d001$gen))

var_res2 <- vc2["units!R", "component"]
R <- diag(var_res2, nrow(d001))

dim(X)
dim(Z)
dim(G)
dim(R)

V = (Z %*% G %*% t(Z)) + R

y = d001$yield

# Estimando os BLUE's e BLUP's pela forma matricial 

b = (solve(t(X) %*% solve(V) %*% X)) %*% (t(X) %*% solve(V) %*% y)

u = G %*% t(Z) %*% solve(V) %*% (y - (X %*% b))

list(BLUE = b, BLUP = u)

#-------------------------------------------------------------------------------

#xtabs(~ gen + col, d001)

ggplot() +
  geom_tile(aes(x=d001$col, y = d001$row, fill = d001$gen), show.legend = F) +
  theme_void()

# quando o teste wald nao significativo -> efeito pode ser retirado no modelo, mas o que acontece se eu coloca-lo como efeito aleatorio?
# uma vez nao seguindo a dist normal, como descobrir qual eh ?
