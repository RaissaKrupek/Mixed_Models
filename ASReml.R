library(asreml)
asreml.license.activate()
# DGAF-AADG-BJDC-CGIF
detach("package:asreml")

# LMM for a randomized complete block design - Wheat varieties
d001 <- read.table(here::here("Data_Exemples", "BESAG_ELBATAN.txt"), header = TRUE)

head(d001)
summary(d001)
str(d001)

d001$gen <- as.factor(d001$gen)
d001$col <- as.factor(d001$col)

asr001 <- asreml(
  fixed = yield ~ col,
  random = ~gen,
  residual = ~units,
  na.action = na.method("include"),
  data = d001)

## units is the residual structure (standard in asreml to denote individual-level residuals).
## "include" significa que as observações com NA não serão removidas do modelo, o que é importante se você quiser estimar efeitos mesmo com dados faltantes

summary(asr001) 

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

# Formas Matriciais
X <- model.matrix(asr001$fixed, data = d001)
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

#-------------------------------------------------------------------------------

# Utilizando mesma base de dados mas ajustando outro modelo
# Aqui adiciona-se a covariavel row -> col and row need to be set as (co)variates.

d001$gen <- as.factor(d001$gen)
d001$row <- as.numeric(d001$row)
d001$col <- as.numeric(d001$col)

asr002 <- asreml(
  fixed = yield ~ col + row,
  random = ~gen,
  residual = ~units,
  data = d001
)

summary(asr002)

wald(asr002, denDF = 'numeric')$Wald

summary(asr002, coef = TRUE)$coef.fixed

vpredict(asr002, H2 ~ V1/(V1+V2))

summary(asr002)$varcomp