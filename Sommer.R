# Simulando dados
set.seed(123)
genotipo <- factor(rep(1:10, each = 3)) # 10 genótipos, 3 repetições
bloco <- factor(rep(1:3, times = 10))
media_gen <- rnorm(10, mean = 100, sd = 5) # efeito genético
erro <- rnorm(30, mean = 0, sd = 2) # erro residual
produtividade <- rep(media_gen, each = 3) + erro

dados <- data.frame(produtividade, genotipo, bloco)


install.packages("lme4")
library(lme4)

# Ajustando o modelo com o genótipo como efeito aleatório
modelo <- lmer(produtividade ~ 1 + (1 | genotipo), data = dados, REML = TRUE)

# Visualizar os componentes de variância (estimados via REML)
summary(modelo)

# BLUPs dos genótipos
ranef(modelo)$genotipo

# Fazendo pelo pacote Sommer

install.packages("sommer")
library(sommer)

modelo_sommer <- mmer(
  produtividade ~ 1,                      # efeito fixo (média)
  random = ~ genotipo,                    # efeito aleatório: genótipos
  data = dados,
  dateWarning = FALSE
)

# BLUPs dos genótipos
blups <- modelo_sommer$U$genotipo

# Acurácias dos BLUPs
acuracias <- summary(modelo_sommer)$Ucorr$genotipo

# Visualizar
resultados <- data.frame(
  Genotipo = rownames(blups),
  BLUP = blups[[1]],
  Acuracia = diag(acuracias)  # só a diagonal da matriz de correlação
)

print(resultados)


