#############################
## Fitting Spatial Model
##  Wheat Dataset
#############################

rm(list=ls())
setwd("C:/Users/sgeza/OneDrive/Desktop/Workshops ASReml-R/ASReml_ESALQ/Material/Session07")
library(asreml)      
library(ASRtriala)   
library(desplot)

# Reading dataset
dwheat <- read.table(file='ALLIANCE.TXT', header=TRUE)
head(dwheat)

# Redefining Factors
dwheat$location <- as.factor(as.character(dwheat$location)) 
dwheat$rep <- as.factor(as.character(dwheat$rep)) 
dwheat$ibk <- as.factor(as.character(dwheat$ibk)) 
dwheat$check <- as.factor(as.character(dwheat$check)) 
dwheat$gen <- as.factor(as.character(dwheat$gen)) 
dwheat$row <- as.factor(as.character(dwheat$row)) 
dwheat$col <- as.factor(as.character(dwheat$col)) 
dwheat$rowc <- as.numeric(dwheat$row)  # Covariates
dwheat$colc <- as.numeric(dwheat$col)  # Covariates
str(dwheat)

# Ordering dataset 
dwheat <- dwheat[order(dwheat$col),]
dwheat <- dwheat[order(dwheat$row),]

# Simple EDA
table(dwheat$check)
head(table(dwheat$gen, dwheat$check),20)
table(dwheat$rep, dwheat$ibk)

# Some plotting
desplot(yield~row+col,dwheat, out1=rep)
desplot(yield~row+col,dwheat, out1=rep, out2=check)

#####################
# Starting with Spatial Analyses

# A simple model (respecting design)
mymod <- asreml(fixed = yield ~ 1 + rep,
                random = ~rep:ibk + gen, 
                residual = ~ar1(row):ar1v(col),
                na.action = list(x = "include", y = "include"),
                data = dwheat)
mymod <- update.asreml(mymod)
plot(mymod)
plot(varioGram(mymod))
wald.asreml(mymod, denDF='numeric', ssType='incremental')$Wald
summary(mymod)$varcomp
vpredict(mymod, H2~V2/(V1+V2+V6))

# But checks are not part of the genotypes of interest
table(dwheat$check)
mymod1 <- asreml(fixed = yield ~ 1 + rep + at(check,'1'):gen,
                random = ~rep:ibk + at(check,'0'):gen, 
                residual = ~ar1(row):ar1v(col),
                na.action = list(x = "include", y = "include"),
                subset = location=='Alliance',
                data = dwheat)
mymod1 <- update.asreml(mymod1)
plot(mymod1)
plot(varioGram(mymod1))
wald.asreml(mymod1, denDF='numeric', ssType='incremental')$Wald
summary(mymod1)$varcomp
vpredict(mymod, H2~V2/(V1+V2+V6))

# We have many spatial models: adding splines, trends, etc.

# We have many spatial models, e.g. adding random row and column trends
mymod.tt <- asreml(fixed = yield ~ 1 + rep + at(check,'1'):gen + pol(rowc,2) + pol(colc,2),
                   random = ~rep:ibk + at(check,'0'):gen + rep:row + rep:col + spl(rowc) + spl(colc), 
                   residual = ~ar1(row):ar1v(col),
                   na.action = list(x = "include", y = "include"),
                   subset = location=='Alliance',
                   data = dwheat)
mymod.tt <- update.asreml(mymod.tt)
plot(mymod.tt)
plot(varioGram(mymod.tt))
summary(mymod.tt)$varcomp

############
# Let's go to final selected model

mymod.sel <- asreml(fixed = yield ~ 1 + rep + at(check,'1'):gen,
                   random = ~rep:ibk + at(check,'0'):gen + rep:row, 
                   residual = ~ar1(row):ar1v(col),
                   na.action = list(x = "include", y = "include"),
                   subset = location=='Alliance',
                   data = dwheat)
mymod.sel <- update.asreml(mymod.sel)
plot(mymod.sel)
plot(varioGram(mymod.sel))
summary(mymod.sel)$varcomp

# H2 - Traditional Definition
(H2<-vpredict(mymod.sel,H2~V3/(V1+V2+V3+V7)))   

# H2 - PEV Definition
BLUP <- as.data.frame(summary(mymod.sel,coef=TRUE)$coef.random)
BLUP <- BLUP[21:293,]
BLUP$PEV <- BLUP$std.error^2
vc <- summary(mymod.sel)$varcomp
(herit <- 1 - mean(BLUP$PEV)/vc[3,1])
BLUP$rel <- round(1 - BLUP$PEV/vc[3,1],4)
View(BLUP)

############
############
# Lets try with ASRtriala a series of spatial analyses
? ASRtriala::select.single

# All possible 640 models?
model.setup <- ASRtriala::model.setup
str(model.setup)
# Filtering for a subset
specs.single <- model.setup[model.setup$add.rep == FALSE & model.setup$add.ibk == FALSE &
                   (model.setup$trend.row == "none" | model.setup$trend.row == "linear") & 
                   (model.setup$trend.col == "none" | model.setup$trend.col == "linear") & 
                   (model.setup$type.residual == "indep" | model.setup$type.residual == "ar1.rowcol") &
                    model.setup$add.nugget == FALSE, ]
dim(specs.single)  # 10 or 40 models
# Fitting all models
head(dwheat)
models.stats <- select.single(data = dwheat, gen = "gen",
                     check = "check", ibk = "ibk", row = "row",
                     col = "col", resp = "yield", type.gen = "random",
                     data.model = specs.single, threshold = 4.0, criteria = 'h2.pev')
models.stats$gof.stats

# Model selected (criteria h2.pev)
sel.m <- models.stats$gof.stats[1, ]
mT1.r <- fit.single(data = dwheat, data.model = sel.m,
                    type.gen = 'random',
                    fix.vc = FALSE, threshold = 3.5)
mT1.r$mod$call
plot(mT1.r$mod)  # outliers'
summary(mT1.r$mod)$varcomp
mT1.r$gof.stats

head(mT1.r$predictions,20)

# Do we want Genotype fixed for BLUE? (but using previous var-comp)
mT1.f <- fit.single(data = dwheat, data.model = sel.m,
                    type.gen = 'fixed',
                    fix.vc = TRUE, threshold = 4.0)
mT1.f$mod$call
plot(mT1.f$mod)  # outliers'
summary(mT1.f$mod)$varcomp
mT1.f$gof.stats

head(mT1.f$predictions,20)
