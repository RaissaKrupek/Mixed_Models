###############################
## Adjusted Means  
## Apricot Study
###############################

rm(list=ls()) 
setwd("C:/Users/sgeza/OneDrive/Desktop/Workshops ASReml-R/ASReml_ESALQ/Material/Session09/Phenotypic")
library(asreml)
library(ASRgwas)

# Apricot Phenotypic
? pheno.apricot  # only 2006
apr <- read.table("APRICOT.txt", header=T)
head(apr)
table(apr$Ind, apr$Years)

apr$Ind <- as.factor(apr$Ind)
apr$Years <- as.factor(apr$Years)
apr$Lots <- as.factor(apr$Lots)

# Some EDA
hist(apr$Glucose)
boxplot(Glucose ~ Years, data=apr)
boxplot(Glucose ~ Years + Lots, data=apr)

# Single Year data
apr <- apr[order(apr$Lots), ]
m0 <- asreml(fixed = Glucose ~ Lots,
             random = ~Ind,
             residual = ~dsum(~units|Lots),
             subset = Years == "2007",
             data = apr)
plot(m0)
summary(m0)$varcomp

# Moving to Multiple Years (treated as a blocking factor)
apr$grp <- paste(apr$Years, apr$Lots, sep="-")
apr$grp <- as.factor(apr$grp)
head(apr)
my <- asreml(fixed = Glucose ~ Years + Years:Lots,
             random = ~Ind + Years:Ind,
             residual = ~dsum(~units|grp),
             data = apr)
plot(my)
summary(my)$varcomp
vpredict(my, r2B~V1/(V1+V2))
vpredict(my, H2~V1/(V1+V2+(V3+V4+V5+V6+V7+V8)/6))

# Now, move to fixed effects (BLUEs)
myf <- asreml(fixed = Glucose ~ Years + Years:Lots + Ind,
             random = ~ Years:Ind,
             residual = ~dsum(~units|grp),
             data = apr)
summary(myf)$varcomp

# Get Predictions
preds <- predict(myf, classify="Ind", vcov=TRUE)
head(preds$pvals)
View(preds$pvals)

# write.csv(preds$pvals, file='apr_preds.csv')
