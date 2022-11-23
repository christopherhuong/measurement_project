library(tidyverse)
library(haven)
library(ggpubr)
library(psych)
library(knitr)
library(corrplot)
library(GPArotation)
library(lavaan)


dat <- read_sav('C:/Users/shg100/Downloads/MRAI432_Class.sav')


kable(describe(dat[,1:21]))




efadat <- dat[,1:21]
alpha(efadat)




revenge <- select(efadat, V3, V6, V10, V12, V13, V19, V20)
rumination <- select(efadat, V1, V2, V4, V7, V9, V15, V17)
suicide <- select(efadat, V5, V8, V11, V14, V16, V18, V21)


alpha(revenge)


revenge <- mutate(revenge, revengesumscore = rowSums(revenge))
rumination <- mutate(rumination, ruminationsumscore = rowSums(rumination))
suicide <- mutate(suicide, suicidesumscore = rowSums(suicide))


# a = 0.907, high internal consistency = high inter-item correlations

cordat <- cor(efadat)
corrplot(cordat,
         method = 'number',
        is.corr = F )

ggplot(dat, aes(x = V1)) + geom_histogram(binwidth = .5)




revengeFA <- fa(revenge1, nfactors = 2,
                rotate = 'promax',
                fm = 'pa',
                use = 'pairwise')

revengeFA
revenge1 <- select(revenge, V3, V6, V10, V13)
alpha(revenge1)





########################

KMO(cordat)   #factorability of data, MSA = 0.89 = very good

cortest.bartlett(cordat, n = 432)      #small p = factorability

parallel <- fa.parallel(efadat)


efamodel <- fa(cordat,
               nfactors  = 3,
               n.obs = 432,
               rotate = "promax",
               fm = "ml",
               use = "pairwise")



efamodel




pca <- prcomp(efadat, center = T, scale. = T)
summary(pca)


### good/adequate fit indices
# significant chi square
# RMSEA < 0.060
# RMSR  < 0.050
# CFI   > 0.90

## remove iems with cross-loadings
efadat2 <- select(efadat, -c())


library(lavaanPlot)


cfa3 <- 'factor1 =~ V1 + V2 + V4 + V7 + V9 + V15 + V17
         factor2 =~ V5 + V8 + V11 + V14 + V16 + V18 + V21
         factor3 =~ V3 + V6 + V10 + V12 + V13+  V19 + V20'



cfa3fit <- cfa(cfa3, data = efadat, std.lv = TRUE
               )

summary(cfa3fit, fit.measures = T)




# Factors
fit <- 'Factor1 =~ lambda_1_1*V1 + lambda_1_2*V2 + lambda_1_3*V4 + lambda_1_4*V7 + lambda_1_5*V9 + lambda_1_6*V15 + lambda_1_7*V17
        Factor2 =~ lambda_2_1*V5 + lambda_2_2*V8 + lambda_2_3*V11 + lambda_2_4*V14 + lambda_2_5*V16 + lambda_2_6*V18 + lambda_2_7*V21
        Factor3 =~ lambda_3_1*V3 + lambda_3_2*V6 + lambda_3_3*V10 + lambda_3_4*V12 + lambda_3_5*V13 + lambda_3_6*V19 + lambda_3_7*V20'


cfafit <- cfa(fit, data = efadat, std.lv=TRUE)
summary(cfafit, fit.measures = T)

lavaanPlot(name = "plot", cfafit, coefs = T, stand = T)

