library(haven)
library(tidyverse)



##########################################################
###                  LOAD IN DATA                      ###
##########################################################
setwd("C:/Users/chris/Downloads/MISC/measureproject/data")

sbqr_demo <- read_sav("BGRD683.sav")

panas <- read_sav("PANAS683.sav")

sadli <- read_sav("SADLI683.sav")

spai <- read_sav("SPAI683.sav")



##########################################################
###                   WRANGLING                        ###   
##########################################################

### CHECK IF SUBJECTS IN DATA SETS ARE THE SAME

all(sbqr_demo$gender == panas$gender,
    sbqr_demo$gender == sadli$gender,
    sbqr_demo$gender == spai$gender,
    sbqr_demo$age == sadli$age,
    sbqr_demo$age == spai$age)

### CHECK FOR MISSING DATA

sum(is.na(sbqr_demo))
sum(is.na(sadli))
sum(is.na(panas))
sum(is.na(spai))

##########################################################
###        SOCIAL ANXIETY AND DEPRESSION LIFE INTERFERENCE INVENTORY - 24
###        SADLI-24
###
###        COMPUTE SUM SCORES FOR SOCIAL ANXIETY LIFE INTERFERENCE - 12
sadli <- mutate(sadli,
                sadli_sa = V3 + V4 + V7 + V10 + V11 + V12 +
                  V14 + V15 + V20 + V21 + V23 + V24)

###        COMPUTE SUM SCORES FOR DEPRESSION LIFE INTERFERENCE - 12

sadli <- mutate(sadli,
                sadli_depr = V1 + V2 + V5 + V6 + V8 + V9 +
                V13 + V16 + V17 + V18 + V19 + V22)

library(psych)
library(knitr)


kable(describe(sadli[,1:24]),
      digits = 2)

### Social Anxiety Life Interference reliability

alpha(sadli[,c("V3", "V4", "V7", "V10", "V11", "V12", "V14",
               "V15", "V20", "V21", "V23", "V24")])

omegah(sadli[,c("V3", "V4", "V7", "V10", "V11", "V12",
                "V14","V15", "V20", "V21", "V23", "V24")],
         nfactors = 1)


### Depression Life Interference reliability

alpha(sadli[, c("V1", "V2", "V5", "V6", "V8", "V9",
                      "V13", "V16", "V17", "V18", "V19", "V22")])

omegah(sadli[, c("V1", "V2", "V5", "V6", "V8", "V9",
                 "V13", "V16", "V17", "V18", "V19", "V22")],
       nfactors = 1)


### Distribution


multi.hist(sadli[,c("V3", "V4", "V7", "V10", "V11", "V12", "V14",
              "V15", "V20", "V21", "V23", "V24")])


multi.hist(sadli[, c("V1", "V2", "V5", "V6", "V8", "V9",
                     "V13", "V16", "V17", "V18", "V19", "V22")])
  
  
  
##########################################################
###       SOCIAL PHOBIA AND ANXIETY INVENTORY - 23 (SPAI-23)
###      
###       COMPUTE SUM SCORES FOR SOCIAL PHOBIA SCALE

spai <- mutate(spai,
               spai_sp = V1 + V2 + V3 + V4 + V5 + V6 + V7 +
                 V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16)

###       COMPUTE SUM SCORES FOR AGORAPHOBIA SCALE

spai <- mutate(spai,
               spai_agora = V17 + V18 + V19 + V20 + V21 + V22 + V23)



### POSTIVE AND NEGATIVE AFFECT SCALE (PANAS)
### POSITIVE AFFECT

panas <- mutate(panas,
                pos = V1 + V3 + V5 + V9 + V10 + V12 + V14 + V16 + V17 + V19)

### NEGATIVE AFFECT
panas <- mutate(panas,
                neg = V2 + V4 + V6 + V7 + V8 + V11 + V13 + V15 + V18 + V20)



#########################################################
###                SUMMARY STATISTICS                 ###
#########################################################





#########################################################

###                         CFA                       ###
#########################################################





library(lavaan)

# cfa_model <- '
# #factor loadings
# sa =~ 1*V3 + V4 + V7 + V10 + V11 + V12 + V14 + V15 + V20 + V21 + V23 + V24
# depr =~ 1*V1 + V2 + V5 + V6 + V8 + V9 + V13 + V16 + V17 + V18 + V19 + V22
# 
# #residual variances
# V1 ~~ V1
# V2 ~~ V2
# V3 ~~ V3
# V4 ~~ V4
# V5 ~~ V5
# V6 ~~ V6
# V7 ~~ V7
# V8 ~~ V8
# V9 ~~ V9
# V10 ~~ V10
# V11 ~~ V11
# V12 ~~ V12
# V13 ~~ V13
# V14 ~~ V14
# V15 ~~ V15
# V16 ~~ V16
# V17 ~~ V17
# V18 ~~ V18
# V19 ~~ V19
# V20 ~~ V20
# V21 ~~ V21
# V22 ~~ V22
# V23 ~~ V23
# V24 ~~ V24
# 
# #factor variance / covariance
# sa ~~ sa
# sa ~~ depr
# depr ~~ depr
# '
# 
# 
# 
# cfafit <- lavaan(cfa_model, data = sadli, std.lv = TRUE)
# 
# summary(cfafit,
#         fit.measures = TRUE, standardized = TRUE)
# 
# 


#  CFI = 0.883        CUT OFF  = ABOVE 0.90
#  TLI = 0.872        CUT OFF = ABOVE 0.95


# RMSEA = 0.079        CUT OFF = < 0.08   =   ADEQUATE FIT
# SRMR = 0.055          CUT OFF = < 0.08 = EXCELLENT FIT






library(lavaan)


cfa_model1 <- '
sa =~ V3 + V4 + V7 + V10 + V11 + V12 + V14 + V15 + V20 + V21 + V23 + V24
depr =~ V1 + V2 + V5 + V6 + V8 + V9 + V13 + V16 + V17 + V18 + V19 + V22
'


cfa_fit1 <- cfa(cfa_model1, data = sadli, std.lv = TRUE)
summary(cfa_fit1,
        fit.measures = TRUE, standardized = TRUE)


library(semPlot)

semPaths(cfa_fit1, "par", "std",
         style = "lisrel",
         theme = "colorblind",
         rotation = 2,
         layout = "tree2",
         manifests = paste0("V",1:24),
         reorder = F,
         cardinal = "lat cov")



sbqr_demo <- sbqr_demo %>%    #add and rename relevant variables to new df
  add_column(  spai_sp = spai$spai_sp,
               spai_agora = spai$spai_agora,
               panas_p = panas$pos,
               panas_n = panas$neg,
               sadli_sa = sadli$sadli_sa,
               sadli_depr = sadli$sadli_depr,
  )
              
  



VAR_COR <- sbqr_demo%>%
  select(sadli_sa, sadli_depr, spai_sp, spai_agora, panas_p, panas_n, sbqr1, sbqr2, sbqr3, sbqr4)


library(psych)

cor_matrix <- cor(VAR_COR, use = "pairwise.complete.obs") 
cor_matrix


# Correlogram w/significance using corrplot
library(corrplot)
correlation.pvalues <- cor.mtest(cor_matrix)

corrplot(cor_matrix, method = "number", type = "full", 
         tl.col = "black", order = "original",
         p.mat = correlation.pvalues[["p"]], sig.level = .05)



corrplot(cor_matrix, method = "number",col="black", tl.col="black", type = "upper")



              
              
              
              
              
              
              
              
              
              
              
              
              
              
              




