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

<<<<<<< HEAD
library(knitr)
kable(describe(sadli[,1:24]))

alpha(sadli)

=======
>>>>>>> a20ad0b45d30809999d6fc47d48c4df5ab3dfdca
##########################################################
###       SOCIAL PHOBIA AND ANXIETY INVENTORY - 23 
###       SPAI-23
###      
###       COMPUTE SUM SCORES FOR SOCIAL PHOBIA SCALE

spai <- mutate(spai,
               spai_sp = V1 + V2 + V3 + V4 + V5 + V6 + V7 +
                 V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16)

###       COMPUTE SUM SCORES FOR AGORAPHOBIA SCALE

spai <- mutate(spai,
               spai_agora = V17 + V18 + V19 + V20 + V21 + V22 + V23)

#########################################################
###       SUICIDAL BEHAVIORS QUESTIONNAIRE - REVISED
###       SBQ-R


<<<<<<< HEAD
### POSTIVE AND NEGATIVE AFFECT SCALE
### POSITIVE AFFECT

panas <- mutate(panas,
                pos = V1 + V3 + V5 + V9 + V10 + V12 + V14 + V16 + V17 + V19)

### NEGATIVE AFFECT
panas <- mutate(panas,
                neg = V2 + V4 + V6 + V7 + V8 + V11 + V13 + V15 + V18 + V20)



#########################################################
###                SUMMARY STATISTICS                 ###
#########################################################


=======
>>>>>>> a20ad0b45d30809999d6fc47d48c4df5ab3dfdca






#########################################################
<<<<<<< HEAD
###                         CFA                       ###
#########################################################





library(lavaan)

cfa_model <- 'sa =~ V3 + V4 + V7 + V10 + V11 + V12 + V14 + V15 + V20 + V21 + V23 + V24
              depr =~ V1 + V2 + V5 + V6 + V8 + V9 + V13 + V16 + V17 + V18 + V19 + V22'



cfafit <- cfa(cfa_model, data = sadli, std.lv = TRUE)

kable(summary(cfafit, fit.measures = T))




#  CFI = 0.883        CUT OFF  = ABOVE 0.90
#  TLI = 0.872        CUT OFF = ABOVE 0.95


# RMSEA = 0.079        CUT OFF = < 0.08   =   ADEQUATE FIT
# SRMR = 0.055          CUT OFF = < 0.08 = EXCELLENT FIT


=======
###                SUMMARY STATISTICS                 ###
#########################################################
>>>>>>> a20ad0b45d30809999d6fc47d48c4df5ab3dfdca















dat <- data.frame()[1:683, ]  # create new DF with n rows

sbqr_demo <- sbqr_demo %>%                    #add and rename relevant variables to new df
  add_column(  sadli_sa = sadli$sadli_sa,
               sadli_depr = sadli$sadli_depr,
               spai_sp = spai$spai_sp,
               spai_agora = spai$spai_agora,
               panas_p = panas$pos,
               panas_n = panas$neg
              
              
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



              
              
              
              
              
              
              
              
              
              
              
              
              
              
              




