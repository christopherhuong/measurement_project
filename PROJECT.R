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





#########################################################
###                SUMMARY STATISTICS                 ###
#########################################################









