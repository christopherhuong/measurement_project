library(tidyverse)
library(haven)
library(knitr)

bgrd <- read_sav('C:/Users/shg100/Downloads/BGRD432.sav')
mrai <- read_sav('C:/Users/shg100/Downloads/MRAI432_Class.sav')
fdi <- read_sav('C:/Users/shg100/Downloads/FDI432_Class.sav')
saei <- read_sav('C:/Users/shg100/Downloads/SAEI432_Class.sav')



dat <- bgrd


mrai <- mutate(mrai, MRAIrevenge = V3 + V6 + V10 + V12 + V13 + V19 + V20)
mrai <- mutate(mrai, MRAIrumination = V1 + V2 + V4 + V7 + V9 + V15 + V17)
mrai <- mutate(mrai, MRAIsuicide = V5 + V8 + V11 + V14 + V16 + V18 + V21)


dat <- add_column(dat,
              MRAIrevenge = mrai$MRAIrevenge,
              MRAIrumination = mrai$MRAIrumination,
              MRAIsuicide = mrai$MRAIsuicide)
  

dat <- add_column(dat,
              FDIsuicideorientation = fdi$FDI24_SO,
              FDIpositivefocus = fdi$FDI24_PF,
              FDInegativefocus = fdi$FDI24_NF)  
  
  
dat <- add_column(dat,
                 SAEIsuiciderumination = saei$Srumi,
                 SAEImaladaptiveexpression = saei$MalExp,
                 SAEIreactivedistress = saei$ReDis,
                 SAEIadaptiveexpression = saei$AdaEx)
  














