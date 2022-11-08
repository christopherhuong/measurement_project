library(tidyverse)

# 
# panas <- read.delim("C:/Users/shg100/Downloads/PANAS20.DAT",
#                   header = F)
# 
# masq <- read.delim("C:/Users/shg100/Downloads/BGRDMASQ.DAT",
#                 header = F)




panas1 <- read.fwf("C:/Users/shg100/Downloads/PANAS20.DAT", width = c(1,1,1,1,1,
                                1,1,1,1,1,
                                1,1,1,1,1,
                                1,1,1,1,1,
                                4,2))

masq1 <- read.fwf("C:/Users/shg100/Downloads/BGRDMASQ.DAT", width = c(4,2,3,2,2,2,3,2,2,2))


mergedat <- cbind(panas1, masq1)

mergedat <- rename(mergedat, interest = 1, 
                        distressed = 2,
                        excited = 3,
                        upset = 4,
                        strong = 5,
                        guilty = 6,
                        scared = 7,
                        hostile = 8,
                        enthus = 9,
                        proud = 10,
                        irritable = 11,
                        alert = 12,
                        ashamed = 13,
                        inspired = 14,
                        nervous = 15,
                        determined = 16,
                        attentive = 17,
                        jittery = 18,
                        active = 19,
                        afraid = 20,
                        ID = 21,
                        sex = 22)

# mergedat <- mutate(mergedat, posaffect =     interest + 
#                                              excited +
#                                              strong +
#                                              enthus +
#                                              proud +
#                                              alert +
#                                              inspired +
#                                              determined +
#                                              attentive +
#                                              active,
#                             negaffect = distressed +
#                                         upset +
#                                         guilty +
#                                         scared +
#                                         hostile +
#                                         irritable +
#                                         ashamed +
#                                         nervous +
#                                         jittery +
#                                         afraid)

library(psych)
library(corrplot)
library(GPArotation)
library(lavaan)
library(ltm)

efadat <- select(mergedat, c(1:20))


cronbach.alpha(efadat)



cormatrix <- cor(efadat)

corrplot(cormatrix)

KMO(cormatrix)   #factorability of data, MSA = 0.89 = very good

cortest.bartlett(cormatrix, n = 700)      #small p = factorability

# fafitfree <- fa(efadat,nfactors = ncol(cormatrix), rotate = "none")
# n_factors <- length(fafitfree$e.values)
# scree     <- data.frame(
#   Factor_n =  as.factor(1:n_factors), 
#   Eigenvalue = fafitfree$e.values)
# ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
#   geom_point() + geom_line() +
#   xlab("Number of factors") +
#   ylab("Initial eigenvalue") +
#   labs( title = "Scree Plot", 
#         subtitle = "(Based on the unreduced correlation matrix)")

parallel <- fa.parallel(cormatrix)


efamodel <- fa(cormatrix,
               nfactors  = 2,
               n.obs = 700,
               rotate = "oblimin",
               fm = "pa",
               use = "pairwise")


efamodel


model1 <- 'posaffect =~ active + alert + attentive + determined + enthus + excited + inspired + interest + proud + strong
           negaffect =~ afraid + ashamed + distressed + guilty + hostile + irritable + jittery + nervous + scared + upset'

model1fit <- cfa(model1, data = efadat)
summary(model1fit)

library(semPlot)
semPaths(model1fit, "std", "est")

#########################################


###########################

b5par <- fa.parallel(big5_cors)

b5model <- fa(big5_cors, nfactors = 5,
              n.obs = 500,
              rotate = 'oblimin',
              fm = "pa",
              use = 'pairwise')


b5model




######################################

Q <- qgraph(cormatrix)


posneggroup <- list(
  posaffect = c(1,3,5,9,10,12,14,16,17,19),
  negaffect = c(2,4,6,7,8,11,13,15,18,20))





affectcor <- cor_auto(efadat, detectOrdinal = F)
QG <- qgraph(cor(efadat), minimum = 0.25, groups = posneggroup,
             sampleSize = 700, graph = "cor",
             legend = T, borders = F, title = "affect")




































