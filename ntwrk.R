library(qgraph)

data("big5")
data("big5groups")

# Compute correlation matrix:
big5_cors <- cor_auto(big5, detectOrdinal = FALSE)
# Correlations:
big5Graph <- qgraph(cor(big5),minimum=0.25,groups=big5groups,
                    legend=TRUE,borders=FALSE, title = "Big 5 correlations")

qgraph(big5Graph, layout = "spring")

qgraph(big5Graph, posCol = "blue", negCol = "purple")



















library(psych)
data(bfi)

bfi<-bfi[, 1:25]

cormat <- cor_auto(bfi[, 1:25])

#compute graph with tuning = 0 (BIC)
BICgraph <- EBICglasso(cormat, nrow(bfi), 0, threshold = T)
#compute graph with tuning = 0.5 (EBIC)
EBICgraph <- EBICglasso(cormat, nrow(bfi), 0.5, threshold = T)

layout(t(1:2))
BICgraph <- qgraph(BICgraph, layout = "spring", title = "BIC", details = T)
EBICgraph <- qgraph(EBICgraph, layout = "spring", title = "BIC", details = T)

#plot centrality indices
layout(1)
centralityPlot(list(BIC = BICgraph, EBIC = EBICgraph))
clusteringPlot(list(BIC = BICgraph, EBIC = EBICgraph))







library(igraph)

g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=5, directed = T)



















