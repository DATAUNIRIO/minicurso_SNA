#-------------------------------------------------
# social network methods
# prof. Steven Ross
#-------------------------------------------------

load('star.Rdata')
net3 <- graph_from_data_frame(relacoes, directed=TRUE)

plot(net3)
cliques(net3)
cliques(net3, min=6)
largest_cliques(net3)


### dados do clube de karate club do Zachary

karate <- make_graph("Zachary")
class(karate)
plot(karate)

hub_score(karate)$vector
authority_score(karate)$vector

grupos <- cluster_optimal(karate)
grupos

### Greedy algorithm
fc <- cluster_fast_greedy(karate)

memb <- membership(fc)

plot(karate, vertex.color=memb)
#ord1 <- order(membership(fc))

### Comparacao de algoritmos
communities <- list()

### cluster_edge_betweenness
ceb <- cluster_edge_betweenness(karate)
communities$`Edge betweenness` <- ceb

### cluster_fast_greedy
fc <- cluster_fast_greedy(karate)
communities$`Fast greedy` <- fc

### cluster_leading_eigen
lec <- cluster_leading_eigen(karate)
communities$`Leading eigenvector` <- lec

### cluster_spinglass
sc <- cluster_spinglass(karate, spins=10)
communities$`Spinglass` <- sc

### cluster_walktrap
wt <- cluster_walktrap(karate)
communities$`Walktrap` <- wt

### cluster_label_prop
labprop <- cluster_label_prop(karate)
communities$`Label propagation` <- labprop

