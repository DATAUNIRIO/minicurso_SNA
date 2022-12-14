#-------------------------------------------------
# social network methods
# prof. Steven Ross
#-------------------------------------------------

# vetor
x <- c(0.3, 0.9, 1.1)          
y <- c("jose","maria","joao")          



w <- c("0.2", "12.9", "45.1")          

# data.frame
funcionarios <- data.frame(nome = c("Marx", "Weber", "Durkheim","Arendt", "Maquiavel"),
                           sexo = c("M", "M", "M", "F","M"),
                           salario = c(1000, 1200, 1300, 2000, 500))
funcionarios


turma <- data.frame(nome_aluno = c("Jurgen Habermas", "Karl Popper", "John Rawls","Paulo Freire"),
                    sexo = c("M", "M", "M","M"),
                    CR = c(6.98, 7.01, 7.03, 8.88))
turma

funcionarios$nome
turma$nome_aluno




#--------------------------------------------------

library(igraph)
dev.off()

load('dados_sna.RData')

class(dados)

net <- graph.data.frame(dados, directed=T)
class(net)
V(net)
E(net)

net[] 
net[1,] 

plot(net)

all_simple_paths(net, from = "Luiz", to = "Jair")


distances(net, mode="all")


transitivity(net)

set.seed(12345)

V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

plot(net)

plot(net,
     vertex.color = 'green',
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)

# multidimensional scaling - MDS

plot(net,
     vertex.color = 'skyblue',
     vertex.size = 2,
     edge.arrow.size = 0.1,
     layout=layout_with_mds(net))

plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold)

plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.graphopt)

plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.kamada.kawai)

#----------------------------------------------------
#----------------------------------------------------
#----------------------------------------------------

degree(net)
degree(net, mode="all")
degree(net, mode="in")
degree(net, mode="out")

closeness(net)
closeness(net, mode="all")
closeness(net, mode="in")
closeness(net, mode="out")

betweenness(net)
betweenness(net,
            v = V(net),
            directed = TRUE,
            normalized = FALSE)

eigen_centrality(net)$vector

## Authority and Hub scores
AS <- authority_score(net)$vector
HS <- hub_score(net)$vector


#----------------------------------------------------
plot(net,
     vertex.size=HS*30,
     main = 'Hub',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)

plot(net,
     vertex.size=AS*30,
     main = 'Authoridade',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)

#--------------------------------------------------------
# Comunidade
#--------------------------------------------------------

cnet <- cluster_edge_betweenness(net)
cnet

plot(cnet,
     net,
     edge.arrow.size=0.1,
     vertex.size = 10,
     vertex.label.cex = 0.8)

