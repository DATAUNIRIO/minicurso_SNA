#-------------------------------------------------
# social network methods
# prof. Steven Ross
# citar a fonte do codigo e dos dados
#-------------------------------------------------


load('star.Rdata')

net3 <- graph_from_data_frame(relacoes, directed=TRUE)

plot(net3)

plot(net3,edge.arrow.size=0.1)

V(net3)
E(net3)


vertex_attr(net3) # all attributes of the nodes
edge_attr(net3) # all attributes of the edges
net3[] # adjacency matrix
net3[1,] # first row of adjacency matrix

par(mar=c(0,0,0,0))
plot(net3)
plot(net3,
     vertex.color = "grey", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = .75, # change size of labels to 75% of original size
     edge.curved=.25, # add a 25% curve to the edges
     edge.color="grey20") # change edge color to grey

V(net3)$size <- strength(net3)
plot(net3)
# taking the log to improve it
V(net3)$size <- log(strength(net3)) 

V(net3)$label <- V(net3)$name
V(net3)$degree <- degree(net3)
V(net3)$size <- degree(net3)
plot(net3)

V(net3)$label <- ifelse(strength(net3)>=10, V(net3)$name, NA)
plot(net3)

# create vectors with characters in each side
dark_side <- c("DARTH VADER", "MOTTI", "TARKIN")
light_side <- c("R2-D2", "CHEWBACCA", "C-3PO", "LUKE", "CAMIE", "BIGGS",
                "LEIA", "BERU", "OWEN", "OBI-WAN", "HAN", "DODONNA",
                "GOLD LEADER", "WEDGE", "RED LEADER", "RED TEN", "GOLD FIVE")
other <- c("GREEDO", "JABBA")

# node we'll create a new color variable as a node property
V(net3)$color <- NA
V(net3)$color[V(net3)$name %in% dark_side] <- "red"
V(net3)$color[V(net3)$name %in% light_side] <- "lightblue"
V(net3)$color[V(net3)$name %in% other] <- "grey20"
vertex_attr(net3)
plot(net3)
legend(x=.75, y=.75, legend=c("Dark side", "Light side", "Other"), 
       pch=21, pt.bg=c("red", "lightblue", "grey20"), pt.cex=2, bty="n")

E(net3)$width <- log(E(net3)$weight) + 1
edge_attr(net3)

par(mfrow=c(2, 3), mar=c(0,0,1,0))
plot(net3, layout=layout_randomly, main="Random")
plot(net3, layout=layout_in_circle, main="Circle")
plot(net3, layout=layout_as_star, main="Star")
plot(net3, layout=layout_as_tree, main="Tree")
plot(net3, layout=layout_on_grid, main="Grid")
plot(net3, layout=layout_with_fr, main="Force-directed")

#The most popular layouts are force-directed. 
#These algorithms, such as Fruchterman-Reingold, 
#try to position the nodes so that the edges have similar length 
#and there are as few crossing edges as possible. 
#The idea is to generate “clean” layouts, where nodes that are 
#closer to each other share more connections in common that those
#that are located further apart. Note that this is a 
#non-deterministic algorithm: choosing a different seed will 
#generate different layouts.

par(mfrow=c(1,1))


#------------------------------------------------------------
set.seed(12345)
par(mfrow=c(2,2))

plot(net3,
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)

plot(net3,
     vertex.color = rainbow(52),
     vertex.size = V(net3)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold)

plot(net3,
     vertex.size=30,
     vertex.color = "skyblue", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = 1.5, # change size of labels to 75% of original size
     #edge.curved=.25, # add a 25% curve to the edges
     edge.arrow.size = 0.01,
     edge.color="grey20") # change edge color to grey

plot(net3,
     vertex.size=30,
     vertex.color = "lightgreen", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = 1.5, # change size of labels to 75% of original size
     #edge.curved=.25, # add a 25% curve to the edges
     edge.arrow.size = 0.8,
     edge.color="grey20") # change edge color to grey


#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Load package
library(networkD3)

# Create data
nomes <- c("Ciro" , "Eymael" , "Felipe" , "Jair" , 
           "Léo" , "Luiz" , "Kelmon" , "Simone" , "Sofia" , "Vera")  

dados <- data.frame(
  src = sample(nomes, 50, TRUE),
  target = sample(nomes, 50, TRUE),
  weight = runif(50)
)
# Plot
simpleNetwork(dados)

# Load data
data(MisLinks)
data(MisNodes)
View(MisNodes)
View(MisLinks)
# Plot

forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)


#### Os Miseraveis (Vitor Hugo)
load('os_miseraveis.Rdata')

forceNetwork(Links = os_miseraveis$links, Nodes = os_miseraveis$nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4, bounded = TRUE,
             opacityNoHover = TRUE)

# Encontrando o Valjean (11)
which(MisNodes == "Valjean", arr = TRUE)[1] - 1
ValjeanInds = which(MisLinks == 11, arr = TRUE)[, 1]
# Criando um vetor de cores
ValjeanCols = ifelse(1:nrow(MisLinks) %in% ValjeanInds, "#bf3eff", "#666")

forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8, linkColour = ValjeanCols)


#--------------------------------------------------------------------
# Uso do igraph to make a rede e o networkD3 para visualização

# igraph
karate <- make_graph("Zachary")
wc <- cluster_walktrap(karate)
members <- membership(wc)

# Convertendo o igraph em networkD3
karate_d3 <- igraph_to_networkD3(karate, group = members)

# networkD3
forceNetwork(Links = karate_d3$links, Nodes = karate_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group')

#simpleNetwork(networkData) %>%
#  saveNetwork(file = 'Net1.html')



