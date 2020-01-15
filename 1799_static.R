library(igraph)
library(dplyr)

#Read in the edge and node lists
edges_1799 <- read.csv("~/Documents/network_viz/data-visualization/data/edges_040816.csv", stringsAsFactors = FALSE) 
nodes_1799 <- read.csv("~/Documents/network_viz/data-visualization/data/nodes_040816.csv", stringsAsFactors = FALSE) 

#Convert the edge list to a graph object
graph_1799 <- graph.data.frame(edges_1799, directed = "FALSE")

#Add the people with no known relationships as extra vertices
graph_1799 <- graph_1799 + vertex("Abram C", "Adam B", "Adam C", "Anthony A", "Arlington A",
                                  "Bacchus A", "Bath A", "Betty E", "Betty O", "Betty P", 
                                  "Bob C", "Bristol A", "CAESAR A", "Caesar B", "Charles G",
                                  "Cloe A", "Cupid A", "Cupid B", "Daphne D", "Daphne F",
                                  "Davy F", "Davy G", "Dicke E", "Dorothy A", "Dublin A",
                                  "Dula A", "Dundee B", "Essex A", "Fattimore A", "Fattimore B",
                                  "Flora B", "Frank B", "Frederick A", "George G", "George P",
                                  "Giles A", "Glasgow A", "Grig A", "Guy B", "Hannah E",
                                  "Isaac B", "Jack B", "Jack G", "Jack H", "Jack Q",
                                  "Jack U", "James D", "James E", "James I", "Jenny B",
                                  "Jenny E", "Jenny G", "Jenny L", "Joe O", "Joe Q",
                                  "Juba A", "Julius A", "Julius B", "Jupiter A", "Jupiter B",
                                  "Kate H", "Kitty B", "Lewis C", "London A", "London B",
                                  "Matt A", "Michael A", "Milly C", "Milly E", "Moll A",
                                  "Molly F", "Morris B", "Moses C", "Murria A", "Nace A",
                                  "Nan K", "Nancy I", "Nanny C", "Ned Holt", "Ned P",
                                  "Nell B", "Nell C", "Neptune A", "Orford A", "Paris A",
                                  "Paris B", "Paul A", "Paul B", "Paul C", "Peter F",
                                  "Robin A", "Robin B", "Sall B", "Sam C", "Sam I",
                                  "Sarah B", "Sarah D", "Sarah G", "Schomberg A", "Schomberg B",
                                  "Scipio B", "Spencer A", "Stafford A", "Sue A", "Sue C",
                                  "Tom D", "TOM E", "Tom Nokes", "Tom S", "Troy A",
                                  "Wally A", "Will F", "Will H", "Will N", "Winny A")

#Add triangle as a vertex shape to match the interactive graph
mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
          stars=cbind(vertex.size, vertex.size, vertex.size),
          add=TRUE, inches=FALSE)
}
add_shape("triangle", clip=shapes("circle")$clip,
          plot=mytriangle)

#Addd customization to graph with vertex and edge attributes: shape and color
V(graph_1799)$shape <- ifelse(nodes_1799$Gender == "Female", "circle", 
                               ifelse(nodes_1799$Gender == "Male", "square", 
                                     "triangle"))
V(graph_1799)$color <- ifelse(nodes_1799$location == "Mansion House" & nodes_1799$Census == "1799", "orange",
                               ifelse(nodes_1799$location == "Muddy Hole" & nodes_1799$Census == "1799", "lightcoral",
                                      ifelse(nodes_1799$location == "Union Farm" & nodes_1799$Census == "1799", "yellowgreen",
                                             ifelse(nodes_1799$location == "River Farm" & nodes_1799$Census == "1799", "royalblue",
                                                    ifelse(nodes_1799$location == "Dogue Run" & nodes_1799$Census == "1799", "mediumturquoise",
                                                           ifelse(nodes_1799$location == "Mill Complex" & nodes_1799$Census == "1799", "mediumturquoise", 
                                                                  ifelse(nodes_1799$location == "Other Location" & nodes_1799$Census == "1799", "gold", "lightgray")))))))

E(graph_1799)$color <- ifelse(edges_1799$relationship == "Spouse", "red",
                        ifelse(edges_1799$relationship == "Child", "darkcyan",
                               ifelse(edges_1799$relationship == "Suspected Child", "darkcyan",
                                      ifelse(edges_1799$relationship == "Suspected Spouse", "red",
                                             ifelse(edges_1799$relationship == "Suspected Sibling", "orange",
                                                    ifelse(edges_1799$relationship == "Sibling", "orange", "gray"))))))

#Plot the graph
igraph.options(vertex.size=2.5)
set.seed(5)
plot(graph_1799, vertex.label = NA, layout = layout.fruchterman.reingold)
title("Social Network of Enslaved Community at Mount Vernon, 1799")
legend("left", legend = c("Mansion House", "Muddy Hole", "Union Farm", "River Farm", "Dogue Run", "Other Location", "Unknown", "Spouse", "Child", "Sibling", "Female", "Male", "Unknown"),
       col = c("orange", "lightcoral", "yellowgreen", "royalblue", "mediumturquoise", "gold", "lightgray", "red", "orange", "darkcyan", "Black", "Black", "Black"), 
       lty = c(NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, NA, NA, NA),
       text.col = c("orange", "lightcoral", "yellowgreen", "royalblue", "mediumturquoise", "gold", "lightgray", "Black", "Black", "Black", "Black", "Black", "Black"),
       lwd = 0.8,
       cex = 1,
       pch = c(19, 19, 19, 19, 19, 19, 19, NA, NA, NA, 21, 22, 24),
       bty = "n")
legend("bottomleft", legend = "v. 08 April 2016, n = 517", bty = "n", cex = 0.7)



