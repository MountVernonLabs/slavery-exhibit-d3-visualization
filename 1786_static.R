library(igraph)
library(dplyr)

#Read in the edge and node lists
edges_1786 <- read.csv("~/Documents/network_viz/data-visualization/data/edges_1786.csv", stringsAsFactors = FALSE) 
nodes_1786 <- read.csv("~/Documents/network_viz/data-visualization/data/nodes_1786.csv", stringsAsFactors = FALSE) 

#Convert the edge list to a graph object
graph_1786 <- graph.data.frame(edges_1786, directed = "FALSE")

#Add the people with no known relationships as extra vertices
graph_1786 <- graph_1786 + vertex("Abram C", "Adam B", "Adam C", "Alce D", "Anthony A",
                                  "Arlington A", "Bacchus A", "Bath A", "Ben G", "Betty E",
                                  "Betty O", "Betty P", "Bob C", "Bristol A", "Caesar A",
                                  "Caesar B", "Charles G", "Cloe A", "Cupid A", "Cupid B",
                                  "Daphne D", "Daphne F", "Davy F", "Davy G", "Dick E",
                                  "Dorothy A", "Dublin A", "Dula A", "Dundee B", "Essex A",
                                  "Fanny B", "Fattimore A", "Fattimore B", "Flora B", "Frank B",
                                  "Frank B", "Frederick A", "George G", "George P", "Giles A",
                                  "Glasgow A", "Grace C", "Grig A", "Guy B", "Hannah E",
                                  "Isaac B", "Jack B", "Jack G", "Jack H", "Jack Q",
                                  "Jack U", "James D", "James E", "James I", "Jenny B", 
                                  "Jenny D", "Jenny E", "Jenny G", "Jenny L", "Joe O",
                                  "Joe Q", "Julius A", "Julius B", "Jupiter A", "Jupiter B",
                                  "Kate H", "Kitty B", "Lewis C", "London A", "London B",
                                  "Lucy G", "Lucy H", "Matt A", "Michael A", "Milly C",
                                  "Milly E", "Moll A", "Molly F", "Morris B", "Moses C",
                                  "Murria A", "Nace A", "Nan K", "Nancy I", "Nanny C",
                                  "Ned Holt", "Ned P", "Nell B", "Nell C", "Neptune A",
                                  "Orford A", "Paris A", "Paris B", "Paul A", "Paul B",
                                  "Paul C", "Peter F", "Rachel B", "Robin A", "Robin B",
                                  "Sabine A", "Sam C", "Sam I", "Sarah B", "Sarah D", 
                                  "Sarah G", "Schomberg A", "Schomberg B", "Scipio B", "Siss B",
                                  "Spencer A", "Stafford A", "Sue A", "Sue C", "Tom D",
                                  "Tom Davis", "Tom E", "Tom Nokes", "Tom S", "Troy A",
                                  "Unknown Person A", "Unknown Person B", "Unknown Person C", "Unknown Person D", "Unknown Person E",
                                  "Unknown Person F", "Unknown Person H", "Unknown Person I", "Unknown Person J", "Wally A",
                                  "Will F", "Will H", "Will N", "Winny A")

#slave_vertex <- data_frame(name = V(graph_1786)$name)

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

v <- data_frame(name = V(graph_1786)$name) %>%
  left_join(nodes_1786, by = c("name" = "id"))

#Addd customization to graph with vertex and edge attributes: shape and color
V(graph_1786)$shape <- ifelse(v$Gender == "Female", "circle", 
                              ifelse(v$Gender == "Male", "square",
                                     ifelse(v$Gender == "Unknown", "triangle", "rectangle")))

V(graph_1786)$color <- ifelse(v$location == "Mansion House" & v$Census >= "1786", "orange",
                              ifelse(v$location == "Muddy Hole" & v$Census >= "1786", "lightcoral",
                                     ifelse(v$location == "Union Farm" & v$Census >= "1786", "yellowgreen",
                                            ifelse(v$location == "River Farm" & v$Census >= "1786", "royalblue",
                                                   ifelse(v$location == "Dogue Run" & v$Census >= "1786", "mediumturquoise",
                                                          ifelse(v$location == "Mill Complex" & v$Census >= "1786", "mediumturquoise", 
                                                                 ifelse(v$location == "Other Location" & v$Census >= "1786", "gold", "lightgray")))))))

E(graph_1786)$color <- ifelse(edges_1786$relationship == "Spouse", "red",
                              ifelse(edges_1786$relationship == "Child", "darkcyan",
                                     ifelse(edges_1786$relationship == "Suspected Child", "darkcyan",
                                            ifelse(edges_1786$relationship == "Suspected Spouse", "red",
                                                   ifelse(edges_1786$relationship == "Suspected Sibling", "orange",
                                                          ifelse(edges_1786$relationship == "Sibling", "orange", "gray"))))))

#Plot the graph
igraph.options(vertex.size=2.5)
set.seed(5)
plot(graph_1786, vertex.label = NA, layout = layout.fruchterman.reingold)
title("Social Network of Enslaved Community at Mount Vernon, 1786")
legend("left", legend = c("Mansion House", "Muddy Hole", "Union Farm", "River Farm", "Dogue Run", "Other Location", "Unknown", "Spouse", "Child", "Sibling", "Female", "Male", "Unknown"),
       col = c("orange", "lightcoral", "yellowgreen", "royalblue", "mediumturquoise", "gold", "lightgray", "red", "orange", "darkcyan", "Black", "Black", "Black"), 
       lty = c(NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, NA, NA, NA),
       text.col = c("orange", "lightcoral", "yellowgreen", "royalblue", "mediumturquoise", "gold", "lightgray", "Black", "Black", "Black", "Black", "Black", "Black"),
       lwd = 0.8,
       cex = 1,
       pch = c(19, 19, 19, 19, 19, 19, 19, NA, NA, NA, 21, 22, 24),
       bty = "n")
legend("bottomleft", legend = "v. 08 April 2016, n = 365", bty = "n", cex = 0.7)
