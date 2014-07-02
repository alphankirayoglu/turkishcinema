require(igraph)
require(network)
require(SPARQL)
require(ergm)

actor_movie_matrix <- as.matrix(ifelse(table(oyuncular$oyuncu,oyuncular$film) > 0, 1, 0))
sums<-rowSums(actor_movie_matrix)
bigger<-which(sums>1)
actor_movie_matrix<-actor_movie_matrix[bigger,]
a_m <- graph.incidence(actor_movie_matrix)

movie_director_matrix <- as.matrix(ifelse(table(oyuncular$film,oyuncular$yonetmen) > 0, 1, 0))
el <- get.edgelist(graph.incidence(movie_director_matrix))
movie_directors <- el[!duplicated(el[,1]),]

n_actors <- dim(actor_movie_matrix)[1]
n_movies <- dim(actor_movie_matrix)[2]
n_directors <- length(unique(oyuncular$yonetmen))


md <- cbind(oyuncular$film,oyuncular$yonetmen)
movie_directors <- md[!duplicated(md[,1]),]

V(a_m)[1:n_actors]$kind <- "actor"
V(a_m)[n_actors+1:n_movies]$kind <- "movie"
V(a_m)[n_actors+1:n_movies]$director <- movie_directors[,2]

V(a_m)$name <- iconv(V(a_m)$name,to='ASCII',sub="")
V(a_m)$director <- iconv(V(a_m)$director,to='ASCII',sub="")

V(a_m)$Label <- V(a_m)$name

write.graph(a_m,'/Users/alphan/Desktop/oyuncular.graphml',format="graphml")


----
  
movie_director_matrix <- as.matrix(ifelse(table(oyuncular$film,oyuncular$yonetmen) > 0, 1, 0))
actor_director_matrix <- actor_movie_matrix %*% movie_director_matrix

a_d <- graph.incidence(actor_director_matrix,directed=FALSE,weighted=TRUE)
V(a_d)[1:n_actors]$kind <- "actor"
V(a_d)[n_actors+1:n_directors]$kind <- "director"
V(a_d)$name<-iconv(V(a_d)$name,to='ASCII',sub="")
V(a_d)$Label<-V(a_d)$name

E(a_d)$Weight<-E(a_d)$weight / max(E(a_d)$weight)

write.graph(a_d,'/Users/alphan/Desktop/yonetmenler.graphml',format="graphml")
