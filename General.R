
##ag1<-random.graph.game(100,0.5)
##g2<-barabasi.game(100,directed=FALSE)
graph_from_data_frame()

library(igraph)
library(BoolNet)



func_muy_descriptora <- function(grafo_demo, modo_para_degree=1, valor_max=0, ...){

los_tipos_de_degree <- c("all","in","out")

plot(grafo_demo)
grafx_red <- recordPlot()

grado_nodos <- degree(grafo_demo,mode=los_tipos_de_degree[modo_para_degree])

distr_grado <- degree.distribution(grafo_demo,mode=los_tipos_de_degree[modo_para_degree])

distr_grado_plus_indexes <- rbind(0:(length(dist_grado)-1),dist_grado)

plot(dist_grado_plus_indexes[2,]~dist_grado_plus_indexes[1,])
grafx_dist_lineal <- recordPlot()

distr_logs <- log(dist_grado_plus_indexes+0.0001)

plot(dist_logs[2,]~dist_logs[1,])
grafx_dist_log <- recordPlot()

prom_grado <- mean(grado_nodos)

distancias <- distances(grafo_demo)

if (valor_max != 0) {distancias[distancias > valor_max] <- valor_max}

dist_promedio <- mean(distancias)

dist_nodo_a_red <- c()

for (nodo in 1:dim(distancias)[1]){
dist_nodo_a_red <- c(dist_nodo_a_red,mean(distancias[nodo,]))
}

dist_nodo_a_red

num_conec <- length(E(grafo_demo))

num_nodos <- length(V(grafo_demo))

densidad <- num_conec / (num_nodos*(num_nodos-1))

if(!is.directed(grafo_demo)){densidad <- densidad*2}

#as_adjacency_matrix(
#  graph,
#  type = c("both", "upper", "lower"),
#  attr = NULL,
#  edges = FALSE,
#  names = TRUE,
#  sparse = igraph_opt("sparsematrices")
#)

matriz_adj <- as_adjacency_matrix(grafo_demo)

list_adj <- as_adj_list(grafo_demo)

diametro <- diameter(grafo_demo)

coefs_clust <- transitivity(grafo_demo, type="local")

clust_glob <- transitivity(grafo_demo)

todo <- list(grafx_red,grado_nodos,distr_grado_plus_indexes,grafx_dist_lineal,distr_logs,grafx_dist_log,prom_grado,distancias,dist_promedio,dist_nodo_a_red,num_conec,num_nodos,densidad,matriz_adj,list_adj,diametro,coefs_clust,clust_glob)

}

grafo_prueba<-random.graph.game(10,0.5)

que_cosas <- func_muy_descriptora(grafo_demo= grafo_prueba, modo_para_degree= 1, valor_max=0)

que_cosas[[1]]

heatmap(distances(grafo_prueba))
heatmap(as_adjacency_matrix(grafo_prueba,sparse=F))

func_reversora_de_direccion <- function(grafo_demo){
grafo_revert <- NULL
if(is_directed(grafo_demo)){grafo_revert <-as.undirected(grafo_demo)}
if(!is_directed(grafo_demo)){grafo_revert <-as.directed(grafo_demo)}
return(grafo_revert)
}





SimplePathsLengthN <- function(graph, node1, node2, pathlength=2) {
  SP = list()
  if(pathlength == 1) {
    if(node2 %in% neighbors(graph, node1)) {
      SP[[1]] = c(node1, node2) }
    return(SP) }
  Nbrs2 = neighbors(graph, node2)
  for(nbr2 in Nbrs2) {
    ASPn2 = SimplePathsLengthN(graph, node1, nbr2, pathlength-1)
    for(i in seq_along(ASPn2)) {
      if(!(node2 %in% ASPn2[[i]])) {
        SP = append(SP, list(c(ASPn2[[i]], node2))) }
    }
  }
  return(SP)
}
#
trayectorias_largas <- function(red_igraph, nodo1, nodo2, min=c(), max=c()){
  tam_min <- distances(red_igraph)[nodo1,nodo2] #Utilizamos como minimo la trayectoria minima, para
  #no buscar caminos más pequeños de lo posible.
  tam_max <- length(E(red_igraph)) #La trayectoria más larga en una red dirigida y no pesada no debería poder ser más grande que el conjunto de todas las conexiones, así que utilizaremos este número como el límite superior de nuestra función.
  if (is.null(min)==F){tam_min <- min}
  if (is.null(max)==F){tam_max <- max} #Asignamos minimo y maximo si se especificaron al correr la funcion. Podria considerar si los argumentos estan especificados desde antes para no tener que calcular tam_min y tam_max pero no son operaciones caras, asi que no hay problema en este caso.
  
  trayectoria_temp <- 0
  trayectorias_mem <- c()
  for (tamaño in tam_min:tam_max){
    trayectoria_temp <- SimplePathsLengthN(red_igraph,nodo1,nodo2,pathlength=tamaño)
    if (is.null(unlist(trayectoria_temp))){trayectoria_temp <- list("No hay")} #Esto nos ayuda a evitar un error si no hay caminos de ese tamaño.
    trayectoria_temp <- cbind(tamaño, trayectoria_temp)
    trayectorias_mem <- rbind(trayectorias_mem,trayectoria_temp)
  } 
  return(trayectorias_mem)
  
}

las_trayectorias <- trayectorias_largas(red_igraph=grafo_prueba,nodo1=1,nodo2=5,min=6,max=6)
las_trayectorias[97,2]

shortest_paths(grafo_prueba,1,5)$vpath[[1]]

pal <- rev(rainbow(ceiling(max(closeness(grafo_prueba)*1000))))
plot(grafo_prueba, vertex.color = pal[ceiling(closeness(grafo_prueba)*1000)])



func_destructora_samp <- function(objeto_igraph, los_num_vertex=c(), los_name_vertex=c(),cuantos_desapar=c()){
  cuantos_elimina <- c(ceiling(length(V(objeto_igraph))/100))
  if (is.null(cuantos_desapar)==F){
    cuantos_elimina <- cuantos_desapar
  }
  cuales_elimina <- sample(size=cuantos_elimina, x=c(1:length(V(objeto_igraph))))
  if (is.null(los_num_vertex)==F){
    cuales_elimina <- los_num_vertex
  }
  if (is.null(los_name_vertex)==F){
    cuales_elimina <- los_num_vertex
  }
  new_igraph_objeto <- delete_vertices(objeto_igraph, cuales_elimina) %>% delete_vertices(los_name_vertex)
  par(mfrow=c(2,1))
  promedio1 <- mean(distances(objeto_igraph))
  diametro1 <- diameter(objeto_igraph)
  promedio2 <- mean(distances(new_igraph_objeto))
  diametro2 <- diameter(new_igraph_objeto)
  grafx1 <- plot(objeto_igraph)
  grafx2 <- plot(new_igraph_objeto)
  todo <- list(promedio1,diametro1,grafx1,promedio2,diametro2,grafx2,new_igraph_objeto)
  par(mfrow=c(1,1))
  return(todo)
}

func_destructora_samp(grafo_prueba,los_num_vertex =  3)

clustering_edges <- cluster_edge_betweenness(grafo_prueba)

plot(grafo_prueba, vertex.color = clustering_edges$membership)
