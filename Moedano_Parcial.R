library(igraph)

list_adj_1 <- read.csv(paste0("treeoflife.interactomes/","1000569.txt"), header=F, as.is=F,sep=" ")
list_adj_1
dim(list_adj_1)

red_1 <- graph_from_edgelist(as.matrix(list_adj_1))



list_adj_2 <- read.csv(paste0("treeoflife.interactomes/","469606.txt"), header=F, as.is=F,sep=" ")
list_adj_2
dim(list_adj_2)

red_2 <- graph_from_edgelist(as.matrix(list_adj_2))



list_adj_3 <- read.csv(paste0("treeoflife.interactomes/","562983.txt"), header=F, as.is=F,sep=" ")
list_adj_3
dim(list_adj_3)

red_3 <- graph_from_edgelist(as.matrix(list_adj_3))



red_1
red_2
red_3



descrita_red_1 <- func_muy_descriptora(grafo_demo=red_1, modo_para_degree=1)
descrita_red_2 <- func_muy_descriptora(grafo_demo=red_2, modo_para_degree=1)
descrita_red_3 <- func_muy_descriptora(grafo_demo=red_3, modo_para_degree=1)

#Promedio conectividades
descrita_red_1[[7]]
descrita_red_2[[7]]
descrita_red_3[[7]]

#Densidad
descrita_red_1[[13]]
descrita_red_2[[13]]
descrita_red_3[[13]]

###Proceso para funcion de components

components(red_1)


nombres_1 <- names(sort(degree(red_1,mode="all"),decreasing=T)[1:10])
nombres_2 <- names(sort(degree(red_2,mode="all"),decreasing=T)[1:10])
nombres_3 <- names(sort(degree(red_3,mode="all"),decreasing=T)[1:10])

n_destruc_data <- c()
n_destruc <- function(base,var=10,names=c(),n_destruc_data=c()){
  for (a in 1:var){
    n_destruc_data <- list(n_destruc_data,    count_components(func_destructora_samp(base,cuantos_desapar = 0,los_num_vertex = NULL,los_name_vertex=names[1:a])[[7]])   )}
return(n_destruc_data)
  }


componentes_data_1 <- n_destruc(base=red_1,var=10,names=nombres_1)
componentes_data_2 <- n_destruc(base=red_2,var=10,names=nombres_2)
componentes_data_3 <- n_destruc(base=red_3,var=10,names=nombres_3)

###Numero de componentes al ir quitando los nodos mas importantes de la red.
#Te los da en formato de lista, con un NULL inicial, seguido de el numero de componentes al quitar 1, luego 2, etc.

componentes_data_1
componentes_data_2
componentes_data_3

### Robustez proceso

red_1_quitando_nodos <- func_destructora_samp(red_1,cuantos_desapar = 0,los_num_vertex = NULL,los_name_vertex=nombres_1[1:10])[[7]]
red_2_quitando_nodos <- func_destructora_samp(red_2,cuantos_desapar = 0,los_num_vertex = NULL,los_name_vertex=nombres_2[1:10])[[7]]
red_3_quitando_nodos <- func_destructora_samp(red_3,cuantos_desapar = 0,los_num_vertex = NULL,los_name_vertex=nombres_3[1:10])[[7]]


#Revisamos que los nodos mas populares ya no esten en nuestras nuevas redes. revisamos el 1er nodo mas popular
any(V(red_1)$name==nombres_1[1])
any(V(red_1_quitando_nodos)$name==nombres_1[1])

any(V(red_2)$name==nombres_2[1])
any(V(red_2_quitando_nodos)$name==nombres_2[1])

any(V(red_3)$name==nombres_3[1])
any(V(red_3_quitando_nodos)$name==nombres_3[1])

#Hay 10 nodos menos en cada red
length(V(red_1))
length(V(red_1_quitando_nodos))

length(V(red_2))
length(V(red_2_quitando_nodos))

length(V(red_3))
length(V(red_3_quitando_nodos))


#Hacemos una funcion comparativa

compara_las_dis 
mean(distances(red_1)[which(distances(red_1)<1000)])
mean(distances(red_1_quitando_nodos)[which(distances(red_1_quitando_nodos)<1000)])

