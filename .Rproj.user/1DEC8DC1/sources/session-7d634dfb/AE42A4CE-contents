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

#Al correr la func_muy_descriptora a veces puede haber errores debido a los plots que genera,
#ampliar los margenes en caso de error

descrita_red_1 <- func_muy_descriptora(grafo_demo=red_1, modo_para_degree=1) 
descrita_red_2 <- func_muy_descriptora(grafo_demo=red_2, modo_para_degree=1)
descrita_red_3 <- func_muy_descriptora(grafo_demo=red_3, modo_para_degree=1)

########## Promedio conectividades ##########
descrita_red_1[[7]]
descrita_red_2[[7]]
descrita_red_3[[7]]

########## Densidad ##########
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

########## Numero de componentes al ir quitando los nodos mas importantes de la red. ##########
#Te los da en formato de lista, con un NULL inicial, seguido de el numero de componentes al quitar 1, luego 2, etc.

componentes_data_1
componentes_data_2
componentes_data_3

### Robustez proceso

red_1_quitando_nodos <- func_destructora_samp(red_1,cuantos_desapar = 0,los_num_vertex = NULL,los_name_vertex=nombres_1[1:10])[[7]]
red_2_quitando_nodos <- func_destructora_samp(red_2,cuantos_desapar = 0,los_num_vertex = NULL,los_name_vertex=nombres_2[1:10])[[7]]
red_3_quitando_nodos <- func_destructora_samp(red_3,cuantos_desapar = 0,los_num_vertex = NULL,los_name_vertex=nombres_3[1:10])[[7]]

#Al correr la func_destructora_samp a veces puede haber errores debido a los plots que genera,
#ampliar los margenes en caso de error






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

compara_las_dis <- function(red_normal,red_nodos_quitados){
dato_distancias_red <- mean(distances(red_normal)[which(distances(red_normal)<1000000)])
dato_distancias_quitados <- mean(distances(red_nodos_quitados)[which(distances(red_nodos_quitados)<1000000)])
dato_dist_comparativo <- c(dato_distancias_red,dato_distancias_quitados,dato_distancias_red-dato_distancias_quitados)
return(dato_dist_comparativo)
}
#usamos el truco de distancia menor a un millon para quitar las distancias infinitas

robustez_1 <- compara_las_dis(red_normal=red_1,red_nodos_quitados = red_1_quitando_nodos)
robustez_2 <- compara_las_dis(red_normal=red_2,red_nodos_quitados = red_2_quitando_nodos)
robustez_3 <- compara_las_dis(red_normal=red_3,red_nodos_quitados = red_3_quitando_nodos)


########## Comparacion de robustez ##########
#Distancia media en la red original, en la red con menos nodos, y su diferencia
robustez_1
robustez_2
robustez_3


#Las 3 redes tienen una conectividad promedio de 3.6, 4.07 y 4.13
#Todas son muy poco densas, teniendo una densidad menor a 0.02, la menos densa es la red 2
#Los componentes de las redes incrementan conforme se van quitando nodos de la red

#La robustez obtenida a partir de la distancia promedio parece ser paradojica:
#cuando quitamos los nodos, la distancia promedio disminuye. Sin embargo, quitar los nodos
#con mas conectividad debería de incrementar la distancia. Esto es debido a que en este caso,
#cuando quitamos los nodos más conectados, aparecen más distancias infinitas en lugar de distancias largas (porque ahora
#las comunidades que estaban lejanas entre ellas y solo estaban conectadas por los hubs, ya dejan
#de estar conectadas y reemplazan sus valores por infinitos) que son ignoradas
#para calcular la distancia promedio. Es por ello que disminuye.
#Entonces lo podemos interpretar como que la red 2 tubo el cambio más dramático en cuestión de que tuvo
#un mayor numero de distancias (o distancias más grandes) reemplazadas por valores infinitos, por lo que es la 
#menos robusta.

#########Funcion de las proteinas mas importantes ##########
#Red 3 Gemella sanguinis
#HMPREF0433_01178 Adenilato cinasa
#HMPREF0433_00316 glucose-6-phosphate isomerase 
#HMPREF0433_00802 fructose-1,6-bisphosphate aldolase, class II
#HMPREF0433_00234 hypothetical protein HMPREF0433_00234
#HMPREF0433_00235 ribonucleoside-diphosphate reductase, alpha subunit
#HMPREF0433_00782 cytidylate kinase
#HMPREF0433_00807 uridylate kinase
#HMPREF0433_01354 purine nucleoside phosphorylase
#HMPREF0433_00297 glyceraldehyde-3-phosphate dehydrogenase, type I
#HMPREF0433_01682 2,3-bisphosphoglycerate-dependent phosphoglycerate mutase
