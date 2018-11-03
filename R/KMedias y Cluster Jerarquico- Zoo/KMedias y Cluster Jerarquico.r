setwd(choose.dir())

install.packages('cluster', dep = TRUE)
library(cluster)

install.packages('factoextra', dep = TRUE)
library(factoextra)

install.packages('FactoMineR', dep = TRUE)
library(FactoMineR)

install.packages('ggplot2', dep = TRUE)
library(ggplot2)

data_norm  =  read.csv("zoo-cluster - class_type.csv")
data  =  read.csv("zoo-cluster - class_type.csv")

data_norm["animal_name"] <- NULL

#--------------------------------------------------------------
#modelo con 7 clusters
#Distancia Hamming
data_cluster <- dist(data_norm, method = "binary", diag = FALSE, upper = FALSE, p = 2)

data_matrix <- as.matrix(data_cluster)

data_kmeans <- pam(data_matrix, 7)

data$class_type[data_kmeans$id.med] # centroides con distancia hamming
#--------------------------------------------------------------
#Distancia manhattan o euclidea
# datos dicotomicos a variables cuantitativas por medio de "multiple correspondence analysis"
for (i in 1:ncol(data_norm)) data_norm[,i]=as.factor(data_norm[,i])

mca1 = MCA(data_norm, graph = FALSE)

data_matrix <- as.matrix(mca1)
#hacer tabla que explique a que especie corresponde cada cluster (para las dos distancias ("hamming" y "manhattan"))
data_kmeans <- pam(mca1$ind$cos2, 7, metric = "manhattan") 
data$class_type[data_kmeans$id.med]
#--------------------------------------------------------------
#Desaprobar 7 clusters para clasificar dataset
fviz_silhouette(silhouette(data_kmeans))
sil <- silhouette(data_kmeans)
# se seleccionan los animales con altas probabilidades de  pertenecer a otro cluster
neg_sil_index <- which(sil[, 'sil_width'] < 0.2)
sil[neg_sil_index, , drop = FALSE]
#--------------------------------------------------------------
#modelo con 6 clusters
data_cluster <- dist(data_norm, method = "binary", diag = FALSE, upper = FALSE, p = 2)

data_matrix <- as.matrix(data_cluster)

data_kmeans <- pam(data_matrix, 6)#se ha elegido 6 clusters
data$class_type[data_kmeans$id.med] # centroides con distancia hamming
data[data_kmeans$id.med,23:29]#informacion centroides

#Grafica de siluetas
fviz_silhouette(silhouette(data_kmeans)) 

#Agrupaciones sin nombres
fviz_cluster(data_kmeans, stand = FALSE, geom = "point", ellipse.type = "norm", show.clust.cent = TRUE)
# Agrupaciones de animales en particular
fviz_cluster(data_kmeans, stand = FALSE, data = data_norm, ellipse.type = "norm", show.clust.cent = TRUE)
#Graficas de agrupacion y silueta 
plot(data$class_type, col = data_kmeans$cluster)
plot(data[data_kmeans$id.med,23:29], col = data_kmeans$cluster)
#Informacion global de clusters
summary(data_kmeans)
plot(data_kmeans)
names(data_kmeans)
table(data_kmeans$clustering)

#BONUS-Algoritmo jerarquico
data  =  read.csv("zoo-cluster.csv")
data_norm["animal_name"] <- NULL
data_cluster <- dist(data_norm, method = "manhattan", diag = FALSE, upper = FALSE, p = 2)
 
clusters <- hclust(data_cluster)
plot(clusters)

clusterCut <- cutree(clusters, 6) # a distancia 0.78 en el dendrograma
table(clusterCut, data$class_type)
plot(clusterCut, data$class_type)
