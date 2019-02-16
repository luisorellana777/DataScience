<<<<<<< HEAD
<<<<<<< HEAD
setwd(choose.dir())#Entregar ruta de trabajo

#Instala paquete “aruleViz”
install.packages('arulesViz', dep = TRUE)
library(arulesViz)

data  =  read.csv("zoo.csv") #Carga dataset

summary(data) #Resumen estadístico

data <- data.frame(sapply(data,as.factor)) #Discretiza datos

rules <- apriori(data, parameter=list(support=0.37, confidence=0.5, maxlen=50))#5736 Encontradas

inspect(head(sort(rules, by ="lift"),3))#Tres primeras reglas

plot(rules, measure=c("support", "lift"), shading="confidence")#Diagrama de dispersión

inspect(head(sort(rules, by ="lift"),260)) #Visualizar reglas para seleccionar


subrules <- rules[quality(rules)$lift > 1.46]#Visualizar reglas que tengan más de “1.46” lift

subrules #Número de reglas obtenidas: 1928

#Matrices de reglas
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))
plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))
plot(subrules, method="matrix3D", measure="support", control=list(reorder=TRUE))
plot(subrules, method="matrix", measure=c("lift", "confidence"),control=list(reorder=TRUE))

#Matriz de agrupación (5.736 reglas). Siete agrupadores de reglas en eje “X”.
plot(rules, method="grouped", control=list(k=7))

subrules2 <- head(sort(rules, by="lift"), 10)#Sub Set de 10 reglas.

plot(subrules2, method="graph")#Grafico de vértices para diez reglas.

#Grafico de coordenadas paralelas para diez reglas.
plot(subrules2, method="paracoord", control=list(reorder=TRUE))
=======
setwd(choose.dir())#Entregar ruta de trabajo

#Instala paquete “aruleViz”
install.packages('arulesViz', dep = TRUE)
library(arulesViz)

data  =  read.csv("zoo.csv") #Carga dataset

summary(data) #Resumen estadístico

data <- data.frame(sapply(data,as.factor)) #Discretiza datos

rules <- apriori(data, parameter=list(support=0.37, confidence=0.5, maxlen=50))#5736 Encontradas

inspect(head(sort(rules, by ="lift"),3))#Tres primeras reglas

plot(rules, measure=c("support", "lift"), shading="confidence")#Diagrama de dispersión

inspect(head(sort(rules, by ="lift"),260)) #Visualizar reglas para seleccionar


subrules <- rules[quality(rules)$lift > 1.46]#Visualizar reglas que tengan más de “1.46” lift

subrules #Número de reglas obtenidas: 1928

#Matrices de reglas
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))
plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))
plot(subrules, method="matrix3D", measure="support", control=list(reorder=TRUE))
plot(subrules, method="matrix", measure=c("lift", "confidence"),control=list(reorder=TRUE))

#Matriz de agrupación (5.736 reglas). Siete agrupadores de reglas en eje “X”.
plot(rules, method="grouped", control=list(k=7))

subrules2 <- head(sort(rules, by="lift"), 10)#Sub Set de 10 reglas.

plot(subrules2, method="graph")#Grafico de vértices para diez reglas.

#Grafico de coordenadas paralelas para diez reglas.
plot(subrules2, method="paracoord", control=list(reorder=TRUE))
>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
=======
setwd(choose.dir())#Entregar ruta de trabajo

#Instala paquete “aruleViz”
install.packages('arulesViz', dep = TRUE)
library(arulesViz)

data  =  read.csv("zoo.csv") #Carga dataset

summary(data) #Resumen estadístico

data <- data.frame(sapply(data,as.factor)) #Discretiza datos

rules <- apriori(data, parameter=list(support=0.37, confidence=0.5, maxlen=50))#5736 Encontradas

inspect(head(sort(rules, by ="lift"),3))#Tres primeras reglas

plot(rules, measure=c("support", "lift"), shading="confidence")#Diagrama de dispersión

inspect(head(sort(rules, by ="lift"),260)) #Visualizar reglas para seleccionar


subrules <- rules[quality(rules)$lift > 1.46]#Visualizar reglas que tengan más de “1.46” lift

subrules #Número de reglas obtenidas: 1928

#Matrices de reglas
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))
plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))
plot(subrules, method="matrix3D", measure="support", control=list(reorder=TRUE))
plot(subrules, method="matrix", measure=c("lift", "confidence"),control=list(reorder=TRUE))

#Matriz de agrupación (5.736 reglas). Siete agrupadores de reglas en eje “X”.
plot(rules, method="grouped", control=list(k=7))

subrules2 <- head(sort(rules, by="lift"), 10)#Sub Set de 10 reglas.

plot(subrules2, method="graph")#Grafico de vértices para diez reglas.

#Grafico de coordenadas paralelas para diez reglas.
plot(subrules2, method="paracoord", control=list(reorder=TRUE))
>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
