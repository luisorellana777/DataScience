<<<<<<< HEAD

install.packages('modeest', dep = TRUE)
library(modeest)

install.packages('ggplot2', dep = TRUE)
library(ggplot2)

install.packages('corrplot', dep = TRUE)
library('corrplot')

install.packages('mclust', dep = TRUE)
library(mclust)

library('corrplot')
library(ggplot2)
library(modeest)
library(mclust)

setwd("C:/Users/Luis.O.A/Documents/USACH/Mineria de Datos/grupo 6 Wine Quality")

data <- read.csv('winequality-red.csv', sep=';')

############ Estadistica Descriptiva ############

######## Medida de Tendencias Centrales

## Media ##

mean(data$fixed.acidity)
8.319637
mean(data$volatile.acidity)
0.5278205
mean(data$citric.acid)
0.2709756
mean(data$residual.sugar)
2.538806
mean(data$chlorides)
0.08746654
mean(data$free.sulfur.dioxide)
15.87492
mean(data$total.sulfur.dioxide)
46.46779
mean(data$density)
0.9967467
mean(data$pH)
3.311113
mean(data$sulphates)
0.6581488
mean(data$alcohol)
10.42298

## Moda ##

mlv(data$fixed.acidity, method = "mfv")[1]
7.2
mlv(data$volatile.acidity, method = "mfv")[1]
0.6
mlv(data$citric.acid, method = "mfv")[1]
0
mlv(data$residual.sugar, method = "mfv")[1]
2
mlv(data$chlorides, method = "mfv")[1]
0.08
mlv(data$free.sulfur.dioxide, method = "mfv")[1]
6
mlv(data$density, method = "mfv")[1]
0.9972
mlv(data$pH, method = "mfv")[1]
3.3
mlv(data$sulphates, method = "mfv")[1]
0.6
mlv(data$alcohol, method = "mfv")[1]
9.5

## Mediana ##

median(data$fixed.acidity)
7.9
median(data$volatile.acidity)
0.52
median(data$citric.acid)
0.26
median(data$residual.sugar)
2.2
median(data$chlorides)
0.079
median(data$free.sulfur.dioxide)
14
median(data$density)
0.99675
median(data$pH)
3.31
median(data$sulphates)
0.62
median(data$alcohol)
10.2

######## Medidas de Dispersion

## Rango ##

range(data$fixed.acidity)
4.6 15.9
range(data$volatile.acidity)
0.12 1.58
range(data$citric.acid)
0 1
range(data$residual.sugar)
0.9 15.5
range(data$chlorides)
0.012 0.611
range(data$free.sulfur.dioxide)
1 72
range(data$density)
0.99007 1.00369
range(data$pH)
 2.74 4.01
range(data$sulphates)
 0.33 2.00
range(data$alcohol)
8.4 14.9

summary(data)# Vista resumida de lo anteriormente visto. Seria util presentarlo en una tabla
boxplot(data$nombre_variable)# grafico de caja por variable

## Cuartiles ##

#El siguiente grafico sera muy util para explicr que incidencia tiene cada una de las variables e la calidad del vino.
boxplot(data$nombre_variable ~ data$quality, xlab = "Calidad", ylab = "Acides Fijada", col = c("red"))

## Varianza ##

var(data$fixed.acidity)
3.031416
var(data$volatile.acidity)
0.03206238
var(data$citric.acid)
0.03794748
var(data$residual.sugar)
1.987897
var(data$chlorides)
0.002215143
var(data$free.sulfur.dioxide)
109.4149
var(data$density)
3.562029e-06
var(data$pH)
0.02383518
var(data$sulphates)
0.02873262
var(data$alcohol)
1.135647

## Distribucion ##

#Nos servira para saber de manera mas grafica el rango y la mayor dispercion de cada una de las variables
hist(data$Nombre_variable, main = "Nombre_variable", xlab = "Acides Fijada", col = "grey", breaks = 40, 
     xlim = c(0,300))
	 
	 
############ Estadistica Inferencial ############

## Prueba de chi-cuadrado ## 

#NOTA: 
# "p-value" = 0 Alta dependencia entre variables

# "p-value" = 1 Nula dependencia entre variables

# "p-value" < 0.05 Alta dependencia entre variables (95% de dependencia)

summary(table(data$fixed.acidity,data$quality))
p-value = 1.416e-13
summary(table(data$volatile.acidity,data$quality))
p-value = 3.429e-69
summary(table(data$citric.acid,data$quality))
p-value = 6.425e-19
summary(table(data$residual.sugar,data$quality))
p-value = 1.649e-28
summary(table(data$chlorides,data$quality))
p-value = 1.181e-34
summary(table(data$free.sulfur.dioxide,data$quality))
p-value = 0.2399 #Esta es la unica variable que tiene una baja dependencia
summary(table(data$density,data$quality))
p-value = 7.547e-33
summary(table(data$pH,data$quality))
p-value = 8.719e-10
summary(table(data$sulphates,data$quality))
p-value = 2.382e-31
summary(table(data$alcohol,data$quality))
p-value = 5.345e-90

## Correlacion ##

M <- cor(data)
corrplot(M, method="circle")


################################### Noralizacion de los datos ################################### 

# NOTA:
# Cuando los rangos de las variables son muy dispares (ver "Rango") se aplica el siguiente modelo:
# X1 = (X1 - Media X1) / (max X1 - min X1) 
dataNorm <- as.data.frame( scale(data[1:11] )) # no se normaliza la clase

##https://www.analyticsvidhya.com/blog/2015/10/inferential-descriptive-statistics-beginners-r/


BIC = mclustBIC(dataNorm)
plot(BIC)

Mclust(dataNorm, modelNames ="VVV", G = 3)$BIC
Mclust(dataNorm, modelNames ="VVV", G = 4)$BIC
Mclust(dataNorm, modelNames ="VVV", G = 5)$BIC 
Mclust(dataNorm, modelNames ="VVV", G = 6)$BIC 
Mclust(dataNorm, modelNames ="VVV", G = 7)$BIC 

Mclust(dataNorm, modelNames ="VEV", G = 3)$BIC 
Mclust(dataNorm, modelNames ="VEV", G = 4)$BIC 
Mclust(dataNorm, modelNames ="VEV", G = 5)$BIC 
Mclust(dataNorm, modelNames ="VEV", G = 6)$BIC 
Mclust(dataNorm, modelNames ="VEV", G = 7)$BIC 

Mclust(dataNorm, modelNames ="VVE", G = 3)$BIC 
Mclust(dataNorm, modelNames ="VVE", G = 4)$BIC 
Mclust(dataNorm, modelNames ="VVE", G = 5)$BIC 
Mclust(dataNorm, modelNames ="VVE", G = 6)$BIC 
Mclust(dataNorm, modelNames ="VVE", G = 7)$BIC

# Mclust(dataNorm, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), G = 4, modelNames ="VEV")$BIC

# Comprabar el error de clasificacion
m = Mclust(dataNorm, modelNames ="VVE", G = 7)
classError(data$quality, m$classification)$errorRate

m = Mclust(dataNorm, modelNames ="VEV", G = 7)
classError(data$quality, m$classification)$errorRate

m = Mclust(dataNorm, modelNames ="VEV", G = 6)
classError(data$quality, m$classification)$errorRate

m = Mclust(dataNorm, modelNames ="VEV", G = 5)
classError(data$quality, m$classification)$errorRate

m = Mclust(dataNorm, modelNames ="VEV", G = 4)
classError(data$quality, m$classification)$errorRate

m = Mclust(dataNorm, modelNames ="VVV", G = 4)
classError(data$quality, m$classification)$errorRate

m = Mclust(dataNorm, modelNames ="VVV", G = 7)
classError(data$quality, m$classification)$errorRate


################################### Sin Noralizacion de los datos ################################### 

dataS = data[1:11]

BIC = mclustBIC(dataS)
plot(BIC)

m1 = Mclust(dataS, modelNames ="VVE", G = 3)
m2 = Mclust(dataS, modelNames ="VVE", G = 4)
m3 = Mclust(dataS, modelNames ="VVE", G = 5)
m4 = Mclust(dataS, modelNames ="VVE", G = 6)

m5 = Mclust(dataS, modelNames ="EEV", G = 3)
m6 = Mclust(dataS, modelNames ="EEV", G = 4)
m7 = Mclust(dataS, modelNames ="EEV", G = 5)
m8 = Mclust(dataS, modelNames ="EEV", G = 6)

m9 = Mclust(dataS, modelNames ="VEV", G = 3)
m10 = Mclust(dataS, modelNames ="VEV", G = 4)
m11 = Mclust(dataS, modelNames ="VEV", G = 5)
m12 = Mclust(dataS, modelNames ="VEV", G = 6)

m13 = Mclust(dataS, modelNames ="EVV", G = 3)
m14 = Mclust(dataS, modelNames ="EVV", G = 4)
m15 = Mclust(dataS, modelNames ="EVV", G = 5)
m16 = Mclust(dataS, modelNames ="EVV", G = 6)

m1$BIC
m2$BIC
m3$BIC
m4$BIC
m5$BIC
m6$BIC
m7$BIC
m8$BIC
m9$BIC
m10$BIC
m11$BIC
m12$BIC
m13$BIC
m14$BIC
m15$BIC
m16$BIC

# Comprabar el error de clasificacion

classError(data$quality, m4$classification)$errorRate

classError(data$quality, m3$classification)$errorRate

classError(data$quality, m12$classification)$errorRate

classError(data$quality, m2$classification)$errorRate

classError(data$quality, m11$classification)$errorRate

classError(data$quality, m15$classification)$errorRate

classError(data$quality, m14$classification)$errorRate

classError(data$quality, m10$classification)$errorRate

# Mejor Cluster -> m14

# Grafico de clusters
drmod <- MclustDR(m14, lambda = 1)

plot(drmod, what = "contour")

plot(drmod, what = "boundaries", ngrid = 200)

#Grafica de densidad
plot(drmod, what = "density", type = "persp", theta = -25, phi = 20,
      border = adjustcolor(grey(0.1), alpha.f = 0.3))


table(data$quality, m14$classification) ##comprobar ERRORES segun quality


########################################

install.packages('plotly', dep = TRUE)
# segun calidad
data$quality.cut <- cut(data$quality, breaks = c(0,4,6,10))
table(data$quality.cut, m14$classification)
# segun pH
table(cut(data$pH, breaks = c(2.5, 3, 3.5, 4)), m14$classification)




library(plotly)

p <- plot_ly(
  x = data$quality.cut,
  y = data$pH,
  name = "pH",
  type = "bar"
)
# segun acides citrica
table(cut(data$citric.acid, breaks = c(0, 0.25, 0.5, 0.75, 1)), m14$classification)




library(plotly)

p <- plot_ly(
  x = data$quality.cut,
  y = data$citric.acid,
  name = "citric.acid",
  type = "bar"
)

# segun alcohol
table(cut(data$alcohol, breaks = c(8, 9, 10, 11, 12, 13 ,14, 15)), m14$classification)




library(plotly)

p <- plot_ly(
  x = data$quality.cut,
  y = data$alcohol,
  name = "alcohol",
  type = "bar"
)

###########################################

source("http://goo.gl/UUyEzD")
outlierKD(dataS, dataS$fixed.acidity)
outlierKD(dataS, dataS$volatile.acidity)
outlierKD(dataNorm, dataNorm$citric.acid)
outlierKD(dataNorm, dataNorm$residual.sugar)
outlierKD(dataNorm, dataNorm$chlorides)
outlierKD(dataNorm, dataNorm$free.sulfur.dioxide)
outlierKD(dataNorm, dataNorm$pH)
outlierKD(dataNorm, dataNorm$sulphates)
outlierKD(dataNorm, dataNorm$alcohol)
##


install.packages('outliers', dep = TRUE)
library(outliers)

dataS = data[1:11]

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- mean(x)#NA
  y[x > (qnt[2] + H)] <- mean(x)#NA
  y
}

dataS$alcohol <- remove_outliers(dataS$alcohol)
dataS$sulphates <- remove_outliers(dataS$sulphates)
dataS$pH <- remove_outliers(dataS$pH)
dataS$free.sulfur.dioxid <- remove_outliers(dataS$free.sulfur.dioxid)
dataS$chlorides <- remove_outliers(dataS$chlorides)
dataS$residual.sugar <- remove_outliers(dataS$residual.sugar)
dataS$citric.acid <- remove_outliers(dataS$citric.acid)
dataS$volatile.acidity <- remove_outliers(dataS$volatile.acidity)
dataS$fixed.acidity <- remove_outliers(dataS$fixed.acidity)
=======

install.packages('modeest', dep = TRUE)
library(modeest)

install.packages('ggplot2', dep = TRUE)
library(ggplot2)

install.packages('corrplot', dep = TRUE)
library('corrplot')

install.packages('mclust', dep = TRUE)
library(mclust)

library('corrplot')
library(ggplot2)
library(modeest)
library(mclust)

setwd("C:/Users/Luis.O.A/Documents/USACH/Mineria de Datos/grupo 6 Wine Quality")

data <- read.csv('winequality-red.csv', sep=';')

############ Estadistica Descriptiva ############

######## Medida de Tendencias Centrales

## Media ##

mean(data$fixed.acidity)
8.319637
mean(data$volatile.acidity)
0.5278205
mean(data$citric.acid)
0.2709756
mean(data$residual.sugar)
2.538806
mean(data$chlorides)
0.08746654
mean(data$free.sulfur.dioxide)
15.87492
mean(data$total.sulfur.dioxide)
46.46779
mean(data$density)
0.9967467
mean(data$pH)
3.311113
mean(data$sulphates)
0.6581488
mean(data$alcohol)
10.42298

## Moda ##

mlv(data$fixed.acidity, method = "mfv")[1]
7.2
mlv(data$volatile.acidity, method = "mfv")[1]
0.6
mlv(data$citric.acid, method = "mfv")[1]
0
mlv(data$residual.sugar, method = "mfv")[1]
2
mlv(data$chlorides, method = "mfv")[1]
0.08
mlv(data$free.sulfur.dioxide, method = "mfv")[1]
6
mlv(data$density, method = "mfv")[1]
0.9972
mlv(data$pH, method = "mfv")[1]
3.3
mlv(data$sulphates, method = "mfv")[1]
0.6
mlv(data$alcohol, method = "mfv")[1]
9.5

## Mediana ##

median(data$fixed.acidity)
7.9
median(data$volatile.acidity)
0.52
median(data$citric.acid)
0.26
median(data$residual.sugar)
2.2
median(data$chlorides)
0.079
median(data$free.sulfur.dioxide)
14
median(data$density)
0.99675
median(data$pH)
3.31
median(data$sulphates)
0.62
median(data$alcohol)
10.2

######## Medidas de Dispersion

## Rango ##

range(data$fixed.acidity)
4.6 15.9
range(data$volatile.acidity)
0.12 1.58
range(data$citric.acid)
0 1
range(data$residual.sugar)
0.9 15.5
range(data$chlorides)
0.012 0.611
range(data$free.sulfur.dioxide)
1 72
range(data$density)
0.99007 1.00369
range(data$pH)
 2.74 4.01
range(data$sulphates)
 0.33 2.00
range(data$alcohol)
8.4 14.9

summary(data)# Vista resumida de lo anteriormente visto. Seria util presentarlo en una tabla
boxplot(data$nombre_variable)# grafico de caja por variable

## Cuartiles ##

#El siguiente grafico sera muy util para explicr que incidencia tiene cada una de las variables e la calidad del vino.
boxplot(data$nombre_variable ~ data$quality, xlab = "Calidad", ylab = "Acides Fijada", col = c("red"))

## Varianza ##

var(data$fixed.acidity)
3.031416
var(data$volatile.acidity)
0.03206238
var(data$citric.acid)
0.03794748
var(data$residual.sugar)
1.987897
var(data$chlorides)
0.002215143
var(data$free.sulfur.dioxide)
109.4149
var(data$density)
3.562029e-06
var(data$pH)
0.02383518
var(data$sulphates)
0.02873262
var(data$alcohol)
1.135647

## Distribucion ##

#Nos servira para saber de manera mas grafica el rango y la mayor dispercion de cada una de las variables
hist(data$Nombre_variable, main = "Nombre_variable", xlab = "Acides Fijada", col = "grey", breaks = 40, 
     xlim = c(0,300))
	 
	 
############ Estadistica Inferencial ############

## Prueba de chi-cuadrado ## 

#NOTA: 
# "p-value" = 0 Alta dependencia entre variables

# "p-value" = 1 Nula dependencia entre variables

# "p-value" < 0.05 Alta dependencia entre variables (95% de dependencia)

summary(table(data$fixed.acidity,data$quality))
p-value = 1.416e-13
summary(table(data$volatile.acidity,data$quality))
p-value = 3.429e-69
summary(table(data$citric.acid,data$quality))
p-value = 6.425e-19
summary(table(data$residual.sugar,data$quality))
p-value = 1.649e-28
summary(table(data$chlorides,data$quality))
p-value = 1.181e-34
summary(table(data$free.sulfur.dioxide,data$quality))
p-value = 0.2399 #Esta es la unica variable que tiene una baja dependencia
summary(table(data$density,data$quality))
p-value = 7.547e-33
summary(table(data$pH,data$quality))
p-value = 8.719e-10
summary(table(data$sulphates,data$quality))
p-value = 2.382e-31
summary(table(data$alcohol,data$quality))
p-value = 5.345e-90

## Correlacion ##

M <- cor(data)
corrplot(M, method="circle")


################################### Noralizacion de los datos ################################### 

# NOTA:
# Cuando los rangos de las variables son muy dispares (ver "Rango") se aplica el siguiente modelo:
# X1 = (X1 - Media X1) / (max X1 - min X1) 
dataNorm <- as.data.frame( scale(data[1:11] )) # no se normaliza la clase

##https://www.analyticsvidhya.com/blog/2015/10/inferential-descriptive-statistics-beginners-r/


BIC = mclustBIC(dataNorm)
plot(BIC)

Mclust(dataNorm, modelNames ="VVV", G = 3)$BIC
Mclust(dataNorm, modelNames ="VVV", G = 4)$BIC
Mclust(dataNorm, modelNames ="VVV", G = 5)$BIC 
Mclust(dataNorm, modelNames ="VVV", G = 6)$BIC 
Mclust(dataNorm, modelNames ="VVV", G = 7)$BIC 

Mclust(dataNorm, modelNames ="VEV", G = 3)$BIC 
Mclust(dataNorm, modelNames ="VEV", G = 4)$BIC 
Mclust(dataNorm, modelNames ="VEV", G = 5)$BIC 
Mclust(dataNorm, modelNames ="VEV", G = 6)$BIC 
Mclust(dataNorm, modelNames ="VEV", G = 7)$BIC 

Mclust(dataNorm, modelNames ="VVE", G = 3)$BIC 
Mclust(dataNorm, modelNames ="VVE", G = 4)$BIC 
Mclust(dataNorm, modelNames ="VVE", G = 5)$BIC 
Mclust(dataNorm, modelNames ="VVE", G = 6)$BIC 
Mclust(dataNorm, modelNames ="VVE", G = 7)$BIC

# Mclust(dataNorm, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), G = 4, modelNames ="VEV")$BIC

# Comprabar el error de clasificacion
m = Mclust(dataNorm, modelNames ="VVE", G = 7)
classError(data$quality, m$classification)$errorRate

m = Mclust(dataNorm, modelNames ="VEV", G = 7)
classError(data$quality, m$classification)$errorRate

m = Mclust(dataNorm, modelNames ="VEV", G = 6)
classError(data$quality, m$classification)$errorRate

m = Mclust(dataNorm, modelNames ="VEV", G = 5)
classError(data$quality, m$classification)$errorRate

m = Mclust(dataNorm, modelNames ="VEV", G = 4)
classError(data$quality, m$classification)$errorRate

m = Mclust(dataNorm, modelNames ="VVV", G = 4)
classError(data$quality, m$classification)$errorRate

m = Mclust(dataNorm, modelNames ="VVV", G = 7)
classError(data$quality, m$classification)$errorRate


################################### Sin Noralizacion de los datos ################################### 

dataS = data[1:11]

BIC = mclustBIC(dataS)
plot(BIC)

m1 = Mclust(dataS, modelNames ="VVE", G = 3)
m2 = Mclust(dataS, modelNames ="VVE", G = 4)
m3 = Mclust(dataS, modelNames ="VVE", G = 5)
m4 = Mclust(dataS, modelNames ="VVE", G = 6)

m5 = Mclust(dataS, modelNames ="EEV", G = 3)
m6 = Mclust(dataS, modelNames ="EEV", G = 4)
m7 = Mclust(dataS, modelNames ="EEV", G = 5)
m8 = Mclust(dataS, modelNames ="EEV", G = 6)

m9 = Mclust(dataS, modelNames ="VEV", G = 3)
m10 = Mclust(dataS, modelNames ="VEV", G = 4)
m11 = Mclust(dataS, modelNames ="VEV", G = 5)
m12 = Mclust(dataS, modelNames ="VEV", G = 6)

m13 = Mclust(dataS, modelNames ="EVV", G = 3)
m14 = Mclust(dataS, modelNames ="EVV", G = 4)
m15 = Mclust(dataS, modelNames ="EVV", G = 5)
m16 = Mclust(dataS, modelNames ="EVV", G = 6)

m1$BIC
m2$BIC
m3$BIC
m4$BIC
m5$BIC
m6$BIC
m7$BIC
m8$BIC
m9$BIC
m10$BIC
m11$BIC
m12$BIC
m13$BIC
m14$BIC
m15$BIC
m16$BIC

# Comprabar el error de clasificacion

classError(data$quality, m4$classification)$errorRate

classError(data$quality, m3$classification)$errorRate

classError(data$quality, m12$classification)$errorRate

classError(data$quality, m2$classification)$errorRate

classError(data$quality, m11$classification)$errorRate

classError(data$quality, m15$classification)$errorRate

classError(data$quality, m14$classification)$errorRate

classError(data$quality, m10$classification)$errorRate

# Mejor Cluster -> m14

# Grafico de clusters
drmod <- MclustDR(m14, lambda = 1)

plot(drmod, what = "contour")

plot(drmod, what = "boundaries", ngrid = 200)

#Grafica de densidad
plot(drmod, what = "density", type = "persp", theta = -25, phi = 20,
      border = adjustcolor(grey(0.1), alpha.f = 0.3))


table(data$quality, m14$classification) ##comprobar ERRORES segun quality


########################################

install.packages('plotly', dep = TRUE)
# segun calidad
data$quality.cut <- cut(data$quality, breaks = c(0,4,6,10))
table(data$quality.cut, m14$classification)
# segun pH
table(cut(data$pH, breaks = c(2.5, 3, 3.5, 4)), m14$classification)




library(plotly)

p <- plot_ly(
  x = data$quality.cut,
  y = data$pH,
  name = "pH",
  type = "bar"
)
# segun acides citrica
table(cut(data$citric.acid, breaks = c(0, 0.25, 0.5, 0.75, 1)), m14$classification)




library(plotly)

p <- plot_ly(
  x = data$quality.cut,
  y = data$citric.acid,
  name = "citric.acid",
  type = "bar"
)

# segun alcohol
table(cut(data$alcohol, breaks = c(8, 9, 10, 11, 12, 13 ,14, 15)), m14$classification)




library(plotly)

p <- plot_ly(
  x = data$quality.cut,
  y = data$alcohol,
  name = "alcohol",
  type = "bar"
)

###########################################

source("http://goo.gl/UUyEzD")
outlierKD(dataS, dataS$fixed.acidity)
outlierKD(dataS, dataS$volatile.acidity)
outlierKD(dataNorm, dataNorm$citric.acid)
outlierKD(dataNorm, dataNorm$residual.sugar)
outlierKD(dataNorm, dataNorm$chlorides)
outlierKD(dataNorm, dataNorm$free.sulfur.dioxide)
outlierKD(dataNorm, dataNorm$pH)
outlierKD(dataNorm, dataNorm$sulphates)
outlierKD(dataNorm, dataNorm$alcohol)
##


install.packages('outliers', dep = TRUE)
library(outliers)

dataS = data[1:11]

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- mean(x)#NA
  y[x > (qnt[2] + H)] <- mean(x)#NA
  y
}

dataS$alcohol <- remove_outliers(dataS$alcohol)
dataS$sulphates <- remove_outliers(dataS$sulphates)
dataS$pH <- remove_outliers(dataS$pH)
dataS$free.sulfur.dioxid <- remove_outliers(dataS$free.sulfur.dioxid)
dataS$chlorides <- remove_outliers(dataS$chlorides)
dataS$residual.sugar <- remove_outliers(dataS$residual.sugar)
dataS$citric.acid <- remove_outliers(dataS$citric.acid)
dataS$volatile.acidity <- remove_outliers(dataS$volatile.acidity)
dataS$fixed.acidity <- remove_outliers(dataS$fixed.acidity)
>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
