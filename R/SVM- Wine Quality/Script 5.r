<<<<<<< HEAD

install.packages('e1071', dep = TRUE)
install.packages('xlsx', dep = TRUE)
install.packages('caret', dep = TRUE)
install.packages('caTools', dep = TRUE)
install.packages('plotly', dep = TRUE)

library(e1071)
library(xlsx)
library(caret)
library(caTools)
library(plotly)

#Modifica estas rutas segun tu ambiente de desarrollo (la variable RUTA_TXT_XLS esta explicada en el correo)
RUTA_DATASET <- "C:\\Users\\Luis.O.A\\Documents\\USACH\\Mineria de Datos\\grupo 6 Wine Quality"
RUTA_TXT_XLS <- "C:\\Users\\Luis.O.A\\Documents\\USACH\\Mineria de Datos\\Trabajos\\5"

setwd(RUTA_DATASET)

wine <- read.csv('winequality-red.csv', sep=';')

#Funcion que permite normalizar los datos entre 0 a 1
normalize <- function(x) {
	x = (x-min(x))/(max(x)-min(x))
	x
}

#Se normalizan los datos menos la clase (wine quality)
wine$fixed.acidity <- NULL
wine$volatile.acidity = normalize(wine$volatile.acidity)
wine$citric.acid = normalize(wine$citric.acid)
wine$residual.sugar = normalize(wine$residual.sugar)
wine$chlorides = normalize(wine$chlorides)
wine$free.sulfur.dioxide = normalize(wine$free.sulfur.dioxide)
wine$total.sulfur.dioxide = normalize(wine$total.sulfur.dioxide)
wine$density = normalize(wine$density)
wine$pH = normalize(wine$pH)
wine$sulphates = normalize(wine$sulphates)
wine$alcohol = normalize(wine$alcohol)

#hist(wine$fixed.acidity,breaks=10, xlab="wine",col="lightblue", main="")
#la clase es transformada a factor.
wine$quality = factor(wine$quality,labels=c("III", "IV", "V", "VI", "VII", "VIII"))

#Se comprobara el error con cada uno de los tipos de kernel
#Los rangos de los parametros de cada kernel estan sugeridos aqui: (rstudio)
#http://rstudio-pubs-static.s3.amazonaws.com/252840_176bda2bbab2428cbf042f8282d717e1.html
modelo <- function(wine, k){
	#Semilla para no obtener casos destintos en cada ejecucion
	set.seed(123)
	
	#No es necesario separar los datos en test y training porque el metodo ya provee validacion cruzada
	if(k == "radial"){
		# Kernel radial
		obj <- tune(svm, quality~., data = wine, kernel = k, ranges = list(gamma = 2^(-7:12), cost = 2^(-7:14) ), tunecontrol = tune.control(sampling = "cross", cross = 2))
		
	}else if(k == "polynomial"){
		# Kernel polynomial
		obj <- tune(svm, quality~., data = wine, kernel = k, degree = 2, ranges = list(cost = 2^(-2:9)), tunecontrol = tune.control(sampling = "cross", cross = 2))
		#obj <- tune(svm, quality~., data = wine, kernel = k, degree = 2, ranges = list(cost=10^seq(-2, 1, by=0.25)), tunecontrol = tune.control(sampling = "cross", cross = 2))
		
	}else if(k == "linear"){
		# Kernel linear
		obj <- tune(svm, quality~., data = wine, kernel = k, ranges = list(cost = 2^(-3:6)), tunecontrol = tune.control(sampling = "cross", cross = 2))
	}else{
		# Kernel sigmoid
		obj <- tune(svm, quality~., data = wine, kernel = k, ranges=list(gamma = 2^(-7:12), cost = 2^(-7:14)), tunecontrol = tune.control(sampling = "cross", cross = 2))
	}
	summary(obj)

	x <- subset(wine, select = -quality)
	y <- wine$quality

	pred <- predict(obj$best.model, x)
	tabla.resultados <- table(pred, y)
	
	#Almacena los resultados en un excel para luego analizarlos
	setwd(RUTA_TXT_XLS)
	write.xlsx(obj$performances, paste(k, "xlsx", sep = "."), sheetName="Sheet1", append = FALSE)
	
	escribir.tabla <- function(x,row.names=FALSE,col.names=TRUE,...) {
		write.table(x,paste(k, "txt", sep = "."),sep="\t",row.names=row.names,col.names=col.names,...)
	}

	#Almacena la tabla de resultados de clasificacion en txt para luego analizarlos
	escribir.tabla(tabla.resultados)

	return(obj)
}

#Grafica el error de cada uno de los modelos segun su kernel
graficar <- function(model, k){
	#Ordeno los errores de mayor a menor paraque el grafico no sea tan feo
	Error <- sort(model$performances$error, decreasing = TRUE)
	x <- c(1:length(model$performances$error))
	data <- data.frame(x, sort(model$performances$error, decreasing = TRUE))
	plot_ly(data, x = ~x, y = ~Error, name = k, type = 'scatter', mode = 'lines+markers') 


}

#Mejor modelo = radial; gamma = 0.5; cost = 2
model.r <- modelo(wine, "radial")
#Porcentaje de Error en la clasificacion de wine
error.r <- mean(model.r$best.model$fitted!=wine$quality)*100 #Error = 8.380238
graficar(model.r, "radial");								 #Al eliminar "" 10.69418

model.p <- modelo(wine, "polynomial")
#Porcentaje de Error en la clasificacion de wine
error.p <- mean(model.p$best.model$fitted!=wine$quality)*100#Error = 39.27455
graficar(model.p, "polynomial");

model.l <- modelo(wine, "linear")
#Porcentaje de Error en la clasificacion de wine
error.l <- mean(model.l$best.model$fitted!=wine$quality)*100#Error = 41.2758
graficar(model.l, "linear");

model.s <- modelo(wine, "sigmoid")
#Porcentaje de Error en la clasificacion de wine
error.s <- mean(model.s$best.model$fitted!=wine$quality)*100#Error = 43.15197
graficar(model.s, "sigmoid");

#Grafico de los porcentajes de error
plot_ly(
  x = c("radial","polynomial","linear","sigmoid"),
  y = c(error.r, error.p, error.l, error.s),
  name = "% Error Segun Kernel",
  type = "bar"
)
=======

install.packages('e1071', dep = TRUE)
install.packages('xlsx', dep = TRUE)
install.packages('caret', dep = TRUE)
install.packages('caTools', dep = TRUE)
install.packages('plotly', dep = TRUE)

library(e1071)
library(xlsx)
library(caret)
library(caTools)
library(plotly)

#Modifica estas rutas segun tu ambiente de desarrollo (la variable RUTA_TXT_XLS esta explicada en el correo)
RUTA_DATASET <- "C:\\Users\\Luis.O.A\\Documents\\USACH\\Mineria de Datos\\grupo 6 Wine Quality"
RUTA_TXT_XLS <- "C:\\Users\\Luis.O.A\\Documents\\USACH\\Mineria de Datos\\Trabajos\\5"

setwd(RUTA_DATASET)

wine <- read.csv('winequality-red.csv', sep=';')

#Funcion que permite normalizar los datos entre 0 a 1
normalize <- function(x) {
	x = (x-min(x))/(max(x)-min(x))
	x
}

#Se normalizan los datos menos la clase (wine quality)
wine$fixed.acidity <- NULL
wine$volatile.acidity = normalize(wine$volatile.acidity)
wine$citric.acid = normalize(wine$citric.acid)
wine$residual.sugar = normalize(wine$residual.sugar)
wine$chlorides = normalize(wine$chlorides)
wine$free.sulfur.dioxide = normalize(wine$free.sulfur.dioxide)
wine$total.sulfur.dioxide = normalize(wine$total.sulfur.dioxide)
wine$density = normalize(wine$density)
wine$pH = normalize(wine$pH)
wine$sulphates = normalize(wine$sulphates)
wine$alcohol = normalize(wine$alcohol)

#hist(wine$fixed.acidity,breaks=10, xlab="wine",col="lightblue", main="")
#la clase es transformada a factor.
wine$quality = factor(wine$quality,labels=c("III", "IV", "V", "VI", "VII", "VIII"))

#Se comprobara el error con cada uno de los tipos de kernel
#Los rangos de los parametros de cada kernel estan sugeridos aqui: (rstudio)
#http://rstudio-pubs-static.s3.amazonaws.com/252840_176bda2bbab2428cbf042f8282d717e1.html
modelo <- function(wine, k){
	#Semilla para no obtener casos destintos en cada ejecucion
	set.seed(123)
	
	#No es necesario separar los datos en test y training porque el metodo ya provee validacion cruzada
	if(k == "radial"){
		# Kernel radial
		obj <- tune(svm, quality~., data = wine, kernel = k, ranges = list(gamma = 2^(-7:12), cost = 2^(-7:14) ), tunecontrol = tune.control(sampling = "cross", cross = 2))
		
	}else if(k == "polynomial"){
		# Kernel polynomial
		obj <- tune(svm, quality~., data = wine, kernel = k, degree = 2, ranges = list(cost = 2^(-2:9)), tunecontrol = tune.control(sampling = "cross", cross = 2))
		#obj <- tune(svm, quality~., data = wine, kernel = k, degree = 2, ranges = list(cost=10^seq(-2, 1, by=0.25)), tunecontrol = tune.control(sampling = "cross", cross = 2))
		
	}else if(k == "linear"){
		# Kernel linear
		obj <- tune(svm, quality~., data = wine, kernel = k, ranges = list(cost = 2^(-3:6)), tunecontrol = tune.control(sampling = "cross", cross = 2))
	}else{
		# Kernel sigmoid
		obj <- tune(svm, quality~., data = wine, kernel = k, ranges=list(gamma = 2^(-7:12), cost = 2^(-7:14)), tunecontrol = tune.control(sampling = "cross", cross = 2))
	}
	summary(obj)

	x <- subset(wine, select = -quality)
	y <- wine$quality

	pred <- predict(obj$best.model, x)
	tabla.resultados <- table(pred, y)
	
	#Almacena los resultados en un excel para luego analizarlos
	setwd(RUTA_TXT_XLS)
	write.xlsx(obj$performances, paste(k, "xlsx", sep = "."), sheetName="Sheet1", append = FALSE)
	
	escribir.tabla <- function(x,row.names=FALSE,col.names=TRUE,...) {
		write.table(x,paste(k, "txt", sep = "."),sep="\t",row.names=row.names,col.names=col.names,...)
	}

	#Almacena la tabla de resultados de clasificacion en txt para luego analizarlos
	escribir.tabla(tabla.resultados)

	return(obj)
}

#Grafica el error de cada uno de los modelos segun su kernel
graficar <- function(model, k){
	#Ordeno los errores de mayor a menor paraque el grafico no sea tan feo
	Error <- sort(model$performances$error, decreasing = TRUE)
	x <- c(1:length(model$performances$error))
	data <- data.frame(x, sort(model$performances$error, decreasing = TRUE))
	plot_ly(data, x = ~x, y = ~Error, name = k, type = 'scatter', mode = 'lines+markers') 


}

#Mejor modelo = radial; gamma = 0.5; cost = 2
model.r <- modelo(wine, "radial")
#Porcentaje de Error en la clasificacion de wine
error.r <- mean(model.r$best.model$fitted!=wine$quality)*100 #Error = 8.380238
graficar(model.r, "radial");								 #Al eliminar "" 10.69418

model.p <- modelo(wine, "polynomial")
#Porcentaje de Error en la clasificacion de wine
error.p <- mean(model.p$best.model$fitted!=wine$quality)*100#Error = 39.27455
graficar(model.p, "polynomial");

model.l <- modelo(wine, "linear")
#Porcentaje de Error en la clasificacion de wine
error.l <- mean(model.l$best.model$fitted!=wine$quality)*100#Error = 41.2758
graficar(model.l, "linear");

model.s <- modelo(wine, "sigmoid")
#Porcentaje de Error en la clasificacion de wine
error.s <- mean(model.s$best.model$fitted!=wine$quality)*100#Error = 43.15197
graficar(model.s, "sigmoid");

#Grafico de los porcentajes de error
plot_ly(
  x = c("radial","polynomial","linear","sigmoid"),
  y = c(error.r, error.p, error.l, error.s),
  name = "% Error Segun Kernel",
  type = "bar"
)
>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
