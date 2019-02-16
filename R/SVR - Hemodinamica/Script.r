<<<<<<< HEAD
RUTA_DATASET <- "C:/Users/Luis.O.A/Documents/USACH/Mineria de Datos/Trabajos/6"
CANTIDAD_NUCLEOS <- 4

setwd(RUTA_DATASET)

#install.packages('plotly', dep = TRUE)
library(e1071)

#install.packages('doParallel', dep = TRUE)
library(doParallel)

#install.packages('xlsx', dep = TRUE)
library('xlsx')

library(plotly)
library(plyr)

registerDoParallel(cores = CANTIDAD_NUCLEOS)


retardos_multi <- function( signalData, lags){
	signal.uni <- signalData
	max.lag <- max(unlist(lags)) + 1
	indices <- 1:nrow(signal.uni)
	lag.mat <- embed(indices, max.lag)
	col.names <- list("PAMn","VFSCn")
	columns <- NULL
	lagged.columns.names <- c()
	for(colname in col.names){
		lag.order <- lags[[colname]]
		columns[[colname]] <- signal.uni[lag.mat[, 1], colname]
		if(!is.null(lag.order) && lag.order > 0)
		for(i in 1:lag.order){
			new.colname <- paste(colname, paste0("lag", i), sep = ".")
			lagged.columns.names <- c(lagged.columns.names, new.colname)
			columns[[new.colname]] <- signal.uni[lag.mat[, i+1], colname]
		}
	}
	folded.signal <- data.frame(columns)
	sorting <- order(lag.mat[, 1])
	folded.signal <- folded.signal[sorting, ]
	list(folded.signal = folded.signal, lagged.columns.names = lagged.columns.names)
}

entrenar <- function(data.1, data.2, parms, retardos_multi){
	salida <- (c( foreach(i = 1:nrow(parms), combine = rbind, .inorder = FALSE)
	%dopar% {
		c <- parms[i, ]$cost
		n <- parms[i, ]$nu
		g <- parms[i, ]$gamma
		l <- parms[i, ]$lagsList
		
		lag<-list(PAMn = l,VFSCn = 0)
		#Etrenamiento 1
		signal.train.1 <- retardos_multi(data.1, lag)
		retDataset1=signal.train.1$folded.signal
		x.1=subset(retDataset1, select = -VFSCn)
		y.1=retDataset1$VFSCn
		modelo <- e1071::svm(x.1, y.1, type = "nu-regression", kernel = "radial", cost =
		c, nu = n, gamma=g)
		#Test 2
		signal.train.2 <- retardos_multi(data.2, lag)
		retDataset2=signal.train.2$folded.signal
		x.2=subset(retDataset2, select = -VFSCn)
		pred <- predict(modelo, x.2)
		y.2=retDataset2$VFSCn
		corr_pred<-cor(pred,y.2,method = "pearson")
		c(l, c, n, g, corr_pred)
	}))
	output <- matrix(unlist(salida), ncol = 5, byrow = TRUE)
	mejoresModelos<-output[order(output[,5], decreasing = TRUE),]
	return(mejoresModelos)
}

plot.signal <- function(datos, retardos_multi, mejoresModelos, i){

	Ts=0.2
	inverseStep=matrix(1,180/Ts,1)
	inverseStep[(90/Ts):(180/Ts),1]=0

	if(i == 0){
		for (i in 1:length(mejoresModelos[,1])){

			lag<-list(PAMn = mejoresModelos[i,1],VFSCn = 0)
			signal.train <- retardos_multi(datos, lag)
			retDataset1=signal.train$folded.signal

			x=subset(retDataset1, select = -VFSCn)
			y=retDataset1$VFSCn
			
			mejorModelo <- svm(x, y, kernel = "radial",type = "nu-regression", cost = mejoresModelos[i,2],
			nu = mejoresModelos[i,3], gamma=mejoresModelos[i,4])

			#PAMn=inverseStep
			#VFSCn=inverseStep
			
			##################PRUEBA CON LOS ESCALONES
			data.esc = read.table("esc.txt")
			PAMn = matrix(data.esc$V1)
			VFSCn = matrix(1,length(PAMn),1)
			#############
			
			data <- data.frame(PAMn,VFSCn)

			signal.train <- retardos_multi(data, lag)
			retDataset1=signal.train$folded.signal

			x=subset(retDataset1, select = -VFSCn)
			y=retDataset1$VFSCn
			
			stepTime=seq(Ts,(length(retDataset1$PAMn))*Ts,Ts)
			stepResponse <- predict(mejorModelo, x )


			plot(stepTime,retDataset1$PAMn,type="l", col="red")
			lines(stepTime,stepResponse, col = "blue")
			legend("topright", c("Escalon de presión", "respuesta al escalon", paste("i = ",i)), title = "autorregulacion", pch =
			1, col=c("red","blue"),lty=c(1,1),inset = 0.01)
			print(paste("corr=",mejoresModelos[i,5]))
			readline(prompt="Press [enter] to continue")

			
		}
	}else if(i > 0){
		lag<-list(PAMn = mejoresModelos[i,1],VFSCn = 0)
		signal.train <- retardos_multi(datos, lag)
		retDataset1=signal.train$folded.signal

		x=subset(retDataset1, select = -VFSCn)
		y=retDataset1$VFSCn
		
		mejorModelo <- svm(x, y, kernel = "radial",type = "nu-regression", cost = mejoresModelos[i,2],
		nu = mejoresModelos[i,3], gamma=mejoresModelos[i,4])

		#PAMn=inverseStep
		#VFSCn=inverseStep
		
		##################PRUEBA CON LOS ESCALONES
		data.esc = read.table("esc.txt")
		PAMn = matrix(data.esc$V1)
		VFSCn = matrix(1,length(PAMn),1)
		#############
		
		data <- data.frame(PAMn,VFSCn)

		signal.train <- retardos_multi(data, lag)
		retDataset1=signal.train$folded.signal

		x=subset(retDataset1, select = -VFSCn)
		y=retDataset1$VFSCn
		
		stepTime=seq(Ts,(length(retDataset1$PAMn))*Ts,Ts)
		stepResponse <- predict(mejorModelo, x )
		
		data.escalon <- data.frame(Time = stepTime, PAM = retDataset1$PAMn, z = stepResponse)
		
		p <- plot_ly(data.escalon, x = ~Time, y = ~PAM, name = 'Escalon de presión', type = 'scatter', mode = 'lines') %>%
		add_trace(y = ~z, name = 'respuesta al escalon', mode = 'lines')
		p
		
	}
}

cost <- 2^(-4:12)
nu <- seq(0.1, 1.0, 0.1)
gamma<- 2^(-4:12)
lagsList<-(1:8)
Dataset1=read.csv("G5_002.csv")


set.seed(100)
trainingRows <- sample(1:nrow(Dataset1), 0.5*nrow(Dataset1))
A <- Dataset1[trainingRows, ]
B <- Dataset1[-trainingRows, ]


data.1 <- data.frame(PAMn = (A$PAM-min(A$PAM))/(max(A$PAM)-min(A$PAM)),VFSCn = (A$VFSC-min(A$VFSC))/(max(A$VFSC)-min(A$VFSC)))

data.2 <- data.frame(PAMn = (B$PAM-min(B$PAM))/(max(B$PAM)-min(B$PAM)),VFSCn = (B$VFSC-min(B$VFSC))/(max(B$VFSC)-min(B$VFSC)))

parms <- expand.grid(lagsList=lagsList, cost = cost, nu = nu, gamma=gamma)


mejoresModelos.1 <- entrenar(data.1, data.2, parms, retardos_multi)
mejoresModelos.2 <- entrenar(data.2, data.1, parms, retardos_multi)

write.xlsx(data.frame(LagList = mejoresModelos.1[,1], Cost = mejoresModelos.1[,2], Nu = mejoresModelos.1[,3], Gamma = mejoresModelos.1[,4], Correlacion = mejoresModelos.1[,5]), "mejoresModelos_1.xlsx", sheetName="Sheet1", append = FALSE)
write.xlsx(data.frame(LagList = mejoresModelos.2[,1], Cost = mejoresModelos.2[,2], Nu = mejoresModelos.2[,3], Gamma = mejoresModelos.2[,4], Correlacion = mejoresModelos.2[,5]), "mejoresModelos_2.xlsx", sheetName="Sheet1", append = FALSE)
############################CORRER HASTA ESTA LINEA

mm1=read.csv("mm1.csv")
mm1 <- as.matrix(setNames(mm1,NULL)) #sano

mm2=read.csv("mm2.csv")
mm2 <- as.matrix(setNames(mm2,NULL)) #Enfermo


######### Eleccion de modelos. Si el ultimo parametro es 0, se veran todos los modelos generados. Si el parametro es mayor a 0 se visualizara el modelo ingresado
plot.signal(data.1, retardos_multi, mm1, 85)#17-21-25(mejor)-39-40(mejor)-44(mejor)-56-59-85(mejor)
plot.signal(data.2, retardos_multi, mm2, 22)#4-8-22(mejor)

####################GRAFICA SEÑAL
Dataset1=read.csv("G5_002.csv")
largo.d1 = length(Dataset1$PAM)

Dataset2=read.csv("G6_001.csv")[1:largo.d1,]
largo.d2 = length(Dataset2$PAM)

data.escalon <- data.frame(Time = c(1:largo.d2), PAM = Dataset1$PAM, PAM_Sujeto_2 = Dataset2$PAM)
		
p <- plot_ly(data.escalon, x = ~Time, y = ~PAM, name = 'PAM Sujeto 1', type = 'scatter', mode = 'lines') %>%
add_trace(y = ~PAM_Sujeto_2, name = 'PAM Sujeto 2', mode = 'lines')
p
		
		


data.escalon <- data.frame(Time = c(1:largo.d2), VFSC_Sujeto_1 = Dataset1$VFSC, VFSC_Sujeto_2 = Dataset2$VFSC)
		
p <- plot_ly(data.escalon, x = ~Time, y = ~VFSC_Sujeto_1, name = 'VFSC Sujeto 1', type = 'scatter', mode = 'lines') %>%
add_trace(y = ~VFSC_Sujeto_2, name = 'VFSC Sujeto 2', mode = 'lines')
p
		
		
=======
RUTA_DATASET <- "C:/Users/Luis.O.A/Documents/USACH/Mineria de Datos/Trabajos/6"
CANTIDAD_NUCLEOS <- 4

setwd(RUTA_DATASET)

#install.packages('plotly', dep = TRUE)
library(e1071)

#install.packages('doParallel', dep = TRUE)
library(doParallel)

#install.packages('xlsx', dep = TRUE)
library('xlsx')

library(plotly)
library(plyr)

registerDoParallel(cores = CANTIDAD_NUCLEOS)


retardos_multi <- function( signalData, lags){
	signal.uni <- signalData
	max.lag <- max(unlist(lags)) + 1
	indices <- 1:nrow(signal.uni)
	lag.mat <- embed(indices, max.lag)
	col.names <- list("PAMn","VFSCn")
	columns <- NULL
	lagged.columns.names <- c()
	for(colname in col.names){
		lag.order <- lags[[colname]]
		columns[[colname]] <- signal.uni[lag.mat[, 1], colname]
		if(!is.null(lag.order) && lag.order > 0)
		for(i in 1:lag.order){
			new.colname <- paste(colname, paste0("lag", i), sep = ".")
			lagged.columns.names <- c(lagged.columns.names, new.colname)
			columns[[new.colname]] <- signal.uni[lag.mat[, i+1], colname]
		}
	}
	folded.signal <- data.frame(columns)
	sorting <- order(lag.mat[, 1])
	folded.signal <- folded.signal[sorting, ]
	list(folded.signal = folded.signal, lagged.columns.names = lagged.columns.names)
}

entrenar <- function(data.1, data.2, parms, retardos_multi){
	salida <- (c( foreach(i = 1:nrow(parms), combine = rbind, .inorder = FALSE)
	%dopar% {
		c <- parms[i, ]$cost
		n <- parms[i, ]$nu
		g <- parms[i, ]$gamma
		l <- parms[i, ]$lagsList
		
		lag<-list(PAMn = l,VFSCn = 0)
		#Etrenamiento 1
		signal.train.1 <- retardos_multi(data.1, lag)
		retDataset1=signal.train.1$folded.signal
		x.1=subset(retDataset1, select = -VFSCn)
		y.1=retDataset1$VFSCn
		modelo <- e1071::svm(x.1, y.1, type = "nu-regression", kernel = "radial", cost =
		c, nu = n, gamma=g)
		#Test 2
		signal.train.2 <- retardos_multi(data.2, lag)
		retDataset2=signal.train.2$folded.signal
		x.2=subset(retDataset2, select = -VFSCn)
		pred <- predict(modelo, x.2)
		y.2=retDataset2$VFSCn
		corr_pred<-cor(pred,y.2,method = "pearson")
		c(l, c, n, g, corr_pred)
	}))
	output <- matrix(unlist(salida), ncol = 5, byrow = TRUE)
	mejoresModelos<-output[order(output[,5], decreasing = TRUE),]
	return(mejoresModelos)
}

plot.signal <- function(datos, retardos_multi, mejoresModelos, i){

	Ts=0.2
	inverseStep=matrix(1,180/Ts,1)
	inverseStep[(90/Ts):(180/Ts),1]=0

	if(i == 0){
		for (i in 1:length(mejoresModelos[,1])){

			lag<-list(PAMn = mejoresModelos[i,1],VFSCn = 0)
			signal.train <- retardos_multi(datos, lag)
			retDataset1=signal.train$folded.signal

			x=subset(retDataset1, select = -VFSCn)
			y=retDataset1$VFSCn
			
			mejorModelo <- svm(x, y, kernel = "radial",type = "nu-regression", cost = mejoresModelos[i,2],
			nu = mejoresModelos[i,3], gamma=mejoresModelos[i,4])

			#PAMn=inverseStep
			#VFSCn=inverseStep
			
			##################PRUEBA CON LOS ESCALONES
			data.esc = read.table("esc.txt")
			PAMn = matrix(data.esc$V1)
			VFSCn = matrix(1,length(PAMn),1)
			#############
			
			data <- data.frame(PAMn,VFSCn)

			signal.train <- retardos_multi(data, lag)
			retDataset1=signal.train$folded.signal

			x=subset(retDataset1, select = -VFSCn)
			y=retDataset1$VFSCn
			
			stepTime=seq(Ts,(length(retDataset1$PAMn))*Ts,Ts)
			stepResponse <- predict(mejorModelo, x )


			plot(stepTime,retDataset1$PAMn,type="l", col="red")
			lines(stepTime,stepResponse, col = "blue")
			legend("topright", c("Escalon de presión", "respuesta al escalon", paste("i = ",i)), title = "autorregulacion", pch =
			1, col=c("red","blue"),lty=c(1,1),inset = 0.01)
			print(paste("corr=",mejoresModelos[i,5]))
			readline(prompt="Press [enter] to continue")

			
		}
	}else if(i > 0){
		lag<-list(PAMn = mejoresModelos[i,1],VFSCn = 0)
		signal.train <- retardos_multi(datos, lag)
		retDataset1=signal.train$folded.signal

		x=subset(retDataset1, select = -VFSCn)
		y=retDataset1$VFSCn
		
		mejorModelo <- svm(x, y, kernel = "radial",type = "nu-regression", cost = mejoresModelos[i,2],
		nu = mejoresModelos[i,3], gamma=mejoresModelos[i,4])

		#PAMn=inverseStep
		#VFSCn=inverseStep
		
		##################PRUEBA CON LOS ESCALONES
		data.esc = read.table("esc.txt")
		PAMn = matrix(data.esc$V1)
		VFSCn = matrix(1,length(PAMn),1)
		#############
		
		data <- data.frame(PAMn,VFSCn)

		signal.train <- retardos_multi(data, lag)
		retDataset1=signal.train$folded.signal

		x=subset(retDataset1, select = -VFSCn)
		y=retDataset1$VFSCn
		
		stepTime=seq(Ts,(length(retDataset1$PAMn))*Ts,Ts)
		stepResponse <- predict(mejorModelo, x )
		
		data.escalon <- data.frame(Time = stepTime, PAM = retDataset1$PAMn, z = stepResponse)
		
		p <- plot_ly(data.escalon, x = ~Time, y = ~PAM, name = 'Escalon de presión', type = 'scatter', mode = 'lines') %>%
		add_trace(y = ~z, name = 'respuesta al escalon', mode = 'lines')
		p
		
	}
}

cost <- 2^(-4:12)
nu <- seq(0.1, 1.0, 0.1)
gamma<- 2^(-4:12)
lagsList<-(1:8)
Dataset1=read.csv("G5_002.csv")


set.seed(100)
trainingRows <- sample(1:nrow(Dataset1), 0.5*nrow(Dataset1))
A <- Dataset1[trainingRows, ]
B <- Dataset1[-trainingRows, ]


data.1 <- data.frame(PAMn = (A$PAM-min(A$PAM))/(max(A$PAM)-min(A$PAM)),VFSCn = (A$VFSC-min(A$VFSC))/(max(A$VFSC)-min(A$VFSC)))

data.2 <- data.frame(PAMn = (B$PAM-min(B$PAM))/(max(B$PAM)-min(B$PAM)),VFSCn = (B$VFSC-min(B$VFSC))/(max(B$VFSC)-min(B$VFSC)))

parms <- expand.grid(lagsList=lagsList, cost = cost, nu = nu, gamma=gamma)


mejoresModelos.1 <- entrenar(data.1, data.2, parms, retardos_multi)
mejoresModelos.2 <- entrenar(data.2, data.1, parms, retardos_multi)

write.xlsx(data.frame(LagList = mejoresModelos.1[,1], Cost = mejoresModelos.1[,2], Nu = mejoresModelos.1[,3], Gamma = mejoresModelos.1[,4], Correlacion = mejoresModelos.1[,5]), "mejoresModelos_1.xlsx", sheetName="Sheet1", append = FALSE)
write.xlsx(data.frame(LagList = mejoresModelos.2[,1], Cost = mejoresModelos.2[,2], Nu = mejoresModelos.2[,3], Gamma = mejoresModelos.2[,4], Correlacion = mejoresModelos.2[,5]), "mejoresModelos_2.xlsx", sheetName="Sheet1", append = FALSE)
############################CORRER HASTA ESTA LINEA

mm1=read.csv("mm1.csv")
mm1 <- as.matrix(setNames(mm1,NULL)) #sano

mm2=read.csv("mm2.csv")
mm2 <- as.matrix(setNames(mm2,NULL)) #Enfermo


######### Eleccion de modelos. Si el ultimo parametro es 0, se veran todos los modelos generados. Si el parametro es mayor a 0 se visualizara el modelo ingresado
plot.signal(data.1, retardos_multi, mm1, 85)#17-21-25(mejor)-39-40(mejor)-44(mejor)-56-59-85(mejor)
plot.signal(data.2, retardos_multi, mm2, 22)#4-8-22(mejor)

####################GRAFICA SEÑAL
Dataset1=read.csv("G5_002.csv")
largo.d1 = length(Dataset1$PAM)

Dataset2=read.csv("G6_001.csv")[1:largo.d1,]
largo.d2 = length(Dataset2$PAM)

data.escalon <- data.frame(Time = c(1:largo.d2), PAM = Dataset1$PAM, PAM_Sujeto_2 = Dataset2$PAM)
		
p <- plot_ly(data.escalon, x = ~Time, y = ~PAM, name = 'PAM Sujeto 1', type = 'scatter', mode = 'lines') %>%
add_trace(y = ~PAM_Sujeto_2, name = 'PAM Sujeto 2', mode = 'lines')
p
		
		


data.escalon <- data.frame(Time = c(1:largo.d2), VFSC_Sujeto_1 = Dataset1$VFSC, VFSC_Sujeto_2 = Dataset2$VFSC)
		
p <- plot_ly(data.escalon, x = ~Time, y = ~VFSC_Sujeto_1, name = 'VFSC Sujeto 1', type = 'scatter', mode = 'lines') %>%
add_trace(y = ~VFSC_Sujeto_2, name = 'VFSC Sujeto 2', mode = 'lines')
p
		
		
>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
