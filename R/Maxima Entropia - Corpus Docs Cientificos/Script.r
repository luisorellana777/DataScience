<<<<<<< HEAD
<<<<<<< HEAD
install.packages('lime', dep = TRUE)
install.packages('maxent', dep = TRUE)
install.packages('tm', dep = TRUE)
install.packages('SnowballC', dep = TRUE)
install.packages('xlsx', dep = TRUE)
install.packages('wordcloud', dep = TRUE)

rm(list=ls(all=TRUE))
library(lime)
library(maxent)
library(tm)
library(SnowballC)
library(xlsx)
library(wordcloud)

data(train_sentences)

data(test_sentences)

train_corpus <- Corpus(VectorSource(train_sentences$text))
test_corpus <- Corpus(VectorSource(test_sentences$text))

for (i in 1:10) print (train_corpus[[i]]$content) 

#Remover Puntuación xx
train_corpus <- tm_map(train_corpus, content_transformer(removePunctuation))
test_corpus <- tm_map(test_corpus, content_transformer(removePunctuation))

#Remover Stop Words xx
train_corpus <- tm_map(train_corpus, content_transformer(removeWords), stopwords("english"))
test_corpus <- tm_map(test_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case xx
train_corpus <- tm_map(train_corpus, content_transformer(tolower))
train_corpus <- tm_map(train_corpus, content_transformer(removeWords), stopwords("english"))
test_corpus <- tm_map(test_corpus, content_transformer(tolower))
test_corpus <- tm_map(test_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming xx
train_corpus <- tm_map(train_corpus, stemDocument)
test_corpus <- tm_map(test_corpus, stemDocument)

#Remover White Space xx
train_corpus <- tm_map(train_corpus, stripWhitespace)
test_corpus <- tm_map(test_corpus, stripWhitespace)

#Remover Números xx
train_corpus <- tm_map(train_corpus, content_transformer(removeNumbers))
test_corpus <- tm_map(test_corpus, content_transformer(removeNumbers))

train_matrix <- DocumentTermMatrix(train_corpus)
test_matrix <- DocumentTermMatrix(test_corpus)

train_sparse <- as.compressed.matrix(train_matrix) 
test_sparse <- as.compressed.matrix(test_matrix) 

f <- tune.maxent(train_sparse,train_sentences$class.text,nfold=3,showall=TRUE, verbose=TRUE) 

print(f)

model<-maxent(train_sparse,train_sentences$class.text, l1_regularizer=0.0,
l2_regularizer=0.8, use_sgd=FALSE, set_heldout=0, verbose=TRUE)

results <- predict(model,test_sparse) 

out = matrix(, nrow = length(results[,1]), ncol = 2)

for(i in 1:length(results[,1])){
	value <- results[i,2]
	mejorJ <- 2
	for(j in 3:16){
		if(value > results[i,j]){
			#results[i,j] <- FALSE
		}else{
			value <- results[i,j]
			mejorJ <- j
			#results[i,2] <- FALSE
		}
	}
	
	out[i, 1] <- results[i,1]
	if(results[i,1] == substr(names(results[i,mejorJ]), 1, 4)){
		out[i, 2] <- 1
	}else{
		out[i, 2] <- 0
	}
	#results[i,mejorJ] <- TRUE
}

out_charts_correcto = matrix(unique(substr(names(results[1,])[2:16], 1, 4)),, nrow = length(unique(substr(names(results[1,])[2:16], 1, 4))), ncol = 2)
out_charts_noCorrecto = matrix(unique(substr(names(results[1,])[2:16], 1, 4)),, nrow = length(unique(substr(names(results[1,])[2:16], 1, 4))), ncol = 2)

out_charts_correcto[,2] <- 0
out_charts_noCorrecto[,2] <- 0

cantDatosCorrectos = 0
cantDatosIncorrectos = 0
for(i in 1:length(out_charts_correcto[,1])){
	cantUnos = 0
	cantCeros = 0
	for(j in 1:length(results[,1])){
		if(substr(out_charts_correcto[i,1], 1, 4) == substr(out[j,1], 1, 4) && out[j,2] == 1){
			cantUnos = cantUnos + 1
			out_charts_correcto[i, 2] <- cantUnos
		}else if(substr(out_charts_correcto[i,1], 1, 4) == substr(out[j,1], 1, 4) && out[j,2] == 0){
			cantCeros = cantCeros + 1
			out_charts_noCorrecto[i, 2] <- cantCeros
		}	
	}
	cantDatosCorrectos <- cantDatosCorrectos + as.numeric(out_charts_correcto[i, 2])
	cantDatosIncorrectos <- cantDatosIncorrectos + as.numeric(out_charts_noCorrecto[i, 2])
}

out_charts_noCorrecto[1:5,]
out_charts_correcto[1:5,]

##grafico de distribucion de etiquetas - TRAIN
distribucion_etiquetas_train <- matrix(names(results[1,])[2:6],, nrow = length(names(results[1,])[2:6]), ncol = 2)
distribucion_etiquetas_train[,2] <- 0
for(i in 1:length(distribucion_etiquetas_train[,1])){
	for(j in 1:length(train_sentences$class.text)){
		if(distribucion_etiquetas_train[i,1] == substr(train_sentences$class.text[j], 1, 4)){
			distribucion_etiquetas_train[i,2] = as.numeric(distribucion_etiquetas_train[i,2]) + 1
		}
	}
}
barplot(as.numeric(distribucion_etiquetas_train[1:5,2]), main=paste("Distribución Train", toString(length(train_sentences$class.text)), sep=" - "), 
  	xlab="Etiquetas",names.arg = distribucion_etiquetas_train[1:5,1])


##grafico de distribucion de etiquetas - TEST
distribucion_etiquetas_test = matrix(names(results[1,])[2:6],, nrow = length(names(results[1,])[2:6]), ncol = 2)
distribucion_etiquetas_test[,2] <- 0
for(i in 1:length(results[1:5,1])){
	distribucion_etiquetas_test[i,2] <- as.numeric(out_charts_correcto[i,2]) + as.numeric(out_charts_noCorrecto[i,2])
	
}
barplot(as.numeric(distribucion_etiquetas_test[1:5,2]), main=paste("Distribución Test", toString(length(test_sentences$class.text)), sep=" - "), 
  	xlab="Etiquetas",names.arg = distribucion_etiquetas_test[1:5,1])
	
	
##grafico de frecuencia de palabras por etiqueta
etiquetas <- c("OWNX", "MISC", "CONT", "AIMX", "BASE")

n_Text = 1
#_--------------OWNX
m_OWNX = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[1]){
		m_OWNX[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
OWNX_corpus <- Corpus(VectorSource(m_OWNX))
#Remover Puntuación
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(removePunctuation))

#Remover Stop Words
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(tolower))
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
OWNX_corpus <- tm_map(OWNX_corpus, stemDocument)

#Remover White Space
OWNX_corpus <- tm_map(OWNX_corpus, stripWhitespace)

#Remover Números
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(removeNumbers))

tdm_OWNX <- TermDocumentMatrix(OWNX_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_OWNX)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
		  
		  
n_Text = 1  
#_--------------MISC
m_MISC = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[2]){
		m_MISC[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
MISC_corpus <- Corpus(VectorSource(m_MISC))

#Remover Puntuación
MISC_corpus <- tm_map(MISC_corpus, content_transformer(removePunctuation))

#Remover Stop Words
MISC_corpus <- tm_map(MISC_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
MISC_corpus <- tm_map(MISC_corpus, content_transformer(tolower))
MISC_corpus <- tm_map(MISC_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
MISC_corpus <- tm_map(MISC_corpus, stemDocument)

#Remover White Space
MISC_corpus <- tm_map(MISC_corpus, stripWhitespace)

#Remover Números
MISC_corpus <- tm_map(MISC_corpus, content_transformer(removeNumbers))

tdm_MISC <- TermDocumentMatrix(MISC_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_MISC)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
		  
		  
n_Text = 1  
#_--------------CONT
m_CONT = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[3]){
		m_CONT[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
CONT_corpus <- Corpus(VectorSource(m_CONT))

#Remover Puntuación
CONT_corpus <- tm_map(CONT_corpus, content_transformer(removePunctuation))

#Remover Stop Words
CONT_corpus <- tm_map(CONT_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
CONT_corpus <- tm_map(CONT_corpus, content_transformer(tolower))
CONT_corpus <- tm_map(CONT_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
CONT_corpus <- tm_map(CONT_corpus, stemDocument)

#Remover White Space
CONT_corpus <- tm_map(CONT_corpus, stripWhitespace)

#Remover Números
CONT_corpus <- tm_map(CONT_corpus, content_transformer(removeNumbers))

tdm_CONT <- TermDocumentMatrix(CONT_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_CONT)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
	
	
n_Text = 1  
#_--------------AIMX
m_AIMX = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[4]){
		m_AIMX[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
AIMX_corpus <- Corpus(VectorSource(m_AIMX))

#Remover Puntuación
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(removePunctuation))

#Remover Stop Words
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(tolower))
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
AIMX_corpus <- tm_map(AIMX_corpus, stemDocument)

#Remover White Space
AIMX_corpus <- tm_map(AIMX_corpus, stripWhitespace)

#Remover Números
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(removeNumbers))

tdm_AIMX <- TermDocumentMatrix(AIMX_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_AIMX)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
		  
		  
n_Text = 1  
#_--------------BASE
m_BASE = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[5]){
		m_BASE[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
BASE_corpus <- Corpus(VectorSource(m_BASE))

#Remover Puntuación
BASE_corpus <- tm_map(BASE_corpus, content_transformer(removePunctuation))

#Remover Stop Words
BASE_corpus <- tm_map(BASE_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
BASE_corpus <- tm_map(BASE_corpus, content_transformer(tolower))
BASE_corpus <- tm_map(BASE_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
BASE_corpus <- tm_map(BASE_corpus, stemDocument)

#Remover White Space
BASE_corpus <- tm_map(BASE_corpus, stripWhitespace)

#Remover Números
BASE_corpus <- tm_map(BASE_corpus, content_transformer(removeNumbers))

tdm_BASE <- TermDocumentMatrix(BASE_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_BASE)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)

####################
##################
##grafico de torta Incorectos
pie(as.numeric(out_charts_noCorrecto[1:5,2]), labels = out_charts_noCorrecto[1:5,2], main=paste("Pie Chart Predicción Incorrecta", toString(cantDatosIncorrectos), sep=" - "),col = rainbow(length(out_charts_noCorrecto[1:5,1])))
legend("topright", out_charts_noCorrecto[1:5,1], cex = 0.8,
   fill = rainbow(length(out_charts_noCorrecto[1:5,1])))

##grafio de lineas entre correctos e incorrectos   
plot(as.numeric(out_charts_correcto[1:5,2]),type = "o",col = "blue", xlab = "Etiquetas", ylab = "Cantidad Predicciones", 
   main = "Clasificación")
lines(out_charts_noCorrecto[1:5,2], type = "o", col = "red")
legend("topright", out_charts_correcto[1:5,1], cex = 0.8)

##grafico de barra entre correctos e incorrectos 
set.union <- function(a, b) {
  u <- a
  for (i in 1:length(b)) {
    u <- append(u, b[i])
    
  }
  return(u)
}
Values <- matrix(set.union(as.numeric(out_charts_correcto[1:5,2]), as.numeric(out_charts_noCorrecto[1:5,2])),nrow = 2,ncol = 5,byrow = TRUE)
colors <- c("green","red")
et <- c(paste("Correctas", toString(cantDatosCorrectos), sep=" "),paste("Incorrectos", toString(cantDatosIncorrectos), sep=" "))

barplot(Values, main="Predicción", 
  	ylab="Número de Predicciones",names.arg = out_charts_correcto[1:5,1], 
	xlab = "Etiquetas",col = colors)
  
legend("topright", et, cex = 1.3, fill = colors)
############# MATRIZ DE CONFUCION  


Labels <- c("MISC", "OWNX", "AIMX", "BASE", "CONT");

for(l in 1:length(Labels)){

	m_confucion = matrix(nrow = 3, ncol = 3)
	m_confucion[,] <- 0
	m_confucion[1,1] <- ""
	m_confucion[1,2] <- "Relevantes"
	m_confucion[1,3] <- "No Relevantes"   
	m_confucion[2,1] <- "Recuperados"
	m_confucion[3,1] <- "No Recuperados" 

	#Recuperados
	for(i in 1:length(out_charts_correcto[1:5,1])){
		#Relevante#
		if(out_charts_correcto[i,1] == Labels[l]){
			if(as.numeric(out_charts_correcto[i,2])>0){
				#Verdadero Positivo
				m_confucion[2,2] <- as.numeric(m_confucion[2,2]) + as.numeric(out_charts_correcto[i,2])
			}
		}else{
			#No Relevante#
			if(as.numeric(out_charts_correcto[i,2])>0){
				#Falso Positivo
				m_confucion[2,3] <- as.numeric(m_confucion[2,3]) + as.numeric(out_charts_correcto[i,2])
			}
		}
	}

	#No Recuperados
	for(i in 1:length(out_charts_noCorrecto[1:5,1])){
		#Relevante#
		if(out_charts_noCorrecto[i,1] == Labels[l]){
			if(as.numeric(out_charts_noCorrecto[i,2])>0){
				#Falso Negativo
				m_confucion[3,2] <- as.numeric(m_confucion[3,2]) + as.numeric(out_charts_noCorrecto[i,2])
			}
		}else{
			#No Relevante#
			if(as.numeric(out_charts_noCorrecto[i,2])>0){
				#Verdadero Negativo
				m_confucion[3,3] <- as.numeric(m_confucion[3,3]) + as.numeric(out_charts_noCorrecto[i,2])
			}
		}
	}
	print(Labels[l])
	print(m_confucion)  

	#Precision
	Precision <- as.numeric(m_confucion[2,2])/(as.numeric(m_confucion[2,2]) + as.numeric(m_confucion[2,3]))

	#Recall
	Recall <- as.numeric(m_confucion[2,2])/(as.numeric(m_confucion[2,2]) + as.numeric(m_confucion[3,2]))

	##Fscore
	Fscore = (2*Precision*Recall)/(Precision+Recall)
	print("Fscore")
	print(Fscore)
	print("Precision")
	print(Precision)
	print("Recall")
	print(Recall)
	
	print("-----------------------------------------");
	
}



###########################################################
##Grafico Resultados Fscore
fScore <- c(0.0685155,0.06896552,0.096,0.07830343,0.09888357, 0.09888357, 0.1032258, 0.08064516)
Etiquetas <- c("1- Sin Pre-Pro", "2- Remover Puntuación", "3- Remover Stop Words","4- To Lower Case","5- Stremming","6- Remover White Space","7- Remover Números", "8- Sin To Lower Case")

plot(fScore,type = "o", col = "red", xlab = "Procesamiento", ylab = "Fscore",
   main = "Procesamiento")
legend("bottomright", Etiquetas, cex = 0.8)
   
   
setwd("C:\\Users\\Luis.O.A\\Documents\\USACH\\Mineria de Datos\\Trabajos\\5")
write.xlsx(as.data.frame(results), "Prediccion.xlsx", sheetName="Sheet1")

##Grafico de Lineas Nuevo
 library(plotly)

 MISC <- c(0.7673216, 0.7983952, 0.7549531, 0.7431579, 0.7214514, 0.7241747, 0.7983952)
 OWNX <- c(0.3541076, 0.3239437, 0.4717949, 0.498094, 0.4627451, 0.4728682, 0.3638889)
 AIMX <- c(0.1855346, 0.17737, 0.06168831, 0.05280528, 0.1064516, 0.09983897, 0.08780488)
 BASE <- c(0.003460208, 0.01666667, 0.01666667, 0.01013514, 0.02356902, 0.02013423, 0.03672788)
 CONT <- c(0.07023411, 0.06188925, 0.08064516, 0.06885246, 0.1033926, 0.09677419, 0.06896552)




x <- c(1:7)

data <- data.frame(x, MISC, OWNX, AIMX, BASE, CONT)

p <- plot_ly(data, x = ~x, y = ~MISC, name = 'MISC', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~OWNX, name = 'OWNX', mode = 'lines+markers') %>%
  add_trace(y = ~AIMX, name = 'AIMX', mode = 'lines+markers') %>%
  add_trace(y = ~BASE, name = 'BASE', mode = 'lines+markers') %>%
  add_trace(y = ~CONT, name = 'CONT', mode = 'lines+markers') %>%
  layout(title = "Fscore Segun Pre-Procesamiento",
         xaxis = list(title = "Pre-Procesamiento"),
         yaxis = list (title = "Fscore"))


p


## 7 - Remover Números
[1] "MISC"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "398"        "190"          
[3,] "No Recuperados" "11"         "1"            
[1] "Fscore"
[1] 0.7983952
[1] "Precision"
[1] 0.6768707
[1] "Recall"
[1] 0.9731051
[1] "-----------------------------------------"
[1] "OWNX"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "131"        "457"          
[3,] "No Recuperados" "1"          "11"           
[1] "Fscore"
[1] 0.3638889
[1] "Precision"
[1] 0.2227891
[1] "Recall"
[1] 0.9924242
[1] "-----------------------------------------"
[1] "AIMX"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "27"         "561"          
[3,] "No Recuperados" "0"          "12"           
[1] "Fscore"
[1] 0.08780488
[1] "Precision"
[1] 0.04591837
[1] "Recall"
[1] 1
[1] "-----------------------------------------"
[1] "BASE"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "11"         "577"          
[3,] "No Recuperados" "0"          "12"           
[1] "Fscore"
[1] 0.03672788
[1] "Precision"
[1] 0.01870748
[1] "Recall"
[1] 1
[1] "-----------------------------------------"
[1] "CONT"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "21"         "567"          
[3,] "No Recuperados" "0"          "12"           
[1] "Fscore"
[1] 0.06896552
[1] "Precision"
[1] 0.03571429
[1] "Recall"
[1] 1
[1] "-----------------------------------------"

=======
install.packages('lime', dep = TRUE)
install.packages('maxent', dep = TRUE)
install.packages('tm', dep = TRUE)
install.packages('SnowballC', dep = TRUE)
install.packages('xlsx', dep = TRUE)
install.packages('wordcloud', dep = TRUE)

rm(list=ls(all=TRUE))
library(lime)
library(maxent)
library(tm)
library(SnowballC)
library(xlsx)
library(wordcloud)

data(train_sentences)

data(test_sentences)

train_corpus <- Corpus(VectorSource(train_sentences$text))
test_corpus <- Corpus(VectorSource(test_sentences$text))

for (i in 1:10) print (train_corpus[[i]]$content) 

#Remover Puntuación xx
train_corpus <- tm_map(train_corpus, content_transformer(removePunctuation))
test_corpus <- tm_map(test_corpus, content_transformer(removePunctuation))

#Remover Stop Words xx
train_corpus <- tm_map(train_corpus, content_transformer(removeWords), stopwords("english"))
test_corpus <- tm_map(test_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case xx
train_corpus <- tm_map(train_corpus, content_transformer(tolower))
train_corpus <- tm_map(train_corpus, content_transformer(removeWords), stopwords("english"))
test_corpus <- tm_map(test_corpus, content_transformer(tolower))
test_corpus <- tm_map(test_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming xx
train_corpus <- tm_map(train_corpus, stemDocument)
test_corpus <- tm_map(test_corpus, stemDocument)

#Remover White Space xx
train_corpus <- tm_map(train_corpus, stripWhitespace)
test_corpus <- tm_map(test_corpus, stripWhitespace)

#Remover Números xx
train_corpus <- tm_map(train_corpus, content_transformer(removeNumbers))
test_corpus <- tm_map(test_corpus, content_transformer(removeNumbers))

train_matrix <- DocumentTermMatrix(train_corpus)
test_matrix <- DocumentTermMatrix(test_corpus)

train_sparse <- as.compressed.matrix(train_matrix) 
test_sparse <- as.compressed.matrix(test_matrix) 

f <- tune.maxent(train_sparse,train_sentences$class.text,nfold=3,showall=TRUE, verbose=TRUE) 

print(f)

model<-maxent(train_sparse,train_sentences$class.text, l1_regularizer=0.0,
l2_regularizer=0.8, use_sgd=FALSE, set_heldout=0, verbose=TRUE)

results <- predict(model,test_sparse) 

out = matrix(, nrow = length(results[,1]), ncol = 2)

for(i in 1:length(results[,1])){
	value <- results[i,2]
	mejorJ <- 2
	for(j in 3:16){
		if(value > results[i,j]){
			#results[i,j] <- FALSE
		}else{
			value <- results[i,j]
			mejorJ <- j
			#results[i,2] <- FALSE
		}
	}
	
	out[i, 1] <- results[i,1]
	if(results[i,1] == substr(names(results[i,mejorJ]), 1, 4)){
		out[i, 2] <- 1
	}else{
		out[i, 2] <- 0
	}
	#results[i,mejorJ] <- TRUE
}

out_charts_correcto = matrix(unique(substr(names(results[1,])[2:16], 1, 4)),, nrow = length(unique(substr(names(results[1,])[2:16], 1, 4))), ncol = 2)
out_charts_noCorrecto = matrix(unique(substr(names(results[1,])[2:16], 1, 4)),, nrow = length(unique(substr(names(results[1,])[2:16], 1, 4))), ncol = 2)

out_charts_correcto[,2] <- 0
out_charts_noCorrecto[,2] <- 0

cantDatosCorrectos = 0
cantDatosIncorrectos = 0
for(i in 1:length(out_charts_correcto[,1])){
	cantUnos = 0
	cantCeros = 0
	for(j in 1:length(results[,1])){
		if(substr(out_charts_correcto[i,1], 1, 4) == substr(out[j,1], 1, 4) && out[j,2] == 1){
			cantUnos = cantUnos + 1
			out_charts_correcto[i, 2] <- cantUnos
		}else if(substr(out_charts_correcto[i,1], 1, 4) == substr(out[j,1], 1, 4) && out[j,2] == 0){
			cantCeros = cantCeros + 1
			out_charts_noCorrecto[i, 2] <- cantCeros
		}	
	}
	cantDatosCorrectos <- cantDatosCorrectos + as.numeric(out_charts_correcto[i, 2])
	cantDatosIncorrectos <- cantDatosIncorrectos + as.numeric(out_charts_noCorrecto[i, 2])
}

out_charts_noCorrecto[1:5,]
out_charts_correcto[1:5,]

##grafico de distribucion de etiquetas - TRAIN
distribucion_etiquetas_train <- matrix(names(results[1,])[2:6],, nrow = length(names(results[1,])[2:6]), ncol = 2)
distribucion_etiquetas_train[,2] <- 0
for(i in 1:length(distribucion_etiquetas_train[,1])){
	for(j in 1:length(train_sentences$class.text)){
		if(distribucion_etiquetas_train[i,1] == substr(train_sentences$class.text[j], 1, 4)){
			distribucion_etiquetas_train[i,2] = as.numeric(distribucion_etiquetas_train[i,2]) + 1
		}
	}
}
barplot(as.numeric(distribucion_etiquetas_train[1:5,2]), main=paste("Distribución Train", toString(length(train_sentences$class.text)), sep=" - "), 
  	xlab="Etiquetas",names.arg = distribucion_etiquetas_train[1:5,1])


##grafico de distribucion de etiquetas - TEST
distribucion_etiquetas_test = matrix(names(results[1,])[2:6],, nrow = length(names(results[1,])[2:6]), ncol = 2)
distribucion_etiquetas_test[,2] <- 0
for(i in 1:length(results[1:5,1])){
	distribucion_etiquetas_test[i,2] <- as.numeric(out_charts_correcto[i,2]) + as.numeric(out_charts_noCorrecto[i,2])
	
}
barplot(as.numeric(distribucion_etiquetas_test[1:5,2]), main=paste("Distribución Test", toString(length(test_sentences$class.text)), sep=" - "), 
  	xlab="Etiquetas",names.arg = distribucion_etiquetas_test[1:5,1])
	
	
##grafico de frecuencia de palabras por etiqueta
etiquetas <- c("OWNX", "MISC", "CONT", "AIMX", "BASE")

n_Text = 1
#_--------------OWNX
m_OWNX = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[1]){
		m_OWNX[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
OWNX_corpus <- Corpus(VectorSource(m_OWNX))
#Remover Puntuación
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(removePunctuation))

#Remover Stop Words
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(tolower))
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
OWNX_corpus <- tm_map(OWNX_corpus, stemDocument)

#Remover White Space
OWNX_corpus <- tm_map(OWNX_corpus, stripWhitespace)

#Remover Números
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(removeNumbers))

tdm_OWNX <- TermDocumentMatrix(OWNX_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_OWNX)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
		  
		  
n_Text = 1  
#_--------------MISC
m_MISC = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[2]){
		m_MISC[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
MISC_corpus <- Corpus(VectorSource(m_MISC))

#Remover Puntuación
MISC_corpus <- tm_map(MISC_corpus, content_transformer(removePunctuation))

#Remover Stop Words
MISC_corpus <- tm_map(MISC_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
MISC_corpus <- tm_map(MISC_corpus, content_transformer(tolower))
MISC_corpus <- tm_map(MISC_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
MISC_corpus <- tm_map(MISC_corpus, stemDocument)

#Remover White Space
MISC_corpus <- tm_map(MISC_corpus, stripWhitespace)

#Remover Números
MISC_corpus <- tm_map(MISC_corpus, content_transformer(removeNumbers))

tdm_MISC <- TermDocumentMatrix(MISC_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_MISC)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
		  
		  
n_Text = 1  
#_--------------CONT
m_CONT = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[3]){
		m_CONT[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
CONT_corpus <- Corpus(VectorSource(m_CONT))

#Remover Puntuación
CONT_corpus <- tm_map(CONT_corpus, content_transformer(removePunctuation))

#Remover Stop Words
CONT_corpus <- tm_map(CONT_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
CONT_corpus <- tm_map(CONT_corpus, content_transformer(tolower))
CONT_corpus <- tm_map(CONT_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
CONT_corpus <- tm_map(CONT_corpus, stemDocument)

#Remover White Space
CONT_corpus <- tm_map(CONT_corpus, stripWhitespace)

#Remover Números
CONT_corpus <- tm_map(CONT_corpus, content_transformer(removeNumbers))

tdm_CONT <- TermDocumentMatrix(CONT_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_CONT)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
	
	
n_Text = 1  
#_--------------AIMX
m_AIMX = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[4]){
		m_AIMX[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
AIMX_corpus <- Corpus(VectorSource(m_AIMX))

#Remover Puntuación
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(removePunctuation))

#Remover Stop Words
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(tolower))
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
AIMX_corpus <- tm_map(AIMX_corpus, stemDocument)

#Remover White Space
AIMX_corpus <- tm_map(AIMX_corpus, stripWhitespace)

#Remover Números
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(removeNumbers))

tdm_AIMX <- TermDocumentMatrix(AIMX_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_AIMX)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
		  
		  
n_Text = 1  
#_--------------BASE
m_BASE = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[5]){
		m_BASE[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
BASE_corpus <- Corpus(VectorSource(m_BASE))

#Remover Puntuación
BASE_corpus <- tm_map(BASE_corpus, content_transformer(removePunctuation))

#Remover Stop Words
BASE_corpus <- tm_map(BASE_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
BASE_corpus <- tm_map(BASE_corpus, content_transformer(tolower))
BASE_corpus <- tm_map(BASE_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
BASE_corpus <- tm_map(BASE_corpus, stemDocument)

#Remover White Space
BASE_corpus <- tm_map(BASE_corpus, stripWhitespace)

#Remover Números
BASE_corpus <- tm_map(BASE_corpus, content_transformer(removeNumbers))

tdm_BASE <- TermDocumentMatrix(BASE_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_BASE)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)

####################
##################
##grafico de torta Incorectos
pie(as.numeric(out_charts_noCorrecto[1:5,2]), labels = out_charts_noCorrecto[1:5,2], main=paste("Pie Chart Predicción Incorrecta", toString(cantDatosIncorrectos), sep=" - "),col = rainbow(length(out_charts_noCorrecto[1:5,1])))
legend("topright", out_charts_noCorrecto[1:5,1], cex = 0.8,
   fill = rainbow(length(out_charts_noCorrecto[1:5,1])))

##grafio de lineas entre correctos e incorrectos   
plot(as.numeric(out_charts_correcto[1:5,2]),type = "o",col = "blue", xlab = "Etiquetas", ylab = "Cantidad Predicciones", 
   main = "Clasificación")
lines(out_charts_noCorrecto[1:5,2], type = "o", col = "red")
legend("topright", out_charts_correcto[1:5,1], cex = 0.8)

##grafico de barra entre correctos e incorrectos 
set.union <- function(a, b) {
  u <- a
  for (i in 1:length(b)) {
    u <- append(u, b[i])
    
  }
  return(u)
}
Values <- matrix(set.union(as.numeric(out_charts_correcto[1:5,2]), as.numeric(out_charts_noCorrecto[1:5,2])),nrow = 2,ncol = 5,byrow = TRUE)
colors <- c("green","red")
et <- c(paste("Correctas", toString(cantDatosCorrectos), sep=" "),paste("Incorrectos", toString(cantDatosIncorrectos), sep=" "))

barplot(Values, main="Predicción", 
  	ylab="Número de Predicciones",names.arg = out_charts_correcto[1:5,1], 
	xlab = "Etiquetas",col = colors)
  
legend("topright", et, cex = 1.3, fill = colors)
############# MATRIZ DE CONFUCION  


Labels <- c("MISC", "OWNX", "AIMX", "BASE", "CONT");

for(l in 1:length(Labels)){

	m_confucion = matrix(nrow = 3, ncol = 3)
	m_confucion[,] <- 0
	m_confucion[1,1] <- ""
	m_confucion[1,2] <- "Relevantes"
	m_confucion[1,3] <- "No Relevantes"   
	m_confucion[2,1] <- "Recuperados"
	m_confucion[3,1] <- "No Recuperados" 

	#Recuperados
	for(i in 1:length(out_charts_correcto[1:5,1])){
		#Relevante#
		if(out_charts_correcto[i,1] == Labels[l]){
			if(as.numeric(out_charts_correcto[i,2])>0){
				#Verdadero Positivo
				m_confucion[2,2] <- as.numeric(m_confucion[2,2]) + as.numeric(out_charts_correcto[i,2])
			}
		}else{
			#No Relevante#
			if(as.numeric(out_charts_correcto[i,2])>0){
				#Falso Positivo
				m_confucion[2,3] <- as.numeric(m_confucion[2,3]) + as.numeric(out_charts_correcto[i,2])
			}
		}
	}

	#No Recuperados
	for(i in 1:length(out_charts_noCorrecto[1:5,1])){
		#Relevante#
		if(out_charts_noCorrecto[i,1] == Labels[l]){
			if(as.numeric(out_charts_noCorrecto[i,2])>0){
				#Falso Negativo
				m_confucion[3,2] <- as.numeric(m_confucion[3,2]) + as.numeric(out_charts_noCorrecto[i,2])
			}
		}else{
			#No Relevante#
			if(as.numeric(out_charts_noCorrecto[i,2])>0){
				#Verdadero Negativo
				m_confucion[3,3] <- as.numeric(m_confucion[3,3]) + as.numeric(out_charts_noCorrecto[i,2])
			}
		}
	}
	print(Labels[l])
	print(m_confucion)  

	#Precision
	Precision <- as.numeric(m_confucion[2,2])/(as.numeric(m_confucion[2,2]) + as.numeric(m_confucion[2,3]))

	#Recall
	Recall <- as.numeric(m_confucion[2,2])/(as.numeric(m_confucion[2,2]) + as.numeric(m_confucion[3,2]))

	##Fscore
	Fscore = (2*Precision*Recall)/(Precision+Recall)
	print("Fscore")
	print(Fscore)
	print("Precision")
	print(Precision)
	print("Recall")
	print(Recall)
	
	print("-----------------------------------------");
	
}



###########################################################
##Grafico Resultados Fscore
fScore <- c(0.0685155,0.06896552,0.096,0.07830343,0.09888357, 0.09888357, 0.1032258, 0.08064516)
Etiquetas <- c("1- Sin Pre-Pro", "2- Remover Puntuación", "3- Remover Stop Words","4- To Lower Case","5- Stremming","6- Remover White Space","7- Remover Números", "8- Sin To Lower Case")

plot(fScore,type = "o", col = "red", xlab = "Procesamiento", ylab = "Fscore",
   main = "Procesamiento")
legend("bottomright", Etiquetas, cex = 0.8)
   
   
setwd("C:\\Users\\Luis.O.A\\Documents\\USACH\\Mineria de Datos\\Trabajos\\5")
write.xlsx(as.data.frame(results), "Prediccion.xlsx", sheetName="Sheet1")

##Grafico de Lineas Nuevo
 library(plotly)

 MISC <- c(0.7673216, 0.7983952, 0.7549531, 0.7431579, 0.7214514, 0.7241747, 0.7983952)
 OWNX <- c(0.3541076, 0.3239437, 0.4717949, 0.498094, 0.4627451, 0.4728682, 0.3638889)
 AIMX <- c(0.1855346, 0.17737, 0.06168831, 0.05280528, 0.1064516, 0.09983897, 0.08780488)
 BASE <- c(0.003460208, 0.01666667, 0.01666667, 0.01013514, 0.02356902, 0.02013423, 0.03672788)
 CONT <- c(0.07023411, 0.06188925, 0.08064516, 0.06885246, 0.1033926, 0.09677419, 0.06896552)




x <- c(1:7)

data <- data.frame(x, MISC, OWNX, AIMX, BASE, CONT)

p <- plot_ly(data, x = ~x, y = ~MISC, name = 'MISC', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~OWNX, name = 'OWNX', mode = 'lines+markers') %>%
  add_trace(y = ~AIMX, name = 'AIMX', mode = 'lines+markers') %>%
  add_trace(y = ~BASE, name = 'BASE', mode = 'lines+markers') %>%
  add_trace(y = ~CONT, name = 'CONT', mode = 'lines+markers') %>%
  layout(title = "Fscore Segun Pre-Procesamiento",
         xaxis = list(title = "Pre-Procesamiento"),
         yaxis = list (title = "Fscore"))


p


## 7 - Remover Números
[1] "MISC"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "398"        "190"          
[3,] "No Recuperados" "11"         "1"            
[1] "Fscore"
[1] 0.7983952
[1] "Precision"
[1] 0.6768707
[1] "Recall"
[1] 0.9731051
[1] "-----------------------------------------"
[1] "OWNX"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "131"        "457"          
[3,] "No Recuperados" "1"          "11"           
[1] "Fscore"
[1] 0.3638889
[1] "Precision"
[1] 0.2227891
[1] "Recall"
[1] 0.9924242
[1] "-----------------------------------------"
[1] "AIMX"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "27"         "561"          
[3,] "No Recuperados" "0"          "12"           
[1] "Fscore"
[1] 0.08780488
[1] "Precision"
[1] 0.04591837
[1] "Recall"
[1] 1
[1] "-----------------------------------------"
[1] "BASE"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "11"         "577"          
[3,] "No Recuperados" "0"          "12"           
[1] "Fscore"
[1] 0.03672788
[1] "Precision"
[1] 0.01870748
[1] "Recall"
[1] 1
[1] "-----------------------------------------"
[1] "CONT"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "21"         "567"          
[3,] "No Recuperados" "0"          "12"           
[1] "Fscore"
[1] 0.06896552
[1] "Precision"
[1] 0.03571429
[1] "Recall"
[1] 1
[1] "-----------------------------------------"

>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
=======
install.packages('lime', dep = TRUE)
install.packages('maxent', dep = TRUE)
install.packages('tm', dep = TRUE)
install.packages('SnowballC', dep = TRUE)
install.packages('xlsx', dep = TRUE)
install.packages('wordcloud', dep = TRUE)

rm(list=ls(all=TRUE))
library(lime)
library(maxent)
library(tm)
library(SnowballC)
library(xlsx)
library(wordcloud)

data(train_sentences)

data(test_sentences)

train_corpus <- Corpus(VectorSource(train_sentences$text))
test_corpus <- Corpus(VectorSource(test_sentences$text))

for (i in 1:10) print (train_corpus[[i]]$content) 

#Remover Puntuación xx
train_corpus <- tm_map(train_corpus, content_transformer(removePunctuation))
test_corpus <- tm_map(test_corpus, content_transformer(removePunctuation))

#Remover Stop Words xx
train_corpus <- tm_map(train_corpus, content_transformer(removeWords), stopwords("english"))
test_corpus <- tm_map(test_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case xx
train_corpus <- tm_map(train_corpus, content_transformer(tolower))
train_corpus <- tm_map(train_corpus, content_transformer(removeWords), stopwords("english"))
test_corpus <- tm_map(test_corpus, content_transformer(tolower))
test_corpus <- tm_map(test_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming xx
train_corpus <- tm_map(train_corpus, stemDocument)
test_corpus <- tm_map(test_corpus, stemDocument)

#Remover White Space xx
train_corpus <- tm_map(train_corpus, stripWhitespace)
test_corpus <- tm_map(test_corpus, stripWhitespace)

#Remover Números xx
train_corpus <- tm_map(train_corpus, content_transformer(removeNumbers))
test_corpus <- tm_map(test_corpus, content_transformer(removeNumbers))

train_matrix <- DocumentTermMatrix(train_corpus)
test_matrix <- DocumentTermMatrix(test_corpus)

train_sparse <- as.compressed.matrix(train_matrix) 
test_sparse <- as.compressed.matrix(test_matrix) 

f <- tune.maxent(train_sparse,train_sentences$class.text,nfold=3,showall=TRUE, verbose=TRUE) 

print(f)

model<-maxent(train_sparse,train_sentences$class.text, l1_regularizer=0.0,
l2_regularizer=0.8, use_sgd=FALSE, set_heldout=0, verbose=TRUE)

results <- predict(model,test_sparse) 

out = matrix(, nrow = length(results[,1]), ncol = 2)

for(i in 1:length(results[,1])){
	value <- results[i,2]
	mejorJ <- 2
	for(j in 3:16){
		if(value > results[i,j]){
			#results[i,j] <- FALSE
		}else{
			value <- results[i,j]
			mejorJ <- j
			#results[i,2] <- FALSE
		}
	}
	
	out[i, 1] <- results[i,1]
	if(results[i,1] == substr(names(results[i,mejorJ]), 1, 4)){
		out[i, 2] <- 1
	}else{
		out[i, 2] <- 0
	}
	#results[i,mejorJ] <- TRUE
}

out_charts_correcto = matrix(unique(substr(names(results[1,])[2:16], 1, 4)),, nrow = length(unique(substr(names(results[1,])[2:16], 1, 4))), ncol = 2)
out_charts_noCorrecto = matrix(unique(substr(names(results[1,])[2:16], 1, 4)),, nrow = length(unique(substr(names(results[1,])[2:16], 1, 4))), ncol = 2)

out_charts_correcto[,2] <- 0
out_charts_noCorrecto[,2] <- 0

cantDatosCorrectos = 0
cantDatosIncorrectos = 0
for(i in 1:length(out_charts_correcto[,1])){
	cantUnos = 0
	cantCeros = 0
	for(j in 1:length(results[,1])){
		if(substr(out_charts_correcto[i,1], 1, 4) == substr(out[j,1], 1, 4) && out[j,2] == 1){
			cantUnos = cantUnos + 1
			out_charts_correcto[i, 2] <- cantUnos
		}else if(substr(out_charts_correcto[i,1], 1, 4) == substr(out[j,1], 1, 4) && out[j,2] == 0){
			cantCeros = cantCeros + 1
			out_charts_noCorrecto[i, 2] <- cantCeros
		}	
	}
	cantDatosCorrectos <- cantDatosCorrectos + as.numeric(out_charts_correcto[i, 2])
	cantDatosIncorrectos <- cantDatosIncorrectos + as.numeric(out_charts_noCorrecto[i, 2])
}

out_charts_noCorrecto[1:5,]
out_charts_correcto[1:5,]

##grafico de distribucion de etiquetas - TRAIN
distribucion_etiquetas_train <- matrix(names(results[1,])[2:6],, nrow = length(names(results[1,])[2:6]), ncol = 2)
distribucion_etiquetas_train[,2] <- 0
for(i in 1:length(distribucion_etiquetas_train[,1])){
	for(j in 1:length(train_sentences$class.text)){
		if(distribucion_etiquetas_train[i,1] == substr(train_sentences$class.text[j], 1, 4)){
			distribucion_etiquetas_train[i,2] = as.numeric(distribucion_etiquetas_train[i,2]) + 1
		}
	}
}
barplot(as.numeric(distribucion_etiquetas_train[1:5,2]), main=paste("Distribución Train", toString(length(train_sentences$class.text)), sep=" - "), 
  	xlab="Etiquetas",names.arg = distribucion_etiquetas_train[1:5,1])


##grafico de distribucion de etiquetas - TEST
distribucion_etiquetas_test = matrix(names(results[1,])[2:6],, nrow = length(names(results[1,])[2:6]), ncol = 2)
distribucion_etiquetas_test[,2] <- 0
for(i in 1:length(results[1:5,1])){
	distribucion_etiquetas_test[i,2] <- as.numeric(out_charts_correcto[i,2]) + as.numeric(out_charts_noCorrecto[i,2])
	
}
barplot(as.numeric(distribucion_etiquetas_test[1:5,2]), main=paste("Distribución Test", toString(length(test_sentences$class.text)), sep=" - "), 
  	xlab="Etiquetas",names.arg = distribucion_etiquetas_test[1:5,1])
	
	
##grafico de frecuencia de palabras por etiqueta
etiquetas <- c("OWNX", "MISC", "CONT", "AIMX", "BASE")

n_Text = 1
#_--------------OWNX
m_OWNX = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[1]){
		m_OWNX[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
OWNX_corpus <- Corpus(VectorSource(m_OWNX))
#Remover Puntuación
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(removePunctuation))

#Remover Stop Words
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(tolower))
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
OWNX_corpus <- tm_map(OWNX_corpus, stemDocument)

#Remover White Space
OWNX_corpus <- tm_map(OWNX_corpus, stripWhitespace)

#Remover Números
OWNX_corpus <- tm_map(OWNX_corpus, content_transformer(removeNumbers))

tdm_OWNX <- TermDocumentMatrix(OWNX_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_OWNX)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
		  
		  
n_Text = 1  
#_--------------MISC
m_MISC = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[2]){
		m_MISC[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
MISC_corpus <- Corpus(VectorSource(m_MISC))

#Remover Puntuación
MISC_corpus <- tm_map(MISC_corpus, content_transformer(removePunctuation))

#Remover Stop Words
MISC_corpus <- tm_map(MISC_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
MISC_corpus <- tm_map(MISC_corpus, content_transformer(tolower))
MISC_corpus <- tm_map(MISC_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
MISC_corpus <- tm_map(MISC_corpus, stemDocument)

#Remover White Space
MISC_corpus <- tm_map(MISC_corpus, stripWhitespace)

#Remover Números
MISC_corpus <- tm_map(MISC_corpus, content_transformer(removeNumbers))

tdm_MISC <- TermDocumentMatrix(MISC_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_MISC)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
		  
		  
n_Text = 1  
#_--------------CONT
m_CONT = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[3]){
		m_CONT[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
CONT_corpus <- Corpus(VectorSource(m_CONT))

#Remover Puntuación
CONT_corpus <- tm_map(CONT_corpus, content_transformer(removePunctuation))

#Remover Stop Words
CONT_corpus <- tm_map(CONT_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
CONT_corpus <- tm_map(CONT_corpus, content_transformer(tolower))
CONT_corpus <- tm_map(CONT_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
CONT_corpus <- tm_map(CONT_corpus, stemDocument)

#Remover White Space
CONT_corpus <- tm_map(CONT_corpus, stripWhitespace)

#Remover Números
CONT_corpus <- tm_map(CONT_corpus, content_transformer(removeNumbers))

tdm_CONT <- TermDocumentMatrix(CONT_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_CONT)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
	
	
n_Text = 1  
#_--------------AIMX
m_AIMX = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[4]){
		m_AIMX[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
AIMX_corpus <- Corpus(VectorSource(m_AIMX))

#Remover Puntuación
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(removePunctuation))

#Remover Stop Words
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(tolower))
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
AIMX_corpus <- tm_map(AIMX_corpus, stemDocument)

#Remover White Space
AIMX_corpus <- tm_map(AIMX_corpus, stripWhitespace)

#Remover Números
AIMX_corpus <- tm_map(AIMX_corpus, content_transformer(removeNumbers))

tdm_AIMX <- TermDocumentMatrix(AIMX_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_AIMX)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
		  
		  
n_Text = 1  
#_--------------BASE
m_BASE = matrix()
for(i in 1:length(test_sentences$class.text)){
	if(substr(test_sentences$class.text[i], 1, 4) == etiquetas[5]){
		m_BASE[n_Text] <- test_sentences$text[i]
		n_Text = n_Text + 1
	}
}
BASE_corpus <- Corpus(VectorSource(m_BASE))

#Remover Puntuación
BASE_corpus <- tm_map(BASE_corpus, content_transformer(removePunctuation))

#Remover Stop Words
BASE_corpus <- tm_map(BASE_corpus, content_transformer(removeWords), stopwords("english"))

#To Lower Case
BASE_corpus <- tm_map(BASE_corpus, content_transformer(tolower))
BASE_corpus <- tm_map(BASE_corpus, content_transformer(removeWords), stopwords("english"))

#Stremming
BASE_corpus <- tm_map(BASE_corpus, stemDocument)

#Remover White Space
BASE_corpus <- tm_map(BASE_corpus, stripWhitespace)

#Remover Números
BASE_corpus <- tm_map(BASE_corpus, content_transformer(removeNumbers))

tdm_BASE <- TermDocumentMatrix(BASE_corpus, control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm_BASE)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(main = "Clasificación", words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)

####################
##################
##grafico de torta Incorectos
pie(as.numeric(out_charts_noCorrecto[1:5,2]), labels = out_charts_noCorrecto[1:5,2], main=paste("Pie Chart Predicción Incorrecta", toString(cantDatosIncorrectos), sep=" - "),col = rainbow(length(out_charts_noCorrecto[1:5,1])))
legend("topright", out_charts_noCorrecto[1:5,1], cex = 0.8,
   fill = rainbow(length(out_charts_noCorrecto[1:5,1])))

##grafio de lineas entre correctos e incorrectos   
plot(as.numeric(out_charts_correcto[1:5,2]),type = "o",col = "blue", xlab = "Etiquetas", ylab = "Cantidad Predicciones", 
   main = "Clasificación")
lines(out_charts_noCorrecto[1:5,2], type = "o", col = "red")
legend("topright", out_charts_correcto[1:5,1], cex = 0.8)

##grafico de barra entre correctos e incorrectos 
set.union <- function(a, b) {
  u <- a
  for (i in 1:length(b)) {
    u <- append(u, b[i])
    
  }
  return(u)
}
Values <- matrix(set.union(as.numeric(out_charts_correcto[1:5,2]), as.numeric(out_charts_noCorrecto[1:5,2])),nrow = 2,ncol = 5,byrow = TRUE)
colors <- c("green","red")
et <- c(paste("Correctas", toString(cantDatosCorrectos), sep=" "),paste("Incorrectos", toString(cantDatosIncorrectos), sep=" "))

barplot(Values, main="Predicción", 
  	ylab="Número de Predicciones",names.arg = out_charts_correcto[1:5,1], 
	xlab = "Etiquetas",col = colors)
  
legend("topright", et, cex = 1.3, fill = colors)
############# MATRIZ DE CONFUCION  


Labels <- c("MISC", "OWNX", "AIMX", "BASE", "CONT");

for(l in 1:length(Labels)){

	m_confucion = matrix(nrow = 3, ncol = 3)
	m_confucion[,] <- 0
	m_confucion[1,1] <- ""
	m_confucion[1,2] <- "Relevantes"
	m_confucion[1,3] <- "No Relevantes"   
	m_confucion[2,1] <- "Recuperados"
	m_confucion[3,1] <- "No Recuperados" 

	#Recuperados
	for(i in 1:length(out_charts_correcto[1:5,1])){
		#Relevante#
		if(out_charts_correcto[i,1] == Labels[l]){
			if(as.numeric(out_charts_correcto[i,2])>0){
				#Verdadero Positivo
				m_confucion[2,2] <- as.numeric(m_confucion[2,2]) + as.numeric(out_charts_correcto[i,2])
			}
		}else{
			#No Relevante#
			if(as.numeric(out_charts_correcto[i,2])>0){
				#Falso Positivo
				m_confucion[2,3] <- as.numeric(m_confucion[2,3]) + as.numeric(out_charts_correcto[i,2])
			}
		}
	}

	#No Recuperados
	for(i in 1:length(out_charts_noCorrecto[1:5,1])){
		#Relevante#
		if(out_charts_noCorrecto[i,1] == Labels[l]){
			if(as.numeric(out_charts_noCorrecto[i,2])>0){
				#Falso Negativo
				m_confucion[3,2] <- as.numeric(m_confucion[3,2]) + as.numeric(out_charts_noCorrecto[i,2])
			}
		}else{
			#No Relevante#
			if(as.numeric(out_charts_noCorrecto[i,2])>0){
				#Verdadero Negativo
				m_confucion[3,3] <- as.numeric(m_confucion[3,3]) + as.numeric(out_charts_noCorrecto[i,2])
			}
		}
	}
	print(Labels[l])
	print(m_confucion)  

	#Precision
	Precision <- as.numeric(m_confucion[2,2])/(as.numeric(m_confucion[2,2]) + as.numeric(m_confucion[2,3]))

	#Recall
	Recall <- as.numeric(m_confucion[2,2])/(as.numeric(m_confucion[2,2]) + as.numeric(m_confucion[3,2]))

	##Fscore
	Fscore = (2*Precision*Recall)/(Precision+Recall)
	print("Fscore")
	print(Fscore)
	print("Precision")
	print(Precision)
	print("Recall")
	print(Recall)
	
	print("-----------------------------------------");
	
}



###########################################################
##Grafico Resultados Fscore
fScore <- c(0.0685155,0.06896552,0.096,0.07830343,0.09888357, 0.09888357, 0.1032258, 0.08064516)
Etiquetas <- c("1- Sin Pre-Pro", "2- Remover Puntuación", "3- Remover Stop Words","4- To Lower Case","5- Stremming","6- Remover White Space","7- Remover Números", "8- Sin To Lower Case")

plot(fScore,type = "o", col = "red", xlab = "Procesamiento", ylab = "Fscore",
   main = "Procesamiento")
legend("bottomright", Etiquetas, cex = 0.8)
   
   
setwd("C:\\Users\\Luis.O.A\\Documents\\USACH\\Mineria de Datos\\Trabajos\\5")
write.xlsx(as.data.frame(results), "Prediccion.xlsx", sheetName="Sheet1")

##Grafico de Lineas Nuevo
 library(plotly)

 MISC <- c(0.7673216, 0.7983952, 0.7549531, 0.7431579, 0.7214514, 0.7241747, 0.7983952)
 OWNX <- c(0.3541076, 0.3239437, 0.4717949, 0.498094, 0.4627451, 0.4728682, 0.3638889)
 AIMX <- c(0.1855346, 0.17737, 0.06168831, 0.05280528, 0.1064516, 0.09983897, 0.08780488)
 BASE <- c(0.003460208, 0.01666667, 0.01666667, 0.01013514, 0.02356902, 0.02013423, 0.03672788)
 CONT <- c(0.07023411, 0.06188925, 0.08064516, 0.06885246, 0.1033926, 0.09677419, 0.06896552)




x <- c(1:7)

data <- data.frame(x, MISC, OWNX, AIMX, BASE, CONT)

p <- plot_ly(data, x = ~x, y = ~MISC, name = 'MISC', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~OWNX, name = 'OWNX', mode = 'lines+markers') %>%
  add_trace(y = ~AIMX, name = 'AIMX', mode = 'lines+markers') %>%
  add_trace(y = ~BASE, name = 'BASE', mode = 'lines+markers') %>%
  add_trace(y = ~CONT, name = 'CONT', mode = 'lines+markers') %>%
  layout(title = "Fscore Segun Pre-Procesamiento",
         xaxis = list(title = "Pre-Procesamiento"),
         yaxis = list (title = "Fscore"))


p


## 7 - Remover Números
[1] "MISC"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "398"        "190"          
[3,] "No Recuperados" "11"         "1"            
[1] "Fscore"
[1] 0.7983952
[1] "Precision"
[1] 0.6768707
[1] "Recall"
[1] 0.9731051
[1] "-----------------------------------------"
[1] "OWNX"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "131"        "457"          
[3,] "No Recuperados" "1"          "11"           
[1] "Fscore"
[1] 0.3638889
[1] "Precision"
[1] 0.2227891
[1] "Recall"
[1] 0.9924242
[1] "-----------------------------------------"
[1] "AIMX"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "27"         "561"          
[3,] "No Recuperados" "0"          "12"           
[1] "Fscore"
[1] 0.08780488
[1] "Precision"
[1] 0.04591837
[1] "Recall"
[1] 1
[1] "-----------------------------------------"
[1] "BASE"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "11"         "577"          
[3,] "No Recuperados" "0"          "12"           
[1] "Fscore"
[1] 0.03672788
[1] "Precision"
[1] 0.01870748
[1] "Recall"
[1] 1
[1] "-----------------------------------------"
[1] "CONT"
     [,1]             [,2]         [,3]           
[1,] ""               "Relevantes" "No Relevantes"
[2,] "Recuperados"    "21"         "567"          
[3,] "No Recuperados" "0"          "12"           
[1] "Fscore"
[1] 0.06896552
[1] "Precision"
[1] 0.03571429
[1] "Recall"
[1] 1
[1] "-----------------------------------------"

>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
   