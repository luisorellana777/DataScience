<<<<<<< HEAD
<<<<<<< HEAD
library(nnet)
#https://www.kaggle.com/vmalyi/run-or-walk/data
setwd("C:/Users/Luis.O.A/Documents/Charla ML/DataScience")

wr <- read.csv('dataset.csv', sep=',')

wr <- wr[,-c(1,2,3)]

normalize <- function(x) {
	x = (x-min(x))/(max(x)-min(x))
	x
}
#hist(wine$fixed.acidity,breaks=10, xlab="wine",col="lightblue", main="")

wr$activity <- factor(wr$activity, levels=sort(unique(wr$activity)))
wr$wrist <- normalize(wr$wrist)
wr$acceleration_x <- normalize(wr$acceleration_x)
wr$acceleration_y <- normalize(wr$acceleration_y)
wr$acceleration_z <- normalize(wr$acceleration_z)
wr$gyro_x <- normalize(wr$gyro_x)
wr$gyro_y <- normalize(wr$gyro_y)
wr$gyro_z <- normalize(wr$gyro_z)

set.seed(100)
trainingRows <- sample(1:nrow(wr), 0.7*nrow(wr))
training <- wr[trainingRows, ]
test <- wr[-trainingRows, -2]
test.activity <- wr[-trainingRows, -c(1,3,4,5,6,7, 8)]#Solo la clase


model <- multinom(activity ~ ., data = training)

##Prueba de clasificacion con las 10 primeras instancias
predicted=predict(model,wr[750:760,], 'probs')

predicted
##Verificar las clases
wr[750:760,]

#############################################

predicted=predict(model,test)

result <- table(predicted, test.activity)

#Correctos
(result[1,1] + result[2,2]) * 100 / (result[1,1] + result[1,2] + result[2,1] + result[2,2])
#86.12334

#Error
mean(as.character(predicted) != as.character(test.activity)) * 100
#13.87666

Precision <- as.numeric(result[1,1])/(as.numeric(result[1,1]) + as.numeric(result[1,2]))

#Recall
Recall <- as.numeric(result[1,1])/(as.numeric(result[1,1]) + as.numeric(result[2,1]))

##Fscore
Fscore = (2*Precision*Recall)/(Precision+Recall)
print("Fscore")
print(Fscore)
print("Precision")
print(Precision)
print("Recall")
print(Recall)
	


=======
library(nnet)
#https://www.kaggle.com/vmalyi/run-or-walk/data
setwd("C:/Users/Luis.O.A/Documents/Charla ML/DataScience")

wr <- read.csv('dataset.csv', sep=',')

wr <- wr[,-c(1,2,3)]

normalize <- function(x) {
	x = (x-min(x))/(max(x)-min(x))
	x
}
#hist(wine$fixed.acidity,breaks=10, xlab="wine",col="lightblue", main="")

wr$activity <- factor(wr$activity, levels=sort(unique(wr$activity)))
wr$wrist <- normalize(wr$wrist)
wr$acceleration_x <- normalize(wr$acceleration_x)
wr$acceleration_y <- normalize(wr$acceleration_y)
wr$acceleration_z <- normalize(wr$acceleration_z)
wr$gyro_x <- normalize(wr$gyro_x)
wr$gyro_y <- normalize(wr$gyro_y)
wr$gyro_z <- normalize(wr$gyro_z)

set.seed(100)
trainingRows <- sample(1:nrow(wr), 0.7*nrow(wr))
training <- wr[trainingRows, ]
test <- wr[-trainingRows, -2]
test.activity <- wr[-trainingRows, -c(1,3,4,5,6,7, 8)]#Solo la clase


model <- multinom(activity ~ ., data = training)

##Prueba de clasificacion con las 10 primeras instancias
predicted=predict(model,wr[750:760,], 'probs')

predicted
##Verificar las clases
wr[750:760,]

#############################################

predicted=predict(model,test)

result <- table(predicted, test.activity)

#Correctos
(result[1,1] + result[2,2]) * 100 / (result[1,1] + result[1,2] + result[2,1] + result[2,2])
#86.12334

#Error
mean(as.character(predicted) != as.character(test.activity)) * 100
#13.87666

Precision <- as.numeric(result[1,1])/(as.numeric(result[1,1]) + as.numeric(result[1,2]))

#Recall
Recall <- as.numeric(result[1,1])/(as.numeric(result[1,1]) + as.numeric(result[2,1]))

##Fscore
Fscore = (2*Precision*Recall)/(Precision+Recall)
print("Fscore")
print(Fscore)
print("Precision")
print(Precision)
print("Recall")
print(Recall)
	


>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
=======
library(nnet)
#https://www.kaggle.com/vmalyi/run-or-walk/data
setwd("C:/Users/Luis.O.A/Documents/Charla ML/DataScience")

wr <- read.csv('dataset.csv', sep=',')

wr <- wr[,-c(1,2,3)]

normalize <- function(x) {
	x = (x-min(x))/(max(x)-min(x))
	x
}
#hist(wine$fixed.acidity,breaks=10, xlab="wine",col="lightblue", main="")

wr$activity <- factor(wr$activity, levels=sort(unique(wr$activity)))
wr$wrist <- normalize(wr$wrist)
wr$acceleration_x <- normalize(wr$acceleration_x)
wr$acceleration_y <- normalize(wr$acceleration_y)
wr$acceleration_z <- normalize(wr$acceleration_z)
wr$gyro_x <- normalize(wr$gyro_x)
wr$gyro_y <- normalize(wr$gyro_y)
wr$gyro_z <- normalize(wr$gyro_z)

set.seed(100)
trainingRows <- sample(1:nrow(wr), 0.7*nrow(wr))
training <- wr[trainingRows, ]
test <- wr[-trainingRows, -2]
test.activity <- wr[-trainingRows, -c(1,3,4,5,6,7, 8)]#Solo la clase


model <- multinom(activity ~ ., data = training)

##Prueba de clasificacion con las 10 primeras instancias
predicted=predict(model,wr[750:760,], 'probs')

predicted
##Verificar las clases
wr[750:760,]

#############################################

predicted=predict(model,test)

result <- table(predicted, test.activity)

#Correctos
(result[1,1] + result[2,2]) * 100 / (result[1,1] + result[1,2] + result[2,1] + result[2,2])
#86.12334

#Error
mean(as.character(predicted) != as.character(test.activity)) * 100
#13.87666

Precision <- as.numeric(result[1,1])/(as.numeric(result[1,1]) + as.numeric(result[1,2]))

#Recall
Recall <- as.numeric(result[1,1])/(as.numeric(result[1,1]) + as.numeric(result[2,1]))

##Fscore
Fscore = (2*Precision*Recall)/(Precision+Recall)
print("Fscore")
print(Fscore)
print("Precision")
print(Precision)
print("Recall")
print(Recall)
	


>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
