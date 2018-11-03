
install.packages('randomForest', dep = TRUE)
library(randomForest)

install.packages('party', dep = TRUE)
library(party)


setwd("C:/Users/Luis.O.A/Documents/USACH/Mineria de Datos/grupo 6 Wine Quality")

wine <- read.csv('winequality-red.csv', sep=';')

##Mostrar variable con mas outliers
source("http://goo.gl/UUyEzD")
outlierKD(wine, wine$residual.sugar)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- mean(x)#NA
  y[x > (qnt[2] + H)] <- mean(x)#NA
  y
}

wine$alcohol <- remove_outliers(wine$alcohol)
wine$sulphates <- remove_outliers(wine$sulphates)
wine$pH <- remove_outliers(wine$pH)
wine$free.sulfur.dioxid <- remove_outliers(wine$free.sulfur.dioxid)
wine$chlorides <- remove_outliers(wine$chlorides)
wine$residual.sugar <- remove_outliers(wine$residual.sugar)
wine$citric.acid <- remove_outliers(wine$citric.acid)
wine$volatile.acidity <- remove_outliers(wine$volatile.acidity)
wine$fixed.acidity <- remove_outliers(wine$fixed.acidity)


#Normalizacion
dataNorm <- as.data.frame( scale(wine[1:12] ))

#Etiquetar instancias
dataNorm$taste <- ifelse(dataNorm$quality <= -2.02586, 'malo', 'regular')
dataNorm$taste[dataNorm$quality >= 1.68899] <- 'bueno'
dataNorm$taste <- as.factor(dataNorm$taste)

barplot(table(dataNorm$taste))
############ Ramdom Forest ############

set.seed(123)
samp <- sample(nrow(dataNorm), 0.75 * nrow(dataNorm))
train <- dataNorm[samp, ]
test <- dataNorm[-samp, ]

model <- randomForest(taste ~ . - quality, data = train, importance=TRUE, proximity=TRUE, ntree=1000)

model
####Con outliers: OOB estimate of  error rate: 10.93%
####Sin outliers: OOB estimate of  error rate: 11.01%

pred <- predict(model, newdata = test)
table(pred, test$taste)

#################
##Sin Outliers
#Accuracy 
(29 + 0 + 344) / nrow(test)
#0.9109375 = 91%
#P = 29/(29+19) = 0.604
#R = 29/(29+5) = 0.853
#FScore
(2*0.604*0.853)/(0.604 + 0.853)
# 0.7072231
#################
##Con Outliers
#Accuracy 
(29 + 0 + 342) / nrow(test)

#P = 29/(29+19) = 0.604
#R = 29/(29+7) = 0.8055556
#FScore
(2*0.604*0.8055556)/(0.604 + 0.8055556)
#0.6903674
#################
#Configuracion optima
model <- randomForest(taste ~ . - quality, data = train, importance=TRUE, proximity=TRUE, ntree=1000, mtry=8)

# Grafica de error vs arboles
plot(model)
legend("bottomright", colnames(model$err.rate),col=1:4,cex=0.8,fill=1:4)

##############

#Ver calidad de las variables
round(importance(model), 2)

varImpPlot(model)



mds <- cmdscale(1 - model$proximity, eig=TRUE) 


op <- par(pty="s")

pairs(cbind(train[1:11], mds$points), cex=0.6, gap=0,
col=c("red", "green", "blue")[as.numeric(test$taste)],
main="Wine Data: Predictors and MDS of Proximity Based on RandomForest")

par(op)

print(mds$GOF)
MDSplot(model, test$taste)
legend("bottomright", colnames(model$classes),col=1:3,cex=0.8,fill=1:3, legend=model$classes)