<<<<<<< HEAD
#ARBOL BINARIO
#Carga dataset ZOO
data_zoo  =  read.csv("zoo-tree1.csv") 
#Discretiza datos
data_zoo["animal_name"] <- NULL
data_zoo <- data.frame(sapply(data_zoo,as.factor)) 
# Clasifica los modelos de árbol usando el algoritmo de Quinlan C5.0 
c50_result<-C5.0(class_type~.,data=data_zoo) 
# Despliega resumen detallado para los modelos C.50 
summary(c50_result) 
# calcula y despliega la importancia de la variable para el modelo C5.0
 importanceC5imp(c50_result,metric='usage')

#REGLAS.

# Generar reglas a partir del arcol de decisión de los modelos C.50 
ruleModel <- C5.0(type ~ ., data = data_zoo, rules = TRUE)
ruleModel
# Despliega resumen detallado para las reglas de los modelos C.50
summary(ruleModel)

#VALIDACION CRUZADA.
library(arulesViz)
install.packages('partykit', dep = TRUE)
install.packages('gmodels', dep = TRUE)
library(partykit)
library(C50)
library(gmodels)
 library(plyr)
data_zoo  =  read.csv("zoo-tree1.csv") #Carga dataset
data_zoo["animal_name"] <- NULL
data_zoo <- data.frame(sapply(data_zoo,as.factor)) #Discretiza datos

 form <- "class_type  ~ hair+feathers+eggs+milk+airborne+aquatic+predator+toothed+backbone+breathes+venomous+fins+legs0+legs2+legs4+legs5+legs6+legs8+tail+domestic+catsize" 

folds <- split(data_zoo , cut(sample(1:nrow(data_zoo)),10)) 
length(folds)
folds
errs.c50 <- rep(NA, length(folds))
for (i in 1:length(folds)) { 
test <- ldply(folds[i], data.frame) 
train <- ldply(folds[-i], data.frame) 
tmp.model <- C5.0(as.formula(form), train) 
tmp.predict <- predict(tmp.model, newdata=test) 
conf.mat <- table(test$class_type , tmp.predict) 
errs.c50[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
 }
print(sprintf("El promedio de error usando la validación cruzada k-fold y el algoritmo de arbol de decision  C5.0 es: %.3f percent", 100*mean(errs.c50))) 
=======
#ARBOL BINARIO
#Carga dataset ZOO
data_zoo  =  read.csv("zoo-tree1.csv") 
#Discretiza datos
data_zoo["animal_name"] <- NULL
data_zoo <- data.frame(sapply(data_zoo,as.factor)) 
# Clasifica los modelos de árbol usando el algoritmo de Quinlan C5.0 
c50_result<-C5.0(class_type~.,data=data_zoo) 
# Despliega resumen detallado para los modelos C.50 
summary(c50_result) 
# calcula y despliega la importancia de la variable para el modelo C5.0
 importanceC5imp(c50_result,metric='usage')

#REGLAS.

# Generar reglas a partir del arcol de decisión de los modelos C.50 
ruleModel <- C5.0(type ~ ., data = data_zoo, rules = TRUE)
ruleModel
# Despliega resumen detallado para las reglas de los modelos C.50
summary(ruleModel)

#VALIDACION CRUZADA.
library(arulesViz)
install.packages('partykit', dep = TRUE)
install.packages('gmodels', dep = TRUE)
library(partykit)
library(C50)
library(gmodels)
 library(plyr)
data_zoo  =  read.csv("zoo-tree1.csv") #Carga dataset
data_zoo["animal_name"] <- NULL
data_zoo <- data.frame(sapply(data_zoo,as.factor)) #Discretiza datos

 form <- "class_type  ~ hair+feathers+eggs+milk+airborne+aquatic+predator+toothed+backbone+breathes+venomous+fins+legs0+legs2+legs4+legs5+legs6+legs8+tail+domestic+catsize" 

folds <- split(data_zoo , cut(sample(1:nrow(data_zoo)),10)) 
length(folds)
folds
errs.c50 <- rep(NA, length(folds))
for (i in 1:length(folds)) { 
test <- ldply(folds[i], data.frame) 
train <- ldply(folds[-i], data.frame) 
tmp.model <- C5.0(as.formula(form), train) 
tmp.predict <- predict(tmp.model, newdata=test) 
conf.mat <- table(test$class_type , tmp.predict) 
errs.c50[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
 }
print(sprintf("El promedio de error usando la validación cruzada k-fold y el algoritmo de arbol de decision  C5.0 es: %.3f percent", 100*mean(errs.c50))) 
>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
