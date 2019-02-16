<<<<<<< HEAD
<<<<<<< HEAD
#Ruta del Dataset
RUTA_DATASET <- "C:/Users/Luis.O.A/Documents/Analisis Cervezas"
setwd(RUTA_DATASET)

#Leer Dataset
data=read.csv("beer_reviews.csv")

#Eliminar datos NA
data= na.omit(data)

#Resumen medidas de tendencia 
summary(data)

#Desviacion estandar para observar desviacion con respecto a la media
sd(data$review_appearance)
sd(data$review_aroma)
sd(data$review_palate)
sd(data$review_taste)


#¿Qué cervecería produce la cerveza más fuerte según ABV? 
unique(data$brewery_name[data$beer_abv == max(data$beer_abv)])

#¿Si tuviera que elegir 3 cervezas para recomendar usando sólo estos datos, cuáles elegiría?
#El siguente sumatoria de los campos "review" es equivalente al campo "review_overall"
new_data <- data.frame(nom = data$beer_name, suma = data$review_aroma + data$review_appearance + data$review_palate + data$review_taste)
grupo <- aggregate(new_data$suma, by=list(nom=new_data$nom), FUN=sum)

head(grupo[order(grupo$x, decreasing = TRUE),], 3)

#¿Cual de los factores (aroma, taste, aperance, palete) es ams importante para 
#determinar la calidad general de una cerveza?

hist(data$review_aroma, xlab="review_aroma", main="review_aroma")
hist(data$review_taste, xlab="review_taste", main="review_taste")
hist(data$review_appearance, xlab="review_appearance", main="review_appearance")
hist(data$review_palate, xlab="review_palate", main="review_palate")

hist(data$review_overall, xlab="review_overall", main="review_overall")

d <- data.frame(data$review_aroma, 
data$review_taste, 
data$review_appearance, 
data$review_palate)

cor(d, data$review_overall, method = "pearson")

#Si yo tipicamente disfruto una cerveza debido a su 
#aroma y apariencia, ¿que estilo de cerveza deberia probar?
new_ar_ap <- data.frame(nom = data$beer_style, suma = data$review_aroma + data$review_appearance)
    
grupo <- aggregate(new_ar_ap$suma, by=list(nom=new_ar_ap$nom), FUN=sum)
head(grupo[order(grupo$x, decreasing = TRUE),], 1)
=======
#Ruta del Dataset
RUTA_DATASET <- "C:/Users/Luis.O.A/Documents/Analisis Cervezas"
setwd(RUTA_DATASET)

#Leer Dataset
data=read.csv("beer_reviews.csv")

#Eliminar datos NA
data= na.omit(data)

#Resumen medidas de tendencia 
summary(data)

#Desviacion estandar para observar desviacion con respecto a la media
sd(data$review_appearance)
sd(data$review_aroma)
sd(data$review_palate)
sd(data$review_taste)


#¿Qué cervecería produce la cerveza más fuerte según ABV? 
unique(data$brewery_name[data$beer_abv == max(data$beer_abv)])

#¿Si tuviera que elegir 3 cervezas para recomendar usando sólo estos datos, cuáles elegiría?
#El siguente sumatoria de los campos "review" es equivalente al campo "review_overall"
new_data <- data.frame(nom = data$beer_name, suma = data$review_aroma + data$review_appearance + data$review_palate + data$review_taste)
grupo <- aggregate(new_data$suma, by=list(nom=new_data$nom), FUN=sum)

head(grupo[order(grupo$x, decreasing = TRUE),], 3)

#¿Cual de los factores (aroma, taste, aperance, palete) es ams importante para 
#determinar la calidad general de una cerveza?

hist(data$review_aroma, xlab="review_aroma", main="review_aroma")
hist(data$review_taste, xlab="review_taste", main="review_taste")
hist(data$review_appearance, xlab="review_appearance", main="review_appearance")
hist(data$review_palate, xlab="review_palate", main="review_palate")

hist(data$review_overall, xlab="review_overall", main="review_overall")

d <- data.frame(data$review_aroma, 
data$review_taste, 
data$review_appearance, 
data$review_palate)

cor(d, data$review_overall, method = "pearson")

#Si yo tipicamente disfruto una cerveza debido a su 
#aroma y apariencia, ¿que estilo de cerveza deberia probar?
new_ar_ap <- data.frame(nom = data$beer_style, suma = data$review_aroma + data$review_appearance)
    
grupo <- aggregate(new_ar_ap$suma, by=list(nom=new_ar_ap$nom), FUN=sum)
head(grupo[order(grupo$x, decreasing = TRUE),], 1)
>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
=======
#Ruta del Dataset
RUTA_DATASET <- "C:/Users/Luis.O.A/Documents/Analisis Cervezas"
setwd(RUTA_DATASET)

#Leer Dataset
data=read.csv("beer_reviews.csv")

#Eliminar datos NA
data= na.omit(data)

#Resumen medidas de tendencia 
summary(data)

#Desviacion estandar para observar desviacion con respecto a la media
sd(data$review_appearance)
sd(data$review_aroma)
sd(data$review_palate)
sd(data$review_taste)


#¿Qué cervecería produce la cerveza más fuerte según ABV? 
unique(data$brewery_name[data$beer_abv == max(data$beer_abv)])

#¿Si tuviera que elegir 3 cervezas para recomendar usando sólo estos datos, cuáles elegiría?
#El siguente sumatoria de los campos "review" es equivalente al campo "review_overall"
new_data <- data.frame(nom = data$beer_name, suma = data$review_aroma + data$review_appearance + data$review_palate + data$review_taste)
grupo <- aggregate(new_data$suma, by=list(nom=new_data$nom), FUN=sum)

head(grupo[order(grupo$x, decreasing = TRUE),], 3)

#¿Cual de los factores (aroma, taste, aperance, palete) es ams importante para 
#determinar la calidad general de una cerveza?

hist(data$review_aroma, xlab="review_aroma", main="review_aroma")
hist(data$review_taste, xlab="review_taste", main="review_taste")
hist(data$review_appearance, xlab="review_appearance", main="review_appearance")
hist(data$review_palate, xlab="review_palate", main="review_palate")

hist(data$review_overall, xlab="review_overall", main="review_overall")

d <- data.frame(data$review_aroma, 
data$review_taste, 
data$review_appearance, 
data$review_palate)

cor(d, data$review_overall, method = "pearson")

#Si yo tipicamente disfruto una cerveza debido a su 
#aroma y apariencia, ¿que estilo de cerveza deberia probar?
new_ar_ap <- data.frame(nom = data$beer_style, suma = data$review_aroma + data$review_appearance)
    
grupo <- aggregate(new_ar_ap$suma, by=list(nom=new_ar_ap$nom), FUN=sum)
head(grupo[order(grupo$x, decreasing = TRUE),], 1)
>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
  