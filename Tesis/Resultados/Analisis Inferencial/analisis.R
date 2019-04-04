setwd("C:/Users/Luis/Documents/DataScience/Tesis/Calculo ARI/v15.9.10/stats")

data <- read.csv('maniobras-stats-v15.9.10.csv', sep=';')


left <- data[data$Hemisferio=="Left",]$mfARI
right <- data[data$Hemisferio=="Right",]$mfARI

#Kolmogorov-Smirnov no funciona porque hay datos repetidos (modelo matematica no lo permite)
#ks.test(left, "pnorm")
#ks.test(right, "pnorm")
#no normal
#wilcox.test(left, right, alternative = "two.sided")

shapiro.test(left)#normal
shapiro.test(right)#normal

hist(left)
hist(right)

#normal
t.test(left, right, alternative = "two.sided") #Los hemisferios son similares

##############################################
#mean mfARI
mfARI <- rowMeans(data.frame(left, right))
dat = data.frame("Maniobra" = data[data$Hemisferio=="Left",]$Maniobra, "mfARI" = mfARI)

fm = aov( dat$mfARI ~ dat$Maniobra, data = dat )

summary(fm)

tuk <- TukeyHSD(fm)

plot (tuk)

#Conclusion
##La mayor diferencia de posiciones van en este orde:
### PIE-ACOSTADO = 0.7775675 
### SENTADO-ACOSTADO = 0.8830272
### SENTADO-PIE = 0.9778065