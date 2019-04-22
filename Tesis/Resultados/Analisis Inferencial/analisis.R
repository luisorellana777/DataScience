setwd("C:/Users/Luis/Documents/DataScience/Tesis/Calculo ARI/v15.9.10/stats")

data <- read.csv('maniobras-stats-v15.9.10.csv', sep=';')


left <- data[data$Hemisferio=="Left",]$mfARI
right <- data[data$Hemisferio=="Right",]$mfARI

#Kolmogorov-Smirnov ya que son 54 datos pero tiene valores repetidos
#ks.test(left, "pnorm")#no normal
#ks.test(right, "pnorm")#no normal
#no normal
#wilcox.test(left, right, alternative = "two.sided")#Los hemisferios son similares

shapiro.test(left)#normal
shapiro.test(right)#normal

hist(left)
hist(right)

#normal
t.test(left, right, alternative = "two.sided") #Los hemisferios son similares por quep-value > 0.05

##############################################
#mean mfARI
mfARI <- rowMeans(data.frame(left, right))
dat = data.frame("Maniobra" = data[data$Hemisferio=="Left",]$Maniobra, "mfARI" = mfARI)

fm = aov( dat$mfARI ~ dat$Maniobra, data = dat )

summary(fm)

tuk <- TukeyHSD(fm)

plot (tuk)

boxplot(dat$mfARI ~ dat$Maniobra, 
        main="Comparacion de Posiciones mfARI", 
        col= rainbow(3), 
        horizontal = TRUE)

#Conclusion
##La mayor diferencia de posiciones van en este orde:
### PIE-ACOSTADO = 0.7775675 
### SENTADO-ACOSTADO = 0.8830272
### SENTADO-PIE = 0.9778065

###############################mejor esto que el calculo anterior del post hoc -> es mejor pairwise
tyres.aov<- aov(mfARI~Maniobra, dat)
class(tyres.aov)

summary(tyres.aov)

pairwise.t.test(dat$mfARI,dat$Maniobra,p.adjust.method = "none")

##
model = lm(mfARI ~ Maniobra, 
           data=dat)

Anova(model, type="III")

summary(model)

boxplot(mfARI ~ Maniobra,
        data = dat,
        ylab="aam / height",
        xlab="Location")

