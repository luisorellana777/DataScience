<<<<<<< HEAD
<<<<<<< HEAD
library(bnlearn)
library(plotly)
packageVersion('plotly')
source('http://bioconductor.org/biocLite.R')
biocLite('Rgraphviz')
library(xlsx)

data.alarm <- data.frame(alarm)
# create and plot the network structure.
modelstring = paste("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF]",
  "[LVF][STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA]",
  "[HRSA|ERCA:HR][ANES][APL][TPR|APL][ECO2|ACO2:VLNG][KINK]",
  "[MINV|INT:VLNG][FIO2][PVS|FIO2:VALV][SAO2|PVS:SHNT][PAP|PMB][PMB]",
  "[SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC][MVS][VMCH|MVS]",
  "[VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG][ACO2|VALV]",
  "[CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]", sep = "")
dag = model2network(modelstring)


#se comparan los 3 metodos
set.seed(6)
res = hc(data.alarm)##MEJOR MODELO
unlist(compare(dag, res))
sc<-score(res,data.alarm)
print(sc)
#tp fp fn 
#22 31 24 
#BIC: -220761.7

set.seed(6)
res = mmhc(data.alarm)
unlist(compare(dag, res))
sc<-score(res,data.alarm)
print(sc)
#tp fp fn 
#16 13 30 
#BIC: -311189.8

set.seed(6)
res = mmpc(data.alarm)
unlist(compare(dag, res))
sc<-score(res,data.alarm)
print(sc)
#tp fp fn 
#0 29 46
#the graph is only partially directed

tp <- c()
Restart <- c()
Seed <- c()

maximoTp = 0
bestRest = 0;
bestSeed = 0;
for(seed in 1:500){
  for(i in 1:100){
	if((seed%%10) == 0 & (i%%10) == 0){
		set.seed(seed)
		res = hc(data.alarm,restart = i)
		tph = unlist(compare(dag, res))["tp"]
		
		tp <- c(tp, tph)
		Restart <- c(Restart, i)
		Seed <- c(Seed, seed)

		if(maximoTp < tph){
		  maximoTp = tph
		  bestRest = i
		  bestSeed = seed
		}
	}
  }
}


##Grafico Curva Arendizaje

datos <- data.frame(Restart, Seed, tp, co = as.numeric(c(1:length(Restart))))
setwd("C:\\Users\\Luis.O.A\\Documents\\USACH\\Mineria de Datos\\Trabajos\\4")
write.xlsx(datos, "Ajuste_TP.xlsx", sheetName="Sheet1")

p <- plot_ly(datos, x = ~Restart, y = ~Seed, z = ~tp, type = 'scatter3d', mode = 'lines',
        line = list(color = '#1f77b4', width = 1))
		
p
		


##MEJOR MODELO
set.seed(bestSeed)#Seed=40
res = hc(data.alarm,restart = bestRest)#Restart=90
unlist(compare(dag, res))
sc<-score(res,data.alarm)
print(sc)
#tp fp fn 
#34 17 12 
#BIC: -218690.1

##Grafico de la mejor red
graphviz.plot(res)

###################################################################33CONSULTAS ORACULO
fittedbn <- bn.fit(res, data = data.alarm)

#¿Qué tipo de entubación se le suministra a pacientes con anafilaxis? (PPT 15)
set.seed(6)
cpquery(fittedbn, event = (INT=="NORMAL"), evidence = (APL=="TRUE") )
#0.9194631

#¿Cuál es la probabilidad de que el paciente padezca de embolia pulmonar (PMB) si la catecolamina es alta(CCHL) y el ritmo cardiaco es alto (HR)?
#PPT 16
set.seed(6)
cpquery(fittedbn, event = (HR=="HIGH" & CCHL == "HIGH"), evidence = (PMB=="TRUE") )
#0.6923077

#¿Cuál es la probabilidad de que el paciente padezca de embolia pulmonar (PMB) si la catecolamina es alta(CCHL), si el ritmo cardiaco es alto (HR) y el CO es alto?
#PPT 17
set.seed(6)
cpquery(fittedbn, event = (HR=="HIGH" & CCHL == "HIGH" & CO == "HIGH"), evidence = (PMB=="TRUE") )
#0.5605096

#¿Cuál es la probabilidad de que un paciente con ritmo cardiaco alto, catecolamina alta e irrigación sanguínea pulmonar baja padezca de anafilaxis?
#PPT 18
set.seed(6)
cpquery(fittedbn, event = (HR =="HIGH" & CCHL =="HIGH" & TPR == "LOW"), evidence = (APL=="TRUE") )
#0.8636364




=======
library(bnlearn)
library(plotly)
packageVersion('plotly')
source('http://bioconductor.org/biocLite.R')
biocLite('Rgraphviz')
library(xlsx)

data.alarm <- data.frame(alarm)
# create and plot the network structure.
modelstring = paste("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF]",
  "[LVF][STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA]",
  "[HRSA|ERCA:HR][ANES][APL][TPR|APL][ECO2|ACO2:VLNG][KINK]",
  "[MINV|INT:VLNG][FIO2][PVS|FIO2:VALV][SAO2|PVS:SHNT][PAP|PMB][PMB]",
  "[SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC][MVS][VMCH|MVS]",
  "[VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG][ACO2|VALV]",
  "[CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]", sep = "")
dag = model2network(modelstring)


#se comparan los 3 metodos
set.seed(6)
res = hc(data.alarm)##MEJOR MODELO
unlist(compare(dag, res))
sc<-score(res,data.alarm)
print(sc)
#tp fp fn 
#22 31 24 
#BIC: -220761.7

set.seed(6)
res = mmhc(data.alarm)
unlist(compare(dag, res))
sc<-score(res,data.alarm)
print(sc)
#tp fp fn 
#16 13 30 
#BIC: -311189.8

set.seed(6)
res = mmpc(data.alarm)
unlist(compare(dag, res))
sc<-score(res,data.alarm)
print(sc)
#tp fp fn 
#0 29 46
#the graph is only partially directed

tp <- c()
Restart <- c()
Seed <- c()

maximoTp = 0
bestRest = 0;
bestSeed = 0;
for(seed in 1:500){
  for(i in 1:100){
	if((seed%%10) == 0 & (i%%10) == 0){
		set.seed(seed)
		res = hc(data.alarm,restart = i)
		tph = unlist(compare(dag, res))["tp"]
		
		tp <- c(tp, tph)
		Restart <- c(Restart, i)
		Seed <- c(Seed, seed)

		if(maximoTp < tph){
		  maximoTp = tph
		  bestRest = i
		  bestSeed = seed
		}
	}
  }
}


##Grafico Curva Arendizaje

datos <- data.frame(Restart, Seed, tp, co = as.numeric(c(1:length(Restart))))
setwd("C:\\Users\\Luis.O.A\\Documents\\USACH\\Mineria de Datos\\Trabajos\\4")
write.xlsx(datos, "Ajuste_TP.xlsx", sheetName="Sheet1")

p <- plot_ly(datos, x = ~Restart, y = ~Seed, z = ~tp, type = 'scatter3d', mode = 'lines',
        line = list(color = '#1f77b4', width = 1))
		
p
		


##MEJOR MODELO
set.seed(bestSeed)#Seed=40
res = hc(data.alarm,restart = bestRest)#Restart=90
unlist(compare(dag, res))
sc<-score(res,data.alarm)
print(sc)
#tp fp fn 
#34 17 12 
#BIC: -218690.1

##Grafico de la mejor red
graphviz.plot(res)

###################################################################33CONSULTAS ORACULO
fittedbn <- bn.fit(res, data = data.alarm)

#¿Qué tipo de entubación se le suministra a pacientes con anafilaxis? (PPT 15)
set.seed(6)
cpquery(fittedbn, event = (INT=="NORMAL"), evidence = (APL=="TRUE") )
#0.9194631

#¿Cuál es la probabilidad de que el paciente padezca de embolia pulmonar (PMB) si la catecolamina es alta(CCHL) y el ritmo cardiaco es alto (HR)?
#PPT 16
set.seed(6)
cpquery(fittedbn, event = (HR=="HIGH" & CCHL == "HIGH"), evidence = (PMB=="TRUE") )
#0.6923077

#¿Cuál es la probabilidad de que el paciente padezca de embolia pulmonar (PMB) si la catecolamina es alta(CCHL), si el ritmo cardiaco es alto (HR) y el CO es alto?
#PPT 17
set.seed(6)
cpquery(fittedbn, event = (HR=="HIGH" & CCHL == "HIGH" & CO == "HIGH"), evidence = (PMB=="TRUE") )
#0.5605096

#¿Cuál es la probabilidad de que un paciente con ritmo cardiaco alto, catecolamina alta e irrigación sanguínea pulmonar baja padezca de anafilaxis?
#PPT 18
set.seed(6)
cpquery(fittedbn, event = (HR =="HIGH" & CCHL =="HIGH" & TPR == "LOW"), evidence = (APL=="TRUE") )
#0.8636364




>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
=======
library(bnlearn)
library(plotly)
packageVersion('plotly')
source('http://bioconductor.org/biocLite.R')
biocLite('Rgraphviz')
library(xlsx)

data.alarm <- data.frame(alarm)
# create and plot the network structure.
modelstring = paste("[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF]",
  "[LVF][STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA]",
  "[HRSA|ERCA:HR][ANES][APL][TPR|APL][ECO2|ACO2:VLNG][KINK]",
  "[MINV|INT:VLNG][FIO2][PVS|FIO2:VALV][SAO2|PVS:SHNT][PAP|PMB][PMB]",
  "[SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC][MVS][VMCH|MVS]",
  "[VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG][ACO2|VALV]",
  "[CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]", sep = "")
dag = model2network(modelstring)


#se comparan los 3 metodos
set.seed(6)
res = hc(data.alarm)##MEJOR MODELO
unlist(compare(dag, res))
sc<-score(res,data.alarm)
print(sc)
#tp fp fn 
#22 31 24 
#BIC: -220761.7

set.seed(6)
res = mmhc(data.alarm)
unlist(compare(dag, res))
sc<-score(res,data.alarm)
print(sc)
#tp fp fn 
#16 13 30 
#BIC: -311189.8

set.seed(6)
res = mmpc(data.alarm)
unlist(compare(dag, res))
sc<-score(res,data.alarm)
print(sc)
#tp fp fn 
#0 29 46
#the graph is only partially directed

tp <- c()
Restart <- c()
Seed <- c()

maximoTp = 0
bestRest = 0;
bestSeed = 0;
for(seed in 1:500){
  for(i in 1:100){
	if((seed%%10) == 0 & (i%%10) == 0){
		set.seed(seed)
		res = hc(data.alarm,restart = i)
		tph = unlist(compare(dag, res))["tp"]
		
		tp <- c(tp, tph)
		Restart <- c(Restart, i)
		Seed <- c(Seed, seed)

		if(maximoTp < tph){
		  maximoTp = tph
		  bestRest = i
		  bestSeed = seed
		}
	}
  }
}


##Grafico Curva Arendizaje

datos <- data.frame(Restart, Seed, tp, co = as.numeric(c(1:length(Restart))))
setwd("C:\\Users\\Luis.O.A\\Documents\\USACH\\Mineria de Datos\\Trabajos\\4")
write.xlsx(datos, "Ajuste_TP.xlsx", sheetName="Sheet1")

p <- plot_ly(datos, x = ~Restart, y = ~Seed, z = ~tp, type = 'scatter3d', mode = 'lines',
        line = list(color = '#1f77b4', width = 1))
		
p
		


##MEJOR MODELO
set.seed(bestSeed)#Seed=40
res = hc(data.alarm,restart = bestRest)#Restart=90
unlist(compare(dag, res))
sc<-score(res,data.alarm)
print(sc)
#tp fp fn 
#34 17 12 
#BIC: -218690.1

##Grafico de la mejor red
graphviz.plot(res)

###################################################################33CONSULTAS ORACULO
fittedbn <- bn.fit(res, data = data.alarm)

#¿Qué tipo de entubación se le suministra a pacientes con anafilaxis? (PPT 15)
set.seed(6)
cpquery(fittedbn, event = (INT=="NORMAL"), evidence = (APL=="TRUE") )
#0.9194631

#¿Cuál es la probabilidad de que el paciente padezca de embolia pulmonar (PMB) si la catecolamina es alta(CCHL) y el ritmo cardiaco es alto (HR)?
#PPT 16
set.seed(6)
cpquery(fittedbn, event = (HR=="HIGH" & CCHL == "HIGH"), evidence = (PMB=="TRUE") )
#0.6923077

#¿Cuál es la probabilidad de que el paciente padezca de embolia pulmonar (PMB) si la catecolamina es alta(CCHL), si el ritmo cardiaco es alto (HR) y el CO es alto?
#PPT 17
set.seed(6)
cpquery(fittedbn, event = (HR=="HIGH" & CCHL == "HIGH" & CO == "HIGH"), evidence = (PMB=="TRUE") )
#0.5605096

#¿Cuál es la probabilidad de que un paciente con ritmo cardiaco alto, catecolamina alta e irrigación sanguínea pulmonar baja padezca de anafilaxis?
#PPT 18
set.seed(6)
cpquery(fittedbn, event = (HR =="HIGH" & CCHL =="HIGH" & TPR == "LOW"), evidence = (APL=="TRUE") )
#0.8636364




>>>>>>> 4966aae922150d281e7f47d431009d8054258bc0
