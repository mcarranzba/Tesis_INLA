#Cargamos paquetes
library(cluster) 
library(fpc)
library(ggplot2)
library(reshape2)
library(corrplot)
library(rgdal)
library(animation)
library(spatstat)
library(Matrix)

#invPerm
Orden_Pob_Pobr <- c(1:4,7,8,5,6,9:32)
Orden_Pob_Pobr_Inv <- invPerm(Orden_Pob_Pobr)
Orden_Econ <- c(1:4,7,8,9,5,6,10:32)
Orden_Econ_Inv <- invPerm(Orden_Econ)


#Cargamos los datos
PoblEst <- read.csv("Poblacion_Estado_90-10.csv",header = T)
Estados <- read.csv("Estados.txt",header = F)
Abrv <- read.csv("Abrev.csv",header = T)
AgGeo <- read.csv("agresiones_geocod_feb2019.csv",header = T)
EjGeo <- read.csv("ejecuciones_geocod_feb2019.csv",header = T)
EnGeo <- read.csv("enfrentamientos_geocod_feb2019.csv",header = T)
AgEst <- read.csv("A-A.csv",header = T)
EnEst <- read.csv("A-E.csv",header = T)
EjEst <- read.csv("EJECUCIONES(2006-2011).csv",header = T)
EjEst <- EjEst[,-c(1,3)]
#cargamos los puntos espaciales
shape <- readOGR(dsn = ".", layer = "gadm36_MEX_1")

tt <- 6
plot(c(1,60/tt),c(0,250),type="n")
AA <- c()
for( e in 1:32){
  auxi <- EnEst[which(EnEst$ESTADO == e & EnEst$ANIO != 2006),4:5]
  auxi[,1] <- ceiling(auxi[,1]/tt)
  lines(matrix(table(auxi),60/tt,1))
  AA <- rbind(AA,t(matrix(table(auxi),60/tt,1)))
}
AA
EnSemestre <- as.data.frame(AA)
EnSemestreV <- as.vector(matrix(as.matrix(EnSemestre),1,320))

tt <- 6
plot(c(1,60/tt),c(0,100),type="n")
AA <- c()
for( e in 1:32){
  auxi <- AgEst[which(AgEst$ESTADO == e & AgEst$ANIO != 2006),4:5]
  auxi[,1] <- ceiling(auxi[,1]/tt)
  lines(matrix(table(auxi),60/tt,1))
  AA <- rbind(AA,t(matrix(table(auxi),60/tt,1)))
}

AgSemestre <- as.data.frame(AA)
AgSemestre[is.na(AgSemestre)] <- 0
AgSemestreV <- as.vector(matrix(as.matrix(AgSemestre),1,320))

tt <- 6
plot(c(1,60/tt),c(0,1500),type="n")
AA <- c()
for( e in 1:32){
  auxi <- EjEst[which(EjEst$ESTADO == e & EjEst$ANIO != 2006),4:5]
  auxi[,1] <- ceiling(auxi[,1]/tt)
  lines(matrix(table(auxi),60/tt,1))
  AA <- rbind(AA,t(matrix(table(auxi),60/tt,1)))
}

EjSemestre <- as.data.frame(AA)
EjSemestreV <- as.vector(matrix(as.matrix(EjSemestre),1,320))
#Para regresar as.data.frame(matrix(EjSemestreV,32,10))
head(EjSemestre)

?matrix
y1<- PoblEst[-1,c(5)]
y2<- PoblEst[-1,c(6)]
x1 <- 2005
x2 <- 2010
XX <- seq(2007,2011.5,0.5)
m <- (y2-y1)/(x2-x1)

QQ <- c()
for(x in XX){
  y <- m*(x-x1)+y1
  QQ <- cbind(QQ,y)
}
colnames(QQ) <- XX
dim(QQ)
#logPonder <- log(EjSemestre)-log(QQ)
#totalporSemestre <- colSums(EjSemestre)
PropEjSem <- colSums(EjSemestre)/colSums(QQ)
EspEj <- QQ*matrix(rep(PropEjSem ,32),32,10)
PropEnSem <- colSums(EnSemestre)/colSums(QQ)
EspEn <- QQ*matrix(rep(PropEnSem ,32),32,10)
PropAgSem <- colSums(AgSemestre)/colSums(QQ)
EspAg <- QQ*matrix(rep(PropAgSem ,32),32,10)
Poblacion <- as.numeric(as.vector(matrix(as.matrix(QQ),1,320)))

EspEjV <- as.vector(matrix(as.matrix(EspEj),1,320))
EspEnV <- as.vector(matrix(as.matrix(EspEn),1,320))
EspAgV <- as.vector(matrix(as.matrix(EspAg),1,320))


desocE <- read.csv("desocE.csv",header = T)
desocT <- read.csv("desocT.csv",header = T)

Centr <- read.csv("CentroidesEstadoC.csv",header = T)
XX <- rep(x = Centr[,2],times=10)
YY <- rep(x=Centr[,3],times=10)
Centr
XX2 <- XX^2
YY2 <- YY^2
XXYY <- XX*YY
#XX + YY + XX2 + YY2 + XXYY


#pobreza y Gini
PobrGini <- read.csv("PobrezaGini.csv",header = T)
PobrGini
PobreGiniDF <- c()
for(w in 0:3){
  y1<- PobrGini[,(2+w*3)]
  y2<- PobrGini[,(3+w*3)]
  x1 <- 2000
  x2 <- 2010
  XXA <- seq(2007,2011.5,0.5)
  m <- (y2-y1)/(x2-x1)
  QQ <- c()
  for(x in XXA){
    y <- m*(x-x1)+y1
    QQ <- cbind(QQ,y)
  }
  QQ <- matrix(t(QQ),c(320,1))
  PobreGiniDF <- cbind(PobreGiniDF,QQ)
}
PobrAlim <- PobreGiniDF[,1]
PobrCapa<-PobreGiniDF[,2]
PobrPatri<- PobreGiniDF[,3]
CoehSocGini <- PobreGiniDF[,4]

#PobrAlim + PobrCapa + PobrPatri + CoehSocGini + Desocup


gg <- desocE[,2]/mean(desocE[,2])
hh <- ((c(desocT[,2],0)+c(0,desocT[,2]))/2)[2*1:10]
kk <- matrix(rep(gg,10),32,10)*t(matrix(rep(hh,32),10,32))
Desocup <- as.vector(kk)

library(tidyverse)
OcupTab<- read.csv("Poblacion_Ocupada_Actividad_Economica.csv",header = T)
dim(OcupTab)
table(OcupTab[,1])
table(OcupTab[,2])
table(OcupTab[,3])
table(OcupTab[,4])
table(OcupTab[,5])
VtoString <- Vectorize(toString)
realdate <- as.Date(VtoString(unique(OcupTab[,1])), "%Y%m%d")#as.Date(VtoString(unique(OcupTab[,1])), "%Y%m%d")
kk <- data.frame(date=realdate)
year=as.numeric (format(realdate,"%Y"))
month=as.numeric (format(realdate,"%m"))
day=as.numeric (format(realdate,"%d"))
head(OcupTab[,1])
library(tidyverse)
?separate
library(dplyr)
Semestre <- function(x){
  case_when(
    20070100<x && x<20070700 ~ "1",
    20070700<x && x<20080100 ~ "2",
    20080100<x && x<20080700 ~ "3",
    20080700<x && x<20090100 ~ "4",
    20090100<x && x<20090700 ~ "5",
    20090700<x && x<20100100 ~ "6",
    20100100<x && x<20100700 ~ "7",
    20100700<x && x<20110100 ~ "8",
    20110100<x && x<20110700 ~ "9",
    20110700<x && x<20120100 ~ "10"#,
    #TRUE ~ "other"
  )
}

VSemestre <- Vectorize(Semestre)
semestre <- VSemestre(OcupTab[,1])
hist(as.integer(semestre))
OcupTabS <- cbind(OcupTab,semestre)[-which(is.na(semestre)),]
dim(OcupTabS)
unique(OcupTabS[,6])

unique(OcupTabS[,2])[1:32]
tablaSum <- matrix(rep(0,(4*10*32*12)),(10*32*12),4)
j <- 1
for (sem in 1:10){
  for (esta in unique(OcupTabS[,2])[1:32]){
    for (ocup in unique(OcupTabS[,5])){
      cant <- sum(OcupTabS[which(OcupTabS$semestre==sem & OcupTabS$Entidad_Federativa==esta & OcupTabS$Actividad_economica==ocup ),6])
      tablaSum[j,] <- c(sem,esta,ocup,cant)
      j <- j+1
    }
  }
}
head(tablaSum)
resc <- rep(0,dim(tablaSum)[1])
for(e in 0:((dim(tablaSum)[1]/12))-1){
  indic <- (e*12)+1:12
  SS<- sum(as.integer(tablaSum[indic,4]))
  resc[indic] <- as.double(tablaSum[indic,4])/(SS)
}
tablaSum <- as.data.frame(tablaSum)
tablaAct <- cbind(tablaSum,resc)
names(tablaAct)<- c("Semestre","Estado","Industria","NumTrab","PropTrab")
TT <- as.data.frame(tablaAct)
PrimerSem <- TT[(9*(12*32)+1):(10*(32*12)),]
head(TT)
names(TT)<- c("Semestre","Estado","Industria","NumTrab","PropTrab")
df <- c()
for (e in 0:31){
  df <- rbind(df,PrimerSem[(e*12)+1:12,5])
}
rownames(df) <-  unique(TT$Estado)

unique(TT$Industria)
stars(df)
?stars
IndustDF <- c()
TT[12*10*32,5]
dim(TT)
for(j in 1:12){
  auxivec <- c()
  for(e in 0:(10*32-1)){
    auxivec <- c(auxivec,as.double(TT[(j+e*12),5]))
  }
  IndustDF <- cbind(IndustDF,auxivec)
}
dim(IndustDF)
names(IndustDF) <-  unique(TT$Industria)
Agric <- IndustDF[,1]
ExtrElec<- IndustDF[,2]
Manuf <- IndustDF[,3]
Constr <- IndustDF[,4]
Comer <- IndustDF[,5]
RestHot <- IndustDF[,6]
TransComun <- IndustDF[,7]
ServProf <- IndustDF[,8]
ServSoc <- IndustDF[,9]
ServDiv <- IndustDF[,10]
GobIntern<- IndustDF[,11]
NoEspec <- IndustDF[,12]

library(readr)

EST <-  as.vector(read.csv("Estados.csv"))[,1]
Estados <- as.vector(rep(EST,times=10))
?rep
Semestre <- rep(seq(2007,2011.5,0.5),each=32)

TodoEstados <- as.data.frame(cbind(EjSemestreV,EnSemestreV,AgSemestreV,Poblacion,EspEjV,EspEnV,EspAgV, Agric, ExtrElec, Manuf, Constr, Comer, RestHot, TransComun, ServProf, ServSoc, ServDiv, GobIntern, NoEspec, XX, YY, XX2, YY2, XXYY, PobrAlim, PobrCapa, PobrPatri, CoehSocGini, Desocup, Estados, Semestre))

head(TodoEstados)
save(TodoEstados, file = "TodoEstados.RData")
load("TodoEstados.RData")
DF <- as.data.frame(scale(get(load('TodoEstados.RData'))[,5:26]))
head(isfar)
