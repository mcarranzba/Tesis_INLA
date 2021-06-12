library(tidyverse)
library(reshape2)

ListaMun <- read.csv("temp.csv",header = T)
PoblMun <- read.csv("poblacion.csv",header = T)
PoblMunInd <- read.csv("codigo_municipio_inegi.csv",header = T)
head(ListaMun[,1])
head(PoblMun)
head(PoblMunInd)
dim (PoblMunInd)
toStringV <- Vectorize(toString)
sum(is.na(PoblMun))
row.has.not.na <- apply(PoblMun[,c(3,4)], 1, function(x){any(is.na(x))})
length(row.has.not.na)
rownames(PoblMun) <- PoblMun[,1]
HH <- PoblMun[-which(row.has.not.na),]
dim(HH)
head(HH)

head(PoblMun)
match(ListaMun[,1],HH[,1])
length(ListaMun[,1])
indic <- which(ListaMun[,1] %in% HH[,1])
Noestan <- which(!ListaMun[,1] %in% HH[,1])
FFF <- PoblMun[indic,]
EstadoMun <- PoblMunInd$NOM_ENT[indic]
Estados <- read.csv("Estados.txt",header = F)
#rownames(PoblEst) <- Estados[,1]
PonTresCeros <- function(x){
  formatC(x, width = 3, format = "d", flag = "0")
}
PonTresCeros <- Vectorize(PonTresCeros)
PonDosCeros <- function(x){
  formatC(x, width = 2, format = "d", flag = "0")
}
PonDosCeros <- Vectorize(PonDosCeros)
PonCincoCeros <- function(x){
  formatC(x, width = 5, format = "d", flag = "0")
}
PonCincoCeros <- Vectorize(PonCincoCeros)

PoblEstA <- as.data.frame(cbind(t(PoblEst), tiempo=1:5))

PoblEstA.long <- melt(PoblEstA,id.vars="tiempo",variable.name = "ESTADOS", value.name = "Poblacion")
head(PoblEstA.long)
ggplot(PoblEstA.long,aes(x=tiempo, y =Poblacion))+geom_line()+facet_wrap(~ESTADOS)

plot(c(1,5),c(0,15000000),type="n")
for( e in 1:32){
  lines(1:5,PoblEst[e,])
}
dim(EjSemestre)

y1<- FFF[,c(3)]
y2<- FFF[,c(4)]
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
logPonder <- log(EjSemestre)-log(QQ)
Ponder <- exp(logPonder)
dim(Ponder)
rownames(Ponder) <- III
Ponderr <- cbind(Ponder,rowSums(Ponder))
write.csv(Ponderr, file = "EvPorPob.csv")

hist(log(Ponder[,9]))
colSums(is.na(log(Ponder)==-Inf))
FaltanCensos <- c(266, 656, 928, 1570, 1571, 1572, 1573, 1574, 1575, 1576, 1577, 1578, 1579, 1580, 1581, 1582, 1583, 1584, 1585, 1586, 1587, 1588, 1589, 1590, 1591, 1592, 1593, 1594, 1595, 1596, 1597, 1598, 1599, 1840, 1841, 1842)
  EjSemestre[which(is.na(log(Ponder)[,1]==-Inf)),]
  NOSONCEROS <- c(656,1840)
  EstadoMun[which(is.na(log(Ponder)[,1]==-Inf))]
which(is.na(FFF[,c(3)]))
QQ[which(is.na(log(Ponder)[,1]==-Inf)),1]

Ponder[which(is.na(Ponder[,1]==0)),] <- rep(0,10)


CerosTodoInd <- which(colSums(t(log(Ponder)==-Inf))==10)
CerosTodo <- matrix(rep(0,length(III)),c(length(III),1))
rownames(CerosTodo) <- III
CerosTodo[CerosTodoInd] <- 1
head(CerosTodo)
write.csv(CerosTodo,file = "CerosTodo.csv")

totalporSemestre <- colSums(EjSemestre)
PropporSemestre <- colSums(EjSemestre)/colSums(QQ)
Esperado <- QQ*matrix(rep(PropporSemestre,2454),2454,10)
dim(QQ)
#cetes <- approx   (x, y , xout, method="linear", n=50, ties = mean)
#plot(cetes) lines(cetes)
save(Esperado,file="Esperados.Rda")
load("Esperados.Rda")
Esperado

load("data.Rda")



head(FFF)


#Cargamos paquetes
#install.packages("rgdal")
library(cluster)
library(fpc)
library(ggplot2)
library(reshape2)
library(corrplot)
#library(mxmaps)
library(animation)
library(spatstat)
library(rgdal)
#Cargamos los datos


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
plot(shape)
hist(AgGeo$latitud)
hist(AgGeo$longitud)

tt <- 2


for (e in 2007:2011) {
  for (w in 1:(12/tt)) {
    nombre <- paste("rplot",e,"_",w,"de",tt,".jpg",sep="")
    png(nombre)
    puntos <- cbind(EjGeo[which(EjEst$ANIO == e & (ceiling(EjEst$MES/tt) == w)),]$longitud,EjGeo[which(EjEst$ANIO == e & (ceiling(EjEst$MES/tt) == w)),]$latitud)
    #ploteamos el pais y despues los eventos
    rr <- dim(puntos)[1]
    p <- ppp(puntos[,1]+rnorm(rr,0,.1),puntos[,2]+rnorm(rr,0,.1), window=owin(c(-120,-80),c(14,34)))
    #se agrega un ruido (como Scater), para evitar puntos exactamente iguales
    #puede repetirse la estimacion para asegurarse de que no es mucha la diferencia
    plot(country)
    plot(density(p,kernel = "gaussian",bw = "nrd"),main = "Proceso Poisson",zlim=c(0,15),col=heat.colors(36,alpha=0.6)[36:1],add=TRUE)
    points(puntos, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.25),pch=19,cex=.5)
    dev.off()
  }
}
?ppp
points(puntos, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.25),pch=19,cex=.5)
d
#Graficamos clusters calculados con k medias
d <- puntos[which(puntos[,1]<50),]#quitamos los posibles puntos que
#se salen de Mexico por la longitud
plot(d)
MM <- kmeans(d,centers = 3)
KM_DF <- as.data.frame(cbind(d,MM$cluster))
ggplot(KM_DF, aes(V1, V2, colour = factor(V3))) + 
  geom_point()
#plotcluster(d, MM$cluster)


#Serie de tiempo nacional mensual
plot(matrix(table(EjEst[,4:5]),72,1) ,type="l")

#serie de tiempo por estado mensual
plot(c(0,72),c(0,300),type="n")
for( e in 1:32){
  lines(matrix(table(EjEst[which(EjEst$ESTADO == e),4:5]),72,1)+rnorm(72,0,0.1))
}
head(EjEst)
#serie de tiempo por estado por tt-mestre
tt <- 6
plot(c(1,60/tt),c(0,1500),type="n")
Munip <- paste(PonDosCeros(EjEst$ESTADO),PonTresCeros(EjEst$Municipio),sep = "")
III <- PonCincoCeros(FFF$CVEGEO)
AA <- c()
for( e in III){
  auxi <- EjEst[which(Munip == e & EjEst$ANIO != 2006),4:5]
  auxi[,1] <- ceiling(auxi[,1]/tt)
  lines(matrix(table(auxi),60/tt,1))
  AA <- rbind(AA,t(matrix(table(auxi),60/tt,1)))
}
head(AA)
AA[is.na(AA)] <- 0

EjSemestre <- as.data.frame(AA)
AA <- exp(logPonder)
rownames(AA) <- Abrv[,1]
#serie de tiempo por estado por tt-mestre desplejada en ggplot
AAA <- as.data.frame(cbind(t(AA), tiempo=1:(60/tt)))
AAA.long <- melt(AAA,id.vars="tiempo",variable.name = "ESTADOS", value.name = "Ejecuciones")
head(AAA.long)
ggplot(AAA.long,aes(x=tiempo, y =Ejecuciones))+geom_line()+facet_wrap(~ESTADOS)+theme_bw(base_size = 16)

#PCA de las series de tiempo vistas como vectores
rownames(AA) <- Abrv[,2]
head(AA)
p<-princomp(AA,cor=F)
#dd <- t(t(temp)-colMeans(temp))
#p<-princomp(p,cor=T)
colMeans(AA)
NT <- dim(AA)[2]
p$loadings[,1]
plot(1:NT,p$loadings[,1],main="Primera componente",xlab="Tiempo", ylab="Componente")
lines(1:NT,p$loadings[,1])

plot(1:NT,p$loadings[,2],main="Segunda componente",xlab="Tiempo", ylab="Componente")
lines(1:NT,p$loadings[,2])

biplot(p)
summary(p)
#Hierarquical clustering de las series de tiempo vistas como vectores
rownames(AA) <- Abrv[,2]
HC <- hclust(dist(AA),method = "complete", members = NULL)
plot(HC)
#?hclust

#Multidimentional Scaling de las series de tiempo vistas como vectores

loc <- cmdscale(dist(AA))
x <- loc[, 1]
y <- loc[, 2]

plot(x, y, type = "n", xlab = "", ylab = "", axes = FALSE)
text(x, y, rownames(loc), cex = 0.6)
plot(x, y,pch = 19, xlab = "", ylab = "", axes = FALSE)

#Matriz de correlacion de las series de tiempo vistas como vectores

corrplot(cor(t(AA)), method = "square")


plot(matrix(table(EjEst[which(AgEst$ESTADO == "Guanajuato"),4:5]),72,1) ,type="l")


#Para ver la cantidad de NA en una variable (por ejemplo, PF)
#Para ver el histograma de las variables observadas
names(AgEst)
head(AgEst[,1:15])
NumNAs <- c()
for (u in 1:dim(AgEst)[2]){
  NumNAs <- c(NumNAs,sum(AgEst[,u] == 9999 ))
}
NumNAs <- matrix(NumNAs,1,dim(AgEst)[2])
colnames(NumNAs) <- colnames(AgEst)
dim(AgEst)
NumCeros <- c()
for (u in c(1:8,38:dim(AgEst)[2])){
  NumCeros <- c(NumCeros,sum(AgEst[,u] == 0 ))
}
NumCeros <- matrix(NumCeros,1,length(NumCeros))
colnames(NumCeros) <- colnames(AgEst[,c(1:8,38:dim(AgEst)[2])])

hist(AgEst[which(AgEst$PF != 9999 ),]$PF,breaks=50)


##########################################################################################################
############################################################################################
#                                 Ejecuciones Semestrales                         #                                  #
############################################################################################
#Import the data
data <- cbind(EjSemestre,Esperado)

#install.packages("tidyverse")
#-- Prepare the map --#
library(maptools)
library(sp)
library(lattice)
#install.packages("spdep")
library(spdep)
mexico <- rgdal::readOGR("Todos.shp")
## Need to drop extra polygons 98 (Macon county polygon), 100, 105 (Taylor county polygons) + 137 (Lee county polygon)
## These are very small and always adjacent to "main" polygon, so we can base neighborhood structure on "main" polygon:
#rmIdx <- c(98, 100, 105, 137)
mexico <- mexico[-Noestan,]
dim(mexico)
data.mexico = attr(mexico, "data")
#################################################
#Create the graph for adjacencies in INLA
#Need the non thinned sph file to do the adjacency matrix!!!
zzz <- poly2nb(mexico)
nb2INLA("mexico.graph", zzz)
#this create a file called "LDN-INLA.adj" with the graph for INLA
mexico.adj <- paste(getwd(),"/mexico.graph",sep="")

#Order based on the map
#order <- match(data.mexico$NAME,data[,1])
#data<- data[order,]

#--Transform the data to be in the right format for INLA--#
low.vector <- as.vector(as.matrix(data[,1:10]))#by column
E.vector <- as.vector(as.matrix(data[,11:20]))#by column
year <- numeric(0)
for(i in 1:10){ 
  year<- append(year,rep(i,dim(data)[1]))
}
county<- as.factor(rep(data[,1],10))

data<- data.frame(y= low.vector, E= E.vector, ID.area=as.numeric(county), ID.area1=as.numeric(county), year=year,
                  ID.year = year, ID.year1=year, ID.area.year = seq(1,length(county)))
#############################################################
head(data)
names(inla.models()$likelihood)
#--Prepare the model and run inla--#
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
#Parametric model alpha + csii + (deltai + beta)*year
formula.ST1<- y ~ 1 + f(ID.area,model="bym",graph=mexico.adj) +
  f(ID.area1,year,model="iid") + (year-mean(year))
model.inla.ST1 <- inla(formula.ST1,family="zeroinflated.poisson0",data=data,E=E, control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE))
summary(model.inla.ST1)
#Non Parametric model alpha + csii + gammaj + phij #No space time interaction yet!
#csii and are modelled through BYM
#gammaj are modelled as RW1
#phij are modelled as exchangeable
formula.ST2<- y ~ 1 + f(ID.area,model="bym",graph=mexico.adj) +
  f(ID.year,model="rw1") + f(ID.year1,model="iid")
model.inla.ST2 <- inla(formula.ST2,family="poisson",data=data,E=E, control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE))

#Non Parametric model alpha + csii + gammaj + phij + deltaij
#csii are modelled through BYM
#gammaj are modelled as RW1
#phij are modelled as exchangeable
#Interaction (deltaij) is modelled as exchangeable
formula.ST3<- y ~ 1 + f(ID.area,model="bym",graph=mexico.adj) +
  f(ID.year,model="ar1") + f(ID.year1,model="iid") + f(ID.area.year,model="iid")

#To obtain the marginal of phij + gammaj we need to create the corresponding linear combinations and include these in the model 
lcs = inla.make.lincombs(ID.year = diag(10),  ID.year1 = diag(10))

model.inla.ST3 <- inla(formula.ST3,family="zeroinflated.poisson0",data=data,E=E, 
                       control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE),
                       lincomb=lcs,control.inla = list(lincomb.derived.only=TRUE))
?inla

summary(model.inla.ST1)
#Put the temporal effect  (gammaj+phij) on the natural scale
temporal<-lapply(model.inla.ST3$marginals.lincomb.derived, function(X){
  #marg <- inla.marginal.transform(function(x) exp(x), X)
  marg <- inla.tmarginal(function(x) exp(x), X)
  inla.emarginal(mean, marg)
})

#############################################################
#Computethe DIC as a tool for model choice
model.inla.ST1$dic$dic
model.inla.ST2$dic$dic
model.inla.ST3$dic$dic

#DIC components: Effective number of parameter (pd)
model.inla.ST1$dic$p.eff
model.inla.ST2$dic$p.eff
model.inla.ST3$dic$p.eff
#DIC components: mean.deviance
model.inla.ST1$dic$mean.deviance
model.inla.ST2$dic$mean.deviance
model.inla.ST3$dic$mean.deviance
######################################################
#The last model (with interaction) shows the best fit. Look at the results
#Obtain zetai exponentiating csii 

m <- model.inla.ST1$marginals.random[[1]][1:2454]
zeta <- unlist(lapply(m,function(x)inla.emarginal(exp,x)))
head(III)


m <- model.inla.ST3$marginals.random[[1]][1:32]
zeta <- unlist(lapply(m,function(x)inla.emarginal(exp,x)))
#length(model.inla.ST3$marginals.random[[1]][1:32])
#Probability that theta>1
a=0
inlaprob<-lapply(model.inla.ST3$marginals.random[[1]][1:32], function(X){
  1-inla.pmarginal(a, X)
})

#Spatial.results<- data.frame(NAME=data.georgia$NAME,zeta=unlist(zeta), pp=unlist(inlaprob))
Spatial.results<- data.frame(NAME=Estados,zeta=unlist(zeta), pp=unlist(inlaprob))

#length(data.georgia$NAME)
#length(unlist(inlaprob))
#Maps
#Create classes of SMRs
zeta.cutoff<- c(0.1, 1, 5, 10,15)
pp.cutoff <- c(0,0.2,0.8,1)
aux <- as.data.frame(cbind(III,zeta))
write.csv(aux, file = "RiesgoMed.csv")
zeta=cut(Spatial.results$zeta,breaks=zeta.cutoff,include.lowest=TRUE)
pp=cut(Spatial.results$pp,breaks=pp.cutoff,include.lowest=TRUE)

maps.factors <- data.frame(NAME=Estados, zeta=zeta,pp=pp)
attr(mexico, "data")=data.frame(data.mexico, maps.factors)

trellis.par.set(axis.line=list(col=NA))
spplot(obj=mexico, zcol= "zeta", col.regions=gray(3.5:0.5/4),main="",par.settings=list(fontsize=list(text=17)))
trellis.par.set(axis.line=list(col=NA))
spplot(obj=mexico, zcol= "pp", col.regions=gray(2.5:0.5/3),main="",par.settings=list(fontsize=list(text=17)))

#Plot the National temporal trend
plot(seq(1,10),seq(0.3,2.5,length=10),type="n",xlab="year",ylab=expression(exp(gamma[t]+phi[t])))
lines(unlist(temporal))
abline(h=1,lty=2)
#######################
#Space-Time Interaction
delta <- data.frame(delta=model.inla.ST3$summary.random$ID.area.year[,2],year=data$ID.year,ID.area=data$ID.area)
#delta.matrix <- matrix(delta[,1], 163,11,byrow=FALSE)
delta.matrix <- matrix(delta[,1], 32,10,byrow=FALSE)
#rownames(delta.matrix)<- delta[1:163,3]
rownames(delta.matrix)<- delta[1:32,3]

#Space time probability>1
a=0
inlaprob.delta<-lapply(model.inla.ST3$marginals.random[[4]], function(X){
  1-inla.pmarginal(a, X)
})

pp.delta<-unlist(inlaprob.delta)
pp.cutoff.interaction <- c(0,0.2,0.8,1)
#pp.delta.matrix <- matrix(pp.delta, 163,11,byrow=FALSE)
pp.delta.matrix <- matrix(pp.delta, 32,10,byrow=FALSE)
pp.delta.factor <- data.frame(NAME=Estados)
for(i in 1:10){
  pp.delta.factor.temp <- cut(pp.delta.matrix[,i],breaks=pp.cutoff.interaction,include.lowest=TRUE) 
  pp.delta.factor <- cbind(pp.delta.factor,pp.delta.factor.temp)
}
colnames(pp.delta.factor)<- c("NAME",seq(2007,2011.5,0.5))

#Maps
attr(mexico, "data")=data.frame(data.mexico, pp.delta.factor)
trellis.par.set(axis.line=list(col=NA))
spplot(obj=mexico, zcol="X2007", col.regions=gray(2.5:0.5/3),main="",par.settings=list(fontsize=list(text=17)))
trellis.par.set(axis.line=list(col=NA))
spplot(obj=mexico, zcol="X2008", col.regions=gray(2.5:0.5/3),main="",par.settings=list(fontsize=list(text=17)))
trellis.par.set(axis.line=list(col=NA))
spplot(obj=mexico, zcol="X2009", col.regions=gray(2.5:0.5/3),main="",par.settings=list(fontsize=list(text=17)))
trellis.par.set(axis.line=list(col=NA))
spplot(obj=mexico, zcol="X2010", col.regions=gray(2.5:0.5/3),main="",par.settings=list(fontsize=list(text=17)))
trellis.par.set(axis.line=list(col=NA))
spplot(obj=mexico, zcol="X2011", col.regions=gray(2:0/2),main="",par.settings=list(fontsize=list(text=17)))
##########


