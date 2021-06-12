load("TodoEstados.RData")
#install.packages("lm.beta")
library(lm.beta)
LM <- glm(data=TodoEstados, formula=EjSemestreV~.-NoEspec  -EnSemestreV-AgSemestreV-Estados-PobrCapa -PobrPatri)
?glm
NuevoOrden <- c(1:4,7,8,5,6,9:32)
AUX <- rep(NuevoOrden, 10)+rep(0:9*32, each=32)

TodoEstadosS <- TodoEstados[AUX,]
TodoEstadosS[,-c(27,28)] <- scale(TodoEstados[,-c(27,28)])
library(pscl)
m1 <- zeroinfl(count ~ child + camper | persons, data = zinb)
m1 <- lm(data=TodoEstadosS, formula= EjSemestreV~.-NoEspec  -EnSemestreV-AgSemestreV-Estados-PobrCapa )
m1 <- zeroinfl(data=TodoEstadosS, formula= EjSemestreV~ EsperadoV+ServDiv+GobIntern+XX+YY +PobrAlim+CoehSocGini+Desocup )
m1 <- zeroinfl(data=TodoEstadosS, formula= EnSemestreV~ ServDiv+GobIntern+XX+YY +PobrAlim+CoehSocGini+Desocup )
m1 <- zeroinfl(data=TodoEstadosS,  formula= AgSemestreV~ ServDiv+GobIntern+XX+YY +PobrAlim+CoehSocGini+Desocup )

m1 <- lm(data=TodoEstadosS,  formula= AgSemestreV~ ServDiv+GobIntern+XX+YY +PobrAlim+CoehSocGini+Desocup )
m1 <- lm(data=TodoEstadosS, formula= EnSemestreV~ ServDiv+GobIntern+XX+YY +PobrAlim+CoehSocGini+Desocup )


?zeroinfl
summary(m1)
names(TodoEstados)


zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")

summary(m1)
summary(LM)

data <- cbind(as.data.frame(matrix(TodoEstados$EjSemestreV,32,10)),as.data.frame(matrix(TodoEstados$EsperadoV,32,10)))
data <- cbind(as.data.frame(matrix(TodoEstados$EnSemestreV,32,10)),as.data.frame(matrix(TodoEstados$EsperadoV,32,10)))
data <- cbind(as.data.frame(matrix(TodoEstados$AgSemestreV,32,10)),as.data.frame(matrix(TodoEstados$EsperadoV,32,10)))
data <- cbind(EjSemestre,Esperado)
#data <- cbind(AgSemestre,Esperado)
source("Nad_Wat_Bivariado.R", echo = F)
library(corrplot)
#View(TodoEstados)
var1 <- length(TodoEstados)
corrplot(cor(TodoEstados[,-c((var1-1),var1)], method = "spearman"))
hist(TodoEstados$YY)
summary(TodoEstados)
#install.packages("CCA")
library("CCA")
#correl <- matcor(TodoEstados[,1:3], TodoEstados[,4:26] )
#img.matcor(correl, type = 2)
#cc1 <- cancor(TodoEstados[,1:3], TodoEstados[,4:26])  ### function from standard R instalation
#cc2 <- cc(TodoEstados[,1:3], TodoEstados[,4:26])      ### function for the R package 'CCA'
#plt.cc(cc2, var.label = TRUE, ind.names = data[,1])

#Estados2 <- read.csv("Estados2.txt",header = F)
Estados <- read.csv("Estados.txt",header = F)
DF <- TodoEstados
#-- Prepare the map --#
library(maptools)
library(sp)
library(lattice)
#install.packages("spdep")
library(spdep)
mexico <- rgdal::readOGR("gadm36_MEX_1.shp")
## Need to drop extra polygons 98 (Macon county polygon), 100, 105 (Taylor county polygons) + 137 (Lee county polygon)
## These are very small and always adjacent to "main" polygon, so we can base neighborhood structure on "main" polygon:
#rmIdx <- c(98, 100, 105, 137)
#georgia <- georgia[-rmIdx,]

data.mexico = attr(mexico, "data")
#################################################
#Create the graph for adjacencies in INLA
#Need the non thinned sph file to do the adjacency matrix!!!
#zzz <- poly2nb(mexico) # Solo si es nuevo
#nb2INLA("mexico.graph", zzz) #Solo si es nuevo
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
#--Prepare the model and run inla--#
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
#Parametric model alpha + csii + (deltai + beta)*year
#formula.ST1<- y ~ 1 + DF$XX + DF$YY + DF$XX2 + DF$YY2 + DF$XXYY + DF$PobrAlim + DF$PobrCapa + DF$PobrPatri + DF$CoehSocGini + DF$Desocup +DF$Agric + DF$ExtrElec + DF$Manuf + DF$Constr + DF$Comer + DF$RestHot + DF$TransComun + DF$ServProf + DF$ServSoc + DF$ServDiv + DF$GobIntern + DF$NoEspec + f(ID.area,model="bym",graph=mexico.adj) +
#  f(ID.area1,year,model="iid") + (year-mean(year))
#model.inla.ST1 <- inla(formula.ST1,family="zeroinflatedpoisson0",data=data,E=E, control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, mlik=TRUE))

#?inla
model.inla.ST1$cpo
summary(model.inla.ST1)

hist(model.inla.ST1$cpo$cpo)

formula.ST1<- y ~ 1 +  f(ID.area,model="bym",graph=mexico.adj) +
  f(ID.area1,year,model="rw1") + (year-mean(year))
model.inla.ST1 <- inla(formula.ST1,family="poisson",data=data,E=E, control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE))


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
formula.ST3<- y ~ 1 +  DF$XX + DF$YY + DF$XX2 + DF$YY2 + DF$XXYY + DF$PobrAlim + DF$PobrCapa + DF$PobrPatri + DF$CoehSocGini + DF$Desocup +DF$Agric + DF$ExtrElec + DF$Manuf + DF$Constr + DF$Comer + DF$RestHot + DF$TransComun + DF$ServProf + DF$ServSoc + DF$ServDiv + DF$GobIntern + DF$NoEspec  + f(ID.area,model="bym",graph=mexico.adj) +
  f(ID.year,model="rw1") + f(ID.year1,model="iid") + f(ID.area.year,model="iid")
formula.ST3<- y ~ 1 +  DF$XX + DF$YY + DF$XXYY + DF$PobrAlim + DF$CoehSocGini + DF$Desocup  + DF$ServProf + DF$ServDiv + DF$GobIntern  + f(ID.area,model="bym",graph=mexico.adj) +
  f(ID.year,model="ar1") + f(ID.year1,model="iid") + f(ID.area.year,model="iid")

#To obtain the marginal of phij + gammaj we need to create the corresponding linear combinations and include these in the model 
lcs = inla.make.lincombs(ID.year = diag(10),  ID.year1 = diag(10))

model.inla.ST3 <- inla(formula.ST3,family="poisson",data=data,E=E, 
                       control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, mlik=TRUE),#lincomb=lcs,
                       control.inla = list(lincomb.derived.only=TRUE))

summary(model.inla.ST3)
model.inla.ST3$summary.linear.predictor$mean[(8*32+1):(9*32)]
model.inla.ST3$dic$dic
model.inla.ST3$dic$p.eff
model.inla.ST3$dic$mean.deviance
model.inla.ST3$waic$waic
model.inla.ST3$waic$
model.inla.ST3$waic$p.eff
model.inla.ST3$mlik
-sum(log(model.inla.ST3$cpo$cpo))/320
-sum(log(model.inla.ST3$cpo$cpo),na.rm = T)/(320-sum(is.na(model.inla.ST3$cpo$cpo)))
sum(model.inla.ST3$cpo$pit)/320
df <- as.data.frame(model.inla.ST3$cpo$pit)
library(ggplot2)
p<-ggplot(df, aes(x=model.inla.ST3$cpo$pit)) + 
  geom_histogram(color="black",fill="grey" ) + 
  labs(x="PIT",y="Frecuencia", title="Histograma PIT")
p
hist(model.inla.ST3$cpo$cpo)
plot(TodoEstados$AgSemestreV,model.inla.ST3$cpo$cpo,pch=19,col=rgb(1,0,0,.3),xlab="Conteo Observado",ylab="CPO", main="CPO contra observado")
plot(TodoEstados$AgSemestreV,model.inla.ST3$cpo$pit,pch=19,col=rgb(1,0,0,.3))
hist(model.inla.ST3$cpo$pit,xlab="PIT",ylab="Frecuencia", main="Histograma PIT")
plot(1:320,model.inla.ST3$cpo$pit)

model.inla.ST3$summary.fixed
  
PX <- 1:nrow(model.inla.ST3$summary.fixed)
Efecto <- row.names(model.inla.ST3$summary.fixed)
FF <- model.inla.ST3$summary.fixed$`0.5quant`
L <-  model.inla.ST3$summary.fixed$`0.025quant`
U <-  model.inla.ST3$summary.fixed$`0.975quant`


DFA <- cbind(L,FF,U)
DFAR <- DFA[order(DFA[,2]),]
?order
length(PX)
#install.packages("plotrix")
library(plotrix)
par(mar=c(5.1,8.1,4.1,2.1))
#plotCI(y=PX, DFA[,2], ui=DFA[,3], li=DFA[,1],yaxt='n',ylab="",err="x",xlim=c(-2,2),xlab="")
#abline(v=0)
plotCI(y=PX, DFAR[,2], ui=DFAR[,3], li=DFAR[,1],yaxt='n',ylab="",err="x",xlim=c(-5,5),xlab="")
#?plotCI
abline(v=0)

#labs <- colnames(TodoEstados[5:26])
labs <- Efecto[order(DFA[,3])]
text(cex=1, y=PX, x=-2.5, labs, xpd=TRUE, srt=0)


formula.ST3<- y ~ 1 + XX + YY + XX2 + YY2 + XXYY + f(ID.area,model="bym",graph=mexico.adj) +
  f(ID.year,model="rw1") + f(ID.year1,model="iid") + f(ID.area.year,model="iid")


model.inla.ST3 <- inla(formula.ST3,family="poisson",data=data,E=E, 
                       control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE),
                       lincomb=lcs,control.inla = list(lincomb.derived.only=TRUE))


summary(model.inla.ST3)

formula.ST3<- y ~ 1 + f(ID.area,model="bym",graph=mexico.adj) +
  f(ID.year,model="rw1") + f(ID.year1,model="iid") + f(ID.area.year,model="iid")


model.inla.ST3 <- inla(formula.ST3,family="poisson",data=data,E=E, 
                       control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE),
                       lincomb=lcs,control.inla = list(lincomb.derived.only=TRUE))

summary(model.inla.ST3)


model.inla.ST1$mlik
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


m <- model.inla.ST3$marginals.random[[1]][1:32]
zeta <- unlist(lapply(m,function(x)inla.emarginal(exp,x)))


model.inla.ST3$marginals.random[[1]][1:32]

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
zeta.cutoff<- c(0, 0.5, 1.0, 1.5,2,10,40)
pp.cutoff <- c(0,0.2,0.8,1)
zeta=cut(Spatial.results$zeta,breaks=zeta.cutoff,include.lowest=TRUE)
pp=cut(Spatial.results$pp,breaks=pp.cutoff,include.lowest=TRUE)

maps.factors <- data.frame(NAME=Estados, zeta=zeta,pp=pp)
attr(mexico, "data")=data.frame(data.mexico, maps.factors)

trellis.par.set(axis.line=list(col=NA))
spplot(obj=mexico, zcol= "zeta", col.regions=heat.colors(7,rev = T),main="",par.settings=list(fontsize=list(text=17)))
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
##################################
