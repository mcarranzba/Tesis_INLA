load("TodoEstados.RData")
library(tidyverse) 
library(Matrix)
library(plotly)
#invPerm
Orden_Pob_Pobr <- c(1:4,7,8,5,6,9:32)
Orden_Pob_Pobr_Inv <- invPerm(Orden_Pob_Pobr)
Orden_Econ <- c(1:6,9,7,8,10:32)
Orden_Econ_Inv <- invPerm(Orden_Econ)
TodoEstados


TodoEstados
#NuevoOrden <- c(1:4,7,8,5,6,9:32)

#Orden_Econ <- c(1:4,7,8,9,5,6,10:32)
names(TodoEstados)

DF <- TodoEstados
DF <- DF %>% mutate(Poblacion=as.numeric(as.character(Poblacion))) %>% 
  mutate(Poblacion=as.numeric(as.character(Poblacion)))

AUX <- rep(Orden_Pob_Pobr_Inv, 10)+rep((0:9)*32,each=32)
DF [, c("EjSemestreV","EnSemestreV","AgSemestreV","Poblacion" ,"EspEjV", "EspEnV", "EspAgV","PobrAlim","PobrCapa","PobrPatri","CoehSocGini","Desocup" )]<- TodoEstados[AUX, c("EjSemestreV","EnSemestreV","AgSemestreV","Poblacion" ,"EspEjV", "EspEnV", "EspAgV","PobrAlim","PobrCapa","PobrPatri","CoehSocGini","Desocup" )]
AUX <- rep(Orden_Econ_Inv, 10)+rep((0:9)*32, each=32)
DF [, c("Agric","ExtrElec","Manuf","Constr" ,"Comer" , "RestHot", "TransComun","ServProf","ServSoc","ServDiv", "GobIntern" ,"NoEspec"  )]<- TodoEstados[AUX, c("Agric","ExtrElec","Manuf","Constr" ,"Comer" , "RestHot", "TransComun","ServProf","ServSoc","ServDiv", "GobIntern" ,"NoEspec" )]



gg <- DF %>% ggplot(aes(Semestre,as.numeric(as.character(Poblacion)), group=Estados, color=Estados ))+geom_line()
ggplotly(gg)



gg <- DF %>% ggplot(aes(Semestre,as.numeric(as.character(EjSemestreV)), group=Estados, color=Estados ))+geom_line()
ggplotly(gg)

gg <- DF %>% ggplot(aes(Semestre,as.numeric(as.character(EjSemestreV))/as.numeric(as.character(Poblacion)), group=Estados, color=Estados ))+geom_line()
ggplotly(gg)


DF$Poblacion
#library(Matrix)
#invPerm(NuevoOrden)
#invPerm(c(4,2,7,3,1,5,6))
library("rgdal")
library(spdep)
mexico.tr <- rgdal::readOGR("gadm36_MEX_1.shp")

Estados <- read.csv("Estados.txt",header = F)
load("TodoEstados.RData")

TodoEstados[rep(10,),c()]


   

Estados
mexico.tr$NAME_1
mexico.adj <- poly2nb(mexico.tr)
#View(TodoEstados)
W.mexico <- nb2mat(mexico.adj, style = "B") 
W.mexico.rs <- nb2mat(mexico.adj, style = "W") 
mexico.tr$CMEDV2 <- mexico.tr$CMEDV
mexico.tr$CMEDV2 [mexico.tr$CMEDV2 == 50.0] <- NA

data <- cbind(as.data.frame(matrix(TodoEstados$EjSemestreV,32,10)),as.data.frame(matrix(TodoEstados$EspEjV,32,10)))
data <- cbind(as.data.frame(matrix(TodoEstados$EnSemestreV,32,10)),as.data.frame(matrix(TodoEstados$EspEnV,32,10)))
data <- cbind(as.data.frame(matrix(TodoEstados$AgSemestreV,32,10)),as.data.frame(matrix(TodoEstados$EspAgV,32,10)))




low.vector <- as.vector(as.matrix(data[,1:10]))#by column
E.vector <- as.vector(as.matrix(data[,11:20]))#by column
year <- numeric(0)
for(i in 1:10){ 
  year<- append(year,rep(i,dim(data)[1]))
}
county<- as.factor(rep(data[,1],10))

data<- data.frame(y= low.vector, E= E.vector, ID.area=as.numeric(county), ID.area1=as.numeric(county), year=year,
                  ID.year = year, ID.year1=year, ID.area.year = seq(1,length(county)))



library(INLA)

formula.ST3<- y ~ 1 +  DF$XX + DF$YY + DF$XXYY + DF$PobrAlim + DF$CoehSocGini + DF$Desocup  + DF$ServProf + DF$ServDiv + DF$GobIntern  + f(ID.area,model="bym",graph=mexico.adj) +
  f(ID.year,model="ar1") + f(ID.year1,model="iid") + f(ID.area.year,model="iid")

#To obtain the marginal of phij + gammaj we need to create the corresponding linear combinations and include these in the model 
lcs = inla.make.lincombs(ID.year = diag(10),  ID.year1 = diag(10))

model.inla.ST3 <- inla(formula.ST3,family="poisson",data=data,E=E, 
                       control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, mlik=TRUE),#lincomb=lcs,
                       control.inla = list(lincomb.derived.only=TRUE))

model.inla.ST3 <- inla(formula.ST3,family="zeroinflatedpoisson1",data=data,E=E, 
                       control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, mlik=TRUE),#lincomb=lcs,
                       control.inla = list(lincomb.derived.only=TRUE))
#

summary(model.inla.ST3)


formula <- Y ~ 1 + mu.z + mu.o +
  f(idx1,model = "bym2", graph=g, scale.model = TRUE,
    constr = TRUE,
    hyper=list(phi = list(prior = "pc",
                          param = c(0.5, 2/3), initial = 3),
               prec = list(prior = "pc.prec",
                           param = c(1, 0.01),
                           initial = 1.5))) +
  f(idx2, copy=’idx1’, fixed=FALSE)

r.bym2 <- inla(formula, family=c('binomial', 'poisson'),
                 data= Diseasedata, E=E, verbose=F,
                 control.predictor=list(compute=TRUE, link=fam),
                 control.compute=list(dic=TRUE, cpo=TRUE))

