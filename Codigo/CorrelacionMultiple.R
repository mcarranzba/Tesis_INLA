load("TodoEstados.RData")
source("Nad_Wat_Bivariado.R", echo = F)
library(corrplot)


var1 <- length(TodoEstados)
corrplot(CC)
e <- 1
RR <- rep(0,dim(CC)[1])
for( e in 1:dim(CC)[1]){
  YY <- matrix(CC[e,][-e])
  XX <- CC[-e,-e]
  RR[e] <- t(YY)%*% solve(XX)%*% YY
}
VV <- TodoEstados[-c(1,2,3,4,27,28)]
dim(TodoEstados)
CC <- cor(VV)
Include <- rep(T,dim(VV)[2])
#Include[sample(1:dim(VV)[2],10)] <- rep(F,10)
which(Include)
Salen <- 12
KappaI <- c(50,rep(1,Salen))
KappaProp <- rep(1,dim(CC)[2])
i <- 1
while  (i<Salen & KappaI[i]>30){
  KappaI[i+1] <-  kappa(CC[which(Include),which(Include)])
  for( e in 1:dim(CC)[1]){
    IncAux <- Include
    IncAux[e] <- IncAux[e]==F
    KappaProp[e] <- kappa(CC[which(IncAux),which(IncAux)])
  }
  prop <- which.min(KappaProp)
  if ( KappaProp[prop]<KappaI[i+1]){
    Include[prop] <- Include[prop]==F
    i <- i+1
  }else{
    i <- Salen
  }
}
Include 
KappaI
names(VV)[which(IncAux)]
corrplot(cor(VV[which(IncAux),which(IncAux)]))


TT <- summary(lm(data=VV, formula = EjSemestreV~.))
TT$r.squared
VV <- TodoEstados[,-c((var1-1),var1)]
corrplot(cor(VV, method = "spearman"))

