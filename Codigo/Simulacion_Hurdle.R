library(INLA)
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(tidyverse)
library(arm) #contiene invlogit
library(mads) # contiene poisson truncada
library(truncdist)
library(actuar)

#install.packages("truncdist")
#Simulacion de una Bernoulli
PP <- 0.7
d <- data.frame(Y=rbinom(n = 100,size = 1,prob=PP))
formula <- Y ~ 1
r.Bern <- inla(formula, family=c('binomial'),
               data=  d, verbose=F,
               control.predictor=list(compute=TRUE),
               control.compute=list(dic=TRUE, cpo=TRUE))
Resultados <- r.Bern %>% 
  summary()

#logit(mean(d$Y))
mean(d$Y)
invlogit(Resultados$fixed[1])
Num <- length(d$Y)
dd <- Resultados$cpo$pit- runif(Num)*Resultados$cpo$cpo
hist(dd)
ks.test(dd,"punif",0,1)
#Simulacion de una Poisson


d <- data.frame(Y=rpois(n = 100,lambda = 5))
formula <- Y ~ 1
r.Poisson <- inla(formula, family=c('poisson'),
               data=  d, verbose=F,
               control.predictor=list(compute=TRUE),
               control.compute=list(dic=TRUE, cpo=TRUE))
Resultados <- r.Poisson %>% 
  summary()
#log(mean(d$Y))
mean(d$Y)
exp(Resultados$fixed[1])
dd <- Resultados$cpo$pit- runif(Num)*Resultados$cpo$cpo
hist(dd)
ks.test(dd,"punif",0,1)

#Simulacion de un Hurdle (Solo ceros estructurales)

NN <- 100
Bin <- rbinom(n = NN,size = 1,prob=0.8)
#Poi <- rtpois(n = 100,a = 0,lambda = 5)*Bin
Poi <- rtrunc(n=100, "pois", a = 0, b = Inf, lambda = 5)*Bin
#Poi <- rpois(n = NN,lambda = 5)*Bin
hist(Poi, breaks = 100)
PoiNA <- na_if(Poi, 0)

d <- matrix(NA, ncol = 2, nrow = NN*2)
d[1:NN, 1] <- as.numeric(Poi!=0)
d[NN+which(Poi!=0), 2] <- Poi[which(Poi!=0)]
#Intercept1 <- c(as.numeric(Poi!=0), rep(NA, 100))
Intercept1 <- c(rep(1, NN), rep(NA, NN))
Intercept2 <-  c( rep(NA, NN),rep(1, NN))


r.Hurdle <- inla(Y ~ -1 + I1 + I2 ,
     data = list(Y = d, I1 = Intercept1, I2 = Intercept2),
     family = c("binomial", "poisson"),
     control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, mlik=TRUE))
summary(r.Hurdle)
Resultados <- r.Hurdle %>% 
  summary()
logit(mean(Poi!=0))
log(mean(Poi[Poi!=0]))
logit(0.8)
invlogit(1.2)
exp(1.542)

dd <- Resultados$cpo$pit- runif(Num)*Resultados$cpo$cpo
hist(dd,breaks = 20)
ks.test(dd,"punif",0,1)

  hist(II$cpo$pit,xlab="PIT",ylab="Frecuencia", main="Histograma PIT")
  
  
#Simulacion de un Hurdle con una covariable independiente
xGen <- rnorm(100)
x <- rep(xGen,2)

Bin <- rbinom(n = 100,size = 1,prob=0.7)
Poi <- rtrunc(n=100, "pois", a = 0, b = Inf, lambda = 5)*Bin
hist(Poi)
PoiNA <- na_if(Poi, 0)

d <- matrix(NA, ncol = 2, nrow = 200)
d[1:100, 1] <- as.numeric(Poi!=0)
d[100+which(Poi!=0), 2] <- Poi[which(Poi!=0)]
#Intercept1 <- c(as.numeric(Poi!=0), rep(NA, 100))
Intercept1 <- c(rep(1, 100), rep(NA, 100))
Intercept2 <-  c( rep(NA, 100),rep(1, 100))


r.Hurdle <- inla(Y ~ -1 + I1 + I2 + x,
                 data = list(Y = d, I1 = Intercept1, I2 = Intercept2),
                 family = c("binomial", "poisson"),
                 control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, mlik=TRUE))
summary(r.Hurdle)
Resultados <- r.Hurdle %>% 
  summary()
-logit(mean(Poi==0))
log(mean(Poi[Poi!=0]))

#log(mean(d$Y))
dd <- Resultados$cpo$pit- runif(Num)*Resultados$cpo$cpo
hist(dd)
ks.test(dd,"punif",0,1)

#Simulacion de un Hurdle con una covariable
NN <- 300
AGen <- rnorm(NN)
BGen <- rnorm(NN)
CGen <- rnorm(NN)
AA <- rep(AGen,2)
BB <- rep(BGen,2)
CC <- rep(CGen,2)
Vrbinom <- Vectorize(rbinom, vectorize.args = "prob")
Bin <- Vrbinom(n = 1,size = 1,prob=invlogit(logit(0.8)+0.3*AGen-0.5*BGen+0.7*CGen))
Vrztpois <- Vectorize(rztpois, vectorize.args = "lambda")
Poi <- Vrztpois(n=1,  lambda = exp(log(5)+0.3*AGen-0.5*BGen+0.7*CGen))*Bin
hist(Poi,breaks = NN)
PoiNA <- na_if(Poi, 0)
hist(PoiNA)

d <- matrix(NA, ncol = 2, nrow = NN*2)
d[1:NN, 1] <- as.numeric(Poi!=0)
d[NN+which(Poi!=0), 2] <- Poi[which(Poi!=0)]
#Intercept1 <- c(as.numeric(Poi!=0), rep(NA, 100))
Intercept1 <- c(rep(1, NN), rep(NA, NN))
Intercept2 <-  c( rep(NA, NN),rep(1, NN))



r.Hurdle <- inla(Y ~ -1 + I1 + I2 + AA + BB + CC,
                 data = list(Y = d, I1 = Intercept1, I2 = Intercept2),
                 family = c("binomial", "poisson"),
                 control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, mlik=TRUE))

Poi
HH <- data.frame(Poi,AA,BB,CC)
HH %>% ggplot(aes(AA,Poi))+
  geom_point()
HH %>% ggplot(aes(AA,CC,size=Poi))+
  geom_point()
HH %>% ggplot(aes(CC,Poi))+
  geom_point()

exp(c(0.3,-0.5,0.7))

invlogit(c(0.3,-0.5,0.7))
  
exp(c(0.3,-0.5,0.7))
    
    summary(r.Hurdle)
Resultados <- r.Hurdle %>% 
  summary()

logit(0.8)
log(5)
Num <- 300
#log(mean(d$Y))
dd <- Resultados$cpo$pit- runif(Num)*Resultados$cpo$cpo
hist(dd, breaks=20)
plot(ecdf(dd))
abline(0,1,color="red")   
ks.test(dd,"punif",0,1)



NN <- 1000
RR <- rnorm(NN)
EE <- cumsum(RR+0.5)
plot(EE, type="l")
PP <- 1:NN
PP1 <- 1:NN
II <- inla(Y ~ -1 +f(PP,model="ar1") ,
           data = list(Y = EE))
II %>% summary()
