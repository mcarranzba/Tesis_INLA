#install.packages("kriging")
library(kriging)
df <- read.csv("KrigingCoord.csv",header = T)
Noestan <- c(199,  200,  201 , 202,  203,  661, 1817, 1818, 1819)
head(df)
names(df)
nnn <- length(df$ycoord[-Noestan])
x <- df$xcoord[-Noestan]+rnorm(nnn,0,1)
y <- df$ycoord[-Noestan] +rnorm(nnn,0,1)


mX <- (-117.27-(-87.26))/(max(x)-min(x))
mY <- (34.38-12.20)/(max(y)-min(y))
yy <- mY*(y-min(y))+12.20

xx <- -mX*(x-min(x))+(-117.27)

plot(xx,yy)
plot(shape,add=TRUE)


mX <- (-117.27-(-87.26))/(max(x)-min(x))
mY <- (32.38-14.20)/(max(y)-min(y))
yy <- mY*(y-min(y))+14.20

xx <- -mX*(x-min(x))+(-117.27)

plot(xx,yy)
plot(shape,add=TRUE)

#labbb <- df$CVEGEO
response <- Rescal$V9
response[which(is.na(response))] <- 0
nnn
kriged <- kriging(y, x, response, model = "spherical")
#kriged <- kriging(x, y,(1:2454), model = "spherical")
hist(response,xlim =c(0,10),breaks = 1000)
dim(Rescal)
kriged$nugget
kriged$range
kriged$sill

x <- c(2469560.279592499,2469560.279592499)


plot(kriged)
dim(kriged)
image(kriged, xlim = extendrange(y), ylim = extendrange(x))

lines(y,x,type="p")

plot(y,x)
length(y)

df <-  read.table("RealEstateValuation.csv", sep = ',',header = T)
head(df)
names(df)
df <- df[,-1]
dim(df)
length(df$Latitud)
x <- df$Latitud+rnorm(length(df$Latitud),0,0.0001)
y <- df$Longuitud +rnorm(length(df$Latitud),0,0.0001)
response <- df$PrecioUnidad

kriged <- kriging(x, y, response, model = "spherical")

kriged$nugget
kriged$range
kriged$sill

plot(kriged)
image(kriged, xlim = extendrange(x), ylim = extendrange(y))
image(kriged, xlim = extendrange(y), ylim = extendrange(x))
points(x,y)

install.packages("maps")
library(geoR);library(fields);library(maps)


#Combine the spatial coordinates in a 84x2 matrix
s<-cbind(y,x)
PM <- response
#Plot the data
plot.field.points(s,PM,map.border="county",cex=1.5)
X11()
#Estimate parameters by maximum likelihood:
ml <- likfit(data=PM,coords=s,
             fix.nugget=F,cov.model="spherical",
             ini = c(30, 5),nugget=5)
summary(ml)
#Create grid of prediction points:
sp1<-seq(min(s[,1]),max(s[,1]),length=100)
sp2<-seq(min(s[,2]),max(s[,2]),length=100)
sp<-expand.grid(sp1,sp2)
inCA<-map.where("state",x=sp[,1],y=sp[,2])
inCA[is.na(inCA)]<-"NA"
inCA<-inCA=="california"
#Perform ordinary Kriging (value of cov.pars and nugget are copied from mle output):
pred<-krige.conv(data=PM,coords=s,locations=sp,
                 krige=krige.control(cov.model="spherical",
                                     cov.pars=c(14.73,6.144),
                                     nugget=4.299))
pred$predict[!inCA]<-NA
pred$krige.var[!inCA]<-NA
#Plot the predicted values:
image.plot(sp1,sp2,matrix(pred$predict,100,100),zlim=range(PM))
map("county",add=T)
#Plot the standard errors:
image.plot(sp1,sp2,matrix(sqrt(pred$krige.var),100,100),zlim=c(0,3.5))
map("county",add=T)
points(s)
