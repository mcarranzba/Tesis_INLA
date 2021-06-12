library(animation)
load("TodoEstados.RData")

data <- cbind(as.data.frame(matrix(TodoEstados$EjSemestreV,32,10)),as.data.frame(matrix(TodoEstados$EsperadoV,32,10)))
data <- cbind(as.data.frame(matrix(TodoEstados$EnSemestreV,32,10)),as.data.frame(matrix(TodoEstados$EsperadoV,32,10)))
data <- cbind(as.data.frame(matrix(TodoEstados$AgSemestreV,32,10)),as.data.frame(matrix(TodoEstados$EsperadoV,32,10)))
data <- cbind(EjSemestre,Esperado)
#data <- cbind(AgSemestre,Esperado)
source("Nad_Wat_Bivariado.R", echo = F)
library(corrplot)

ani.options(
  convert = shQuote('C:/Program Files (x86)/ImageMagick-6.8.1-Q16/convert.exe')
)

require(rgdal)
require(ggplot2)

#shp <- rgdal::readOGR("gadm36_MEX_1.shp")
shp <- rgdal::readOGR("gadm36_MEX_1.shp")
i <- "2011-07-01"
saveGIF(
  {
    for (i in unique(TodoEstados$Semestre)) {
      DF <- filter(TodoEstados, Semestre==i)
      p <- GeneraMapa(DF$XX, DF$YY,DF$EnSemestreV, "Enfrentamientos Segundo Semestre 2011",1,c(-118,-85,1),c(14,33,1),1)
      p <- p + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
      plot(p)
    }
     
  },
  movie.name = "test.gif", 
  interval = 2, 
  ani.width = 1000, 
  ani.height = 700,
  outdir = getwd()
)

for (i in unique(TodoEstados$Semestre)) {
  DF <- filter(TodoEstados, Semestre==i)
  p <- GeneraMapa(DF$XX, DF$YY,DF$EnSemestreV, "PH",10,c(-116,-88,1),c(16,31,1),1)
  plot(p)
}

ggplot(TodoEstados,aes(Semestre,EjSemestreV,color=Estados))+ geom_line()

Cross <- TodoEstados %>% group_by(Estados) %>% 
  summarise(AvgEj=mean(EjSemestreV), AvgEn=mean(EnSemestreV),AvgAg=mean(AgSemestreV),lon=mean(XX), lat=mean(YY))

GeneraMapa(Cross$lon, Cross$lat,Cross$AvgEj, "PH",10,c(-116,-88,1),c(16,31,1),1)

GeneraMapa(Cross$lon, Cross$lat,Cross$AvgEj, "PH",10,c(-116,-88,1),c(16,31,1),1)
