#Se cargan los paquetes que se usan

#Pauetes espaciales y de INLA
library(maptools)
library(sp)
library(lattice)
library(rgdal)
library(spdep)
library(INLA)

# Paquetes de tipo Tidy
library(tidyverse)
library(lubridate)
library(janitor)
library(broom)

#Paquetes para correlaciones, graficas interactivas, regularización y tablas Latex
library(corrplot)
library(corrr)
library(plotly)    
library(glmnet)
library(knitr)
library(ggfortify)
library(xtable)

#Se cargan los datos previamente limpiados y ordenados

load("TodoEstados.Rda")   

#Cargamos la información espacial de vecinos a nivel estado
mexico.tr <- rgdal::readOGR("gadm36_MEX_1.shp")

Estados
mexico.tr$NAME_1
mexico.adj <- poly2nb(mexico.tr)
#View(TodoEstados)
#W.mexico <- nb2mat(mexico.adj, style = "B") 
#W.mexico.rs <- nb2mat(mexico.adj, style = "W") 
mexico.tr$CMEDV2 <- mexico.tr$CMEDV
#mexico.tr$CMEDV2 [mexico.tr$CMEDV2 == 50.0] <- NA


#Definimos estructuras de interceptos, efectos fijos y efectos aleatorios

str_Fijos <- "+ DF$xcoord + DF$ycoord  + DF$PobAlimentaria +
  DF$CoefGini + 
  DF$desocupacion  + 
  DF$agricultura_ganader+      
  DF$comercio +   DF$construccion + 
  DF$gobierno_y_organismo + 
  DF$industria_extractiva + DF$industria_manufactur + 
  DF$restaurantes_y_servi + DF$servicios_diversos +
  DF$servicios_profesiona + 
  DF$servicios_sociales + 
  DF$transportes_comunic "  
str_Aleatorios <- '+
  f(dataP$ID.area,model="bym",graph=mexico.adj) +
  f(dataP$ID.year,model="ar1") +
  f(dataP$ID.year1,model="iid") +
  f(dataP$ID.area.year,model="iid")'

#Creamos base donde se guardaran los valores de criterios de selección
Resultados_DF <- tribble(
  ~ Evento, ~Estructura,~Salida, ~DIC, ~ Eff_PD, ~WAIC, ~logMLIK,~SUM_LOG_CPO, ~D_KS , ~pv_KM,
  "_",  "_", "_",0,0,0,0,0,0,0
)
Resultados_Coefs_Caracterizado_DF <- c()


#Cambiamos el orden de la base de datos para ajustar al del grafo con información de vecinos

NuevoOrden <- c(1:4,7,8,5,6,9:32)
AUX <- rep(1:10, 32)+rep((10)*(NuevoOrden-1), each=10)
TodoEstadosS <- TodoEstados[AUX,]

# Se calculan offsets
DF_Reset_Long <- TodoEstadosS %>% group_by(anio,semestre) %>% 
  mutate(Prop_Poblacion=poblacion/sum(poblacion)) %>% 
  mutate(Eje_offset=sum(Eje_fallecidos)*Prop_Poblacion, 
         Enf_offset=sum(Enf_eventos)*Prop_Poblacion, 
         Agr_offset=sum(Agr_eventos)*Prop_Poblacion) %>% 
  mutate(tiempo=yq(paste0(anio,"-",1+2*(semestre-1)))) %>% 
  mutate(tiempo_numerico=as.numeric(tiempo)) %>% 
  ungroup() %>% 
  rename(ejecuciones_fallecidos=Eje_fallecidos,
         enfrentamientos_fallecidos=Enf_fallecidos,
         agresiones_fallecidos=Agr_fallecidos,
         ejecuciones_eventos=Eje_eventos,
         enfrentamientos_eventos=Enf_eventos,
         agresiones_eventos=Agr_eventos,
         ejecuciones_offset=Eje_offset,
         enfrentamientos_offset=Enf_offset,
         agresiones_offset=Agr_offset
  )

# Se reescalan los datos para que sean comparables entre si

Escalar <- function(x){(x-mean(x,na.rm=T))/sd(x,na.rm=T)}
DF_Reset_Long <- DF_Reset_Long %>%
  mutate(across(c(3:4,7:23), Escalar))
DF_Reset <- DF_Reset_Long %>% pivot_longer(c(25:30,32:34), "variable", values_to = "valor") %>% 
  separate(variable, sep="_", into=c("Evento","Variable"))
#DF %>% View()


#Definimos posibles combinaciones de modelos que se evaluaran

Vec_Evento <- c("ejecuciones","enfrentamientos", "agresiones")
Vec_Estructura <- c("Solo Covariables", "Solo aleatorio", "Ambos efectos")
Vec_Salida <- c("Poisson","ZIP")

# Valores para probar el codigo sin correr el loop
#estructura <- "Ambos efectos"
#evento <- "agresiones"
#salida <- "ZIP"

#Se corre el loop que prepara y evalua todos los modelos

for(evento in Vec_Evento){
  for(estructura in Vec_Estructura){
    for(salida in Vec_Salida){
      
      # Se elige el tipo de evento a analizar
      DF <- DF_Reset
      if(evento=="ejecuciones"){
        low.vector <- DF %>% filter(Evento==evento,Variable=="fallecidos") %>% .[,"valor"]
      }else{
        low.vector <- DF %>% filter(Evento==evento,Variable=="eventos") %>% .[,"valor"]
      }
      E.vector <- DF %>% filter(Evento==evento,Variable=="offset") %>% .[,"valor"] 
      DF <-  DF %>% filter(Evento==evento,Variable=="offset")
      
      data.mexico = attr(mexico, "data")
      year <- rep(1:10,32)
      county<- rep(1:32,each=10)
      
      # Se prepara la estructura de efectos aleatorios
      
      data<- data.frame(y= low.vector, E= E.vector, ID.area=county, ID.area1=as.numeric(county), year=year,
                        ID.year = year, ID.year1=year, ID.area.year = seq(1,length(county)))
      dataP <- data %>% rename(y=valor, E=valor.1)
      d <- matrix(NA, ncol = 2, nrow = 320*2)
      d[1:320, 1] <- as.numeric(dataP$y!=0)
      d[320+which(dataP$y!=0), 2] <- dataP$y[which(dataP$y!=0)]
      #Intercept1 <- as.numeric(data$y!=0)
      #Intercept2 <- naniar::replace_with_na(data[,"y"],replace = list(x = -99))
      I1 <- c(rep(1, 320), rep(NA, 320))
      I2 <-  c( rep(NA, 320),rep(1, 320))
      
      #Se elige el número de interceptos adecuados según la función liga o variable de salida
      
      if(salida=="Poisson"){
        str_Base <- "Y ~ -1 + I1"
      }else{
        str_Base <- "Y ~ -1 + I1 + I2"
      }
      
      # Se elige el tipo de efectos
      
      if(estructura=="Solo Covariables"){
        formula.ST3<- as.formula(paste0(str_Base,str_Fijos))
      }else{
        if(estructura=="Solo aleatorio"){
          formula.ST3<- as.formula(paste0(str_Base,str_Aleatorios))
        }else{
          formula.ST3<- as.formula(paste0(str_Base,str_Fijos,str_Aleatorios))
        } 
      }
      
      # Se elige la función liga o variable de salida y se ejecuta el análisis
      
      if(salida=="Poisson"){
        I1 <- rep(1, 320)
        model.inla.ST3 <- inla(formula.ST3,family="poisson",data=list(Y = d[1:320]),E=dataP$E, 
                               control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, mlik=TRUE),#lincomb=lcs,
                               control.inla = list(lincomb.derived.only=TRUE))
      }else{
        DF <- rbind(DF,DF)
        dataP <- rbind(dataP,dataP)
        model.inla.ST3 <- inla(formula.ST3,family=c("binomial", "poisson"),data=list(Y = d),E=dataP$E, 
                               control.predictor=list(compute=TRUE), control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, mlik=TRUE),#lincomb=lcs,
                               control.inla = list(lincomb.derived.only=TRUE))
      }
      
      
      
      # Se recuperan los resultados y se recalculan los CPO y PIT en el caso del ZIP
      
      Resultados_Object <- summary(model.inla.ST3)
      Resultados_Object
      Num <- length(model.inla.ST3$cpo$cpo)
      if(salida=="Poisson"){
        dd <- model.inla.ST3$cpo$pit- runif(Num)*model.inla.ST3$cpo$cpo
        CPO_ZIP <- model.inla.ST3$cpo$cpo
      }else{
        CPO_Bernoulli <- model.inla.ST3$cpo$cpo[1:320]
        verosimilitud_en_cero <- CPO_Bernoulli*(dataP$y[1:320]==0)+(1-CPO_Bernoulli)*(dataP$y[1:320]!=0)
        PIT_POISSON <- model.inla.ST3$cpo$pit[321:640]
        PIT_POISSON[which(is.na(model.inla.ST3$cpo$pit[321:640]))] <- 0
        CPO_POISSON <- model.inla.ST3$cpo$cpo[321:640]
        CPO_POISSON[which(is.na(CPO_POISSON))] <- 0
        PIT_ZIP <- verosimilitud_en_cero + (1-verosimilitud_en_cero)*PIT_POISSON
        CPO_ZIP <- verosimilitud_en_cero+(1-verosimilitud_en_cero)*CPO_POISSON
        dd <- PIT_ZIP- runif(Num)*CPO_ZIP
      }
      
      SUM_LOG_CPO <- sum(log(CPO_ZIP))
      model.inla.ST3$cpo$cpo
      hist(dd)
      KS_TEXT <- ks.test(dd,"punif",0,1)
      
      #Se guardan los criterios como un nuevo renglón del dataframe de resultados
      
      Nuevo_Renglon <- list(evento, estructura, salida,
                            Resultados_Object$dic$dic,
                            Resultados_Object$neffp[1],
                            Resultados_Object$waic$waic,
                            Resultados_Object$mlik[1],
                            SUM_LOG_CPO,
                            KS_TEXT$statistic,
                            KS_TEXT$p.value)
      Resultados_DF <- rbind(Resultados_DF,Nuevo_Renglon)
      
      
      
      #Se guardan los coeficientes de las covariables
      
      Resultados_Coefs <- Resultados_Object$fixed %>% 
        as_tibble(rownames="nombres") %>% 
        mutate(nombres=str_replace(nombres,"DF\\|S\\|",replacement = "") ) %>% 
        clean_names() 
      
      Resultados_Coefs_Caracterizado <- Resultados_Coefs %>% 
        mutate(Evento=evento, Estructura=estructura, Salida=salida, 
               .before=nombres)
      Resultados_Coefs_Caracterizado_DF <- rbind(Resultados_Coefs_Caracterizado_DF,
                                                 Resultados_Coefs_Caracterizado)
      
    }
  }
}
# Se prepara la tabla de datos para analizarla

Resultados_DF  <- Resultados_DF  %>% 
  mutate(Evento=factor(Evento, levels=unique(Evento)))

Tabla_Salida <- Resultados_DF %>% 
  filter(Evento!="_") %>% 
  mutate(Evento =factor(Evento , levels=unique(Evento))) %>% 
  mutate(Estructura =factor(Estructura , levels=unique(Estructura))) %>% 
  mutate(Salida =factor(Salida , levels=unique(Salida))) %>% 
  rename(SLCPO=SUM_LOG_CPO, lpv_KM=pv_KM) %>% 
  mutate(lpv_KM=log10(lpv_KM))
levels(Tabla_Salida$Evento) <- list(Ejec  = "ejecuciones",
                                    Enfr = "enfrentamientos", Agre="agresiones")
levels(Tabla_Salida$Estructura) <- list(Fijos  = "Solo Covariables",
                                        Aleat = "Solo aleatorio", Ambos="Ambos efectos")
levels(Tabla_Salida$Salida) <- list(Pois  = "Poisson",
                                    ZIP = "ZIP")

#Se genera la tabla de resultados como .tex para importarla

print(xtable(Tabla_Salida , type = "latex"), file = "Tabla_Comparacion.tex")

# Se visualizan comparaciones de los criterios

Resultados_DF  %>%  filter(logMLIK>-2500) %>% 
  filter(Salida!="_") %>% 
  ggplot(aes(Evento,logMLIK,color=Estructura, shape=Salida))+
  geom_jitter(size=5, height = 0, width = 0.1, alpha=0.5)

Resultados_DF  %>%  #filter(SUM_LOG_CPO>-2500) %>% 
  filter(Salida!="_") %>% 
  ggplot(aes(Evento,SUM_LOG_CPO,color=Estructura, shape=Salida))+
  geom_jitter(size=5, height = 0, width = 0.1, alpha=0.5)

Resultados_DF %>% 
  filter(Salida!="_") %>% 
  ggplot(aes(Evento,D_KS,color=Estructura, shape=Salida))+
  geom_jitter(size=5, height = 0, width = 0.1, alpha=0.5)

# Se visualizan los coeficientes para poder comparar los modelos

GG <- Resultados_Coefs_Caracterizado_DF %>% 
  filter(Estructura!="Solo aleatorio") %>% 
  filter(Evento=="ejecuciones") %>% 
  filter(nombres!="I1" & nombres!="I2") %>% 
  mutate(nombres=factor(nombres, levels=unique(nombres))) %>%
  mutate(Evento=factor(Evento, levels=unique(Evento))) %>% 
  mutate(liminf= (x0_025quant), med=( x0_5quant), limsup=( x0_975quant )) %>%
  mutate(nombres=fct_rev(f = nombres)) %>% 
  ggplot(aes(x=nombres,y=med,ymin=liminf, ymax=limsup, color=Estructura))+
  geom_errorbar()  +
  geom_point()+
  coord_flip()+
  geom_hline(yintercept = 0)+
  ylim(-5,5)+
  #facet_grid(cols=vars(Evento), rows=vars(Salida))+
  facet_grid(cols=vars(Salida))+
  xlab("")+
  ylab("")

GG
ggplotly(GG)

# Gráficas exploratorias que se incluyen en la tesis

DF_Reset_Long %>% dplyr::select(where(is.numeric)) %>% 
  cor() %>% 
  corrplot()


GG <- DF %>% group_by(entidad_federativa) %>% 
  summarise(Gini=mean(CoefGini), liminf=quantile(Agr_eventos,0.25),
            limsup=quantile(Agr_eventos,0.75), med=median(Agr_eventos)) %>% 
  ggplot(aes(x=Gini,y=med,ymin=liminf, ymax=limsup,color=entidad_federativa))+
  geom_point()+
  geom_errorbar()
GG


GG <- DF %>% group_by(entidad_federativa) %>% 
  summarise(pobreza=mean(PobPatrimonio), liminf=quantile(Agr_eventos,0.25),
            limsup=quantile(Agr_eventos,0.75), med=median(Agr_eventos)) %>% 
  ggplot(aes(x=pobreza,y=med,ymin=liminf, ymax=limsup,color=entidad_federativa))+
  geom_point()+
  geom_errorbar()
GG

GG <- DF %>% group_by(entidad_federativa) %>% 
  mutate(Pob=mean(PobPatrimonio), Gini=mean(CoefGini)) %>% 
  ggplot(aes(PobPatrimonio,CoefGini, color=entidad_federativa, size=anio))+
  geom_point(alpha=0.5)+
  geom_text(aes(x=Pob,y=Gini+0.01, label=entidad_federativa),size=3)
GG

ggplotly(GG)
