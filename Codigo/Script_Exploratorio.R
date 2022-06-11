library(maptools)
library(sp)
library(lattice)
library(tidyverse)
library(lubridate)
library(spdep)
library(janitor)
library(broom)
library(rgdal)
library(corrplot)
library(corrr)
library(plotly)    
library(glmnet)
load("TodoEstados.Rda")    
#
library(knitr)
library(ggfortify)

NuevoOrden <- c(1:4,7,8,5,6,9:32)
AUX <- rep(1:10, 32)+rep((10)*(NuevoOrden-1), each=10)
TodoEstadosS <- TodoEstados[AUX,]

#TodoEstadosS %>% View()

Economia <- TodoEstadosS %>% 
  .[,c(2,8:19)]
   
Economia_DF <- as.data.frame(Economia)
 rownames(Economia_DF) <- Economia$entidad_federativa
 dim(Economia_DF)
 Economia_Dist <- Economia_DF[,-1] %>% 
   scale() %>% 
   dist() 
 Economia_Dist %>% 
   hclust() %>% 
   plot()
 
 PCA <- Economia[,-1] %>% 
   prcomp(center=T, scale=T) %>% 
   .$x %>% 
   as_tibble() %>% 
   cbind(Economia)
 #PCA %>% ggplot(aes(PC1,-PC2, color=rol))+geom_point(size=3)
 RR <- Economia[,-1] %>% prcomp(center=T, scale=T) %>% 
   .$rotation
 #DF_Com2 <- bind_cols(DF,as_tibble(PCA))
 RR_DF <- as.data.frame(RR)*3
 PCA2 <- PCA #%>%   filter(pH==4) %>%   filter(Colorante=="Natural") %>%   filter(Nivel==0.06) 
 GG <- PCA %>% 
   ggplot( mapping=aes(x=PC1,y=PC2))+
     geom_text(data=PCA,aes(PC1,PC2, color=entidad_federativa,label=entidad_federativa),size=3,inherit.aes = FALSE )+
   geom_hline(aes(yintercept=0), size=.2) + geom_vline(aes(xintercept=0), size=.2)+
   geom_segment(data=RR_DF, aes(x=0, y=0, xend=PC1, yend=PC2 ), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="blue")+
   geom_text(data=RR_DF, aes(x=PC1, y=PC2 ,label=row.names(RR_DF)), alpha=0.75, color="blue",size=4)+
   theme_classic()
 GG
 GG %>% ggplotly()
 
 gg <- Economia %>% dplyr::select(where(is.numeric)) %>% 
   #cor(.,use="pairwise.complete.obs")  %>% 
   #correlate(method = "spearman") %>% 
   correlate(method = "pearson") %>% 
   gather("term2", "value", 2:13) %>% 
   mutate(term=factor(term, levels=unique(term)),term2=factor(term2, levels=unique(term2))) %>% 
   ggplot(aes(term, term2, color=value))+ 
   geom_point(size=12,shape=15)+
   theme(axis.text.x = element_text(angle = 45)) +
   scale_y_discrete(limits = rev)+
   scale_color_gradient2(midpoint=0,limits=c(-1,1), low="red", mid="white",high="blue", space ="Lab" )
 
 gg %>% 
   ggplotly()
 
 
 
 Economia <- TodoEstadosS %>% 
    .[,c(2,7,20:23)]
 
 
 Economia_DF <- as.data.frame(Economia)
 rownames(Economia_DF) <- Economia$entidad_federativa
 dim(Economia_DF)
 Textura_Dist <- Economia_DF[,-1] %>% 
    scale() %>% 
    dist() 
 Textura_Dist %>% 
    hclust() %>% 
    plot()
 
 PCA <- Economia[,-1] %>% 
    prcomp(center=T, scale=T) %>% 
    .$x %>% 
    as_tibble() %>% 
    cbind(Economia)
 #PCA %>% ggplot(aes(PC1,-PC2, color=rol))+geom_point(size=3)
 RR <- Economia[,-1] %>% prcomp(center=T, scale=T) %>% 
    .$rotation
 #DF_Com2 <- bind_cols(DF,as_tibble(PCA))
 RR_DF <- as.data.frame(RR)*3
 PCA2 <- PCA #%>%   filter(pH==4) %>%   filter(Colorante=="Natural") %>%   filter(Nivel==0.06) 
 GG <- PCA %>% 
    ggplot( mapping=aes(x=PC1,y=PC2))+
    geom_text(data=PCA,aes(PC1,PC2, color=entidad_federativa,label=entidad_federativa),size=3,inherit.aes = FALSE )+
    geom_hline(aes(yintercept=0), size=.2) + geom_vline(aes(xintercept=0), size=.2)+
    geom_segment(data=RR_DF, aes(x=0, y=0, xend=PC1, yend=PC2 ), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="blue")+
    geom_text(data=RR_DF, aes(x=PC1, y=PC2 ,label=row.names(RR_DF)), alpha=0.75, color="blue",size=4)+
    theme_classic() 
 GG
 GG %>% ggplotly()
 
 gg <- Economia %>% dplyr::select(where(is.numeric)) %>% 
    #cor(.,use="pairwise.complete.obs")  %>% 
    #correlate(method = "spearman") %>% 
    correlate(method = "pearson") %>% 
    gather("term2", "value", 2:6) %>% 
    mutate(term=factor(term, levels=unique(term)),term2=factor(term2, levels=unique(term2))) %>% 
    ggplot(aes(term, term2, color=value))+ 
    geom_point(size=12,shape=15)+
    theme(axis.text.x = element_text(angle = 45)) +
    scale_y_discrete(limits = rev)+
    scale_color_gradient2(midpoint=0,limits=c(-1,1), low="red", mid="white",high="blue", space ="Lab" )
 
 gg %>% 
    ggplotly()
 