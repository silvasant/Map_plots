## Analisis de datos de accidentes de tránsito de Inglaterra (Westminster en particular)

library(ggplot2)
library(ggmap)
library(maps)
library(tidyverse)


register_google("YOUR KEY HERE")


#library(broom)

##Cargo el dataset:
getwd()
project_dir<-choose.dir()
setwd(project_dir)

getwd()

file_dir<-choose.dir()


UK_road_accidents<-read.csv(file=file.choose())
#View(head(UK_road_accidents))
#View(UK_road_accidents[UK_road_accidents$Local_Authority_.District.==1,])


Westminster_road_accidents<-UK_road_accidents[c(UK_road_accidents$Local_Authority_.District.==1,UK_road_accidents$Accident_Severity==3),]


#Dataset sin NA's

#funcion para remover NA's
f4 <- function(dat) {
  dat %>% filter(Reduce(`+`, lapply(., is.na)) != ncol(.))
}

UK_road_accidents<-f4(UK_road_accidents)

#View(Westminster_road_accidents)

filas<-(length(Westminster_road_accidents$Date)-2999):length(Westminster_road_accidents$Date)

Westminster_relevant<-Westminster_road_accidents[,c(4,5,10,26,27)]
#View(Westminster_relevant)
length(Westminster_relevant$Date)


#View(Westminster_relevant[filas,])

Westminster<-as.tibble(Westminster_relevant)

Westminster
View(Westminster)



Westminster<-f4(Westminster)
View(Westminster)


Westminster2<-filter(Westminster,Westminster$Weather_Conditions!=1,Westminster$Road_Surface_Conditions!=1)

View(Westminster2)

Westminster3<-filter(Westminster,Westminster$Weather_Conditions!=c(1,2),Westminster$Road_Surface_Conditions!=c(1,2))
View(Westminster3)
set.seed(132456)
random_index<-round(runif(200,0,length(Westminster2$Date)),0)

Datos_plot<-Westminster2[1000:1010,]
Datos_plot2<-Westminster2[random_index,]
Datos_plot3<-Westminster3[random_index,]
###################### Obetngo los mapas necesarios
Westminster_map<-get_googlemap("Hyde park, London",scale = 2,zoom = 13)
ggmap(Westminster_map)
###################### Graficos de prueba #####
Accidents_plot<-ggmap(Westminster_map)
Accidents_plot + 
  geom_point(data = Datos_plot,aes(x=Datos_plot$Longitude,y=Datos_plot$Latitude),color="red",size=3)

Accidents_plot2<-ggmap(Westminster_map)

Accidents_plot2 + 
  geom_point(data = Datos_plot2,aes(x=Datos_plot2$Longitude,y=Datos_plot2$Latitude),color=factor(Datos_plot2$Weather_Conditions),size=1)

Accidents_plot3<-ggmap(Westminster_map)
Accidents_plot3 + 
  geom_point(data = Datos_plot3,aes(x=Datos_plot3$Longitude,y=Datos_plot3$Latitude),color=factor(Datos_plot2$Weather_Conditions),size=1)

Westminster_accident_grid<-ggmap(Westminster_map)

Westminster_accident_grid+ 
  stat_bin2d(
    data=Westminster2,
    aes(x=Westminster2$Longitude,y=Westminster2$Latitude),
    size=0.5,
    alpha=0.5,
    bins=30
    ) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  scale_fill_gradientn(colours = terrain.colors(10),name="Nro de accidentes")+
  scale_x_continuous(name="Longitud")+
  scale_y_continuous(name="Latitud")+
  ggtitle("Accidentes de tránsito \nen el área de Westminster, Londres \nAños 2005-2015")
  


  



