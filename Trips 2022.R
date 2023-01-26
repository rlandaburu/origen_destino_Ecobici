library(sp)
library(maptools)
library(tidyverse)
library(leaflet)
library(plyr)
library(dplyr)
library(ggplot2)
library(sf)
library(geosphere)
library(dbscan)
library(ggmap)
library(tidyverse)


# data = read.csv("~/Ecobici/trips_2022.csv")
# data$o_d = paste(data$id_estacion_origen, data$id_estacion_destino, sep = "-")
# 
# origen_destino = data %>% group_by(o_d, lat_estacion_origen,long_estacion_origen,lat_estacion_destino,long_estacion_destino) %>% dplyr::summarise(viajes= n())
# 
# write.csv(origen_destino, "~/Ecobici/origen_destino_2022.csv")

origen_destino = read.csv("~/Ecobici/origen_destino_2022.csv")

# long = origen_destino$lat_estacion_destino  
# lat = origen_destino$long_estacion_destino
# 
# origen_destino$lat_estacion_destino = lat
# origen_destino$long_estacion_destino = long
# 
# write.csv(origen_destino, "~/Ecobici/origen_destino_2022.csv")

a=ggplot(origen_destino, aes(viajes))
a+geom_boxplot(color="Black", fill="Blue",outlier.colour = "red")+
  labs(x="Viajes")+
  theme(axis.title=element_text(size=10,face="bold"))



xquiet<- scale_x_continuous("", breaks=200)
yquiet<-scale_y_continuous("", breaks=200)
quiet<-list(xquiet, yquiet)

más_frecuentes = origen_destino[which(origen_destino$viajes<4000&origen_destino$viajes>1000),]
frecuentes = origen_destino[which(origen_destino$viajes<1000&origen_destino$viajes>500),]
menos_frecuente = origen_destino[which(origen_destino$viajes<500&origen_destino$viajes>100),]
poco_frecuentes = origen_destino[which(origen_destino$viajes<100&origen_destino$viajes>20),]
insignificante = origen_destino[which(origen_destino$viajes<20&origen_destino$viajes>1),]

#Grafico 1
xquiet<- scale_x_continuous("", breaks=10)
yquiet<-scale_y_continuous("", breaks=10)
quiet<-list(xquiet, yquiet)

ggplot(origen_destino[which(origen_destino$viajes<4000&origen_destino$viajes>100),], aes(long_estacion_origen, lat_estacion_origen)) + 
  geom_segment(aes(x = long_estacion_origen, y= lat_estacion_origen, xend= long_estacion_destino, yend =  lat_estacion_destino, alpha = viajes), col="white")+
  scale_alpha_continuous(range = c(0.03, 0.3)) + theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()


radios <- st_read("https://bitsandbricks.github.io/data/CABA_rc.geojson")
subte_lineas <- st_read("http://bitsandbricks.github.io/data/subte_lineas.geojson")
subte_estaciones <- st_read("http://bitsandbricks.github.io/data/subte_estaciones.geojson")


#Graficamos
origen_destino$viajes_x_dia = (round(origen_destino$viajes / 360, digits = 2)) 
  
origen_destino_reduce = origen_destino[(which(origen_destino$viajes<4500&origen_destino$viajes>260)),]                                                                                                                                                                                                                        



ggplot() + geom_sf(data = radios, aes(fill = POBLACION/AREA_KM2), color =  "#002451" , show.legend=FALSE ) +
  scale_fill_stepsn(name= "Población/km2" , n.breaks = 4, colours = viridis::cividis(1))+
 geom_segment(aes(x = origen_destino_reduce$long_estacion_origen, y= origen_destino_reduce$lat_estacion_origen, xend= origen_destino_reduce$long_estacion_destino, yend =  origen_destino_reduce$lat_estacion_destino, alpha = origen_destino_reduce$viajes_x_dia), col="orange") + scale_alpha_continuous(name = "Viajes diarios 2022", range = c(0.007, 3), n.breaks = 10) + geom_sf(data = subte_lineas, color = "white", size = 0.05)+
  geom_sf(data = subte_estaciones, color = "black", size = 0.05) + geom_point(aes(y = origen_destino$lat_estacion_origen, x = origen_destino$long_estacion_origen), size = 0.06, alpha = .5)+
  labs(title = "Viajes de Ecobici más frecuentes en el año 2022",subtitle = "Origenes y destinos con más de un viajes por día")+ theme_bw()+ coord_sf() +quiet 

ggsave("VIAJES_ECOBICI_2022.pdf", width = 20, height = 20, units = "cm")


theme_ipsum_map_dark <- function(base_size = 24, title_size = 60, 
                                 subtitle_size = 48, caption_size = 40) {
  
  color.background = "black"
  color.title = "gray80"
  color.axis.title = "gray80"
  
  theme_ipsum(base_size=base_size) +
    theme(rect=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background
                                       , color=color.background)) +
    theme(panel.border=element_rect(color=color.background, fill = NA)) +
    theme(panel.background = element_rect(fill = color.background)) +
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(colour=color.axis.title)) +
    theme(legend.title = element_blank(), legend.position="top",
          legend.direction="horizontal") +
    theme(legend.key.width=unit(1, "cm"), legend.key.height=unit(0.25, "cm"),
          legend.spacing = unit(-0.5,"cm")) +
    theme(plot.title=element_text(colour=color.title, size = title_size)) +
    theme(plot.subtitle=element_text(colour=color.title, size = subtitle_size)) +
    theme(plot.caption=element_text(colour=color.title, size = caption_size)) +
    theme(axis.text.x=element_blank()) +
    theme(axis.text.y=element_blank()) +
    theme(axis.title.y=element_blank()) +
    theme(axis.title.x=element_blank()) +
    theme(strip.background = element_rect(fill=color.background,
                                          color=color.background),
          strip.text = element_text(colour=color.axis.title))
  
}

alpha_range = c(0.1, 0.8)
size_range = c(0.2, 0.4)

