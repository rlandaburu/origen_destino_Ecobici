library(sp)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(sf)

#Obtenemos el dataset del portal de BA Data. Nos entrega un csv con el nombre trips_2022.
# https://data.buenosaires.gob.ar buscar Sistema de transporte público.


origen_destino <- read.csv("trips_2022.csv")

# #Corregimos el dataset ya que la longitud y latitud de destino tienen errores.
# long = origen_destino$lat_estacion_destino  
# lat = origen_destino$long_estacion_destino
# 
# long = unlist(strsplit(long,","))
# long = as.numeric(long)
# long = long[long < -35]
# 
# origen_destino$lat_estacion_destino = lat
# origen_destino$long_estacion_destino = long

#rm(lat,long)
# 
# 
# write.csv(origen_destino,"trips_2022.csv")


data = origen_destino
rm(origen_destino)

data$o_d = paste(data$id_estacion_origen, data$id_estacion_destino, sep = "-")
origen_destino = data %>% group_by(o_d, lat_estacion_origen,long_estacion_origen,lat_estacion_destino,long_estacion_destino) %>% dplyr::summarise(viajes= n())

#Observamos la distribución de los viajes para cada una de las combinaciones.
a=ggplot(origen_destino, aes(viajes))
a+geom_boxplot(color="Black", fill="Blue",outlier.colour = "red")+
  labs(x="Viajes")+
  theme(axis.title=element_text(size=10,face="bold"))

más_frecuentes = origen_destino[which(origen_destino$viajes<4000&origen_destino$viajes>1000),]
frecuentes = origen_destino[which(origen_destino$viajes<1000&origen_destino$viajes>500),]
menos_frecuente = origen_destino[which(origen_destino$viajes<500&origen_destino$viajes>100),]
poco_frecuentes = origen_destino[which(origen_destino$viajes<100&origen_destino$viajes>20),]
insignificante = origen_destino[which(origen_destino$viajes<20&origen_destino$viajes>1),]

#Grafico 1
xquiet<- scale_x_continuous("", breaks=10)
yquiet<-scale_y_continuous("", breaks=10)
quiet<-list(xquiet, yquiet)


#Generamos las capas geográficas.
barrios <-st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/barrios/barrios.geojson")
radios <- st_read("https://bitsandbricks.github.io/data/CABA_rc.geojson")
subte_lineas <- st_read("http://bitsandbricks.github.io/data/subte_lineas.geojson")
subte_estaciones <- st_read("http://bitsandbricks.github.io/data/subte_estaciones.geojson")
red_ferroviaria <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte-y-obras-publicas/estaciones-ferrocarril/estaciones-de-ferrocarril.geojson")
ciclovias = st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte-y-obras-publicas/ciclovias/ciclovias_WGS84.geojson")

#Elegimos las combinaciones con más de 100 viajes al año y graficamos todas las capas
datas = origen_destino[which(origen_destino$viajes<4000&origen_destino$viajes>100),]
ggplot() + geom_sf(data= radios, fill= NA, color =  "#353032" , size = 0.00005 ,show.legend=FALSE )+ geom_sf(data = ciclovias, color= "green", size = 0.005)+ geom_sf(data = subte_estaciones, color = "white", size = 0.01) + geom_sf(data = subte_lineas, color = "white", size = 0.01)+
  geom_segment(data = datas, aes(x = datas$long_estacion_origen, y= datas$lat_estacion_origen, xend= datas$long_estacion_destino, yend =  datas$lat_estacion_destino, alpha = datas$viajes), col="orange", show.legend=FALSE )+ 
  scale_alpha_continuous(name= "Viajes",range = c(0.03, 0.3)) + 
  theme(panel.background = element_rect(fill='black',colour='black')) + 
  labs(title = "Viajes de Ecobici más frecuentes en el año 2022",subtitle = "Origenes y destinos con más de 100 viajes al año")+ 
 quiet

ggsave("VIAJES_BAECOBICI_2022.pdf", width = 20, height = 20, units = "cm")



#Usamos la misma data con menos capas para observar mejor.
ggplot()+ geom_sf(data= radios, fill= NA, color =  "#1B1212" , size = 0.00005 ,show.legend=FALSE )+
  geom_segment(data = datas, aes(x = datas$long_estacion_origen, y= datas$lat_estacion_origen, xend= datas$long_estacion_destino, yend =  datas$lat_estacion_destino, alpha = datas$viajes), col="orange", show.legend=FALSE )+ 
  scale_alpha_continuous(name= "Viajes",range = c(0.09, 0.3)) + 
  theme(panel.background = element_rect(fill='black',colour='black')) + 
  labs(title = "Viajes de Ecobici más frecuentes en el año 2022",subtitle = "Origenes y destinos con más 100 viajes al día")+ 
  quiet

ggsave("VIAJES_2022.pdf", width = 20, height = 20, units = "cm")


#Graficamos los viajes x día 
origen_destino$viajes_x_dia = (round(origen_destino$viajes / 360, digits = 2)) 
origen_destino_reduce = origen_destino[(which(origen_destino$viajes<4500&origen_destino$viajes>260)),]                                                                                                                                                                                                                        

ggplot() + geom_sf(data = radios, aes(fill = POBLACION/AREA_KM2), color =  "#002451" , show.legend=FALSE ) +
  scale_fill_stepsn(name= "Población/km2" , n.breaks = 4, colours = viridis::cividis(1))+
  geom_segment(aes(x = origen_destino_reduce$long_estacion_origen, y= origen_destino_reduce$lat_estacion_origen, xend= origen_destino_reduce$long_estacion_destino, yend =  origen_destino_reduce$lat_estacion_destino, alpha = origen_destino_reduce$viajes_x_dia), col="orange") + scale_alpha_continuous(name = "Viajes diarios 2022", range = c(0.007, 3), n.breaks = 10) + geom_sf(data = subte_lineas, color = "white", size = 0.05)+
  geom_sf(data = subte_estaciones, color = "black", size = 0.05) + geom_point(aes(y = origen_destino$lat_estacion_origen, x = origen_destino$long_estacion_origen), size = 0.06, alpha = .5)+
  labs(title = "Viajes de Ecobici más frecuentes en el año 2022",subtitle = "Origenes y destinos con más de un viajes por día")+ theme_bw()+ coord_sf() +quiet 

ggsave("VIAJES_ECOBICI_2022.pdf", width = 20, height = 20, units = "cm")



#Código de un tema acorde para graficar
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


