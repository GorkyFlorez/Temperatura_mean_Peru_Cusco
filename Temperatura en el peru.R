#  Cargamos las Librerias ----------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
library(elevatr)
library(ggspatial)
library(tmap)
library(ggpubr)
# Cargammos los SHp del Peru ---------------------------------------------------------------
Bolivia           <- getData('GADM', country='Bolivia', level=0) %>% st_as_sf()
Brazil             <- getData('GADM', country='Brazil', level=0) %>% st_as_sf()
Chile              <- getData('GADM', country='Chile', level=0) %>% st_as_sf()
Ecuador            <- getData('GADM', country='Ecuador', level=0) %>% st_as_sf()
Peru          <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Per           <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
Cusco         <- subset(Per, NAME_1  == "Cusco")
Cusco_xy      <- cbind(Cusco, st_coordinates(st_centroid(Cusco$geometry)))
Prec        <- getData("worldclim", var = "tmean", res=0.5, lon=-74.8773, lat=-11.54012)
Cusco_box     = st_as_sfc(st_bbox(Cusco))
Prec_Peru    <- crop(Prec, Peru)
Prec_Peru    <- Prec_Peru <- mask(Prec_Peru,Peru)
PPAnual_Peru <- do.call("sum", unstack(Prec_Peru))

PPAnual_Per = PPAnual_Peru/100
Geo_data       <-  rasterToPoints(PPAnual_Per)
Geo_data_frame <-  data.frame(Geo_data)


Prec_Cusco     <- crop(PPAnual_Per, Cusco)
Prec_Cusco     <- Prec_Cusco<- mask(Prec_Cusco,Cusco)
Geo_dat       <-  rasterToPoints(Prec_Cusco)
Geo_data_fram <-  data.frame(Geo_dat)

colores<- c('#d8e2dc', '#8ecae6', '#023e8a', '#03045e', '#184e77', '#40916c', '#80b918',
            '#55a630','#aacc00','#d4d700','#eeef20','#ffff3f','#ff9e00','#ff9100','#ff6d00','#e36414'
            ,'#9a031e')

A = ggplot()+
  geom_sf(data=Bolivia, fill=NA, color="white", size=0.5)+
  geom_sf(data=Brazil, fill=NA, color="white", size=0.5)+
  geom_sf(data=Chile, fill=NA, color="white", size=0.5)+
  geom_sf(data=Ecuador, fill=NA, color="white", size=0.5)+
  geom_raster(data = Geo_data_frame,aes(x,y, fill = layer))+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30),
                       na.value = 'white',
                       labels = c("[0 -2] ","[2 - 4]", "[4 -6]", "[6 -8]", "[8 -10]", "[10 -12]",
                                  "[12 -14]","[14 - 16]","[16 - 18]","[18 - 20]","[20 - 22]","[22 - 24]"
                                  ,"[24 - 26]","[26 - 28]","[28 - 30]"),
                       name='Temperatura \nPromedio Anual ºC')+
  geom_sf(data=Cusco_box, fill=NA, color="white", size=1)+
  coord_sf(xlim = c(-81.3307, -67), ylim = c(-18.3518 ,-0.03747),expand = FALSE)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  theme_bw()+
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = gray(.4),
                                        linetype = "dashed", size = 0.4),
        axis.text.x  = element_text(face="bold", color="white", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="white", size=8),
        legend.position = c(0.10,0.3),
        plot.title = element_text(size = 16, hjust = 0.5, color = "white", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "white", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "white", family="serif", face = "italic"),
        legend.key.size = unit(0.4, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.7,"cm"), #ancho de cuadrados de referencia 
        legend.direction = "horizontal", #dirección de la leyenda
        legend.title=element_text(size=8, face = "bold"), #tamaño de titulo de leyenda
        text = element_text(size = 9, family = "Tahoma", color="black"),
        axis.title = element_text(face="bold", color="white"),
        legend.text=element_text(size=8))+
  annotate(geom = "text", x = -80, y = -1, hjust = 0, vjust = 1, 
           label = "Ecuador",size = 3, family="serif", color = "white",  fontface="italic")+
  annotate(geom = "text", x = -68, y = -15, hjust = 0, vjust = 1, angle = 90,
           label = "Bolivia",size = 3, family="serif", color = "white",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -8, hjust = 0, vjust = 1, 
           label = "Brasil",size = 3, family="serif", color = "white",  fontface="italic")+
  annotate(geom = "text", x = -73, y = -1, hjust = 0, vjust = 1, 
           label = "Colombia",size = 3, family="serif", color = "white",  fontface="italic")+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  ggtitle("Temperatura Promedio Anual ºC del Peru")+
  labs(subtitle="Ing.Gorky Florez Castillo",  x="Longitud",y="Latitud",tag="B)",
       caption="Fuente: Data: https://www.worldclim.org")

B = ggplot()+
  geom_sf(data=Bolivia, fill=NA, color="white", size=0.5)+
  geom_sf(data=Brazil, fill=NA, color="white", size=0.5)+
  geom_sf(data=Chile, fill=NA, color="white", size=0.5)+
  geom_sf(data=Ecuador, fill=NA, color="white", size=0.5)+
  geom_raster(data = Geo_data_fram,aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30),
                       na.value = 'white',
                       labels = c("[0 -2] ","[2 - 4]", "[4 -6]", "[6 -8]", "[8 -10]", "[10 -12]",
                                  "[12 -14]","[14 - 16]","[16 - 18]","[18 - 20]","[20 - 22]","[22 - 24]"
                                  ,"[24 - 26]","[26 - 28]","[28 - 30]"),
                       name='Temperatura \nPromedio Anual ºC')+
  geom_sf(data=Cusco, color="white", fill=NA, size=1)+
  geom_sf_label(data = Cusco_xy , aes(x= X, y=Y, label = NAME_2), size =2.5, color="black",alpha=0.4
               ,fontfamily = "serif",  fontface="italic")+
  coord_sf(xlim = c(-73.99586, -70.34588), ylim = c(-15.45848 ,-11.21135),expand = FALSE)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  theme_bw()+
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = gray(.4),
                                        linetype = "dashed", size = 0.4),
        axis.text.x  = element_text(face="bold", color="white", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="white", size=8),
        plot.title = element_text(size = 16, hjust = 0.5, color = "white", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "white", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "white", family="serif", face = "italic"),
        legend.position = c(0.10,0.3),
        legend.key.size = unit(0.4, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.7,"cm"), #ancho de cuadrados de referencia 
        legend.direction = "horizontal", #dirección de la leyenda
        legend.title=element_text(size=8, face = "bold"), #tamaño de titulo de leyenda
        text = element_text(size = 9, family = "Tahoma", color="black"),
        axis.title = element_text(face="bold", color="white"),
        legend.text=element_text(size=8))+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  ggtitle("Temperatura Promedio Anual ºC del Cusco")+
  labs(subtitle="Ing.Gorky Florez Castillo",  x="Longitud",y="Latitud",tag="B)",
       caption="Fuente: Data: https://www.worldclim.org")

#Fusionando los gráficos anteriores

C=ggplot() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 22), expand = FALSE) +
  annotation_custom(ggplotGrob(A), xmin = 0, xmax = 15, ymin = 0, ymax = 22) +
  annotation_custom(ggplotGrob(B), xmin = 15, xmax = 30, ymin = 0, ymax = 22) +
  theme(panel.background = element_rect(fill = "black"))
  

ggsave(plot = A,"Mapa/Peru_temperatura.png", units = "cm", width = 21, #ancho
       height = 29, #Largo
       dpi = 1200)
ggsave(plot = B,"Mapa/Cusco_Temperatura.png", units = "cm", width = 21, #ancho
       height = 29, #Largo
       dpi = 1200)
  