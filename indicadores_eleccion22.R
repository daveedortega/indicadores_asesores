## Indicadores Elecciones 2022 
## D ortega. - 06/06/22

## Preparar Espacion ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,janitor,scales,sf,leaflet,htmltools,htmlwidgets)
## Cargar preps----

ags_22 <- read_csv("input/elecciones2022/AGS_GUB_2022.csv",skip = 4, 
                   locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

ags_22$seccion <- gsub("=\\\"","",ags_22$seccion)
ags_22$seccion <- gsub("\\\"","",ags_22$seccion)

## Mapa ags
mapa_ags <- read_sf("~/Desktop/Mapas/marco_electoral_2021/01 AGUASCALIENTES/SECCION.shp") 
mapa_ags <- mapa_ags[st_is_valid(mapa_ags),] # hay un NA medio grande en un distrino, luego lo arreglo

## Aguascalientes
ags_22$morena <- as.numeric(ags_22$morena)
ags_22$co_pan_pri_prd <- as.numeric(ags_22$co_pan_pri_prd)
ags_22$co_pan_prd <- as.numeric(ags_22$co_pan_prd)
ags_22$co_pan_pri <- as.numeric(ags_22$co_pan_pri)
ags_22$co_pri_prd <- as.numeric(ags_22$co_pri_prd)
ags_22$pan <- as.numeric(ags_22$pan)
ags_22$pri <- as.numeric(ags_22$pri)
ags_22$prd <- as.numeric(ags_22$prd)

ags_22$total_votos_calculado <- as.numeric(ags_22$total_votos_calculado)
ags_22$lista_nominal <- as.numeric(ags_22$lista_nominal)

# participación. prian . morena
prian_morena_ags <- ags_22 %>% group_by(seccion) %>% summarise(lista_nominal=sum(lista_nominal,na.rm = T),
                                           total_votos_calculado=sum(total_votos_calculado,na.rm = T),
                                           morena=sum(morena,na.rm = T),
                                           prian=sum(co_pan_pri_prd,pan,pri,prd,co_pan_pri,co_pan_prd,co_pri_prd,na.rm = T)) %>% 
  mutate(participacion=round(total_votos_calculado*100/lista_nominal,2)) %>% 
  mutate(SECCION=as.numeric(seccion)) %>% 
  mutate(prian_vs_morena=morena-prian)


mapa_ags <- mapa_ags %>% left_join(prian_morena_ags) 
mapa_ags <- mapa_ags %>% st_transform(4326)








# color pallette
pal <- colorNumeric("GnBu",mapa_ags$prian_vs_morena,na.color = "red",reverse = T)

# labels
labels <- sprintf(
  "<strong>Sección: %g</strong><br/> Poicentaje de participación %g <br/> Diferencia de Votos: %g",
  mapa_ags$SECCION,mapa_ags$participacion, mapa_ags$prian_vs_morena
) %>% lapply(htmltools::HTML)

rr <- tags$div(
  HTML('<a href="https://www.prep2022-ags-iee.mx/prep-ags2022.html#!/G/ENT/PC?tipoRep=graph"> <img border="0" alt="Diferencia de Votos en Aguascalientes: Votos Morena - PRIANRD" src="/Users/dortega/Desktop/Asesores - CDMX/indicadores/fotos/logo_iee.png" width="200" height="300"> </a>')
)  

mapa_ags21 <- leaflet(mapa_ags) %>% addTiles() %>% addPolygons(fillColor = ~pal(prian_vs_morena),
                                                           weight = 0.5,
                                                           opacity = 1,
                                                           color = "grey",
                                                           dashArray = "1",
                                                           fillOpacity = 0.7, highlightOptions = highlightOptions(
                                                             weight = 2,
                                                             color = "#ffffff",
                                                             dashArray = "",
                                                             fillOpacity = 0.7,
                                                             bringToFront = TRUE),
                                                           label = labels,
                                                           labelOptions = labelOptions(
                                                             style = list("font-weight" = "normal", padding = "3px 8px"),
                                                             textsize = "15px",
                                                             direction = "auto")) %>%
  addLegend(pal = pal, values = ~prian_vs_morena, opacity = 0.7, title = NULL,
                        position = "bottomright") %>% addControl(rr, position = "topleft")

## 


























