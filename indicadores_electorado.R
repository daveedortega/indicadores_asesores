### Indicadores_electorales

# Preparamos entorno ----
pacman::p_load(tidyverse, janitor, leaflet,sf,scales,extrafont,ggthemes,gapminder)
dev.off()
rm(list=ls())
# Cargamos bdd ----

# padron_por_sexo
padron_ene22 <- read_csv("input/poblacion/padron_ine_ene2022.csv") %>% clean_names()
# padron por edad
padron_edad <- read_csv("input/poblacion/padron_rangos_edad_mayo22.csv") %>% clean_names()
#lista de municipios INE
municipios_cdmx_ine <- read_delim("input/poblacion/Catalogo de Municipios.csv",delim = "|") %>% clean_names()
colnames(municipios_cdmx_ine)[3] <- "clave_municipio"
# shape de alcaldías cdmx
alcaldias_cdmx <- read_sf("/Users/dortega/Desktop/Mapas CDMX/alcaldías_cdmx/alcaldias_cdmx.shp")

## Filtrar Padrón Rangos de Edad ----
# padron_por_rangoedad
padron_edad %>% colnames() %>% as.data.frame()

padron_jovenes <- padron_edad %>%select(nombre_entidad,clave_entidad,clave_distrito,nombre_municipio,clave_municipio,nombre_entidad,
                      padron_18_hombres,padron_18_mujeres,padron_19_hombres,padron_19_mujeres,padron_20_24_hombres,
                      padron_20_24_mujeres,padron_25_29_hombres,padron_25_29_mujeres
                      ) %>% pivot_longer(c(padron_18_hombres,padron_19_hombres,padron_20_24_hombres,
                                           padron_25_29_hombres),names_to = "rango_edad_h",values_to = "hombres") %>% 
  pivot_longer(c(padron_18_mujeres,padron_19_mujeres,padron_20_24_mujeres,
                 padron_25_29_mujeres),names_to = "rango_edad_m",values_to = "mujeres") %>% 
  filter(nombre_entidad!="RESIDENTES EXTRANJERO")

## Ordenamos rangos de edad
padron_jovenes$rango_edad_h <- gsub("padron_","",padron_jovenes$rango_edad_h)
padron_jovenes$rango_edad_m <- gsub("padron_","",padron_jovenes$rango_edad_m)
padron_jovenes$rango_edad_h <- gsub("_hombres","",padron_jovenes$rango_edad_h)
padron_jovenes$rango_edad_m <- gsub("_mujeres","",padron_jovenes$rango_edad_m)

## Mantenemos unicos porque se duplican por como funciona pivot longer
padron_jovenes <- padron_jovenes %>% mutate(mantener = ifelse(rango_edad_h==rango_edad_m,1,0)) %>% filter(mantener==1) %>%
  select(nombre_entidad,clave_entidad,clave_distrito,nombre_municipio,clave_municipio,rango_edad=rango_edad_h,hombres,mujeres)
## vemos por estado

jovenes_estado <- padron_jovenes %>% group_by(nombre_entidad,rango_edad) %>% summarise(hombres=sum(hombres),mujeres=sum(mujeres))

## Graficas ------
jovenes_estado %>% filter(nombre_entidad=="CIUDAD DE MEXICO") %>% pivot_longer(c(hombres,mujeres),names_to = "sexo",values_to = "padron") %>% 
  ggplot(aes(rango_edad,padron,group=sexo,fill=sexo))+
    geom_col(position = "stack")+
    geom_label(aes(label=comma(padron)),position="stack")+
  labs(x="Rango de Edad",y="",title="Número de Jóvenes en el padrón de la CDMX",subtitle="Por Sexo",caption="Fuente: INEGI: 05/2022",fill="Sexo")+
  theme(axis.title = element_text(),text = element_text("Arial",size=15),
        legend.text = element_text(size = 10))

jovenes_estado %>% filter(nombre_entidad=="CIUDAD DE MEXICO") %>% pivot_longer(c(hombres,mujeres),names_to = "sexo",values_to = "padron") %>% 
  group_by(sexo) %>% summarise(sum(padron))

## Organizamos padron por sexo ----
padron_cdmx <- padron_ene22 %>% filter(clave_entidad==9) %>% group_by(clave_municipio) %>% 
  summarise(hombres=sum(padron_h),mujeres=sum(padron_m)) %>% 
  left_join(municipios_cdmx_ine,by="clave_municipio") %>% select(clave_municipio,hombres,mujeres)

colnames(padron_cdmx)[1]<-"cve_mun"

alcaldias_cdmx <- alcaldias_cdmx %>% left_join(padron_cdmx,by="cve_mun") 

alcaldias_cdmx <- alcaldias_cdmx %>% mutate(muj_hom = hombres - mujeres )

# Mapa padron por sexo cdmx ----

# Creamos Mapa
mapa_padron_cdmx <- alcaldias_cdmx %>% leaflet() %>% addTiles()

# Color Pallette continuous number
pal <- colorNumeric(
  palette = colorRampPalette(c('pink', 'blue'))(length(alcaldias_cdmx$muj_hom)), 
  domain = alcaldias_cdmx$muj_hom)

# Labels
labels <- sprintf(
"Mujeres: <strong>%s</strong><br/>  Hombres: <strong>%s</strong>",
comma(alcaldias_cdmx$hombres), comma(alcaldias_cdmx$mujeres)
) %>% lapply(htmltools::HTML)


# Agregar elementos al mapa
mapa_padron_cdmx <- mapa_padron_cdmx %>% addPolygons(
  fillColor = ~pal(muj_hom),
  weight = 1,
  opacity = 0.8,
  color = "black",
  dashArray = "1",
  fillOpacity = 0.7,
  highlightOptions = highlightOptions(
    weight = 3,
    color = "#999",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))

# Legend
mapa_padron_cdmx <- mapa_padron_cdmx %>% addLegend(pal = pal, values = ~muj_hom, opacity = 0.7, title = "Predominancia de Electorado Mujer",
             position = "bottomright")





































