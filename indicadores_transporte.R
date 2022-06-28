## Estadísticos de Educación
# David A. Ortega 
# 16/05/22

## Preparar Espacio ----
pacman::p_load(tidyverse, janitor,jsonlite,sf)
dev.off()
rm(list=ls())

## Cargar Bases de Datos----

url <- "https://datos.cdmx.gob.mx/api/3/action/datastore_search?resource_id="
id_trole <- "36fb2606-9f11-4877-9149-d0b40b996551"
id_tren <- "c6f15e48-791d-4ed6-adc3-8d93ed80a055"
id_cable <- "176c0d20-0111-43bc-903c-d7e807ff37c0"
id_metro <- "cce544e1-dc6b-42b4-bc27-0d8e6eb3ed72"
id_mb <- "d122639e-a56a-4f26-a8b7-983464d11aaa"
limite <- "&limit=32000"

# Consultas
trole <- fromJSON(paste0(url,id_trole,limite))
tren <- fromJSON(paste0(url,id_tren,limite))
cable <- fromJSON(paste0(url,id_cable,limite))
metro <- fromJSON(paste0(url,id_metro,limite))
mb <- fromJSON(paste0(url,id_mb,limite))


trole <- trole$result$records
tren <- tren$result$records
cable <- cable$result$records
mb <- mb$result$records


# Para el metro tenemos que hacer un loop:
total_records <- metro$result$total
metro_r <- metro$result$records

# obtenemos el total de registros con una consulta por 32,000 observaciones a la vez hasta consumir toda la base

while (nrow(metro_r)<total_records){
  metro <- fromJSON(paste0("https://datos.cdmx.gob.mx",metro$result$`_links`$`next`))
  metro_r <- rbind(metro_r,metro$result$records)
  print(nrow(metro_r))
}
metro <- metro_r
# Limpiar Espacio
rm(metro_r,id_cable,id_mb,id_metro,id_tren,id_trole,limite,total_records,url)
## Rutas de Transporte Público Concesionada

rtp_map <- read_sf("/Users/dortega/Desktop/Mapas/concesionado_shp/CConcesionado_lineas.shp")

## Análisis de Datos ----

rtp %>% glimpse()

unique(rtp$RUTA)
unique(rtp$EMPRESA)

rtp<- data.frame(ruta=rtp_map$RUTA,empresa=rtp_map$EMPRESA)

rtp %>% count(empresa) %>% arrange(desc(n)) %>% ggplot(aes(reorder(empresa,n),n,fill=empresa))+
  geom_col()+
  labs(x="",y=" Número de Concesiones",fill="Empresa",title="Número de Conceciones de RTP por Empresa",subtitle = "A 2022", caption = "Fuente: SEMOVI")+
  geom_label(aes(label=n))+
  coord_flip()+
  theme(legend.position="none")

rtp_map %>% filter(EMPRESA %in%c("Corredor Tlalpan Xochimilco, S.A. de C.V.","COAVEO, S.A. de C.V.","Corredor Caseta Sur, S.A. de C.V.")) %>% 
  ggplot()+
  geom_sf(aes(EMPRESA))

## Mobilidad Total Cablebus

cable %>% group_by(linea) %>% summarise(afluencia_prepago=sum(afluencia_prepago), afluencia_gratuidad=sum(afluencia_gratuidad)) %>% 
  pivot_longer(c(afluencia_prepago,afluencia_gratuidad),names_to = "tipo_afluencia",values_to = "afluencia") %>% 
  ggplot(aes(x=linea,y=afluencia,fill=tipo_afluencia))+
  geom_bar( stat="identity",position= position_stack(reverse = TRUE))+ # fill = número, stack = porcentaje
  labs(x="",y="Número de Pasajeros",title="Afluencia del Cablebus por Línea",subtitle = "Entre Enero y Marzo 2022",
       fill="Tipo de Afuencia")+
  geom_text(aes(label=format(afluencia,big.mark=",")),  vjust=2)


































