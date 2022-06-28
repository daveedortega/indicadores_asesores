### Indicadores Agricultura
## David A. Ortega

## Preparar Espacio ----
pacman::p_load(tidyverse,scales,janitor,sf,leaflet)
rm(list=ls())
dev.off()
## Cargas Bases ----
#altepetl
padron_altepetl <- read_csv("input/agricultura/padron_altepetl2021.csv") %>% clean_names()
padron_altepetl <- padron_altepetl[,1:5]
#produccion agricola nacional
produccion_agricola_nacional <- read_csv("input/agricultura/Cierre_agricola_mun_2020.csv", 
                                         locale = locale(encoding = "ISO-8859-2")) %>% clean_names() 
produccion_agricola_2015 <- read_csv("input/agricultura/Cierre_agricola_mun_2015.csv",
                                     locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
produccion_agricola_2010 <- read_csv("input/agricultura/Cierre_agricola_mun_2010.csv",
                                     locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
produccion_agricola_2005 <- read_csv("input/agricultura/Cierre_agricola_mun_2005.csv",
                                     locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
produccion_agricola_2000 <- read_csv("input/agricultura/Cierre_agricola_2000.csv",
                                     locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
produccion_agricola_1995 <- read_csv("input/agricultura/Cierre_agricola_1995.csv",
                                     locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
produccion_agricola_1990 <- read_csv("input/agricultura/Cierre_agricola_1990.csv",
                                     locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
produccion_agricola_1985 <- read_csv("input/agricultura/Cierre_agricola_1985.csv",
                                     locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
produccion_agricola_1980 <- read_csv("input/agricultura/Cierre_agricola_1980.csv",
                                     locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
#Balanza Agrícola Banxico
balanza_agricola <- read_csv("input/agricultura/balanza_comercial_agricola.csv")%>% clean_names()
## Agricultura Gringa
#US Total Production exports value in millions of dollars
production_usd <- read_csv("input/agricultura/us_agr_exp_2000-2022.csv") %>% clean_names()
# Mdd in Corn exports state level
state_corn_usd <- read_csv("input/agricultura/corn_exports_statelevelusd.csv") %>% clean_names()
## Mapa EEUU
us_map <- read_sf("/Users/dortega/Desktop/Mapas/us_states_census2018/cb_2018_us_state_500k.shp")
## exports corn to mex
us_mex_corn_exp <- read_csv("input/agricultura/us_mex_cornexp.csv") %>% clean_names()
## Probelmáticas
prob_campo <- read_csv("input/agricultura/principales_problematicas_agr2019.csv") %>% clean_names()
## Suelo Agrícola 
suelo_agricola <- read_sf("/Users/dortega/Desktop/Mapas/suelo_agricola/conjunto_de_datos/cdv_usuev250sVII_cnal.shp")
mapa_mexico <- read_sf("/Users/dortega/Desktop/Mapas/mexico_electoral2021/ENTIDAD.shp")
plot(mapa_mexico)
plot(suelo_agricola)
st_is_valid(mapa_mexico[13,]$geometry)

st_make_valid(mapa_mexico[13,])

st_is_valid(mapa_mexico$geometry, reason = TRUE)

as.character(mapa_mexico$geometry)
st_make_valid(mapa_mexico)
#  produccion para el bienestar, cierre 2021
prod_bienestar <- read_csv("input/agricultura/padron_prodbien_2021cierre.csv") %>% clean_names()

# Superficie fertilizada de cultivos
fertilizante_2019 <- read_csv("input/agricultura/superficie_fertilizada_2019.csv")

# FAO produccion
fao_2020 <- read_csv("/Users/dortega/Downloads/FAOSTAT_data_6-14-2022.csv") %>% clean_names()

# Cultivos Riergo y Temporal por Entidad - ena 2019

ena_rt <- read_csv("input/agricultura/siembra__rtxedo_ena19.csv") %>% clean_names()

## Consumo Estimado de Granes SEMARNAT

consumo_semarnat <- read_csv("input/agricultura/consumo_sermarnat.csv") %>% clean_names()

## Estadísticas Padrón ----
padron_altepetl$alcaldia <- padron_altepetl$alcaldia %>% toupper()

## Alcaldías de Beneficiarios
padron_altepetl %>% count(alcaldia) %>%
  ggplot(aes(reorder(alcaldia,n),n,fill=alcaldia))+
  geom_col()+
  labs(x="",y="Número de Beneficiarios",title="Número de Beneficiarios del Programa ALTEPETL en 2021",
       subtitle = "Por Alcaldía de Residencia del Beneficiario",caption = "Fuente: Altepetl 2021",fill="Alcaldía")+
  geom_label(aes(label=comma(n)))+
  coord_flip()

## Sexo
padron_altepetl$alcaldia <- padron_altepetl$alcaldia %>% toupper()

padron_altepetl %>% count(sexo) %>% mutate(sexo = ifelse(sexo %in% c("H","M"),sexo,NA)) %>% group_by(sexo) %>% summarise(n=sum(n)) %>% 
  ggplot(aes(reorder(sexo,n),n,fill=sexo))+
  geom_col()+
  labs(x="",y="Número de Beneficiarios",title="Número de Beneficiarios del Programa ALTEPETL en 2021",
       subtitle = "Por sexo del beneficiario",caption = "Fuente: Altepetl 2021",fill="Alcaldía")+
  geom_label(aes(label=comma(n)))+
  coord_flip()
## Promedio de Edad

padron_altepetl$ano <- padron_altepetl$ano %>% as.numeric()

padron_altepetl %>% ggplot(aes(ano))+
  geom_freqpoly(stat="count")


mean(padron_altepetl$ano,na.rm = T)

padron_altepetl %>% ggplot(aes(ano))+
  geom_density(color="blue",size=1)


## Estadísticas Producción Nacionales ----

glimpse(produccion_agricola_nacional)

produccion_cdmx <- produccion_agricola_nacional %>% filter(idestado==9) %>% group_by(nommunicipio,nomcultivo_sin_um,nomunidad) %>% 
  summarise(sembrada=sum(sembrada),cosechada=sum(cosechada),valor=sum(valorproduccion)) %>% arrange(desc(cosechada))



produccion_cdmx %>% group_by(nomcultivo_sin_um) %>% filter(nomunidad=="Tonelada") %>% 
  summarise(sembrada=sum(sembrada),cosechada=sum(cosechada),valor=sum(valor)) %>% 
  arrange(desc(cosechada)) %>% 
  ggplot(aes(reorder(nomcultivo_sin_um,cosechada),cosechada,fill=nomcultivo_sin_um))+
  geom_col( )+
  scale_y_log10()+
  geom_label(aes(label=comma(cosechada)))+
  labs(title="Hectáreas Cocechadas en CDMX durante 2020",subtitle = "Por Tipo de Cultivo",
       x="",y="Hectáreas",caption = "Fuente: SIAP 2020",fill="Cultivo")+
  coord_flip()


produccion_cdmx %>% group_by(nomcultivo_sin_um) %>% 
  summarise(sembrada=sum(sembrada),cosechada=sum(cosechada),valor=sum(valor)) %>% 
  arrange(desc(valor)) %>% 
  ggplot(aes(reorder(nomcultivo_sin_um,valor),valor,fill=nomcultivo_sin_um))+
  geom_col( )+
  geom_label(aes(label=comma(valor)),nudge_y=-1)+
  labs(title="Ganancias por Tipo de Cocecha en CDMX durante 2020",subtitle = "En Pesos Mexicanos de 2020",
       x="",y="Pesos",caption = "Fuente: SIAP 2020",fill="Cultivo")+
  coord_flip()+
  scale_y_log10()

produccion_agricola_nacional %>% filter(idestado==9) %>% count(nomcader)

## Agricultura Nacional SIAP



produccion_agricola_nacional %>% count(nomcultivo_sin_um) %>% arrange(desc(n))
produccion_agricola_2015 %>% count(nomcultivo_sin_um) %>% arrange(desc(n))
produccion_agricola_2010 %>% count(nomcultivo) %>% arrange(desc(n))
produccion_agricola_2005 %>% count(nomcultivo) %>% arrange(desc(n))
produccion_agricola_2000 %>% count(nomcultivo) %>% arrange(desc(n))
produccion_agricola_1995 %>% count(nomcultivo) %>% arrange(desc(n))
produccion_agricola_1990 %>% count(nomcultivo) %>% arrange(desc(n))


# Provisional, ver producciones nacionales
produccion_agricola_nacional %>% group_by(nomcultivo_sin_um) %>% summarise(sembrada=sum(sembrada),
                                                                       cosechada=sum(cosechada),valorproduccion=sum(valorproduccion),
                                                                       volumenproduccion=sum(volumenproduccion)) %>% 
  arrange(desc(valorproduccion))

produccion_agricola_2015 %>% group_by(nomcultivo_sin_um) %>% summarise(sembrada=sum(sembrada),
                                                                       cosechada=sum(cosechada),valorproduccion=sum(valorproduccion),
                                                                       volumenproduccion=sum(volumenproduccion),
                                                                       siniestrada=sum(siniestrada)) %>% 
  arrange(desc(valorproduccion))

produccion_agricola_2010 %>% group_by(nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                                 cosechada=sum(cosechada),valorproduccion=sum(valorproduccion),
                                                                 volumenproduccion=sum(volumenproduccion),
                                                                 siniestrada=sum(siniestrada))  %>% 
  arrange(desc(valorproduccion))

produccion_agricola_2005 %>% group_by(nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                                cosechada=sum(cosechada),valorproduccion=sum(valorproduccion),
                                                                volumenproduccion=sum(volumenproduccion),
                                                                siniestrada=sum(siniestrada))  %>% 
  arrange(desc(valorproduccion))

produccion_agricola_2000 %>% group_by(nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                                cosechada=sum(cosechada),valorproduccion=sum(valorproduccion),
                                                                volumenproduccion=sum(volumenproduccion),
                                                                siniestrada=sum(siniestrada))  %>% 
  arrange(desc(valorproduccion))

produccion_agricola_1995 %>% group_by(nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                                cosechada=sum(cosechada),valorproduccion=sum(valorproduccion),
                                                                volumenproduccion=sum(volumenproduccion),
                                                                siniestrada=sum(siniestrada))  %>% 
  arrange(desc(valorproduccion))

produccion_agricola_1990 %>% group_by(nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                                cosechada=sum(cosechada),valorproduccion=sum(valorproduccion),
                                                                volumenproduccion=sum(volumenproduccion),
                                                                siniestrada=sum(siniestrada)) %>% 
  arrange(desc(valorproduccion))

produccion_agricola_1985 %>% group_by(nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                                cosechada=sum(cosechada),valorproduccion=sum(valorproduccion),
                                                                volumenproduccion=sum(volumenproduccion),
                                                                siniestrada=sum(siniestrada)) %>% 
  arrange(desc(valorproduccion))

produccion_agricola_1980 %>% group_by(nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                                cosechada=sum(cosechada),valorproduccion=sum(valorproduccion),
                                                                volumenproduccion=sum(volumenproduccion),
                                                                siniestrada=sum(siniestrada)) %>% 
  arrange(desc(valorproduccion))

## Produccion maiz grano
seriedt <- data.frame(ano=c(1980,1985,1990,1995,2000,2005,2010,2015,2020),
                      maiz_sembrado=c(7596878,8365957,7917517,9079636,8444794,7978603,7860705,7600453,7472357),
                      maiz_cosechado=c(6766120,7589537,7338871,8020392,7131181,6605614,7148046,7099724,7156391),
                      valor_maiz=c(62105017,741669242,8919827723,20033390740,26462179652,30515115368,65629387627,84523647453,114911058927),
                      toneladas=c(12373978,14103454,14635434,18352856,19338713,17556905,23301879,24694046,27424528),
                      frijol_sembrado=c(1670503,2063577,2271619,2342804,2120693,1746020,1887177,1678939,1711963),
                      frijol_cosechado=c(1545234,1766269,2094631,2029501,1502818,1261220,1630225,1555132,1567339),
                      toneladas_frijol=c(945358,907033,1287610,1268255,887868,826892,1156257,969146,1056071))
#cocecha y siembra maiz
seriedt %>%select(ano,maiz_sembrado,maiz_cosechado) %>% 
  pivot_longer(c(maiz_sembrado,maiz_cosechado),names_to = "maiz",values_to = "toneladas_maiz") %>%
  mutate(maiz=ifelse(maiz=="maiz_cosechado","Cosechado","Sembrado")) %>% 
  ggplot(aes(ano,toneladas_maiz,group=maiz,color=maiz))+
  geom_line(size=1)+
  labs(title="Hectáreas de Maíz Sembradas y Cocechadas",subtitle = "Quinquenios entre 1980 a 2020",caption = "Fuente: SIAP",
       x="",y="Hectáreas")+
  geom_label(aes(label=comma(toneladas_maiz)))

# valor maiz
seriedt %>% mutate(precioxt=valor_maiz/maiz_cosechado) %>% 
  ggplot(aes(ano,valor_maiz))+
  geom_col(fill="#9F2241")+
  geom_label(aes(label=comma(valor_maiz)))+
  labs(title="Valor de la Producción de Maíz ",subtitle = "Quinquenios entre 1980 a 2020",caption = "Fuente: SIAP",
       x="",y="Pesos")

# precio xt maiz
seriedt %>% mutate(precioxt=valor_maiz/toneladas) %>% 
  ggplot(aes(ano,precioxt))+
  geom_col(fill="#BC955C")+
  geom_label(aes(label=round(precioxt,3)))+
  labs(title="Precio por tonelada de Maíz en Grano",subtitle = "Quinquenios entre 1980 a 2020",caption = "Fuente: SIAP",
       x="",y="Pesos")

# Volumne de Producción maiz
seriedt %>%select(ano,toneladas) %>%  
  ggplot(aes(ano,toneladas))+
  geom_line(size=1,color="#691C32")+
  labs(title="Toneladas de Maíz Sembradas y Cocechadas",subtitle = "Quinquenios entre 1980 a 2020",caption = "Fuente: SIAP",
       x="",y="Toneladas")+
  geom_label(aes(label=comma(toneladas)))

## Frijol Sembrado - cosechado

seriedt %>%select(ano,frijol_sembrado,frijol_cosechado) %>% 
  pivot_longer(c(frijol_sembrado,frijol_cosechado),names_to = "frijol",values_to = "hectareas_frijol") %>%
  mutate(frijol=ifelse(frijol=="frijol_cosechado","Cosechado","Sembrado")) %>% 
  ggplot(aes(ano,hectareas_frijol,group=frijol,color=frijol))+
  geom_line(size=1)+
  labs(title="Hectáreas de Frijol Sembradas y Cocechadas",subtitle = "Quinquenios entre 1980 a 2020",caption = "Fuente: SIAP",
       x="",y="Hectáreas",color="")+
  geom_label(aes(label=comma(hectareas_frijol)))

# Volumne de Producción frijol
seriedt %>%select(ano,toneladas_frijol) %>%  
  ggplot(aes(ano,toneladas_frijol))+
  geom_line(size=1,color="#691C32")+
  labs(title="Toneladas de Frijol producidas",subtitle = "Quinquenios entre 1980 a 2020",caption = "Fuente: SIAP",
       x="",y="Toneladas")+
  geom_label(aes(label=comma(toneladas_frijol)))

## Cultivos más productivos a nivel nacional por valor de produccion
produccion_agricola_nacional %>%  summarise(sum(valorproduccion))

valor_total_2020 <- 630933192477

produccion_agricola_nacional %>% group_by(nomcultivo_sin_um) %>% summarise(sembrada=sum(sembrada),
                                                                           cosechada=sum(cosechada),valorproduccion=sum(valorproduccion),
                                                                           volumenproduccion=sum(volumenproduccion)) %>% 
  arrange(desc(valorproduccion)) %>% mutate(porcentaje=round(valorproduccion/valor_total_2020*100,2)) %>% 
  mutate(nomcultivo_sin_um=ifelse(porcentaje>2.3,nomcultivo_sin_um,"otros")) %>% filter(nomcultivo_sin_um!="otros")%>% group_by(nomcultivo_sin_um) %>% 
  summarise(porcentaje=sum(porcentaje),valorproduccion=sum(valorproduccion)) %>% 
  ggplot(aes(reorder(nomcultivo_sin_um,porcentaje),porcentaje,fill=nomcultivo_sin_um))+
  geom_col()+
  geom_label(aes(label=paste0(round(porcentaje,2),"%")))+
  labs(title="Porcentaje del Valor de Mercado por cultivo",subtitle = "De los 10 Cultivos más Valiosos en 2020"
       ,caption = "Fuente: SIAP",x="",y="Porcentaje",fill="Cultivo")
## Estado con mayor participación de mercado

produccion_agricola_nacional %>% group_by(nomestado) %>% summarise(valorproduccion=sum(valorproduccion)) %>%
  mutate(porcentaje=round(valorproduccion/valor_total_2020*100,2)) %>% arrange(desc(porcentaje)) %>% 
  ggplot(aes(reorder(nomestado,porcentaje),porcentaje,fill=nomestado))+
  geom_col()+
  geom_label(aes(label=paste0(porcentaje,"%")))+
  labs(title="Porcentaje del Valor de Mercado por Estado",subtitle = "En 2020"
       ,caption = "Fuente: SIAP",x="",y="Porcentaje",fill="Estado")+
  coord_flip()

# Rendimiento de las tierras

produccion_agricola_nacional %>% filter(nomunidad=="Tonelada",nommodalidad=="Riego") %>% select(nomestado,nomcultivo_sin_um,rendimiento) %>%
  group_by(nomestado,nomcultivo_sin_um) %>% filter(rendimiento!=0) %>% 
  summarise(rendimiento=sum(rendimiento)) %>% arrange((rendimiento))

## Datos EEUU -----
# columnas a filas
colnames(production_usd)[1] <- "crop"
production_usd <- production_usd %>% pivot_longer(c(x2000,x2001,x2002,x2003,x2004,x2005,x2006,x2007,x2008,x2009,x2010,
                                  x2011,x2012,x2013,x2014,x2015,x2016,x2017,x2018,x2019,x2020),names_to = "ano",values_to = "mdd")
# a numero
production_usd$ano <- as.numeric(gsub("x","",production_usd$ano))

# maiz - Corn

production_usd %>% filter(crop =="Corn") %>% 
  ggplot(aes(ano,mdd))+
  geom_line(size=1,color = "#3C3B6E")+
  labs(title="Valor de las exportaciones de Maíz en Grano de los EEUU entre 2000 y 2020",subtitle = "En Millones de Dólares",
       caption ="Fuente: USDA",x="",y="MdD")+
  geom_label(data=data.frame(ano = c(2000,2011,2020),mdd = c(4469,13652,9213)),
             aes(label=comma(mdd)))


## Sate corn production

state_corn_usd <- state_corn_usd %>% pivot_longer(c(x2000,x2001,x2002,x2003,x2004,x2005,x2006,x2007,x2008,x2009,x2010,
                                 x2011,x2012,x2013,x2014,x2015,x2016,x2017,x2018,x2019,x2020),names_to = "ano",values_to = "mdd")
state_corn_usd$ano <- as.numeric(gsub("x","",state_corn_usd$ano))

state_corn_usd %>% filter(ano==2020,state!="United States") %>%
  mutate(state=ifelse(mdd>=210,state,NA)) %>% group_by(ano,state) %>% summarise(mdd=sum(mdd)) %>% 
  ggplot(aes(reorder(state,mdd),mdd,fill=state))+
  geom_col()+
  labs(title="Ganancias por las Exportaciones de Maíz de los 10 Estados con mayor producción",subtitle = "En 2020, valuado en Millones de Dólares",
       x="",y="Millones de Dólares",caption = "Fuente: USDA")+
  geom_label(aes(label=comma(mdd)))

## Plotting corn Exports
us_mex_corn_exp %>% select(year,amount) %>% mutate(amount=amount*1000) %>% 
  ggplot(aes(year,amount))+
  geom_line(size=1,color="#3C3B6E")+
  labs(title="Exportaciones de Maíz de EEUU a México entre 1989 - 2021", subtitle = "En toneladas", x="",y="Toneladas",caption="Fuente: USDA")+
  geom_label(data=data.frame(year=c(1989,2006,2018,2021),
                             amount=c(396363.5,13416872.9,18370424.9,11144478.9)),
             aes(label=comma(amount)))+
  geom_line(data=seriedt,aes(ano,toneladas),size=1,color="#006341")

# Comparativa
us_mex_corn_exp %>% select(year,amount) %>% mutate(amount=amount*1000) %>% 
  ggplot(aes(year,amount))+
  geom_line(size=1,color="#3C3B6E")+
  labs(title="Exportaciones de Maíz de EEUU a México entre 1989 - 2021 vs. Producción Nacional de Máiz",
       subtitle = "En toneladas", x="",y="Toneladas",caption="Fuente: USDA, SIAP")+
  geom_label(data=data.frame(year=c(1989,2006,2018,2021),
                        amount=c(396363.5,13416872.9,18370424.9,11144478.9)),
             aes(label=comma(amount)))+
  geom_line(data=seriedt,aes(ano,toneladas),size=1,color="#006341")+
  geom_label(data=seriedt,aes(ano,toneladas,label=comma(toneladas)))


## Todos los granos producidos
produccion_agricola_1980 %>% count(nomcultivo) %>% 
  ggplot(aes(reorder(nomcultivo,n),n,fill=nomcultivo))+
  geom_col()+
  theme(legend.position = "none")+
  scale_y_log10()
cultivos_1980<- produccion_agricola_1980 %>% count(nomcultivo) %>% select(nomcultivo)
cultivos_2020 <- produccion_agricola_nacional %>% count(nomcultivo_sin_um) %>% select(nomcultivo_sin_um) 

## Balanza Comercial Agrícola ----

# Transformar fechas a fechas
balanza_agricola$fecha <- gsub("Ene","01-",balanza_agricola$fecha)
balanza_agricola$fecha <- gsub("Feb","02-",balanza_agricola$fecha)
balanza_agricola$fecha <- gsub("Mar","03-",balanza_agricola$fecha)
balanza_agricola$fecha <- gsub("Abr","04-",balanza_agricola$fecha)
balanza_agricola$fecha <- gsub("May","05-",balanza_agricola$fecha)
balanza_agricola$fecha <- gsub("Jun","06-",balanza_agricola$fecha)
balanza_agricola$fecha <- gsub("Jul","07-",balanza_agricola$fecha)
balanza_agricola$fecha <- gsub("Ago","08-",balanza_agricola$fecha)
balanza_agricola$fecha <- gsub("Sep","09-",balanza_agricola$fecha)
balanza_agricola$fecha <- gsub("Oct","10-",balanza_agricola$fecha)
balanza_agricola$fecha <- gsub("Nov","11-",balanza_agricola$fecha)
balanza_agricola$fecha <- gsub("Dic","12-",balanza_agricola$fecha)
balanza_agricola$fecha <- gsub(" ","",balanza_agricola$fecha)
balanza_agricola <- balanza_agricola %>% mutate(fecha=paste0("01-",fecha)) %>% mutate(fecha=as.Date(fecha,tryFormats="%d-%m-%Y"))
#


# Balanza Comercial- Exports
balanza_agricola %>% ggplot(aes(fecha,balanza_comercial_de_mercancias_de_mexico_exportaciones_totales_no_petroleras_agropecuarias))+
  geom_line(size=1,color="#34b912")+
  geom_smooth()+
  labs(x="",y="Miles de Dólares",title="Balanza Comercial - Exportaciones Agropecuarias", subtitle = "Estacionalizada a Pesos de 2013", caption = "Fuente: Banxico")+
  geom_label(data=data.frame(fecha=as.Date("2022-03-01"),balanza_comercial_de_mercancias_de_mexico_exportaciones_totales_no_petroleras_agropecuarias=2208134),
             aes(label=paste0("Marzo 2022: ",comma(balanza_comercial_de_mercancias_de_mexico_exportaciones_totales_no_petroleras_agropecuarias))),
             nudge_x = -500)
# Balanza Comercial- Imports
balanza_agricola$importacion_de_productos_agropecuarios <- gsub(",","",balanza_agricola$importacion_de_productos_agropecuarios)
balanza_agricola$importacion_de_productos_agropecuarios <- as.numeric(balanza_agricola$importacion_de_productos_agropecuarios)

balanza_agricola %>% ggplot(aes(fecha,importacion_de_productos_agropecuarios))+
  geom_line(size=1,color="#34b912")+
  geom_smooth()+
  labs(x="",y="Miles de Dólares",title="Balanza Comercial - Importaciones Agropecuarias", subtitle = "Estacionalizada a Pesos de 2013", caption = "Fuente: Banxico")+
  geom_label(data=data.frame(fecha=as.Date("2022-03-01"),importacion_de_productos_agropecuarios=1815312),
             aes(label=paste0("Marzo 2022: ",comma(importacion_de_productos_agropecuarios))),
             nudge_x = -500)
# Superavit?
balanza_agricola %>% select(fecha,exports=balanza_comercial_de_mercancias_de_mexico_exportaciones_totales_no_petroleras_agropecuarias,
                            imports=importacion_de_productos_agropecuarios) %>% 
  mutate(balance=exports-imports) %>% 
  ggplot(aes(fecha,balance))+
  geom_line(size=1,color="#34b912")+
  geom_smooth()+
  labs(x="",y="Miles de Dólares",title="Balanza Agrícola: Exports - Imports", subtitle = "Estacionalizada a Pesos de 2013", caption = "Fuente: Banxico")+
  geom_label(data=data.frame(fecha=as.Date("2022-03-01"),balance=392822),
             aes(label=paste0("Marzo 2022: ",comma(balance))),
             nudge_x = -500)+
  geom_line(aes(x=fecha,y=0),size=1)


# Principles problemas

prob_campo %>% ggplot(aes(reorder(tipo_de_problema_presentado,porcentaje),porcentaje,fill=tipo_de_problema_presentado))+
  geom_col()+
  geom_label(aes(label=porcentaje))+
  labs(x="",y="Porcentaje",title="Principales problemas de los Agricultores",subtitle = "Autoreportados, de acuerdo al INEGI en 2019",
       caption = "Fuente: INEGI-ENA 2019")+
  theme(legend.position = "none",title = element_text(size=18,face="bold"))+
  coord_flip()

## Produccion por region

produccion_agricola_nacional %>%filter(nomunidad=="Tonelada") %>%
  group_by(nomcicloproductivo) %>% summarise(volumenproduccion=sum(volumenproduccion),valorproduccion=sum(valorproduccion)) %>% 
  arrange(desc(volumenproduccion)) %>% 
  ggplot(aes(nomcicloproductivo,volumenproduccion))+
  geom_col(fill=c("#235B4E","#DDc9A3","#9F2241"),width = 0.45,
           position = position_nudge(x = -0.225))+
  geom_col(aes(y=valorproduccion),width = 0.45,
           position = position_nudge(x = 0.225),fill="#85bb65")+
  labs(x="",y="Toneladas/Miles de Pesos",title="Toneladas producidas por Ciclor Productivo de cultivo",subtitle = "En 2020", caption = "SIAP: 2020")+
  theme(legend.position = "none",title = element_text(size=18,face="bold"))+
  scale_y_log10()+
  geom_label(aes(y=valorproduccion,label=paste0("$ ",comma(valorproduccion))), position = position_nudge(x = 0.225))+
  geom_label(aes(y=volumenproduccion,label=paste0("Toneladas: ",comma(volumenproduccion))),position = position_nudge(x =-0.225))

## Produccion por regiones
colnames(poblacion_mex)[6] <- "idestado"
produccion_agricola_nacional %>% left_join(poblacion_mex,by="idestado") %>% 
  filter(nomunidad=="Tonelada") %>% group_by(region) %>% summarise(volumenproduccion=sum(volumenproduccion),valorproduccion=sum(valorproduccion)) %>% 
  ggplot(aes(reorder(region,volumenproduccion),volumenproduccion))+
  geom_col(fill=c("#235B4E","#DDc9A3","#9F2241","#BC955C","#6F7271"))+
  labs(x="",y="Toneladas",title="Toneladas de Alimento producidas por Circunscripción Electoral",subtitle = "Total de Cultivos en 2020",caption = "SIAP:2020")+
  geom_label(aes(label=paste0(region,": ",comma(volumenproduccion))))+
  theme(legend.position = "none",title = element_text(size=18,face="bold"))

## Valor por regiones
colnames(poblacion_mex)[6] <- "idestado"
produccion_agricola_nacional %>% left_join(poblacion_mex,by="idestado") %>% 
  filter(nomunidad=="Tonelada") %>% group_by(region) %>% summarise(volumenproduccion=sum(volumenproduccion),valorproduccion=sum(valorproduccion)) %>% 
  ggplot(aes(reorder(region,valorproduccion),valorproduccion))+
  geom_col(fill=c("#235B4E","#DDc9A3","#9F2241","#BC955C","#6F7271"))+
  labs(x="",y="Pesos",title="Valor de Mercado de Alimento",subtitle = "Total de 2020",caption = "SIAP:2020")+
  geom_label(aes(label=paste0(region,": ",comma(valorproduccion))))+
  theme(legend.position = "none",title = element_text(size=18,face="bold"))

##

produccion_agricola_nacional %>% left_join(poblacion_mex,by="idestado") %>% 
  filter(nomunidad=="Tonelada",nomcultivo_sin_um=="Maíz grano") %>% group_by(region) %>% summarise(volumenproduccion=sum(volumenproduccion),valorproduccion=sum(valorproduccion)) %>% 
  ggplot(aes(reorder(region,volumenproduccion),volumenproduccion))+
  geom_col(fill=c("#235B4E","#DDc9A3","#9F2241","#BC955C","#6F7271"))+
  labs(x="",y="Toneladas",title="Producción Regional de Maíz",subtitle = "En toneladas durante 2020",caption = "SIAP:2020")+
  geom_label(aes(label=paste0(region,": ",comma(volumenproduccion))))+
  theme(legend.position = "none",title = element_text(size=18,face="bold"))



























## Prod p Bienestar -----

prod_bienestar %>% glimpse()

# Número de gente con apoyos
prod_bienestar %>% count(nombre_de_la_delegacion) %>% 
  ggplot(aes(reorder(nombre_de_la_delegacion,n),n,fill=nombre_de_la_delegacion))+
  geom_col()+
  labs(title = "Beneficiarios por entidad del programa Producción al Bienestar",subtitle = "Al cierre de 2021 por Entidad",
       caption = "Fuente: SADER 2021",x="",y="Número de Beneficiarios")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),
        legend.position = "none")+
  geom_label(aes(label=comma(n)))+
  coord_flip()

## Monto por estado
prod_bienestar %>% group_by(nombre_de_la_delegacion) %>% 
  summarise(apoyo_total=sum(monto_apoyado)) %>% 
  ggplot(aes(reorder(nombre_de_la_delegacion,apoyo_total),apoyo_total,fill=nombre_de_la_delegacion))+
  geom_col()+
  coord_flip()+
  labs(title = "Monto total entregado por entidad del programa Producción al Bienestar",subtitle = "Al cierre de 2021 por Entidad",
       caption = "Fuente: SADER 2021",x="",y="Pesos")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),
        legend.position = "none")+
  geom_label(aes(label=comma(apoyo_total)),nudge_y = -1000)+
  scale_y_sqrt()

## Monto per capita



## Monto por cultivo
prod_bienestar %>% group_by(cultivo) %>% summarise(total=sum(monto_apoyado)) %>% 
  ggplot(aes(reorder(cultivo,total),total,fill=cultivo))+
  geom_col()+
  labs(title = "Monto total entregado por Cultivo del programa Producción al Bienestar",subtitle = "Al cierre de 2021 por Entidad",
       caption = "Fuente: SADER 2021",x="",y="Pesos")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "none")+
  geom_label(aes(label=comma(total)),nudge_y = -1000)+
  coord_flip()+
  scale_y_sqrt()

## Monto por Grupo Etarea
prod_bienestar %>% group_by(grupo_de_edad) %>% summarise(total=sum(monto_apoyado)) %>% 
  ggplot(aes(reorder(grupo_de_edad,total),total,fill=grupo_de_edad))+
  geom_col()+
  labs(x="",y="Pesos",fill="Grupo de Edad",title ="Monto total entregado por Grupo de Edad del programa Producción para el Bienestar",
       subtitle = "Al cierre de 2021 por Entidad", caption = "Fuente: SADER 2021",x="",y="Pesos")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "none")+
  scale_y_sqrt()+
  geom_label(aes(label=comma(total)))

## por hectarea promedio

prod_bienestar %>% glimpse()

prod_bienestar %>% group_by(nombre_de_la_delegacion) %>% 
  summarise(hectareas=sum(superficie_apoyada),total=sum(monto_apoyado)) %>% 
  mutate(apoyo_xh=total/hectareas) %>% 
  ggplot(aes(reorder(nombre_de_la_delegacion,apoyo_xh),apoyo_xh,fill=nombre_de_la_delegacion))+
  geom_col()+
  geom_label(aes(label=comma(round(apoyo_xh,2))))+
  labs(x="",y="Pesos",title="Apoyo promedio por Hectárea del programa Producción para el Bienestar",subtitle = "Al cierre de 2021 por Entidad",
       caption = "Fuente: SADER 2021")+
  coord_flip()+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "none")

# Por ciclo y regimen hidirico
prod_bienestar %>% group_by(ciclo,regimen_hidrico) %>% summarise(total=sum(monto_apoyado)) %>% 
  mutate(ciclo=ifelse(ciclo=="OI20","Otoño-Invierno 2020","Primavera-Verano 2021")) %>% 
  ggplot(aes(ciclo,total,group=regimen_hidrico,fill=regimen_hidrico))+
  geom_col()+
  scale_y_sqrt()+
  geom_label(aes(label=comma(total)),position="stack")+
  labs(x="",y="Pesos",title="Monto del programa Producción para el Bienestar por ciclo Productivo",
       subtitle = "Al cierre de 2021", caption = "Fuente: SADER 2021", fill="Regimen Hídrico")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16))

  
## Productores Privados

prod_bienestar %>% filter(ejido %in% c(0,NA)) %>% group_by(cultivo) %>% summarise(monto=sum(monto_apoyado)) %>% 
  mutate(porcentaje=round(monto/sum(monto)*100,2)) %>% 
  ggplot(aes(reorder(cultivo,porcentaje),porcentaje,fill=cultivo))+
  geom_col()+
  scale_y_sqrt()+
  labs(x="",y="Porcentaje",title="Porcentaje de apoyos por tipo de cultivo del programa Producción para el Bienestar",
       subtitle = "Para productores privados",caption = "Fuente: SADER 2021")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),
        legend.position = "none")+
  coord_flip()+
  geom_label(aes(label=paste0("%",porcentaje)))

## Apoyos promedios por Ejido 

prod_bienestar %>% mutate(is_ejido=ifelse(nombre_del_ejido=="P,PROPIEDAD","Propiedad Privada","Ejido")) %>% 
  group_by(is_ejido,cultivo) %>% summarise(monto=sum(monto_apoyado)) %>% 
  ggplot(aes(factor(is_ejido),monto,group=cultivo,fill=cultivo))+
  geom_col()

prod_bienestar %>% filter(ejido %in% c(0,NA))  %>% summarise(sum(monto_apoyado))

prod_bienestar %>% filter(!ejido %in% c(0,NA))   %>% summarise(sum(monto_apoyado))


## fertilizante utilizado 

fertilizante_2019 %>% filter(entidad!="Nacional") %>% 
  ggplot(aes(reorder(entidad,porcentaje_fertilizado),porcentaje_fertilizado,fill=entidad))+
  geom_col()+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),
        legend.position = "none")+
  geom_label(aes(label=paste0("%",round(porcentaje_fertilizado,2))))+
  labs(x="",y="Porcentaje",title="Porcentaje de Hectareas de Siembra Fertilizadas",subtitle = "Por Entidad Federativa Durante 2019",
       caption = "Fuente: SIAP 2019")+
  coord_flip()
  
## FAO ----

fao_2020 %>% glimpse()

fao_2020 %>% count(element)

fao_2020 %>% count(item) %>% as.data.frame()


fao_2020 %>% filter(item %in% c("Sugar cane","Sorghum","Cereals, Total","Beans, dry","Avocados","Chillies and peppers, dry","Wine")) %>% 
  filter(element=="Production") %>% 
  filter(!area %in% c("South Africa","Nigeria") ) %>% 
  select(area,year,value,item) %>% 
  ggplot(aes(year,value,group=area,color=area))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~item,scales = "free_y")+
  labs(x="Año",y="Toneladas",title="Toneladas Producidas entre 1960 y 2020 de cultivos selectos",
       subtitle = "De acuerdo a la FAO", caption = "FAO: 2022",fill="País")+
  theme(plot.title = element_text(size=20,face="bold"),
        plot.subtitle=element_text(size=16),legend.position = "bottom")



fao_2020 %>% filter(item %in% c("Sugar cane","Sorghum","Cereals, Total","Beans, dry","Avocados","Chillies and peppers, dry","Wine",
                                "Soybeans","Rice, paddy","Rice, paddy (rice milled equivalent)")) %>% 
  filter(element=="Production") %>% 
  select(area,year,value,item) %>% 
  ggplot(aes(year,value,group=area,color=area))+
  geom_point(size=0.5)+
  geom_smooth()+
  facet_wrap(~item,scales = "free_y")+
  labs(x="Año",y="Toneladas",title="Toneladas Producidas entre 1960 y 2020 de cultivos selectos",
       subtitle = "De acuerdo a la FAO", caption = "FAO: 2022",fill="País")+
  theme(plot.title = element_text(size=20,face="bold"),
        plot.subtitle=element_text(size=16),legend.position = "bottom")

## ENA - 2019 RT ----
ena_rt %>% glimpse()

ena_rt$superficie_sembrada <- gsub(" ","",ena_rt$superficie_sembrada)
ena_rt$superficie_cosechada <- gsub(" ","",ena_rt$superficie_cosechada)
ena_rt$de_temporal_sembrada <- gsub(" ","",ena_rt$de_temporal_sembrada)
ena_rt$de_temporal_cocechada <- gsub(" ","",ena_rt$de_temporal_cocechada)
ena_rt$de_riego_sembrada <- gsub(" ","",ena_rt$de_riego_sembrada)
ena_rt$de_riego_cocechada <- gsub(" ","",ena_rt$de_riego_cocechada)
ena_rt$produccion_total <- gsub(" ","",ena_rt$produccion_total)
ena_rt$produccion_total <- gsub(" ","",ena_rt$produccion_total)
ena_rt$produccion_bajo_riego <- gsub(" ","",ena_rt$produccion_bajo_riego)


ena_rt$superficie_sembrada <- gsub(",","",ena_rt$superficie_sembrada)
ena_rt$superficie_cosechada <- gsub(",","",ena_rt$superficie_cosechada)
ena_rt$de_temporal_sembrada <- gsub(",","",ena_rt$de_temporal_sembrada)
ena_rt$de_temporal_cocechada <- gsub(",","",ena_rt$de_temporal_cocechada)
ena_rt$de_riego_sembrada <- gsub(",","",ena_rt$de_riego_sembrada)
ena_rt$de_riego_cocechada <- gsub(",","",ena_rt$de_riego_cocechada)
ena_rt$produccion_total <- gsub(",","",ena_rt$produccion_total)
ena_rt$produccion_total <- gsub(",","",ena_rt$produccion_total)
ena_rt$produccion_bajo_riego <- gsub(",","",ena_rt$produccion_bajo_riego)


ena_rt$superficie_sembrada <- as.numeric(ena_rt$superficie_sembrada)
ena_rt$superficie_cosechada <- as.numeric(ena_rt$superficie_cosechada)
ena_rt$de_temporal_sembrada <- as.numeric(ena_rt$de_temporal_sembrada)
ena_rt$de_temporal_cocechada <- as.numeric(ena_rt$de_temporal_cocechada)
ena_rt$de_riego_sembrada <- as.numeric(ena_rt$de_riego_sembrada)
ena_rt$de_riego_cocechada <- as.numeric(ena_rt$de_riego_cocechada)
ena_rt$produccion_total <- as.numeric(ena_rt$produccion_total)
ena_rt$produccion_total <- as.numeric(ena_rt$produccion_total)
ena_rt$produccion_bajo_riego <- as.numeric(ena_rt$produccion_bajo_riego)


# De Riego

ena_rt %>% group_by(entidad_federativa) %>% summarise(de_temporal_sembrada= sum(de_temporal_sembrada),
                                                      de_temporal_cocechada= sum(de_temporal_cocechada),
                                                      de_riego_sembrada= sum(de_riego_sembrada),
                                                      de_riego_cocechada= sum(de_riego_cocechada)) %>% ungroup() %>% 
  select(entidad_federativa,de_temporal_sembrada,de_temporal_cocechada,de_riego_sembrada,de_riego_cocechada) %>% 
  pivot_longer(c(de_temporal_sembrada,de_temporal_cocechada,de_riego_sembrada,de_riego_cocechada),names_to = "tipo_hidrico",
               values_to = "hectareas") %>% 
  filter(tipo_hidrico %in% c("de_riego_cocechada","de_temporal_cocechada")) %>% 
  mutate(tipo_hidrico=ifelse(tipo_hidrico == "de_riego_cocechada","De Riego","De Temporal")) %>% 
  ggplot(aes(reorder(entidad_federativa,hectareas),hectareas,group=tipo_hidrico,fill=tipo_hidrico))+
  geom_col(position="stack")+
  coord_flip()+
  geom_label(aes(label=comma(hectareas)),position="stack")+
  labs(x="",y="Hectareas",title = "Hectareas totales Cocechadas de Productos Clave Agrícolas por tipo hídrico en 2019",
       caption = "Fuente: INEGI - ENA",fill="Tipo Hídrico")+
  scale_y_sqrt()+
  theme(plot.title = element_text(size=20,face="bold"))+
  facet_wrap(~tipo_hidrico)


# DE Riego

ena_rt %>% group_by(entidad_federativa) %>% summarise(de_temporal_sembrada= sum(de_temporal_sembrada),
                                                      de_temporal_cocechada= sum(de_temporal_cocechada)) %>% 
  pivot_longer(c(de_temporal_sembrada,de_temporal_cocechada),names_to = "tipo_tierra",values_to = "hectareas") %>% 
  mutate(tipo_tierra=ifelse(tipo_tierra=="de_temporal_cocechada","Cocechada","Sembrada")) %>% 
  ggplot(aes(reorder(entidad_federativa,hectareas),hectareas,group=tipo_tierra,fill=entidad_federativa))+
           geom_col()+
  facet_wrap(~tipo_tierra)+
  labs(x="",y="Hectareas",fill="",title="Hectareas destinadas al temporal por Estado", subtitle = "Durante 2019, cocechadas y sembradas", 
       caption = "Fuente: INEGI - ENA 2019")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "none",
        strip.text = element_text(face="bold",color="black",size=12))+
  coord_flip()+
  geom_label(aes(label=comma(hectareas)),vjust=.5)


ena_rt %>% filter(entidad_federativa=="Zacatecas")

ena_rt %>% group_by(entidad_federativa) %>% summarise(de_riego_sembrada= sum(de_riego_sembrada),
                                                      de_riego_cocechada= sum(de_riego_cocechada)) %>% 
  pivot_longer(c(de_riego_sembrada,de_riego_cocechada),names_to = "tipo_tierra",values_to = "hectareas") %>% 
  mutate(tipo_tierra=ifelse(tipo_tierra=="de_riego_cocechada","Cocechada","Sembrada")) %>% 
  ggplot(aes(reorder(entidad_federativa,hectareas),hectareas,group=tipo_tierra,fill=entidad_federativa))+
  geom_col()+
  facet_wrap(~tipo_tierra)+
  labs(x="",y="Hectareas",fill="",title="Hectareas destinadas al riego por Estado", subtitle = "Durante 2019, cocechadas y sembradas", 
       caption = "Fuente: INEGI - ENA 2019")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "none",
        strip.text = element_text(face="bold",color="black",size=12))+
  coord_flip()+
  geom_label(aes(label=comma(hectareas)),vjust=.5)

## SEMARNAT ----

consumo_semarnat %>% filter(cultivo %in% c("Frijol","Arroz","Maíz","Trigo","Cebada","Sorgo")) %>%
  ggplot(aes(ano,consumo_aparente,group=cultivo,color=cultivo))+
  geom_line(size=1)+
  labs(x="",y="Miles de Toneladas",title="Consumo Aparente de Principales Cultivos de Grano en México",
       subtitle = "Consumo Aparente = Producción + Importaciones, desde 1980", caption = "Fuente: SEMARNAT",colo="Cultivo")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),
        legend.position = "bottom")+
  geom_label(data=labs,aes(label=paste0(cultivo," - 2020: ",comma(consumo_aparente))),nudge_x = -2)+
  scale_y_log10()
  
labs <- consumo_semarnat %>% filter(ano==2020)%>% filter(cultivo %in% c("Frijol","Arroz","Maíz","Trigo","Cebada","Sorgo"))









