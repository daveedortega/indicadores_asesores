## Desaparecidos 

## Preparamos Espacio ----
dev.off()
pacman::p_load(tidyverse,janitor,scales,plotly)
rm(list=ls())
## Cargamos Datos ----

## Nacional 

ano_desaparecidos_cdmx <- read_csv("input/desaparecidos/personas-desaparecidas-n-2.csv") %>% clean_names()

## Análisis ----

# Quitamos copy de sin año de referencia
ano_desaparecidos_cdmx[1,1] <- "01"

## Seccionamos y hacemos comparativa Intersexenal 

# pre 2000
antes_desap <- ano_desaparecidos_cdmx %>% mutate(ano=as.numeric(category)) %>% select(ano,hombre,mujer,indeterminado) %>%
  filter(ano<2007) %>% summarise(hombres=sum(hombre),mujeres=sum(mujer),indeterminados=sum(indeterminado)) %>% 
  mutate(sexenio = "1964-2006")
# Calderon 07-12
calderon_desap <- ano_desaparecidos_cdmx %>% mutate(ano=as.numeric(category)) %>% select(ano,hombre,mujer,indeterminado) %>%
  filter(ano <2013 & ano>2006) %>% summarise(hombres=sum(hombre),mujeres=sum(mujer),indeterminados=sum(indeterminado)) %>% 
  mutate(sexenio = "2007-2012")
# EPN 13-18
epn_desap <- ano_desaparecidos_cdmx %>% mutate(ano=as.numeric(category)) %>% select(ano,hombre,mujer,indeterminado) %>%
  filter(ano <2019 & ano>2012) %>% summarise(hombres=sum(hombre),mujeres=sum(mujer),indeterminados=sum(indeterminado)) %>% 
  mutate(sexenio = "2012-2018")
# AMLO 
amlo_desap <- ano_desaparecidos_cdmx %>% mutate(ano=as.numeric(category)) %>% select(ano,hombre,mujer,indeterminado) %>%
  filter(ano >=2019) %>% summarise(hombres=sum(hombre),mujeres=sum(mujer),indeterminados=sum(indeterminado)) %>% 
  mutate(sexenio = "2018-ACTUAL")

## Desaparecidos_sexenio ----

desaparecidos_sexenio <- rbind(antes_desap,calderon_desap,epn_desap,amlo_desap) %>% 
  pivot_longer(c(hombres,mujeres,indeterminados),names_to = "sexo",values_to = "desaparecidos")

desaparecidos_sexenio %>% ggplot(aes(sexenio,desaparecidos,group=reorder(sexo,-desaparecidos),fill=sexo))+
  geom_col(position="stack")+
  geom_label(aes(label=comma(desaparecidos)),position = "stack",vjust=-.25)+
  labs(title="Número de Desaparecidos en México", fill="sexo",subtitle = "Comparación inter-sexenio",x="",y="Desaparecidos")


## Comparativa inter estatal ----
entidad_desaparecidos_cdmx <- read_csv("input/desaparecidos/personas-desaparecidas-n-3.csv") %>% clean_names()
colnames(entidad_desaparecidos_cdmx)[1] <- "entidad"

# Sólo para apreciarlo, vamos a tomar primeros 5 y CDMX
entidad_desaparecidos_cdmx %>% arrange(desc(desaparecidos)) %>% left_join(poblacion_mex,by="entidad") %>% 
  mutate(por_cada_mil = desaparecidos/(total/1000)) %>% 
  arrange(desc(por_cada_mil)) 
  

# Ploteamos comparativa relativa, desaparecidos por cada mil habitantes
entidad_desaparecidos_cdmx %>% filter(entidad %in% c("JALISCO","TAMAULIPAS","ESTADO DE MEXICO","VERACRUZ",
                                                     "SINALOA","NUEVO LEON","CIUDAD DE MEXICO")) %>% 
  left_join(poblacion_mex,by="entidad") %>% 
  mutate(por_cada_mil = desaparecidos/(total/1000)) %>% 
  arrange(desc(desaparecidos)) %>% 
  ggplot(aes(entidad,por_cada_mil,fill=entidad))+
  geom_col()+
  geom_label(aes(label=round(por_cada_mil,2)))+
  labs(x="",y="Desaparecidos por cada Mil personas",title="Número de Desaparecidos por cada mil personas por entidad",
       subtitle = "Entre 1964 - 20/05/2022")
  
## Alcaldías y Desaparecidos Locales ----
alcaldia_desaparecidos_cdmx <- read_csv("input/desaparecidos/personas-desaparecidas-n-5.csv") %>% clean_names()
colnames(alcaldia_desaparecidos_cdmx)[1] <- "nombre_municipio"

alcaldia_desaparecidos_cdmx %>% pivot_longer(c(hombre,mujer,indeterminado),names_to = "sexo",values_to = "desaparecidos") %>% 
  summarise(sum(desaparecidos))

##Absoluto
alcaldia_desaparecidos_cdmx %>% pivot_longer(c(hombre,mujer,indeterminado),names_to = "sexo",values_to = "desaparecidos") %>% 
  left_join(poblacion_cdmx,by="nombre_municipio") %>% filter(nombre_municipio !="SIN MUNICIPIO DE REFERENCIA") %>%
  group_by(nombre_municipio) %>% summarise(desaparecidos=sum(desaparecidos)) %>% arrange(desc(desaparecidos)) %>% 
  ggplot(aes(reorder(nombre_municipio,desaparecidos),desaparecidos,fill=nombre_municipio))+
  geom_col()+
  labs(x="",y="Desaparecidos",title="Personas Desaparecidas y No Localizadas en Alcaldías de la CDMX", 
       subtitle = "Entre 1964 y 20/052022",fill="Alcaldía")+
  geom_label(aes(label=comma(desaparecidos)))+
  coord_flip()

## Comparativa Relativa inter Alcaldías ----
alcaldia_desaparecidos_cdmx$nombre_municipio <- gsub("É","E",alcaldia_desaparecidos_cdmx$nombre_municipio)
alcaldia_desaparecidos_cdmx$nombre_municipio <- gsub("Ó","O",alcaldia_desaparecidos_cdmx$nombre_municipio)
alcaldia_desaparecidos_cdmx$nombre_municipio <- gsub("Í","I",alcaldia_desaparecidos_cdmx$nombre_municipio)
alcaldia_desaparecidos_cdmx$nombre_municipio <- gsub("Á","A",alcaldia_desaparecidos_cdmx$nombre_municipio)
alcaldia_desaparecidos_cdmx$nombre_municipio <- gsub("Ú","U",alcaldia_desaparecidos_cdmx$nombre_municipio)
alcaldia_desaparecidos_cdmx$nombre_municipio <- gsub("MAGDALENA.*","LA MAGDALENA CONTRERAS",alcaldia_desaparecidos_cdmx$nombre_municipio)

alcaldia_desaparecidos_cdmx %>% pivot_longer(c(hombre,mujer,indeterminado),names_to = "sexo",values_to = "desaparecidos") %>% 
  left_join(poblacion_cdmx,by="nombre_municipio") %>% filter(nombre_municipio !="SIN MUNICIPIO DE REFERENCIA") %>% 
  mutate(por_mil = desaparecidos/(poblacion/1000)) %>% 
  ggplot(aes(reorder(nombre_municipio,-por_mil),por_mil,group=desaparecidos,fill=sexo))+
  geom_col(position="stack")+
  labs(x="",y="Desaparecidos por cada Mil personas",title="Número de Desaparecidos por cada Mil Personas",
       subtitle = "En la CDMX entre 1964 y 18/052022")+
  geom_label(aes(label=round(por_mil,2)),position = position_stack(vjust = 0.5))+
  coord_flip()
  




