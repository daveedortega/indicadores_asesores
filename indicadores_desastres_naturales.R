### Indicadores Medio Ambiente
## D. Ortega

## Preparar Espacio ----
pacman::p_load(tidyverse,janitor,scales,readxl)
rm(list=ls())
dev.off()
## Cargar BdD
# Impactos de  Desastres Naturales desde 2000
impacto_dn <- read_xlsx("input/ma/Impactos_Base_Histo_Anual_Publica_2000_2020.xlsx") %>% clean_names()


## Desastres Naturales por Tipo
impacto_dn <- impacto_dn %>% mutate(poblacion_afectada=as.numeric(poblacion_afectada))

impacto_dn %>% select(ano,estado, tipo_de_fenomeno,total_de_danos_millones_de_pesos,defunciones,poblacion_afectada) %>%
  group_by(ano,estado,tipo_de_fenomeno) %>% 
  summarise(total_daños_mdp=sum(total_de_danos_millones_de_pesos),
            defunciones=sum(defunciones),
            poblacion_afectada=sum(poblacion_afectada))


impacto_dn %>% filter(tipo_de_fenomeno=="CT") %>% group_by(ano) %>% count() %>% 
  ggplot(aes(ano,n))+
  geom_line(size=1,color="#3C3B6E")+
  labs(title="Número de Ciclones Tropicales entre 2000 y 2020",caption = "Fuente: CENAPRED",x="",y="Número de Ciclones")+
  geom_label(aes(label=n))+
  geom_smooth(se=F,color="red")

# Costo CT en estados costeros

impacto_dn %>% filter(tipo_de_fenomeno=="CT", estado %in% c("Veracruz","Tamaulipas","Tamaulipas, Veracruz",
                                                             "Campeche","Chiapas","Guerrero","Nayarit","Michoacán",
                                                             "Yucatán","Quintana Roo","Oaxaca","Tabasco",
                                                            "Colima","Jalisco","Baja California Sur","Baja California","Sinaloa","Sonora")) %>% 
  mutate(estado=ifelse(estado=="Tamaulipas, Veracruz","Tamaulipas",estado))%>% group_by(estado) %>% 
  summarise(defunciones=sum(defunciones), 
            total_de_danos_millones_de_pesos=sum(total_de_danos_millones_de_pesos),
            poblacion_afectada=sum(poblacion_afectada)) %>% 
  ggplot(aes(reorder(estado,total_de_danos_millones_de_pesos),total_de_danos_millones_de_pesos,fill=estado))+
  geom_col()+
  labs(title="Daños Económicos de Ciclones Tropicales entre 2000 y 2020",
       subtitle = "En Estados Costeros",caption = "Fuente: CENAPRED",x="",y="Millones de Pesos")+
  geom_label(aes(label=comma(total_de_danos_millones_de_pesos)))+
  coord_flip()

# Defunciones CT en estados costeros

impacto_dn %>% filter(tipo_de_fenomeno=="CT", estado %in% c("Veracruz","Tamaulipas","Tamaulipas, Veracruz",
                                                            "Campeche","Chiapas","Guerrero","Nayarit","Michoacán",
                                                            "Yucatán","Quintana Roo","Oaxaca","Tabasco",
                                                            "Colima","Jalisco","Baja California Sur","Baja California","Sinaloa","Sonora")) %>% 
  mutate(estado=ifelse(estado=="Tamaulipas, Veracruz","Tamaulipas",estado))%>% group_by(ano) %>% 
  summarise(defunciones=sum(defunciones), 
            total_de_danos_millones_de_pesos=sum(total_de_danos_millones_de_pesos),
            poblacion_afectada=sum(poblacion_afectada)) %>% 
  ggplot(aes(ano,defunciones))+
  geom_line(size=1,color="#3C3B6D")+
  labs(title="Defunciones por Ciclones Tropicales entre 2000 y 2020",
       caption = "Fuente: CENAPRED",x="",y="Defunciones")+
  geom_label(aes(label=defunciones))

## Personas Afectadas por año

impacto_dn %>% filter(ano>2012,tipo_de_fenomeno=="CT", estado %in% c("Veracruz","Tamaulipas","Tamaulipas, Veracruz",
                                                            "Campeche","Chiapas","Guerrero","Nayarit",
                                                            "Yucatán","Oaxaca","Tabasco",
                                                            "Jalisco","Baja California Sur","Sinaloa","Sonora")) %>% 
  mutate(estado=ifelse(estado=="Tamaulipas, Veracruz","Tamaulipas",estado))%>% group_by(estado,ano) %>% 
  summarise(defunciones=sum(defunciones), 
            total_de_danos_millones_de_pesos=sum(total_de_danos_millones_de_pesos),
            poblacion_afectada=sum(poblacion_afectada)) %>% 
  ggplot(aes(reorder(estado,poblacion_afectada),poblacion_afectada,group=estado,fill=factor(ano)))+
  geom_col(position = "stack")+
  labs(title="Personas Afectadas por Ciclones Tropicales entre 2010 y 2020 en Estados Costeros",
       subtitle = "Nota: Para Michoacán, Quintana Roo, Colima y BC no se tienen Datos",
       caption = "Fuente: CENAPRED",x="",y="Defunciones",fill="Año")+
  geom_label(data=data.frame(estado = c("Baja California Sur","Campeche","Chiapas","Guerrero","Jalisco","Nayarit","Oaxaca","Sinaloa",
                                        "Sonora","Tabasco","Tamaulipas","Veracruz","Yucatán"),
                             poblacion_afectada=c(1319966,53707,365328,416373,172687,157117,222041,760906,991891,9604,71091,3335703,176254),
                             ano = c(2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020)),
    aes(label=comma(poblacion_afectada)),position="stack")





