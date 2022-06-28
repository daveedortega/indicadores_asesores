## Indicadores de Agua
# David A. Ortega
###

# Preparar Espacio ----
pacman::p_load(tidyverse,scales,janitor)
rm(list=ls())
dev.off()
## Cargar bases

presas_actual <- read_csv("input/agua/presas_130622.csv") %>% clean_names() %>% select(1:6)



## Niveles de Agua actuales en presas del maís

presas_actual %>% mutate(porcentaje = round(almacenamiento_actual_hm3/namo_almacenamiento_hm3*100,2)) %>% 
  group_by()

## Presas
presas_actual %>% group_by(estado) %>% summarise(potencial = sum(namo_almacenamiento_hm3),actual = sum(almacenamiento_actual_hm3)) %>% 
  mutate(porcentaje = round(actual/potencial * 100,2)) %>% 
  ggplot(aes(reorder(estado,porcentaje),porcentaje,fill=estado))+
  geom_col()+
  coord_flip()+
  labs(x="",y=" Porcentaje", title = "Porcentaje de Nivel de Presas a 13 de Junio de 2022", subtitle = "Por Estado",
       caption = "CONAGUA : SINA")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),
        legend.position = "none")+
  geom_label(aes(label = paste0("%", porcentaje)))

presas_actual %>% filter(estado=="Morelos")






















































































