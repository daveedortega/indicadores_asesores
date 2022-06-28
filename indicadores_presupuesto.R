## Indicadores de Presupuesto
## DAOA - 07/06

## Preparar Espacio ----
rm(list=ls())
pacman::p_load(tidyverse,janitor)
dev.off()
## Cargar Bdd ----

presupuesto_21 <- read_csv("input/presupuesto_2021/Vinculacion_ODS-Pp_PPEF2022.csv",
                             locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
asignacion_ejecusion21 <- read_csv("/Users/dortega/Downloads/asignacion_ejecucion_2021.csv",
                                   locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

# Presupuesto en Salud
presupuesto_21 %>% glimpse()
presupuesto_21 %>% filter(meta_del_objetivo_de_desarrollo_sostenible==4)


presupuesto_21 %>% filter(ramo_descripcion %in% c("Instituto Mexicano del Seguro Social","Salud"))








asignacion_ejecusion21 %>% count(concepto)
asignacion_ejecusion21 %>% count(ramo_o_entidad) %>% data.frame()


asignacion_ejecusion21 %>% glimpse()






