### Indicadores Agricultura
## David A. Ortega

## Preparar Espacio ----
pacman::p_load(tidyverse,scales,janitor,sf,leaflet)
rm(list=ls())
dev.off()
## Cargas Bases ----
ganaderia_80 <- read_csv("input/ganaderia/cierre_1980.csv", 
                         locale = locale(encoding = "ISO-8859-2"))
ganaderia_85 <- read_csv("input/ganaderia/cierre_1985.csv", 
                         locale = locale(encoding = "ISO-8859-2"))
ganaderia_90 <- read_csv("input/ganaderia/cierre_1990.csv", 
                         locale = locale(encoding = "ISO-8859-2"))
ganaderia_95 <- read_csv("input/ganaderia/cierre_1995.csv", 
                         locale = locale(encoding = "ISO-8859-2"))
ganaderia_00 <- read_csv("input/ganaderia/cierre_2000.csv", 
                         locale = locale(encoding = "ISO-8859-2"))
ganaderia_05 <- read_csv("input/ganaderia/cierre_2005.csv", 
                         locale = locale(encoding = "ISO-8859-2"))
ganaderia_10 <- read_csv("input/ganaderia/cierre_2010.csv", 
                         locale = locale(encoding = "ISO-8859-2"))
ganaderia_15 <- read_csv("input/ganaderia/cierre_2015.csv", 
                         locale = locale(encoding = "ISO-8859-2"))
ganaderia_20 <- read_csv("input/ganaderia/cierre_2020.csv", 
                         locale = locale(encoding = "ISO-8859-2"))

## Análisis ----
## Consumo de carne total

ganaderia_80 %>% glimpse()
ganaderia_20 %>% glimpse()

ganaderia_80 %>% count(Nomproducto)
ganaderia_85 %>% count(Nomproducto)
ganaderia_90 %>% count(Nomproducto)
ganaderia_95 %>% count(Nomproducto)
ganaderia_00 %>% count(Nomproducto)
ganaderia_05 %>% count(Nomproducto)
ganaderia_10 %>% count(Nomproducto)
ganaderia_15 %>% count(Nomproducto)
ganaderia_20 %>% count(Nomproducto)


## Carnes

ganaderiasdt <- data.frame()

ganaderiasdt <- rbind(ganaderia_80 %>% group_by(Nomespecie,Nomproducto) %>% summarise(volumen=sum(Volumen)) %>% mutate(ano=1980))
ganaderiasdt <-rbind(ganaderiasdt,ganaderia_85 %>% group_by(Nomespecie,Nomproducto) %>% summarise(volumen=sum(Volumen)) %>% mutate(ano=1985))
ganaderiasdt <-rbind(ganaderiasdt,ganaderia_90 %>% group_by(Nomespecie,Nomproducto) %>% summarise(volumen=sum(Volumen)) %>% mutate(ano=1990))
ganaderiasdt <-rbind(ganaderiasdt,ganaderia_95 %>% group_by(Nomespecie,Nomproducto) %>% summarise(volumen=sum(Volumen)) %>% mutate(ano=1995))
ganaderiasdt <-rbind(ganaderiasdt,ganaderia_00 %>% group_by(Nomespecie,Nomproducto) %>% summarise(volumen=sum(Volumen)) %>% mutate(ano=2000))
ganaderiasdt <-rbind(ganaderiasdt,ganaderia_05 %>% group_by(Nomespecie,Nomproducto) %>% summarise(volumen=sum(Volumen)) %>% mutate(ano=2005))
ganaderiasdt <-rbind(ganaderiasdt,ganaderia_10 %>% group_by(Nomespecie,Nomproducto) %>% summarise(volumen=sum(Volumen)) %>% mutate(ano=2010))
ganaderiasdt <-rbind(ganaderiasdt,ganaderia_15 %>% group_by(Nomespecie,Nomproducto) %>% summarise(volumen=sum(Volumen)) %>% mutate(ano=2015))
ganaderiasdt <-rbind(ganaderiasdt,ganaderia_20 %>% group_by(Nomespecie,Nomproducto) %>% summarise(volumen=sum(Volumen)) %>% mutate(ano=2020))
# Serie de Tiempo
ganaderiasdt %>% filter(Nomproducto=="Carne") %>% 
  ggplot(aes(ano,volumen,group=Nomespecie,color=Nomespecie))+
  geom_line(size=1)+
  labs(x="",y="Toneladas",title = "Volumen de Carne Nacional Por tipo de Especie",subtitle = "Entre 1980 y 2020",caption = "Fuente: SIAP", fill="Especie")+
  geom_label(data=data.frame(ano=c(2020,2020,2020,2020,2020,2020),Nomespecie=c("Ave","Bovino","Caprino","Guajolote","Ovino","Porcino"),
                             volumen=c(3578694,2081262,40001,17083,64758,1652362)),aes(label=comma(volumen)))

# Estados Productivos

ganaderia_20 %>% filter(Nomproducto=="Carne") %>% group_by(Nomestado) %>% summarise(valor=sum(Valor)) %>% arrange(desc(valor)) %>% 
  mutate(porcentaje=round(valor/sum(valor),2)) %>% 
  ggplot(aes(reorder(Nomestado,valor),valor,fill=Nomestado))+
  geom_col()+
  geom_label(aes(label=comma(valor)))+
  coord_flip()+
  theme(legend.position =  "none")+
  labs(x="",y="Miles de Pesos",title="Principales Estados Ganaderos en 2020",subtitle = "Valor de Mercado de Carne en Miles de Pesos",caption = "SIAP: 2020")
  
## Leche
ganaderiasdt %>% filter(Nomproducto=="Leche") %>% 
  ggplot(aes(ano,volumen,group=Nomespecie,color=Nomespecie))+
  geom_line(size=1)+
  geom_label(data=data.frame(Nomespecie=c("Bovino","Caprino"),ano=c(2020,2020),volumen=c(12563699,163590)),aes(label=comma(volumen)))+
  labs(x="",y="",title="Miles de Litros de Leche producidos en México",subtitle = "Entre 1980 y 2020",caption = "Fuente: SIAP",color="Especie")


## Cabezas de Ganado

ganaderia_20 %>% filter(Nomproducto=="Leche",Nomespecie=="Bovino") %>% group_by(Nomestado,Anio) %>% summarise(Valor=sum(Valor)) %>% 
  ggplot(aes(reorder(Nomestado, Valor),Valor,fill=Nomestado))+
  geom_col()+
  theme(legend.position = "none")+
  coord_flip()+
  geom_label(aes(label=comma(Valor)))+
  labs(x="",y="Miles de Litros",title="Estados por Producción de Leche Bovina en 2020",caption = "Fuente:SIAP",subtitle = "En Miles de Litros de Leche")

## Huevo

ganaderia_20 %>% filter(Nomproducto=="Huevo-plato") %>% group_by(Nomestado) %>% summarise(volumen=sum(Volumen)) %>% 
  mutate(porcent=volumen/sum(volumen)*100) %>% arrange(desc(porcent))%>% 
  ggplot(aes(reorder(Nomestado,volumen),volumen,fill=Nomestado))+
  geom_col()+
  theme(legend.position = "none")+
  geom_label(aes(label=comma(volumen)))+
  labs(x="",y="Toneladas",title="Toneladas de Huevo producidas en 2020",subtitle = "Por estado en 2020", caption = "Fuente: SIAP")+
  coord_flip()+
  theme(plot.title = element_text(size=28))


## Huevo SDT

ganaderiasdt %>% filter(Nomproducto=="Huevo-plato") %>% 
  ggplot(aes(ano,volumen))+
  geom_line(size=2,color="#9F2241")+
  geom_label(data=data.frame(ano=c(1980,2010,2020),volumen=c(644427,2381375,3015959)),aes(label=paste0(ano,":  " ,comma(volumen))))+
  labs(x="",y="Toneladas",title="Toneladas de Huevo Producidas en México",subtitle = "Quinquenios entre 1980 y 2020",caption = "Fuente: SIAP")+
  theme(plot.title = element_text(size=22))


## 

ganaderia_20 %>% filter(Nomproducto=="Carne") %>% group_by(Nomespecie) %>% 
  summarise(volumen=sum(Volumen),sacrificado=sum(Asacrificado)) %>% 
  mutate(rendimiento=volumen/sacrificado*1000)


















