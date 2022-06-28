### Indicadores Macroeconomía
## David A. Ortega

## Preparar Espacio ----
pacman::p_load(tidyverse,scales,janitor)
rm(list=ls())
dev.off()
## Cargas Bases----
remesas_banxico <- read_csv("input/macroeconomia/remesas_banxico.csv") %>% clean_names()
# SDT CONASAMI 2020
salario_min_sdt <- read.csv("input/macroeconomia/serie_salariomin_base64.csv")

# enoe 2022 1t

enoe_22 <- read_csv("input/macroeconomia/enoe_2022_1t.csv") %>% clean_names()
## Remesas ----
remesas_banxico$titulo <- remesas_banxico$titulo %>% as.Date(tryFormats=c("%d/%m/%Y"))
colnames(remesas_banxico)[1] <- "fecha"

glimpse(remesas_banxico)

ieps_22 <- read_csv("input/presupuesto_2021/ieps_0522.csv") %>% clean_names()
colnames(ieps_22 )[1] <- "mes"

## Niveles de remesas --
ggplot(remesas_banxico,aes(fecha,remesas_familiares_total_2))+
  geom_line()+
  geom_smooth()+
  geom_label(data= data.frame(fech=c(as.Date("2018-11-01"),as.Date("2021-10-01"),as.Date("2022-03-01")), 
                           remesas_familiares_total =c(2965,4822,4681)) ,
             aes(x=fech,y=remesas_familiares_total,label=paste0(fech," : $", comma(remesas_familiares_total))),nudge_x =-15) +
  labs(x="",y="Millones de Dólares",title="Remesas en Millones de Dólares entre Enero 1995 y 2022", subtitle = "Serie Mensual",
       caption = "Fuente: Banxico")+
  xlim(as.Date("1995-01-01"),as.Date("2022-04-01"))
  
## Cómo entran 

remesas_banxico %>% select(fecha,remesas_familiares_money_orders_8,remesas_familiares_cheques_personales_9,
                           remesas_familiares_transferencias_electronicas_10,remesas_familiares_efectivo_y_especie_11) %>% 
  pivot_longer(c(remesas_familiares_money_orders_8,remesas_familiares_cheques_personales_9,
                 remesas_familiares_transferencias_electronicas_10,remesas_familiares_efectivo_y_especie_11),
               names_to = "remesas",values_to = "dolares") %>% 
  mutate(ano= as.numeric(format(fecha,format="%Y"))) %>% 
  mutate(remesas=case_when(remesas=="remesas_familiares_money_orders_8" ~ "Money Orders",
                           remesas=="remesas_familiares_cheques_personales_9" ~ "Cheques Personales",
                           remesas=="remesas_familiares_transferencias_electronicas_10" ~ "Transferencias Electrónicas",
                           remesas=="remesas_familiares_efectivo_y_especie_11" ~ "Efectivo")) %>% 
  group_by(ano,remesas) %>% summarise(dolares=sum(dolares)) %>% filter(ano>2010) %>% 
  ggplot(aes(factor(ano),dolares,group=remesas,fill=remesas))+
  geom_col(position="stack") +
  geom_label(aes(label=comma(dolares)),position="stack")+
  labs(x="",y="Número de transferencias Electrónicas",title="Número de Operaciones de entrada de Remesas",fill="Tipo de Entrada",
       caption="Fuente: Banxico")+
  scale_y_log10()+coord_flip()

## Promedio de Remesa

glimpse(remesas_banxico)

remesas_banxico %>% pivot_longer(c(remesas_familiares_promedio_money_orders,remesas_familiares_promedio_cheques_personales,
                                   remesas_familiares_promedio_transferencias_electronicas,remesas_familiares_promedio_efectivo_y_especie)
                                 ,names_to = "remesas",values_to = "dolares") %>% 
  select(fecha, remesas, dolares, total=remesas_familiares_promedio_total) %>% 
  mutate(remesas=case_when(remesas=="remesas_familiares_promedio_money_orders"~"Promedio Money Orders",
                           remesas=="remesas_familiares_promedio_cheques_personales"~"Promedio Cheques Personales",
                           remesas=="remesas_familiares_promedio_transferencias_electronicas"~"Promedio Transferencias Electrónicas",
                           remesas=="remesas_familiares_promedio_efectivo_y_especie"~"Promedio Efectivo y Especie"))%>% 
  mutate(porcentaje = round((dolares/total)*100,2)) %>% 
  ggplot(aes(fecha,dolares,group=remesas,color=remesas))+
  geom_line(size=1)+
  labs(title = "Promedio de Dinero por Transacción entre 1995 y Marzo 2022", x="",y="Dólares",
       caption="Fuente: SIE Banxico")+
  geom_label(data=data.frame(fecha= c(as.Date("2022-03-01"),as.Date("2022-03-01"),as.Date("2022-03-01"),as.Date("2022-03-01")),
                             remesas=c("Promedio Money Orders",
                                       "Promedio Cheques Personales",
                                       "Promedio Transferencias Electrónicas",
                                       "Promedio Efectivo y Especie"), dolares= c(1003,0,393,393)), 
             aes(fecha,dolares, group=remesas,label=comma(dolares)), nudge_x =-15)

## Serie Salario Minimo

ggplot(salario_min_sdt,aes(ano,salaio.minimo.real,group=pesos,color=pesos))+
  geom_line(size=1)+
  labs(x="",y="Pesos",title="Salario Minimo Real Promedio en CDMX",subtitle = "Con base en 1964, deflactado con el INPC del INEGI",
       caption = "Fuente: CONASAMI",color="Moneda")+
  geom_label(data = data.frame(ano=c(1976,1994,2018,2020),salaio.minimo.real=c(277.10,99.86,90.86,128.41 ),pesos=c("viejos pesos","nuevos pesos","pesos","pesos")),
             aes(label=paste0(ano,": $",salaio.minimo.real)))+
  theme(plot.title = element_text(size=18,face="bold"))

# Variacion porcentual
ggplot(salario_min_sdt,aes(ano,variacion,group=pesos,color=pesos))+
  geom_line(y=0,size=1,color="black")+
  geom_line(size=1)+
  labs(x="",y="Porcentaje",title="Variación del Salario Minimo Real Promedio en CDMX",subtitle = "Con base en 1964, deflactado con el INPC del INEGI",
       caption = "Fuente: CONASAMI",color="Moneda")+
  geom_label(data = data.frame(ano=c(1938,1956,1976,1994,2018,2020),variacion=c(18.75,23.16,8.33,-0.08,4.69,18.12 ),
                               pesos=c("viejos pesos","viejos pesos","viejos pesos","nuevos pesos","pesos","pesos")),
                               aes(label=paste0(ano, " %",variacion)))


salario_min_sdt %>% arrange(desc(variacion))

## Recaudacion ieps neta

ieps_22 <- ieps_22 %>% pivot_longer(c(x2014,x2015,x2016,x2017,x2018,x2019,x2020,x2021,x2022),names_to = "ano",values_to = "recaudacion")

ieps_22$ano <- gsub("x","",ieps_22$ano)

ieps_22 %>% mutate(fecha = as.Date(paste0(ano,"-",mes,"-01"))) %>% 
  ggplot(aes(fecha,recaudacion))+
  geom_point(size=1,color="#E3381E")+
  geom_smooth(color="#74C649",size=1)+
  labs(x="",y="Millones de Millones de Pesos",title="Recaudación Federal por Concepto de IEPS a la Gasolina",
       subtitle = "Entre Enero 2014 y Abril 2022",caption = "Fuente: SHCP")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16))+
  geom_label(data=data.frame(fecha=c(as.Date("2015-01-01"),as.Date("2021-04-01"),as.Date("2022-04-01")),recaudacion=c(1040, 782,94.6)),
             aes(label=paste0(fecha," $: ",comma(recaudacion))),vjust=-0.5)

## ENOE 2022

enoe_22 <- enoe_22[1:15,]

enoe_22 %>% filter(!actividad %in% c("todas las actividades","total_terciario","total_secundario")) %>% 
  ggplot(aes(sector,total_3,group=actividad,fill=actividad))+
  geom_col(position = "stack")+
  geom_label(aes(label=comma(total_3)),position="stack")+
  labs(x="",y="Número de Mexicanos Empleados",title="Número de Mexicanos Empleados por Sector de Actividad Económica",
       subtitle = "Durante el Primer Trimestre de 2022", caption = "Fuente: ENOE 2022",fill="")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16,face="bold"),
        legend.position = "bottom")+
  scale_y_sqrt()

## Areas uRBANIZADAS
enoe_22 %>% filter(!actividad %in% c("todas las actividades","total_terciario","total_secundario")) %>% 
  ggplot(aes(sector,areas_mas_urbanizadas,group=actividad,fill=actividad))+
  geom_col(position = "stack")+
  geom_label(aes(label=comma(areas_mas_urbanizadas)),position="stack")+
  labs(x="",y="Número de Mexicanos Empleados",title="Número de Mexicanos Empleados por Sector de Actividad Económica",
       subtitle = " En las áreas más urbanizadas del país Durante el Primer Trimestre de 2022", caption = "Fuente: ENOE 2022",fill="")+
  theme(plot.title = element_text(size=18,face="bold"),plot.subtitle = element_text(size=16,face="bold"),
        legend.position = "bottom")+
  scale_y_sqrt()  


enoe_22 %>% filter(!actividad %in% c("todas las actividades","total_terciario","total_secundario")) %>% 
  select(sector,actividad,urbano_medio,urbano_bajo,rural,areas_mas_urbanizadas) %>% 
  pivot_longer(c("urbano_medio","urbano_bajo","rural","areas_mas_urbanizadas"),names_to = "area",values_to = "empleados") %>% 
  filter(sector=="Primario") %>%
  mutate(area=case_when(area=="areas_mas_urbanizadas"~"Áreas más Urbanizadas",
                        area=="rural"~"Rural",
                        area=="urbano_medio"~"Urbano Medio",
                        area=="urbano_bajo"~"Urbána Baja")) %>% 
  mutate(porcentaje=round(empleados/sum(empleados)*100,2)) %>% 
  ggplot(aes(sector,porcentaje,group=area,fill=area))+
  geom_col(position = "stack",color = "black")+
  geom_label(aes(label=paste0("%",porcentaje)),
             position = position_stack(vjust = .9 ),
             show.legend = FALSE,color="white")+
  coord_polar(theta = "y")+
  labs(x="",y="Número de Mexicanos Empleados",title="Número de Mexicanos Empleados en el Sector Primario",
       subtitle = " Por tipo de áreas Durante el Primer Trimestre de 2022", caption = "Fuente: ENOE 2022",fill="")+
  theme(plot.title = element_text(size=18,face="bold"),plot.subtitle = element_text(size=16,face="bold"),
        legend.position = "bottom")





























