## Indicadores Energéticos
# David A. Ortega

# Prepara Entorno ----
pacman::p_load(tidyverse,scales, plotly, janitor)
rm(list=ls())
dev.off()
## Cargar BdD ----

# Compra - Venta Derivados de Petróleo: SIE BANXICO
balanza_energeticos <- read_csv("input/energia/balaza_energetica_sie.csv") %>% clean_names()
# Serie de tiempo, generación cfe
generacion_cfe <- read_csv("input/energia/generacion_nacional.csv") %>% clean_names()
# BP - Refinación miles de barriles diarios
refinacion_bp <- read_csv("input/energia/refinacion_bp2020.csv") %>% clean_names()
colnames(refinacion_bp)[1] <- "pais"
# BP - Consumo miles de barriles diarios
consumo_cbp <- read_csv("input/energia/consumo_petroleo_bp2020.csv") %>% clean_names()
colnames(consumo_cbp)[1] <- "pais"

#
## Análisis Balanza ----
glimpse(balanza_energeticos)
balanza_energeticos <- balanza_energeticos %>% mutate(fecha=as.Date(fecha,tryFormats ="%d/%m/%Y"))

## Gas LP
balanza_energeticos %>% select(fecha,exportaciones_de_gas_l_p,importaciones_de_gas_l_p) %>% 
  pivot_longer(c(exportaciones_de_gas_l_p,importaciones_de_gas_l_p),names_to = "balanza",values_to="mdd") %>% 
  mutate(balanza=ifelse(balanza=="importaciones_de_gas_l_p","Importaciones","Exportaciones")) %>% 
  ggplot(aes(fecha,mdd,group=balanza,color=balanza))+
  geom_point()+
  geom_smooth()+
  geom_label(data=data.frame(fecha=c(as.Date("2022-04-01"),as.Date("2022-04-01"),as.Date("1994-01-01"),as.Date("1994-01-01"),as.Date("2022-03-01")),
                             balanza=c("Exportaciones","Importaciones","Exportaciones","Importaciones","Importaciones")
                             ,mdd=c(7,310084,9963,18597,472633)),
             aes(label=paste0(fecha," - $",comma(mdd))),vjust=-0.1,nudge_x = -250)+
  labs(x="",y="Millones de Dólares",title="Balanza Comercial de Gas L.P. entre 1993 y 2022 para Gas L.P.",
       subtitle = "Niveles en millones de dólares", caption = "Fuente: BAXICO - SIE",color="Concepto")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "bottom")+
  scale_y_sqrt()
  
## Gasolina p. Vehículos

balanza_energeticos %>% select(fecha,exportaciones_de_gasolina_para_vehiculos,importaciones_de_gasolina_para_vehiculos) %>% 
  pivot_longer(c(exportaciones_de_gasolina_para_vehiculos,importaciones_de_gasolina_para_vehiculos),names_to = "balanza",values_to="mdd") %>% 
  mutate(balanza=ifelse(balanza=="importaciones_de_gasolina_para_vehiculos","Importación","Exportación")) %>% 
  ggplot(aes(fecha,mdd,group=balanza,color=balanza))+
  geom_point()+
  scale_y_sqrt()+
  geom_smooth()+
  labs(x="",y="Millones de Dólares",title="Balanza Comercial de Gasolina para vehículos entre 1993 y 2022",
       subtitle = "Niveles en millones de dólares", caption = "Fuente: BAXICO - SIE",color="Concepto")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "bottom")+
  geom_label(data=data.frame(fecha=c(as.Date("2022-04-01"),as.Date("2018-10-01"),as.Date("2003-02-01"),as.Date("2022-04-01")),
                             balanza=c("Importación","Importación","Exportación","Exportación"),
                             mdd=c(2021249,1962816,14743,0)),aes(label=paste0(fecha,"  - $ ",comma(mdd))),nudge_x = -500,vjust=-0.25)+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "bottom")
  



## Combustoleo

balanza_energeticos %>% select(fecha,exportaciones_de_gas_natural,importaciones_de_gas_natural) %>% 
  pivot_longer(c(exportaciones_de_gas_natural,importaciones_de_gas_natural),names_to = "balanza",values_to="mdd") %>% 
  mutate(balanza=ifelse(balanza=="importaciones_de_gas_natural","Importación","Exportación")) %>% 
  ggplot(aes(fecha,mdd,group=balanza,color=balanza))+
  geom_point()+
  scale_y_sqrt()+
  geom_smooth()+
  labs(x="",y="Millones de Dólares",title="Balanza Comercial de Combustóleo entre 1993 y 2022",
       subtitle = "Niveles en millones de dólares", caption = "Fuente: BAXICO - SIE",color="Concepto")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "bottom")+
  geom_label(data=data.frame(fecha=c(as.Date("2008-08-01"),as.Date("2022-04-01"),as.Date("2022-04-01"),as.Date("2022-03-01")),
                             balanza=c("Importación","Importación","Exportación","Exportación"),mdd=c(446541,0,321723,499465)),
             aes(label=paste0(fecha," - $",comma(mdd))),nudge_x = -500,vjust=-0.25)

## Gas Natural

balanza_energeticos %>% select(fecha,exportaciones_de_gas_natural,importaciones_de_gas_natural) %>% 
  pivot_longer(c(exportaciones_de_gas_natural,importaciones_de_gas_natural),names_to = "balanza",values_to="mdd") %>% 
  mutate(balanza=ifelse(balanza=="importaciones_de_gas_natural","Importación","Exportación")) %>% 
  ggplot(aes(fecha,mdd,group=balanza,color=balanza))+
  geom_point()+
  scale_y_sqrt()+
  geom_smooth()+
  labs(x="",y="Millones de Dólares",title="Balanza Comercial de Gas Natural entre 1993 y 2022",
       subtitle = "Niveles en millones de dólares", caption = "Fuente: BAXICO - SIE",color="Concepto")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "bottom")+
  geom_label(data=data.frame(fecha=c(as.Date("2022-04-01"),as.Date("2021-03-01"),as.Date("2007-05-01"),as.Date("2022-04-01")),
                             balanza=c("Importación","Importación","Exportación","Exportación"),mdd=c(991861,3582453,63010,3120)),
             aes(label=paste0(fecha," - $",comma(mdd))),nudge_x = -500,vjust=-0.25)



generacion_cfe %>% glimpse()

generacion_cfe <- generacion_cfe %>% mutate(ene_2021=as.numeric(ene_2021),
                          feb_2021=as.numeric(feb_2021),
                          mar_2021=as.numeric(mar_2021),
                          abr_2021=as.numeric(abr_2021),
                          may_2021=as.numeric(may_2021),
                          jun_2021=as.numeric(jun_2021),
                          jul_2021=as.numeric(jul_2021),
                          ago_2021=as.numeric(ago_2021),
                          sep_2021=as.numeric(sep_2021),
                          oct_2021=as.numeric(oct_2021),
                          nov_2021=as.numeric(nov_2021),
                          dic_2021=as.numeric(dic_2021),
                          ene_2022=as.numeric(ene_2022),
                          feb_2022=as.numeric(feb_2022),
                          mar_2022=as.numeric(mar_2022)) %>% 
  pivot_longer(!c(tipo,generacion),names_to = "fecha",values_to = "megawatts") %>% 
  separate(fecha,into = c("mes","ano"),sep="_")


generacion_cfe <- generacion_cfe %>% mutate(fecha=case_when(
  mes=="ene"~as.Date(paste0(ano,"-01-01")),
  mes=="feb"~as.Date(paste0(ano,"-02-01")),
  mes=="mar"~as.Date(paste0(ano,"-03-01")),
  mes=="abr"~as.Date(paste0(ano,"-04-01")),
  mes=="may"~as.Date(paste0(ano,"-05-01")),
  mes=="jun"~as.Date(paste0(ano,"-06-01")),
  mes=="jul"~as.Date(paste0(ano,"-07-01")),
  mes=="ago"~as.Date(paste0(ano,"-08-01")),
  mes=="sep"~as.Date(paste0(ano,"-09-01")),
  mes=="oct"~as.Date(paste0(ano,"-10-01")),
  mes=="nov"~as.Date(paste0(ano,"-11-01")),
  mes=="dic"~as.Date(paste0(ano,"-12-01")))) %>% mutate(sexenio=case_when(
    ano %in% c(2002,2003,2004,2005,2006)~"Fox",
    ano %in% c(2007,2008,2009,2010,2011,2012)~"Calderón",
    ano %in% c(2013,2014,2015,2016,2017)~"EPN",
    ano >2017 ~"AMLO")) 

# Total por Sexenio
generacion_cfe %>% 
  filter(tipo=="total") %>% 
  ggplot(aes(fecha,megawatts,group=sexenio,color=sexenio))+
  geom_line(size=1)+
  geom_smooth()+
  labs(x="",y="MegaWatts por Hora",title="Generación total de Energía por la CFE", subtitle = "Entre 2002 y Marzo 2021",
       color="Sexenio",caption = "Fuente: SENER")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "bottom")+
  scale_y_sqrt()+
  geom_label(data=data.frame(fecha=c(as.Date("2016-06-01"),as.Date("2019-08-01"),as.Date("2012-08-01"),as.Date("2022-03-01")),
                        sexenio=c("EPN","AMLO","Calderón","AMLO"),
                        megawatts=c(24409857,24960826,24447819,18352702)),aes(label=paste0("MwxH:",comma(megawatts))),nudge_x = -100)

# Promedio Estacional

generacion_cfe %>% 
  filter(tipo=="total") %>% group_by(mes) %>% summarise(promedio = mean(megawatts))%>% 
  ggplot(aes(reorder(mes,promedio),promedio,fill=mes))+
  geom_col()+
  labs(x="",y="MegaWatts por Hora",title="Generación Promedio de Energía por Mes CFE", subtitle = "Entre 2002 y Marzo 2021",caption = "Fuente: SENER")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "none")+
  geom_label(aes(label=paste0(mes,": ",comma(promedio))))+
  coord_flip()
  
## Por tipo de Energía

generacion_cfe %>% count(generacion,tipo)
generacion_cfe %>% filter(!tipo %in% c("ciclo_total","termo_total","total_eolica","total")) %>% 
  group_by(generacion,fecha) %>% summarise(megawatts=sum(megawatts)) %>% 
  ggplot(aes(fecha,megawatts,group=generacion,color=generacion))+
  geom_line(y=0,color="black")+
  geom_smooth()+
  labs(x="",y="MegaWatts por Hora",title="Generación de Energía por Fuente en México", subtitle = "Línea de Tendencia Entre 2002 y Marzo 2021",
       color="Tipo de Energía",caption = "Fuente: SENER")+
  theme(plot.title = element_text(size=24,face="bold",color="#691C32"),plot.subtitle = element_text(size=16),legend.position = "bottom",
        legend.text = element_text(size=12),legend.title = element_text(size=14,face="bold"))+
  scale_y_sqrt()+
  geom_label(data=data.frame(fecha=c(as.Date("2022-03-01"),as.Date("2022-03-01"),as.Date("2022-03-01"),as.Date("2022-03-01"),
                                     as.Date("2022-03-01"),as.Date("2022-03-01"),as.Date("2022-03-01")),
                             generacion = c("Termoeléctrica","Nucleoeléctrica","Geotermoléctrica","Eólica","Dual","Ciclo combinado",
                                            "Carboeléctrica"),megawatts=c(1180556,1005234,426844,2137148,0,12162515,1440405)),
             aes(label=paste0(generacion," en Marzo 2022: ",comma(megawatts))),nudge_x = -500)+
  geom_label(data=data.frame(fecha=c(as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01"),
                                     as.Date("2002-01-01"),as.Date("2002-01-01"),as.Date("2002-01-01")),
                             generacion = c("Termoeléctrica","Nucleoeléctrica","Geotermoléctrica","Eólica","Dual","Ciclo combinado",
                                            "Carboeléctrica"),megawatts=c(6554705,876290,469343,2141249,1233101,3203257,1198640)),
             aes(label=paste0(generacion," en Enero 2002: ",comma(megawatts))),nudge_x = 500)


## Refinación BP ----

refinacion_bp <- refinacion_bp[1:57] %>% pivot_longer(!c(pais),names_to = "ano",values_to = "mdbd")

refinacion_bp$ano <- as.numeric(gsub("x","",refinacion_bp$ano))

refinacion_bp %>% filter(pais %in% c("Brazil","Venezuela","US","Saudi Arabia","Iran","Canada","Mexico")) %>% 
  ggplot(aes(ano,mdbd,group=pais,color=pais))+
  geom_line(size=1)+
  scale_y_log10()+
  geom_label(data=labs,aes(label=paste0(ano, ": ",comma(mdbd))))+
  labs(x="",y="Miles de Barriles Diarios",title="Capacidad de Refinación de Barriles Diaria entre 1965 y 2020",
       subtitle="En Miles de Barriles Diarios",caption="Fuente: BP Statistical Review of World Energy 2021",color="País")+
  theme(plot.title = element_text(size=24,face="bold",color="#691C32"),plot.subtitle = element_text(size=16),legend.position = "bottom",
        legend.text = element_text(size=12),legend.title = element_text(size=14,face="bold"))

labs <- refinacion_bp %>% filter(pais %in% c("Brazil","Venezuela","US","Saudi Arabia","Iran","Canada","Mexico"),ano==2020)
labs <- rbind(labs,refinacion_bp %>% filter(pais %in% c("Brazil","Venezuela","US","Saudi Arabia","Iran","Canada","Mexico"),ano==2000))
## Oil Consumption BP ----

consumo_cbp <- consumo_cbp %>%  pivot_longer(!pais,names_to = "ano",values_to = "mdbd")
consumo_cbp$ano <- as.numeric(gsub("x","",consumo_cbp$ano))
consumo_cbp$mdbd <- as.numeric(consumo_cbp$mdbd)

consumo_cbp %>% filter(pais %in% c("Brazil","Venezuela","US","Saudi Arabia","Iran","Canada","Mexico","France","Spain")) %>% 
  ggplot(aes(ano,mdbd,group=pais,color=pais))+
  geom_line()+
  scale_y_log10()+
  labs(x="",y="Miles de Barriles Diarios",title="Consumo de Barriles de Petroleo Diarios entre 1965 y 2020",
       subtitle="En Miles de Barriles Diarios",caption="Fuente: BP Statistical Review of World Energy 2021",color="País")+
  theme(plot.title = element_text(size=24,face="bold",color="#691C32"),plot.subtitle = element_text(size=16),legend.position = "bottom",
        legend.text = element_text(size=12),legend.title = element_text(size=14,face="bold"))+
  geom_label(data=labs,aes(label=comma(mdbd)))
  
labs <- consumo_cbp %>% filter(pais %in% c("Brazil","Venezuela","US","Saudi Arabia","Iran","Canada","Mexico","France","Spain"),ano==2020)
  
















































