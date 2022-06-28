## Primer Saque de Indicadores
## Feminicidios CDMX y Feminicidios En otros Estados
## Consejería y Asesores de Jefatura --- CDMX
## David A. Ortega 

## Usamos datos de Incidencia Delictiva y la nota clasificatoria del
# Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública
# Fuentes: 
# * Datos SESNSP: https://www.gob.mx/sesnsp/acciones-y-programas/datos-abiertos-de-incidencia-delictiva?state=published
# * Notas: https://www.gob.mx/sesnsp/acciones-y-programas/incidencia-delictiva-299891?state=published 

# Igualmente contrastamos con datos de la FGJ de carpetas de la ciudad para constrastar:

## Preparar Entorno ----
pacman::p_load(tidyverse,janitor,readxl,scales)
dev.off()
rm(list=ls())
## Cargar Datos ----
#Incidencia Delictiva nivel nacional SESNSP
incidencia_historico <-  read_csv("input/seguridad/incidencia_nacional_062021.csv", # 1997 - 2021
                                  locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
incidencia_actual <-  read_csv("input/seguridad/IDEFC_NM_may22.csv", 
                                  locale = locale(encoding = "ISO-8859-2")) %>% clean_names() # 2015 - 2022

# fgj - Datos Abiertos
fgj_cdmx <- read_csv("input/seguridad/victimas_completa_abril_2022.csv") %>% clean_names()

# Atenciones de Abogadas de las Mujeres
abogadas_mujeres <- read_csv("input/seguridad/atenciones_abogadas_de_las_mujeres_marzo.csv")

# Llamadas al 911
llamadas_911 <- read_csv("input/llamadas/911_2022_primer_semestre.csv",
                         locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
# Llamadas a Locatel
locatel <- read_csv("input/llamadas/bases_integrales_020522.csv",
                    locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

## Modelo de Evaluación y Seguimiento de la Consolidación del Sistema de Justicia Penal 
mes_2021 <- read_csv("input/seguridad/mes_2021.csv",
                     locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
glimpse(mes_2021)

## Analisis incidencia delictiva nacional --------

## Filtramos para cdmx 
table(incidencia_actual$entidad)
feminicidios_cdmx_nac <- incidencia_actual %>% filter(tipo_de_delito=="Feminicidio", entidad=="Ciudad de México")

## Filtramos por feminicidios y arreglamos la base
feminicidios_nacional <- incidencia_actual %>% filter(tipo_de_delito=="Feminicidio") %>% 
  pivot_longer(c(enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),
               names_to = "mes",values_to = "feminicidios")  %>% 
  mutate(mes= case_when(mes == "enero" ~ "01",
                        mes == "febrero" ~ "02",
                        mes == "marzo" ~ "03",
                        mes == "abril" ~ "04",
                        mes == "mayo" ~ "05",
                        mes == "junio" ~ "06",
                        mes == "julio" ~ "07",
                        mes == "agosto" ~ "08",
                        mes == "septiembre" ~ "09",
                        mes == "octubre" ~ "10",
                        mes == "noviembre" ~ "11",
                        mes == "diciembre" ~ "12")) %>% 
  group_by(ano,mes,entidad) %>% 
  summarise(feminicidios=sum(feminicidios)) %>% 
  mutate(fecha=as.Date(paste0(ano,"-",mes,"-01"))) %>%
  filter(fecha<"2022-04-01")


feminicidios_cdmx_nac <- pivot_longer(feminicidios_cdmx_nac,
                                      c(enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),
             names_to = "mes",values_to = "feminicidios")  %>% 
              mutate(mes= case_when(mes == "enero" ~ "01",
                     mes == "febrero" ~ "02",
                     mes == "marzo" ~ "03",
                     mes == "abril" ~ "04",
                     mes == "mayo" ~ "05",
                     mes == "junio" ~ "06",
                     mes == "julio" ~ "07",
                     mes == "agosto" ~ "08",
                     mes == "septiembre" ~ "09",
                     mes == "octubre" ~ "10",
                     mes == "noviembre" ~ "11",
                     mes == "diciembre" ~ "12")) %>% group_by(ano,mes) %>% summarise(feminicidios=sum(feminicidios)) %>% 
  mutate(fecha=as.Date(paste0(ano,"-",mes,"-01"))) %>% filter(fecha<"2022-04-01")

# Feminicidios de acuerdo a las estadísticas nacionales -
feminicidios_cdmx_nac %>% group_by(ano) %>% summarise(feminicidios=sum(feminicidios))

feminicidios_por_entidad <- feminicidios_nacional %>% group_by(ano,entidad)%>% select(ano,entidad,feminicidios)%>%
  summarise(tot=sum(feminicidios))

feminicidios_por_entidad %>% filter(ano<=2021,entidad %in% c("México","Ciudad de México","Puebla","Jalisco","Veracruz de Ignacio de la Llave",
                                                             "Michoacán de Ocampo","Nuevo León")) %>% 
  ggplot(aes(ano,tot,group=entidad,color=entidad))+
  geom_line(size=1)+
  labs(title="Número de Carpetas por Feminicidio en México",subtitle = "Entre 2015 y 2022",caption="Fuente: SESNP (Mayo 2022)",
       color="Estado",y="Carpetas")+
  geom_label(aes(label=tot))

############## Revisar número de feminicidios por estado
edo <- "Michoacán de Ocampo"
estado <- incidencia_actual %>% filter(tipo_de_delito=="Feminicidio", entidad==edo) 
estado <- estado %>% group_by(ano) %>% summarise(`01`=sum(enero),
                                                 `02`=sum(febrero),
                                                 `03`=sum(marzo),
                                                 `04`=sum(abril),
                                                 `05`=sum(mayo),
                                                 `06`=sum(junio),
                                                 `07`=sum(julio),
                                                 `08`=sum(agosto),
                                                 `09`=sum(septiembre),
                                                 `10`=sum(octubre),
                                                 `11`=sum(noviembre),
                                                 `12`=sum(diciembre))
estado <- as_tibble(cbind(nms = names(estado), t(estado))) #keep tibble as tibble

colnames(estado) <- as.character(estado[1,]) #make names first row names
estado <- estado[-1,] #delete first row
colnames(estado)[1] <- "mes" # Keep moth

estado <- estado %>% pivot_longer(!mes,names_to = "ano",values_to = "feminicidios") %>% arrange(ano) %>% 
  mutate(fecha=as.Date(paste0(ano,"-",mes,"-01"))) %>% mutate(feminicidios=as.numeric(feminicidios)) 

estado %>% group_by(ano) %>% summarise(feminicidios=sum(feminicidios,na.rm = T))

## Agregamos otras entidades ----

# filtramos la incidencia nacional, agrupamos por año y entidad y sumamos los meses en formato mes
feminicidios_nacional<- incidencia_actual %>% filter(tipo_de_delito=="Feminicidio")%>% group_by(ano,entidad) %>% 
  summarise(enero=sum(enero),
            febrero=sum(febrero),
            marzo=sum(marzo),
            abril=sum(abril),
            mayo=sum(mayo),
            junio=sum(junio),
            julio=sum(julio),
            agosto=sum(agosto),
            septiembre=sum(septiembre),
            octubre=sum(octubre),
            noviembre=sum(noviembre),
            diciembre=sum(diciembre))
  

feminicidios_nacional <- pivot_longer(feminicidios_nacional,c(enero,febrero,marzo,
                        abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),
                        names_to = "mes",values_to = "feminicidios")  %>% 
  mutate(mes= case_when(mes == "enero" ~ "01",
                        mes == "febrero" ~ "02",
                        mes == "marzo" ~ "03",
                        mes == "abril" ~ "04",
                        mes == "mayo" ~ "05",
                        mes == "junio" ~ "06",
                        mes == "julio" ~ "07",
                        mes == "agosto" ~ "08",
                        mes == "septiembre" ~ "09",
                        mes == "octubre" ~ "10",
                        mes == "noviembre" ~ "11",
                        mes == "diciembre" ~ "12")) %>% 
  mutate(fecha=as.Date(paste0(ano,"-",mes,"-01"))) %>% filter(fecha<"2022-04-01")


## Base CDMX FGJ ----

feminicidios_fgj <- fgj_cdmx[grep("FEMINICIDIO",fgj_cdmx$delito),] # hay tres, con violencia, equiparado, 

## Tabla
table(feminicidios_fgj$delito,feminicidios_fgj$ao_hechos)

por_alcaldia <- fgj_cdmx[grep("FEMINICIDIO",fgj_cdmx$delito),]


alcaldias <- c("ALVARO OBREGON","AZCAPOTZALCO","BENITO JUAREZ","COYOACAN","CUAJIMALPA DE MORELOS","CUAUHTEMOC","GUSTAVO A MADERO",
               "IZTACALCO","IZTAPALAPA","LA MAGDALENA CONTRERAS","MIGUEL HIDALGO","MILPA ALTA","TLAHUAC","TLALPAN",
               "VENUSTIANO CARRANZA","XOCHIMILCO")
por_alcaldia <- por_alcaldia[-grep("TENTATIVA",por_alcaldia$delito),] 

por_alcaldia<- por_alcaldia %>% filter(alcaldia_hechos %in% alcaldias)

por_alcaldia %>% count(delito)
# Carpetas por alcaldía
ggplot(por_alcaldia,aes(fct_infreq(alcaldia_hechos),fill=alcaldia_hechos))+
  geom_histogram(stat="count")+
  labs(fill="Alcaldía",x="",y="Carpetas",title="Número de Carpetas Abiertas por Feminicidio por Alcaldía",
       subtitle = "Por la FGJ desde 2016")+
  coord_flip()



por_alcaldia %>% count(ao_inicio) %>% mutate(cambio= (n*100/47)-100) %>% ggplot(aes(ao_inicio,cambio))+
  geom_line()

## MES 2021 ----

# Agregamos población para pesar entidad

mes_2021 <- mes_2021 %>% mutate(cve_entidad=case_when(entidad_federativa == "Aguascalientes" ~ 1,
                                    entidad_federativa == "Baja California" ~ 2,
                                    entidad_federativa == "Baja California Sur" ~ 3,
                                    entidad_federativa == "Campeche" ~ 4,
                                    entidad_federativa == "Chiapas" ~ 7,
                                    entidad_federativa == "Chihuahua" ~ 8,
                                    entidad_federativa == "Ciudad de México" ~ 9,
                                    entidad_federativa == "Coahuila" ~ 5,
                                    entidad_federativa == "Colima" ~ 6,
                                    entidad_federativa == "Durango" ~ 10,
                                    entidad_federativa == "Estado de México" ~ 15,
                                    entidad_federativa == "Guanajuato" ~ 11,
                                    entidad_federativa == "Guerrero"~ 12,
                                    entidad_federativa == "Hidalgo" ~ 13,
                                    entidad_federativa == "Jalisco" ~ 14,
                                    entidad_federativa == "Michoacán" ~ 16,
                                    entidad_federativa == "Morelos" ~ 17,
                                    entidad_federativa == "Nayarit" ~ 18,
                                    entidad_federativa == "Nuevo León"~ 19,
                                    entidad_federativa == "Oaxaca"~ 20,
                                    entidad_federativa == "Puebla"~ 21,
                                    entidad_federativa == "Querétaro" ~ 22,
                                    entidad_federativa == "Quintana Roo"~ 23,
                                    entidad_federativa == "San Luis Potosí" ~ 24,
                                    entidad_federativa == "Sinaloa" ~ 25,
                                    entidad_federativa == "Sonora"~ 26,
                                    entidad_federativa == "Tabasco" ~ 27,
                                    entidad_federativa == "Tamaulipas"~ 28,
                                    entidad_federativa == "Tlaxcala"~ 29,
                                    entidad_federativa == "Veracruz"~ 30,
                                    entidad_federativa == "Yucatán" ~ 31,
                                    entidad_federativa == "Zacatecas" ~ 31
                                    )) %>% left_join(poblacion_mex,by="cve_entidad")

# Primera etapa, Denuncias por delita -
mes_2021 %>% filter(entidad_federativa %in% c("Nuevo León","Morelos","Oaxaca","Guerrero","Jalisco","Ciudad de México")) %>%  
  pivot_longer(c(denuncias,querellas_u_otros_requisitos),names_to = "inicio_penal",values_to = "num_del") %>% 
  select(ano,entidad_federativa,inicio_penal,num_del,total) %>%
  mutate(num_del_mil = num_del/(total/1000))%>% 
  ggplot(aes(ano,num_del_mil,group=inicio_penal,fill=inicio_penal))+
  geom_col(position = "stack")+
  geom_label(aes(label=round(num_del_mil,2)), vjust = 0)+
  labs(x="",y="Porcentaje",titley="Número de Investigaciones Iniciadas por cada mil habitantes",
       subtitle="Por tipo de Inicio: Denuncia y Querella",
       fill="Inicio de Investrigación")+
  facet_wrap(~entidad_federativa)


## Denuncias por cada milenuncia
mes_2021 %>% filter(entidad_federativa %in% c("Nuevo León","Morelos","Oaxaca","Guerrero","Jalisco","Ciudad de México")) %>%  
  pivot_longer(c(denuncias,querellas_u_otros_requisitos),names_to = "inicio_penal",values_to = "num_del") %>% 
  select(ano,entidad_federativa,inicio_penal,num_del,total) %>%
  mutate(num_del_mil = num_del/(total/1000))%>% 
  ggplot(aes(ano,num_del_mil,group=inicio_penal,fill=inicio_penal))+
  geom_col(position = "stack")+
  geom_label(aes(label=round(num_del_mil,2)), vjust = 0)+
  labs(x="",y="Porcentaje",titley="Número de Investigaciones Iniciadas por cada mil habitantes",
       subtitle="Por tipo de Inicio: Denuncia y Querella",
       fill="Inicio de Investrigación")+
  facet_wrap(~entidad_federativa)

## Denuncias fed
#arriba
mes_2021 %>% pivot_longer(c(denuncias,querellas_u_otros_requisitos),names_to = "inicio_penal",values_to = "num_del") %>% 
  select(ano,entidad_federativa,inicio_penal,num_del,total) %>% 
  filter(ano==2021) %>% summarise(sum(num_del))
# abajo
mes_2021 %>%pivot_longer(c(denuncias,querellas_u_otros_requisitos),names_to = "inicio_penal",values_to = "num_del") %>% 
  select(ano,entidad_federativa,inicio_penal,num_del,total) %>% filter(inicio_penal=="denuncias",
                                                                       ano==2021) %>% summarise(sum(total))
## Número de carpetas 

glimpse(mes_2021)

mes_2021 %>% select(ano,estado,cii_con_detenido_en_flagrancia,cii_sin_detenido,total) %>% 
  filter(estado %in% c("CIUDAD DE MEXICO","NUEVO LEON","CAMPECHE","YUCATAN","GUANAJUATO","JALISCO")) %>% 
  pivot_longer(c(cii_con_detenido_en_flagrancia,cii_sin_detenido),names_to = "cii",values_to = "carpetas") %>% 
  mutate(carpetas=carpetas/(total/1000))%>% 
  ggplot(aes(ano,carpetas,group=cii,fill=cii))+
  geom_col(position = "dodge")+
  facet_wrap(~estado)+
  labs(title="Número de carpetas abiertas por cada Mil Habitantes",subtitle = "Por Detenidos y No detenidos",
       y="Número de Carpetas",x="",fill="Detenido y No detenido")+
  geom_label(aes(label= round(carpetas,2)), position = position_dodge(width = .9))

## Carpetas con sentencia



mes_2021 %>% select(ano,estado,pcii_archivo_temporal,pcii_abstencion_de_investigar,pcii_no_ejercicio_accion_penal,
                    pcii_criterio_de_oportunidad,pcii_por_incompetencia,pcii_por_acumulacion,
                    pcii_por_sobreseimiento_ord_juez_control,pcii_por_otra_causa_de_extincion_penal,
                    pcii_por_otra_decision_terminacion_codigo_penal_estatal,cii_por_dqo_ano_vigente) %>% 
  pivot_longer(c(pcii_archivo_temporal,pcii_abstencion_de_investigar,pcii_no_ejercicio_accion_penal,
                 pcii_criterio_de_oportunidad,pcii_por_incompetencia,pcii_por_acumulacion,
                 pcii_por_sobreseimiento_ord_juez_control,pcii_por_otra_causa_de_extincion_penal,
                 pcii_por_otra_decision_terminacion_codigo_penal_estatal),names_to = "carpetas_no_resueltas",values_to = "no_carpetas") %>% 
  group_by(ano,estado,cii_por_dqo_ano_vigente) %>% summarise(no_carpetas=sum(no_carpetas)) %>% 
  mutate(porcentaje_noresueltas = round((no_carpetas/cii_por_dqo_ano_vigente)*100,2)) %>% 
  filter(estado %in% c("CIUDAD DE MEXICO","DURANGO","SONORA","NUEVO LEON","JALISCO","MEXICO"))%>% 
  ggplot(aes(factor(ano),porcentaje_noresueltas,group=estado,fill=factor(ano)))+
  geom_col()+
  facet_wrap(~estado)+
  labs(x="",y="Porcentaje",fill="Año",title="Porcentaje de Carpetas No Resueltas por Año",subtitle = "Por Estado y Timepo")+
  geom_label(aes(label=porcentaje_noresueltas))


## Carpetas 

mes_2021 %>% select(ano,estado,pcii_archivo_temporal,pcii_abstencion_de_investigar,pcii_no_ejercicio_accion_penal,
                    pcii_criterio_de_oportunidad,pcii_por_incompetencia,pcii_por_acumulacion,
                    pcii_por_sobreseimiento_ord_juez_control,pcii_por_otra_causa_de_extincion_penal,
                    pcii_por_otra_decision_terminacion_codigo_penal_estatal,total) %>% 
  pivot_longer(c(pcii_archivo_temporal,pcii_abstencion_de_investigar,pcii_no_ejercicio_accion_penal,
                 pcii_criterio_de_oportunidad,pcii_por_incompetencia,pcii_por_acumulacion,
                 pcii_por_sobreseimiento_ord_juez_control,pcii_por_otra_causa_de_extincion_penal,
                 pcii_por_otra_decision_terminacion_codigo_penal_estatal),names_to = "carpetas_no_resueltas",values_to = "no_carpetas") %>% 
  filter(estado %in% c("CIUDAD DE MEXICO","DURANGO","SONORA","NUEVO LEON","JALISCO","MEXICO"))%>%
  mutate(por_mil = no_carpetas/(total/1000))%>% 
  ggplot(aes(ano,por_mil,group=carpetas_no_resueltas,fill=carpetas_no_resueltas))+
  geom_col(position="stack")+
  facet_wrap(~estado)+
  labs(x="",y="Carpetas",title="Número de Carpetas No Concluidas por cada Mil Habitantes",
       subtitle = "Por Razón de NO ejercicio de la acción penal",fill="Razón")+
  geom_label(data=aes(x=ano,label=total,group=estado))

total_labs <- mes_2021 %>% select(ano,estado,pcii_archivo_temporal,pcii_abstencion_de_investigar,pcii_no_ejercicio_accion_penal,
                    pcii_criterio_de_oportunidad,pcii_por_incompetencia,pcii_por_acumulacion,
                    pcii_por_sobreseimiento_ord_juez_control,pcii_por_otra_causa_de_extincion_penal,
                    pcii_por_otra_decision_terminacion_codigo_penal_estatal,total) %>% 
  pivot_longer(c(pcii_archivo_temporal,pcii_abstencion_de_investigar,pcii_no_ejercicio_accion_penal,
                 pcii_criterio_de_oportunidad,pcii_por_incompetencia,pcii_por_acumulacion,
                 pcii_por_sobreseimiento_ord_juez_control,pcii_por_otra_causa_de_extincion_penal,
                 pcii_por_otra_decision_terminacion_codigo_penal_estatal),names_to = "carpetas_no_resueltas",values_to = "no_carpetas") %>% 
  filter(estado %in% c("CIUDAD DE MEXICO","DURANGO","SONORA","NUEVO LEON","JALISCO","MEXICO"))%>%
  mutate(por_mil = no_carpetas/(total/1000)) %>% group_by(ano,estado) %>% summarise(total=sum(por_mil))

## Llamadas al 911 ----

glimpse(llamadas_911)
## Muchas cosas que no se ven directo. 
as.data.frame(table(llamadas_911$incidente_c4)) %>% arrange(Freq)

## Atenciones de Abogadas de las Mujeres ----
glimpse(abogadas_mujeres)
table(abogadas_mujeres$violencia_modalidad,abogadas_mujeres$vio_feminicida,abogadas_mujeres$riesgo_feminicida)
abogadas_mujeres %>% count(delito)

# Delitos
delitos_abogadas <- abogadas_mujeres %>%
  filter(delito=="DELITOS SEXUALES")



## Analisis llamadas LOCATEL -----

colnames(locatel)[3] <- "ano"

mujeres_locatel <- locatel %>% select(fecha_alta,ano,sexo,edad,ocupacion,escolaridad,municipio_usuaria,tematica_1,
                                      tematica_2,tematica_3) %>% filter()

locatel %>% count(tematica_1) %>% data.frame()

locatel %>% filter(tematica_1=="VIOLENCIA",tematica_2=="DE GÉNERO",ano>=2018) %>% count(tematica_3) %>% 
  ggplot(aes(reorder(tematica_3,n),n))+
  geom_col(fill="#691C32")+
  labs(title = "Número de llamadas A Locatel con Temática de Violencia de Género", x="",y="Llamadas")+
  geom_label(aes(label=comma(n)))

violencias_genero_locatel <- locatel %>% filter(tematica_1=="VIOLENCIA",tematica_2=="DE GÉNERO") %>% count(tematica_3,ano) %>% arrange(ano) 

# Robo de vehículos -----

robo_vehiculo <- fgj_cdmx[grep("ROBO DE VEHICULO",fgj_cdmx$delito),]

# Quitamos tentativo y motopatin
robo_vehiculo <- robo_vehiculo %>% filter(!delito %in% c("ROBO DE VEHICULO ELECTRICO MOTOPATIN","TENTATIVA DE ROBO DE VEHICULO"))


robo_vehiculo %>% filter(ao_hechos>2015) %>% group_by(ao_hechos,mes_hechos) %>% count() %>%
  mutate(mes = case_when(mes_hechos=="Enero"~1,
                         mes_hechos=="Febrero"~2,
                         mes_hechos=="Marzo"~3,
                         mes_hechos=="Abril"~4,
                         mes_hechos=="Mayo"~5,
                         mes_hechos=="Junio"~6,
                         mes_hechos=="Julio"~7,
                         mes_hechos=="Agosto"~8,
                         mes_hechos=="Septiembre"~9,
                         mes_hechos=="Octubre"~10,
                         mes_hechos=="Noviembre"~11,
                         mes_hechos=="Diciembre"~12)) %>% 
  mutate(fecha = as.Date(paste0(ao_hechos,"-",mes,"-01"))) %>% 
  ggplot(aes(fecha,n,color="blue"))+
  geom_line(size=1)+
  labs(x="Fecha",y="Número de Carpetas",title="Carpetas Inciadas en la CDMX por Robo de Vehículo",subtitle = "Entre Enero 2016 y Marzo 2022",
       caption = "Fuente: Datos de la FGJ")+
  geom_point(data = data.frame(fecha=c(as.Date("2018-10-01"),as.Date("2022-03-01")),robos=c(1320,490)),
             aes(fecha,robos,color="pink"))+
  geom_label(data = data.frame(fecha=c(as.Date("2018-10-01"),as.Date("2022-03-01")),robos=c(1320,490)),
             aes(fecha,robos,label=comma(robos)),vjust=-0.25)




robo_vehiculo %>% filter(ao_hechos>2015) %>% group_by(ao_hechos,mes_hechos) %>% count() %>%
  mutate(mes = case_when(mes_hechos=="Enero"~1,
                         mes_hechos=="Febrero"~2,
                         mes_hechos=="Marzo"~3,
                         mes_hechos=="Abril"~4,
                         mes_hechos=="Mayo"~5,
                         mes_hechos=="Junio"~6,
                         mes_hechos=="Julio"~7,
                         mes_hechos=="Agosto"~8,
                         mes_hechos=="Septiembre"~9,
                         mes_hechos=="Octubre"~10,
                         mes_hechos=="Noviembre"~11,
                         mes_hechos=="Diciembre"~12)) %>% 
  mutate(fecha = as.Date(paste0(ao_hechos,"-",mes,"-01"))) %>% arrange(desc(n))










## Final Plots -----

## CDMX feminicidios sobre el tiempo - SESNSP
ggplot(feminicidios_cdmx_nac,aes(fecha,feminicidios))+
  geom_line()+
  geom_smooth()+
  xlab("Meses")+
  ylab("Número de Feminicidios")+
  ggtitle("Número de Carpetas iniciadas entre 2015 - 03/2022 en la CDMX por Feminicidio")

## Número de Feminicidios por Estado - SESNSP
ggplot(feminicidios_nacional,aes(fecha,feminicidios))+
  geom_line()+
  geom_smooth()+
  facet_wrap(~entidad)+
  xlab("Meses")+
  ylab("Número de Feminicidios")+
  ggtitle("Número de Carpetas iniciadas entre 2015 - 03/2022 por Feminicidio")

## Líneas por Estado
estados <- c("Ciudad de México","Guanajuato","Nuevo León","Guerrero")
feminicidios_comparativa <- feminicidios_nacional %>% filter(entidad %in% estados) %>% group_by(ano,entidad) %>% 
  summarise(feminicidios=sum(feminicidios))

ggplot(feminicidios_comparativa,aes(ano,feminicidios,group=entidad,colour=entidad,shape=entidad))+
  geom_line()+
  geom_point()+
  labs(x="", y= "Número de Carpetas", title = "Número de Carpetas Iniciadas por Feminicidio desde 2015",
       subtitle = "Información del SESNAP: 2015-2022")+
  geom_text(aes(label = paste0("(", feminicidios, ")")), nudge_y = 3.5)

## Feminicidios Nacional - CDMX sobre el tiempo
ggplot(feminicidios_fgj,aes(ao_inicio,color=delito,fill=delito))+
  geom_histogram(stat = "count") + 
  xlab("Año de Inicio de la Carpeta") + 
  ylab("Número de Carpetas por Feminicidios")+
  ggtitle("Número de Carpetas de Investigación relacionadas al Feminicidio", subtitle = "Datos Abiertos de la FGJ")

## CDMX feminicidios sobre el tiempo - FGJ
ggplot(feminicidios_fgj,aes(ao_hechos,fill=delito))+
  geom_histogram(stat = "count")+ 
  xlab("Año de los Hecho") + 
  ylab("Número de Carpetas por Feminicidios")

# Número carpertas por feminicidio
ggplot(feminicidios_fgj,aes(ao_inicio,fill=delito))+
  geom_histogram(stat = "count")+
  labs(title="Número de Carpetas de Feminicidio por Año",x="",
       y="Número de Carpetas",fill="Delito de Carpeta",caption = "Fuente: FGJ CDMX - Marzo/2022")

## Locatel Llamadas por año 
ggplot(locatel,aes(EDAD,group=ANO)) +
  geom_histogram(fill="blue")+
  facet_wrap(~ANO)+
  labs(x="Edad",y="Número de Llamadas a Locatel")

## Edad Boxplot 

ggplot(violencia_mujeres_locatel, aes(EDAD,TEMATICA_3,color=TEMATICA_3))+
  geom_boxplot()+
  labs(y=" Tipo de Violencia de Género",x="Edad",title = "Número de Llamadas a Locatel desde 2015",
       subtitle = "Con Motivo de Violencia de Género por tipo de Violencia", color="Tipo de Violencia")+
  coord_flip()

## Atenciones desde 2015

ggplot(abogadas_mujeres,aes(x=anio,fill=anio))+
  geom_histogram(stat="count")+
  annotate()

## Delitos Abogadas de las mujeres

ggplot(abogadas_mujeres,aes(delito,fill=delito)) +
  geom_histogram(stat="count") +
  geom_text(aes(label = paste0("(", delito, ")")), nudge_y = 50)

## Abogadas Mujeres, alto riesgo feminicida y relación
abogadas_mujeres %>% filter(vio_feminicida=="Sí",alto_riesgo=="SÍ") %>%  count(relacion_con_agresora) %>% arrange(n) %>% 
  ggplot(aes(relacion_con_agresora,n,fill=relacion_con_agresora))+
  geom_col()+
  coord_flip()+
  labs(x="Relación con Agresor",y="Llamadas", subtitle="Relación con el Agresor de Violencia Feminicida",
       title = "Atenciones de Abogadas de las Mujeres para Violencia Feminicida de Alto Riesgo",fill="Relación")

## Locatel - Tematica


locatel %>% filter(ANO>2017,TEMATICA_1=="VIOLENCIA",TEMATICA_2=="DE GÉNERO") %>% count(TEMATICA_3) %>% arrange(n) %>% 
  ggplot(aes(reorder(TEMATICA_3, -n),n,))+
  geom_col(fill="#BC955C")+
  labs(x="","Número de Llamadas",title="Número de llamdas a LOCATEL Clasificadas como Violencia de Género",
       subtitle = "Entre 2018 - Marzo 2022, por Temática")+
  scale_y_log10()

## Abogadas de las mujeres, relación con agresor:

abogadas_mujeres %>% filter(vio_feminicida=="Sí") %>% count(relacion_con_agresora) %>% mutate(porcentaje=n/sum(n)*100) %>% 
  ggplot(aes(reorder(relacion_con_agresora, -n),n))+
  geom_col(fill="#691C32")+
  coord_flip()+
  labs(title = "Atenciones Brindadas por Relación al Agresor por Violencia Feminicida",
       x="Número de Atenciónes Brindadas", y="",subtitle = "Por Abogadas de las mujeres entre 03/2019 - 03/2022")

## Proporción de delinos no denunciados, cifra negra, por causas atribuíbles a la autoridad

imp_autoridad <- read_csv("input/seguridad/inegi_impunidad_autoridad.csv") %>% clean_names()

imp_autoridad %>% pivot_longer(c(x2014,x2015,x2016,x2017,x2018,x2019,x2020),names_to = "ano",values_to = "cn_aut") %>% 
  filter(estado %in% c("Jalisco","Nuevo León","Morelos","Oaxaca","Distrito Federal")) %>% 
  ggplot(aes(ano,cn_aut,group=estado,color=estado))+
  geom_line(size=1)+
  labs(x="",y="Porcentaje",title="Porcentaje de la Cifra Negra Atribuíble a la Autoridad",
       subtitle = "INEGI - ENVIPE 2021")+
  geom_label(aes(label=cn_aut))

# Número de llamdas por violencia de género
ggplot(violencias_genero_locatel,aes(ano,n))+
  geom_col(fill="#691C32")+
  labs(title = "Número de llamadas A Locatel con Temática de Violencia de Género",
       subtitle = "Clasificado con temática de Feminicidio", x="Año",y="Llamadas")+
  geom_label(data=data.frame(ano=c(2016,2017,2018,2019,2020,2021,2022),num=c(331,1999,2843,2941,9718,7612,456)),aes(ano,num,label=comma(num)))

# Promedio de Edad de Mujeres
locatel %>% filter(tematica_1=="VIOLENCIA",tematica_2=="DE GÉNERO",ano>2018)  %>% 
  ggplot(aes(edad,tematica_3,color=tematica_3))+
  geom_boxplot()+
  labs(y=" Tipo de Violencia de Género",x="Edad",title = "Edad Promedio de Mujeres que llaman a Locatel",
       subtitle = "Con Motivo de Violencia de Género por tipo de Violencia", color="Tipo de Violencia")+
  coord_flip()

# Número de llamadas con violencia de género con temática feminicida
locatel %>% filter(tematica_1=="VIOLENCIA",tematica_2=="DE GÉNERO",ano>=2018,tematica_3=="FEMINICIDIO") %>% count(tematica_4) %>% 
  ggplot(aes(reorder(tematica_4,n),n))+
  geom_col(fill="#691C32")+
  labs(title = "Número de llamadas A Locatel con Temática de Feminicidio", x="",y="Llamadas",caption="Fuente: LOCATEL")+
  geom_label(aes(label=n))

## Homicidios Dolosos ----


fgj_cdmx <- fgj_cdmx %>% mutate(fecha=as.Date(fecha_hecho,tryFormats="%d/%m/%Y")) 
# mensual


fgj_cdmx %>% filter(categoria=="HOMICIDIO DOLOSO") %>% filter(fecha>"2019-01-01") %>% 
  mutate(mes=months(fecha)) %>% mutate(fec = paste0(ano_hecho,"-",mes)) %>% 
  group_by(fec) %>% count() %>% 
  ggplot(aes(fec,n))+
  geom_col(fill="#9F2241")+
  geom_label(aes(label=comma(n)))+
  labs(x="",y="Carpetas de Investigación",title = "Carpetas de Investigación Iniciadas por Homicidios Dolosos",
       subtitle = "En la CDMX entre Enero 2019 y Abril 2022",caption = "Fuente: FGJ CDMX")+
  theme(plot.title = element_text(size=20,face="bold",color="#9F2241"),
        plot.subtitle = element_text(size=16),axis.text.x = element_text(face="bold", color="#993333", angle=90))


# Anual
fgj_cdmx %>% filter(categoria=="HOMICIDIO DOLOSO") %>% filter(fecha>"2019-01-01") %>% 
  group_by(ano_hecho) %>% count() %>% 
  ggplot(aes(ano_hecho,n))+
  geom_col(fill="#9F2241")+
  geom_line()+
  geom_label(aes(label=comma(n)))+
  labs(x="",y="Carpetas de Investigación",title = "Carpetas de Investigación Iniciadas por Homicidios Dolosos",
       subtitle = "En la CDMX entre Enero 2019 y Abril 2022",caption = "Fuente: FGJ CDMX")+
  theme(plot.title = element_text(size=20,face="bold",color="#9F2241"),
        plot.subtitle = element_text(size=16),axis.text.x = element_text(face="bold", color="#993333", angle=90))

# Promedio

fgj_cdmx %>% filter(categoria=="HOMICIDIO DOLOSO") %>% filter(fecha>"2019-01-01") %>% 
  group_by(ano_hecho) %>% count() %>% mutate(n=ifelse(ano_hecho!=2022,n/365,n/120)) %>% 
  ggplot(aes(ano_hecho,n))+
  geom_col(fill="#9F2241")+
  geom_line()+
  geom_label(aes(label=comma(n)))+
  labs(x="",y="Carpetas de Investigación",title = "Promedio de Carpetas  de Investigación Iniciadas por Homicidios Dolosos diariasmente",
       subtitle = "En la CDMX entre Enero 2019 y Abril 2022",caption = "Fuente: FGJ CDMX")+
  theme(plot.title = element_text(size=20,face="bold",color="#9F2241"),
        plot.subtitle = element_text(size=16),axis.text.x = element_text(face="bold", color="#993333", angle=90))

# Promedio mensual

fgj_cdmx %>% filter(categoria=="HOMICIDIO DOLOSO") %>% filter(fecha>"2019-01-01") %>% 
  mutate(mes=months(fecha)) %>% mutate(fec = paste0(ano_hecho,"-",mes)) %>% 
  group_by(fec) %>% count() %>% mutate(n=round(n/31,2)) %>% 
  ggplot(aes(fec,n))+
  geom_col(fill="#BC955C")+
  geom_label(aes(label=comma(n)))+
  labs(x="",y="Carpetas de Investigación",title = "Promedio de Carpetas de Investigación Iniciadas Diariamente por Homicidios Dolosos",
       subtitle = "En la CDMX entre Enero 2019 y Abril 2022",caption = "Fuente: FGJ CDMX")+
  theme(plot.title = element_text(size=20,face="bold",color="#9F2241"),
        plot.subtitle = element_text(size=16),axis.text.x = element_text(face="bold", color="#993333", angle=90))

## SESNSP Homicidios Dolosos ----
homicidios_nacional<- incidencia_actual %>% filter(tipo_de_delito=="Homicidio")%>% group_by(ano,entidad) %>% 
  summarise(enero=sum(enero),
            febrero=sum(febrero),
            marzo=sum(marzo),
            abril=sum(abril),
            mayo=sum(mayo),
            junio=sum(junio),
            julio=sum(julio),
            agosto=sum(agosto),
            septiembre=sum(septiembre),
            octubre=sum(octubre),
            noviembre=sum(noviembre),
            diciembre=sum(diciembre))

homicidios_nacional <-pivot_longer(homicidios_nacional,c(enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),
                                      names_to = "mes",values_to = "homicidios")  %>% 
  mutate(mes= case_when(mes == "enero" ~ "01",
                        mes == "febrero" ~ "02",
                        mes == "marzo" ~ "03",
                        mes == "abril" ~ "04",
                        mes == "mayo" ~ "05",
                        mes == "junio" ~ "06",
                        mes == "julio" ~ "07",
                        mes == "agosto" ~ "08",
                        mes == "septiembre" ~ "09",
                        mes == "octubre" ~ "10",
                        mes == "noviembre" ~ "11",
                        mes == "diciembre" ~ "12")) %>% 
  mutate(fecha=as.Date(paste0(ano,"-",mes,"-01"))) 


homicidios_nacional %>% filter(entidad=="Ciudad de México") %>% mutate(promedio=homicidios/31) %>% 
  ggplot(aes(fecha,promedio))+
  geom_line(size=1,color="#10312B")+
  geom_smooth()+
  labs(x="",y="Carpetas de Investigación",title = "Promedio de Carpetas de Investigación Iniciadas por Homicidios Dolosos Diariamente",
       subtitle = "En la CDMX entre Enero 2015 y Abril 2022",caption = "Fuente: SESNEP")+
  theme(plot.title = element_text(size=20,face="bold",color="#9F2241"),
        plot.subtitle = element_text(size=16),axis.text.x = element_text(face="bold", color="#993333", angle=90))+
  geom_label(data=labs,aes(label=paste0(format(fecha,format="%Y")," - ",months(fecha),": ",round(promedio,2))))
  
labs <- homicidios_nacional %>% filter(entidad=="Ciudad de México") %>% mutate(promedio=homicidios/31) %>% filter(fecha>"2022-03-01")
labs_2 <- homicidios_nacional %>% filter(entidad=="Ciudad de México") %>% mutate(promedio=homicidios/31) %>% filter(fecha=="2019-03-01")
labs <- rbind(labs,labs_2)



