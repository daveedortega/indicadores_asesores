## Estadísticos Salud
## David Ortega


## De IMSS:
## Puestos de trabajo registrados por los patrones en el Instituto Mexicano del Seguro Social y asegurados sin un empleo asociado:
## http://datos.imss.gob.mx/group/asegurados 

# Diccionario: http://datos.imss.gob.mx/dataset/catalogo-asegurados/resource/e3d6cbba-7305-423c-b2c5-7f8fa4178cd4 

## Población Derechoabiente Adscrita: 
## http://datos.imss.gob.mx/group/población-derechohabiente-adscrita-pda 

# teu - temporales urbano
# tec - temporales campo
# teu - temporales urbano

# Preparar Entorno ----
pacman::p_load(tidyverse,janitor,scales)
rm(list=ls())
dev.off()
## Cargas base de datos ----
## IMSS
pdt_imss <- read_delim("input/salud/asg-2022-04-30.csv",delim ="|",
                        locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

pda_imss <- read_delim("input/salud/pda-2022-03-31.csv",delim ="|",
                          locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

pdt_2015 <- read_delim("input/salud/asg-2015-12-31.csv",delim = "|",
                     locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

pdt_2010 <- read_delim("input/salud/asg-2010-12-31.csv",delim = "|",
                       locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

pdt_2005 <- read_delim("input/salud/asg-2005-12-31.csv",delim = "|",
                       locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

pdt_2000 <- read_delim("input/salud/asg-2000-12-31.csv",delim = "|",
                       locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

## SSA - Nacional
recursos_ssa <- read_csv("input/salud/recursos_ssa.csv",
                         locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

ssa_2020 <- read_csv("input/salud/analisis_sectorial_2020.csv",
                       locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

## IMSS plazas por especialidad 
# Nacional -
plazas_nacional <- read_csv("input/salud/plazas_especialidades_2405.csv")
# Estatales
plazas_estatal <- read_csv("input/salud/plazas_estatales_imss.csv") %>% clean_names()

## SICUENTAS
sicuentas <- read_csv("input/salud/gasto_salud_1990-2020.csv") %>% clean_names()

## Análisis SSA ----
# Limpiar Nombres

ssa_2020 %>% count(nombre_estado)
## Filtramos CDMX
ssa_2020 %>% filter(nombre_estado=="CIUDAD DE MEXICO") %>% count(nombre_municipio) 

ssa_2020 %>% filter(nombre_estado=="CIUDAD DE MEXICO") %>% View()

## Indicadores Salud
## Revisar número de consultorios por especialidad
ssa_2020 %>% filter(nombre_estado=="CIUDAD DE MEXICO") %>% 
  pivot_longer(c(consultorios_de_medicina_general,consultorios_de_medicina_familiar,consultorios_de_estomatologia,
                 consultorios_de_planificacion_familiar,consultorios_de_cirugia_maxilo_facial,consultorios_de_alergologia,
                 consultorios_de_angiologia,consultorios_de_cardiologia,consultorios_de_cirugia_general_y_cirugia,
                 consultorios_de_dermatologia,consultorios_de_endocrinologia,consultorios_de_epidemiologia,
                 consultorios_de_gastroenterologia,consultorios_de_geriatria,consultorios_de_gineco_obstetricia,
                 consultorios_de_hematologia,consultorios_de_infectologia,consultorios_de_medicina_de_rehabilitacion,
                 consultorios_de_medicina_del_dolor,consultorios_de_medicina_interna,consultorios_de_nefrologia,
                 consultorios_de_neumologia,consultorios_de_nutricion,consultorios_de_oncologia,consultorios_de_ortopedia,
                 consultorios_de_otorrinolaringologia,consultorios_de_pediatria,consultorios_de_proctologia,
                 consultorios_de_psicologia,consultorios_de_psiquiatria,consultorios_de_reumatologia,consultorios_de_urologia,
                 consultorios_de_traumatologia,consultorios_en_area_de_urgencias,consultorios_de_valoracion,
                 consultorios_de_otras_especialidades),names_to = "tipo_consultorios",values_to = "numero_consultorios") %>% 
  group_by(tipo_consultorios) %>% summarise(total_cdmx=sum(numero_consultorios,na.rm = T)) %>% 
  ggplot(aes(reorder(tipo_consultorios,total_cdmx),total_cdmx,fill=tipo_consultorios))+
  geom_col()+
  coord_flip()+
  geom_label(aes(label=total_cdmx))+
  labs(x="",y="Número de Consultorios",title = "Consultorios en CDMX a 2020 dentro del IMSS",
       subtitle = "Desglosado por Especialidad",fill="Tipo de Consultorio")



## Cuántos hay por institucion

ssa_2020 %>% filter(nombre_estado=="CIUDAD DE MEXICO") %>% group_by(institucion) %>% 
  summarise(total_de_consultorios = sum(total_de_consultorios)) %>% 
  ggplot(aes(reorder(institucion,total_de_consultorios),total_de_consultorios,fill=institucion))+
  geom_col()+
  geom_label(aes(label=total_de_consultorios))+
  labs(title="Total de Consultorios por Tipo de Institución en CDMX",y="Consultorios",x="")

## Total de Hospitales por institución

ssa_2020 %>% filter(nombre_estado=="CIUDAD DE MEXICO") %>% count(institucion) %>% 
  ggplot(aes(reorder(institucion,n),n,fill=institucion))+
  geom_col()+
  geom_label(aes(label=n))+
  labs(x="",y="Número de Hospitales",title="Número de Hospitales Públicos De Acuerdo a SSA en CDMX", 
       subtitle = "Por Institución en 2020")
  
## Total de camas por alcaldía pesado por número de habitantes

ssa_2020 %>% filter(nombre_estado=="CIUDAD DE MEXICO") %>% 
  group_by(nombre_municipio) %>% summarise(total_camas_h=sum(total_camas_area_hospitalizacion,na.rm=T),
                                           total_camas_nh = sum(total_camas_en_otras_areas_no_considera_hospitalizacion,na.rm=T)) %>% 
  pivot_longer(c(total_camas_h,total_camas_nh),names_to = "tipo_camas",values_to = "numero_de_camas") %>% 
  left_join(poblacion_cdmx,by="nombre_municipio") %>% 
  mutate(numero_de_camas = numero_de_camas/(poblacion/100000)) %>% 
  mutate(tipo_camas = ifelse(tipo_camas=="total_camas_h","Camas de Hospitalización","Camas que no Consideran Hospitalización"))%>% 
  ggplot(aes(reorder(nombre_municipio,numero_de_camas),numero_de_camas,fill=tipo_camas))+
  geom_bar(aes(fill = tipo_camas),stat = "identity",position = "dodge") +
  coord_flip()+
  geom_label(aes(label=round(numero_de_camas,2)),position = position_dodge(0.9))+
  labs(title="Número de Camas en Hospitales Públicos por cada 100,000 habitantes",x="",y="Camas",subtitle = "Camas de Hospitalización y Camas en Áreas que no consideran Hospitalización en la CDMX",fill="Tipo Camas")


## Comparación de Médicos por Entidad ---
ssa_2020 %>% count(nombre_estado)
ssa_2020 %>% filter(nombre_estado %in% c("CIUDAD DE MEXICO","JALISCO","NUEVO LEON",
                                         "VERACRUZ DE IGNACIO DE LA LLAVE","OAXACA")) %>% group_by(clave_estado) %>% 
  summarise(total_medicos=sum(total_medicos_generales_especialistas_y_odonologos),
            total_estudiantes=sum(total_personal_medico_en_formacion)) %>% 
  mutate(cve_entidad=as.numeric(clave_estado)) %>% left_join(poblacion_mex,by="cve_entidad") %>% 
  mutate(medicosx100=total_medicos/(total/1000)) %>% 
  ggplot(aes(reorder(estado,medicosx100),medicosx100))+
  geom_col(fill=c("#9F2241","#ff8200","#ff8200","#006747","#9F2241"))+
  labs(title="Médicos por cada 1,000 habitantes",subtitle="En 2020 de acuerdo a SSA",x="",y="Número de médicos")+
  geom_label(aes(label= round(medicosx100,2)))

## Análisis IMSS ----

# Trabajadores Asegurados
pdt_imss %>% summarise(sum(ta))

# No Trabajadores Asegurados
pdt_imss %>% summarise(sum(no_trabajadores))

# en CDMX

pdt_imss %>% filter(cve_entidad==9) %>% summarise(sum(ta),sum(no_trabajadores))

# Como proporción nacional

pdt_imss %>% group_by(cve_entidad) %>% summarise(asegurados=sum(asegurados),no_trabajadores=sum(no_trabajadores),
                                                 trabajadores=sum(ta),salario=sum(ta_sal,na.rm = T)) %>% 
  left_join(poblacion_mex,by="cve_entidad") %>% 
  select(estado,total,asegurados) %>% mutate(tasa_asegurados=asegurados/total*100) %>% 
  ggplot(aes(reorder(estado,tasa_asegurados),tasa_asegurados, fill=estado))+
  geom_col()+
  coord_flip()+
  labs(title="Porcentaje de Asegurados en el IMSS",subtitle="Por Estado durante 2020",
       x="",y="Porcentaje",fill="Estados")+
  geom_label(aes(label=round(tasa_asegurados,2)))

## Porcentaje de Segurados Nivel Nacional y local, comparativa 2000-2022 ----


colnames(pdt_imss)[colnames(pdt_imss) %in% colnames(pdt_2015)]


asegurados_2000 <- pdt_2000 %>% filter(cve_entidad==9) %>% summarise(sum(asegurados))

asegurados_2005 <- pdt_2005 %>% filter(cve_entidad==9) %>% summarise(sum(asegurados))

asegurados_2010 <- pdt_2010 %>% filter(cve_entidad==9) %>% summarise(sum(asegurados))

asegurados_2015 <- pdt_2015 %>% filter(cve_entidad==9) %>% summarise(sum(asegurados))

asegurados_2022 <- pdt_imss %>% filter(cve_entidad==9) %>% summarise(sum(asegurados))

pdt_2000 %>% filter(cve_entidad==9) %>% summarise(sum(no_trabajadores))
pdt_2005 %>% filter(cve_entidad==9) %>% summarise(sum(no_trabajadores))
pdt_2010 %>% filter(cve_entidad==9) %>% summarise(sum(no_trabajadores))
pdt_2015 %>% filter(cve_entidad==9) %>% summarise(sum(no_trabajadores))
pdt_imss %>% filter(cve_entidad==9) %>% summarise(sum(no_trabajadores))

seriedt_imss <- data.frame(ano=c(2000,2005,2010,2015,2022),asegurados=c(2211549,2307721,3752625, 4423121,5153584))


ggplot(seriedt_imss,aes(ano,asegurados))+
  geom_line(color="#9F2241")+
  labs(title = "Número de Afiliados IMSS CDMX",x="",y="Asegurados")+
  geom_label(aes(label=scales::comma(asegurados)))
  


## Plazas Nacionales ----

plazas_nacional %>% mutate(porcentaje = Plazas / sum(Plazas) *100) %>% mutate(esp=ifelse(porcentaje>=1,Especialidad,"Otros")) %>% 
  group_by(esp) %>% summarise(porcentaje=sum(porcentaje)) %>% 
  ggplot(aes(reorder(esp,porcentaje),porcentaje,fill=esp))+
  geom_col()+
  coord_flip()+
  geom_label(aes(label=round(porcentaje,2)))+
  labs(x="",y="Porcentaje",title="Porcentaje de Plazas de Especialidad en el IMSS",subtitle = "A 24/05/2022",
       fill="Especialidad")

## Plazas Estatales ----
plazas_estatal$x5=NULL
plazas_estatal %>% pivot_longer(c(medicina_interna,cirujia_general,psiquiatria),names_to = "especialidad",values_to = "plazas") %>% 
  filter(estados %in% c("Nuevo León","Durango","Aguascalientes","Oaxaca","Jalisco","Ciudad de México","Veracruz","Chiapas")) %>% 
  ggplot(aes(reorder(estados,plazas),plazas,group=especialidad,fill=especialidad))+
  geom_col(position = "stack")+
  labs(x="",y="Plazas",title="Número de Plazas en Concurso a Nivel Nacional del IMSS",
       subtitle="Para especialidades seleccionadas Al 24 de Mayo de 2022",fill="Especialidad")+
  geom_label(aes(label=plazas),position = "stack",vjust=0.5)+
  facet_wrap(~especialidad,scales="free")




plazas_estatal %>% pivot_longer(c(medicina_interna,cirujia_general,psiquiatria),names_to = "especialidad",values_to = "plazas") %>% 
  ggplot(aes(especialidad,plazas,group=estados,fill=estados))+
  geom_col(position = "stack")+
  facet_wrap(~especialidad,scales = "free_y")











## SICUENTAS ----
sicuentas %>% glimpse()
sicuentas %>% tail()


## Tendencia de Gasto en Salud
# Total Público linea de tiempo
sicuentas %>% filter(entidad_federativa=="Gasto Total") %>% 
  ggplot(aes(ano,x2_gasto_publico_en_salud))+
  geom_line(size=2,color="#691C32")+
  geom_label(x=2019,y=805259688,aes(label=paste0(2020,": $",comma(805259688))))+
  geom_label(x=2017,y=651520370,aes(label=paste0(2018,": $",comma(651520370))))+
  geom_label(x=1990,y=14951665,aes(label=paste0(1990,": $",comma(14951665))),nudge_x = -0.5)+
  labs(x="",y="Miles de Pesos",title="Gasto Público total en Salud entre 1990 y 2020",caption="Fuente: SSA - SICUENTAS")

# Estatal linea de tiempo
sicuentas %>% filter(!entidad_federativa %in% c("Gasto Total","IMSS-Prospera","No Distribuibles","No distribuibles","Extranjero","Unidades centrales")) %>% 
  ggplot(aes(ano,x2_gasto_publico_en_salud,group=entidad_federativa,color=entidad_federativa))+
  geom_line(size=1)+
  geom_label(data=data.frame(entidad_federativa=c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Chiapas","Chihuahua",
                                  "Colima","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","México","Michoacán","Morelos",
                                  "Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco",
                                  "Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas"),
                          x2_gasto_publico_en_salud=c(9200557,21247860,6847689,7101783,18735163,5631898,26184258,22828664,133073997,10196678,
                                                    33225051,18844056,16023172,47546481,90254675,22613737,12812042,7478463,32937409,
                                                    20150376,29514030,12190424,9713253,13941581,16380626,21736150,17265098,24661840,
                                                    7674655,44591204,16486962,9171763)),
             aes(x=2020,label=paste0(entidad_federativa, " - ", comma(x2_gasto_publico_en_salud))))+
  labs(title="Serie Histórica de Gasto Público en Salud por Entidad","Entre 1990 y 2020 en Miles de Pesos",
       caption = "Fuente: SSA - SICUENTAS",x="",y="Miles de Pesos")+
  scale_y_log10()

# Estatal 2020
sicuentas %>% filter(ano==2020,!entidad_federativa %in% c("Gasto Total","IMSS-Prospera","No Distribuibles","No distribuibles","Extranjero","Unidades centrales")) %>% 
  ggplot(aes(reorder(entidad_federativa,x2_gasto_publico_en_salud),x2_gasto_publico_en_salud,fill=entidad_federativa))+
  geom_col()+
  geom_label(aes(label=comma(x2_gasto_publico_en_salud)))+
  labs(title = " Gasto Público en Salud por Entidad en Miles de Pesos",caption = "Fuente: SSA - SICUENTAS",y="Miles de Pesos",x="")+
  coord_flip()+
  theme(legend.position = "none",plot.title = element_text(size=18,face = "bold"))



# Per Capita Linea de Tiempo Total

sicuentas %>% select(ano,entidad_federativa,x39_gasto_federal_per_capita) %>% filter(entidad_federativa=="Gasto Total",ano>1998) %>% 
  ggplot(aes(ano,x39_gasto_federal_per_capita,group=entidad_federativa,color=entidad_federativa))+
  geom_line(size=1)+
  geom_label(aes(label=comma(x39_gasto_federal_per_capita)))+
  labs(x="",y="Pesos",title="Gasto Federal Per Cápita anual en Salud",caption = "Fuente: SSA : SICUENTA",color="Entidad")

# Per Capita Linea de Tiempo Estatal

sicuentas %>% select(ano,entidad_federativa,x39_gasto_federal_per_capita) %>% 
  filter(!entidad_federativa %in% c("Gasto Total","IMSS-Prospera","No Distribuibles","No distribuibles","Extranjero","Unidades centrales"),
         ano>1998) %>% filter(entidad_federativa %in% c("Hidalgo","Nuevo León", "Campeche","Ciudad de México","México")) %>% 
  ggplot(aes(ano,x39_gasto_federal_per_capita,group=entidad_federativa,color=entidad_federativa))+
  geom_line(size=1)+
  geom_label(data=data.frame(entidad_federativa=c("Campeche","Ciudad de México","Hidalgo","México","Nuevo León"),
             gasto_per_capita_federal=c(5847.55,7626.23,3937.45,4026.09,3490.51)),
             aes(x=2020,y=gasto_per_capita_federal,label=comma(gasto_per_capita_federal)))+
  labs(x="",y="Pesos",title="Gasto Federal Per Cápita anual por Entidad",caption = "Fuente: SSA : SICUENTA",color="Entidad")

# Per Capita Estatal 2020

sicuentas %>% select(ano,entidad_federativa,x39_gasto_federal_per_capita) %>% 
  filter(!entidad_federativa %in% c("Gasto Total","IMSS-Prospera","No Distribuibles","No distribuibles","Extranjero","Unidades centrales"),
         ano==2020)%>% 
  ggplot(aes(reorder(entidad_federativa,x39_gasto_federal_per_capita),x39_gasto_federal_per_capita,fill=entidad_federativa))+
  geom_col()+
  labs(x="",y="Pesos",title="Gasto Federal Per Cápita anual por Entidad",caption = "Fuente: SSA : SICUENTA",fill="Entidad")+
  geom_label(aes(label=comma(x39_gasto_federal_per_capita)))+
  coord_flip()

## Cobertura

sicuentas %>% filter( !entidad_federativa %in% c("Gasto Total","IMSS-Prospera","No Distribuibles","No distribuibles","Extranjero","Unidades centrales")) %>% 
                       select(ano,entidad_federativa,x46_poblacion_total,x47_poblacion_con_seguridad_social,x48_poblacion_sin_seguridad_social,
                     x49_poblacion_imss,x50_poblacion_issste,x51_poblacion_pemex) %>% 
  group_by(ano) %>% summarise(poblacion_total=sum(x46_poblacion_total),
                                  `Con Servicio Social`=sum(x47_poblacion_con_seguridad_social),
                                  `Sin Servicio Social`=sum(x48_poblacion_sin_seguridad_social)) %>% 
  pivot_longer(c(poblacion_total,`Con Servicio Social`,`Sin Servicio Social`),names_to = "servicio",values_to = "pob") %>% filter(ano>2000) %>% filter(servicio!="poblacion_total") %>% 
  ggplot(aes(reorder(ano,pob),pob,group=servicio,color=servicio))+
  geom_line(size=1)+
  labs(x="",y="Número de Personas",title = "Personas con y sin seguridad Social",subtitle = "Entre 2000 y 2020",
       caption = "Fuente: SIAP",fill="")+
  geom_label(aes(label=comma(pob),position="stack"))+
  theme(plot.title = element_text(size=18,face = "bold"))

## Cobertura en porcentaje

sicuentas %>% filter( !entidad_federativa %in% c("Gasto Total","IMSS-Prospera","No Distribuibles","No distribuibles","Extranjero","Unidades centrales")) %>% 
  select(ano,entidad_federativa,x46_poblacion_total,x47_poblacion_con_seguridad_social,x48_poblacion_sin_seguridad_social,
         x49_poblacion_imss,x50_poblacion_issste,x51_poblacion_pemex) %>% 
  group_by(ano) %>% summarise(poblacion_total=sum(x46_poblacion_total),
                              `Con Servicio Social`=sum(x47_poblacion_con_seguridad_social),
                              `Sin Servicio Social`=sum(x48_poblacion_sin_seguridad_social)) %>% 
  pivot_longer(c(`Con Servicio Social`,`Sin Servicio Social`),names_to = "servicio",values_to = "pob") %>% filter(ano>2000) %>% 
  mutate(porcentaje=round(pob/poblacion_total*100,2)) %>% 
  ggplot(aes(ano,porcentaje,group=servicio,color=servicio))+
  geom_line(size=2)+
  labs(x="",y="Número de Personas",title = "Personas con y sin seguridad Social",subtitle = "Entre 2000 y 2020",
       caption = "Fuente: SIAP",fill="")+
  geom_label(aes(label=porcentaje))+
  theme(plot.title = element_text(size=18,face = "bold"))


## pib privado vs publico en salud
sicuentas %>% filter(entidad_federativa=="Gasto Total") %>% group_by(ano) %>%  summarise(
                                                                                         Privado=sum(x32_gasto_privado_en_salud_como_percent_del_pib),
                                                                                         Publico=sum(x31_gasto_publico_en_salud_como_percent_del_pib)) %>% 
  pivot_longer(c(Privado,Publico),names_to = "gasto",values_to = "porcentaje") %>% filter(ano>1995) %>% 
  ggplot(aes(ano,porcentaje,group=gasto,color=gasto))+
  geom_line()+
  geom_smooth(se=F)+
  labs(x="",y="Porcentaje",fill="Tipo de Gasto",title = "Porcentaje del PIB como gasto en Salud",subtitle = "Dividido en Gasto Público y Privado",
       color="Tipo de Gasto")+
  theme(plot.title = element_text(size=18,face = "bold"))+
  geom_label(x=2020,y=3.45,aes(label=paste0("%",3.45)))+
  geom_label(x=2020,y=2.93,group="Público",aes(label=paste0("%",2.93)))
  


## Gasto programable en salud

sicuentas %>% filter(entidad_federativa=="Gasto Total") %>% group_by(ano) %>%  summarise(
  programable=sum(x30_gasto_total_en_salud_como_percent_del_pib)) %>%
  filter(ano>1992) %>% 
  ggplot(aes(ano,programable))+
  geom_line(color="235B4E",size=1.5)+
  geom_smooth(se=F)+
  labs(x="",y="Porcentaje",fill="Tipo de Gasto",title = "Porcentaje del gasto programable Destinado al Rubro de Salud",subtitle = "Desde 1993 hasta 2020")+
  theme(plot.title = element_text(size=18,face = "bold"))+
  geom_label(x=2020,y=6.38,aes(label=paste0(ano,": ",programable,"%")))



sicuentas %>% filter(entidad_federativa=="Gasto Total") %>% group_by(ano) %>%  summarise(
  programable=sum(x30_gasto_total_en_salud_como_percent_del_pib)) %>%
  filter(ano>1992) %>% data.frame()








