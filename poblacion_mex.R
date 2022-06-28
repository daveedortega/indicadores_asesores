## Población de los Estados de la República
# Código para estandarizar y poder hacer Left-Join a bases de Datos diversas con nombres o clave del inegi 
# Y pesar por población
pacman::p_load(tidyverse)
# Descargamos la Base del Censo del INEGI 2020 
poblacion_mex <- read_csv("input/poblacion/inegi_2020.csv") %>% clean_names()

# Está dividida por Grupos de Edad, Cambiamos a que sea sólamente por entidad
poblacion_mex <- poblacion_mex %>% group_by(entidad_federativa) %>% filter(grupo_quinquenal_de_edad=="Total")%>% 
  summarise(hombres=sum(hombres_2020),mujeres=sum(mujeres_2020),total=sum(total_2020))


# Limpiamos "temas" de la base, acentos y complementos a los nombres de los estados
# Mantenemos de Origen pero le llamamos entidad sin entidad federativa
poblacion_mex <- poblacion_mex %>% mutate(entidad = toupper(entidad_federativa))
poblacion_mex$entidad  <- gsub("É","E",poblacion_mex$entidad )
poblacion_mex$entidad  <- gsub("Ó","O",poblacion_mex$entidad )
poblacion_mex$entidad  <- gsub("Í","I",poblacion_mex$entidad )
poblacion_mex$entidad  <- gsub("Á","A",poblacion_mex$entidad )
poblacion_mex$entidad  <- gsub("Ú","U",poblacion_mex$entidad )
poblacion_mex$entidad  <- gsub("VERACRUZ.*","VERACRUZ",poblacion_mex$entidad )
poblacion_mex$entidad  <- gsub("^MEXICO$","ESTADO DE MEXICO",poblacion_mex$entidad )
poblacion_mex$entidad  <- gsub("MICHO.*","MICHOACAN",poblacion_mex$entidad )
poblacion_mex$entidad  <- gsub("COAH.*","COAHUILA",poblacion_mex$entidad )
# Todo en Mayúscula
poblacion_mex <- poblacion_mex %>% mutate(entidad_federativa = toupper(entidad_federativa))
# Agregamos clave  del INEGI
poblacion_mex <- cbind(poblacion_mex,cve_entidad= c(001,002,003,004,007,008,009,005,006,010,033,11:32))
poblacion_mex <- poblacion_mex %>% arrange(cve_entidad)

## CDMX por alcaldía INEGI 2020
poblacion <- c(759137,434153,614447, 545884,217686,1173351,
               1835486,414470,699928,443704,442178,152685,
               392313  ,247622,432205,404695)
alcaldias <- c("ALVARO OBREGON","BENITO JUAREZ","COYOACAN","CUAUHTEMOC","CUAJIMALPA DE MORELOS","GUSTAVO A. MADERO",
               "IZTAPALAPA","MIGUEL HIDALGO","TLALPAN","VENUSTIANO CARRANZA","XOCHIMILCO","MILPA ALTA","TLAHUAC",
               "LA MAGDALENA CONTRERAS","AZCAPOTZALCO","IZTACALCO")
poblacion_cdmx <- data.frame(nombre_municipio=alcaldias,poblacion)


## Regiones de Interés----

poblacion_mex <- poblacion_mex %>% mutate(region=case_when(
  entidad %in% c("JALISCO","BAJA CALIFORNIA","BAJA CALIFORNIA SUR","NAYARIT","SINALOA","SONORA","CHIHUAHUA","DURANGO") ~ "R-1",
  entidad %in% c("AGUASCALIENTES","GUANAJUATO","COAHUILA","NUEVO LEON","QUERETARO","TAMAULIPAS","ZACATECAS","SAN LUIS POTOSI") ~ "R-2",
  entidad %in% c("CAMPECHE","CHIAPAS","OAXACA","QUINTANA ROO","TABASCO","VERACRUZ","YUCATAN") ~ "R-3",
  entidad %in% c("CIUDAD DE MEXICO","PUEBLA","TLAXCALA","MORELOS","GUERRERO") ~ "R-4",
  entidad %in% c("COLIMA","ESTADO DE MEXICO","MICHOACAN","HIDALGO") ~ "R-5"))





















