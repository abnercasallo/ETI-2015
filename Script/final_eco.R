library(tidyverse)
library(haven)

#1.Seteamos y cargamos los datos.
setwd("C:/Users/ABNER/Desktop/Cursos temas acadé ect/Artículos/ETI 2015/Data")
datos<- as.data.frame(read_dta("eti_2015_hogar_vivienda.dta"))
datos2<-datos


#2. Escogemos los datos de interés y omitimos los valores na con na.omit ya que están bastante dispersos.

datos <-  datos %>% select(qhdepartamento,qh_10a17,qh502a,qh502b_1,qh502b_2,qh502b_3,qh502b_4,qh502b_5,qh502b_6,qh502b_7,qh502b_8,qh502b_9,qh616_5,qh617_1,qh617_3,qh617_4,qh617_5)
datos<-na.omit(datos)  #Si usas antes la data quedaría vacía.
names(datos)

datos2 <-  datos2 %>% select(qhdepartamento,qh_10a17,qh502a,qh502b_1,qh502b_2,qh502b_3,qh502b_4,qh502b_5,qh502b_6,qh502b_7,qh502b_8,qh502b_9,qh616_5,qh617_1,qh617_3,qh617_4,qh617_5)
datos<-na.omit(datos)
names(datos2)
#Hallamos una suma total de gasto a partir de gastos parciales de la data sumando por filas y seleccionamos.
datos$Gasto_Total=rowSums(datos[3:12], na.rm = TRUE)
datos <-  datos %>% select(qhdepartamento,qh_10a17,qh616_5,qh617_1,qh617_3,qh617_4,Gasto_Total)
datos2 <-  datos2 %>% select(qhdepartamento,qh_10a17,qh502a,qh502b_2,qh502b_4,qh502b_5,qh617_1,qh617_3,qh617_4)
names(datos2)
#3. Nombramos
names(datos) <- c("departamento","mernoes_10_17_anios","abandono_jefe_hogar","recibio_ayuda_degob", "retiro_de_colegio", "incorporacion_del_menor_al_trabajo","Gasto_Total")
names(datos)
names(datos2) <- c("departamento","mernoes_10_17_anios","g_alimentos","g_educación","g_alquiler","g_salud","recibio_ayuda_degob","retiro_de_colegio","incorporacion_del_menor_al_trabajo")
names(datos2)
#Remplazamos: 2 es no, o sea 0; 1 es sí o sea 1, no lo cambiamos
datos$retiro_de_colegio<-replace(datos$retiro_de_colegio,datos$retiro_de_colegio==2, 0)
datos$retiro_de_colegio<-replace(datos$retiro_de_colegio,datos$retiro_de_colegio==9, NA)#Para eliminarlas pues no sirven
datos<-na.omit(datos)

datos2$retiro_de_colegio<-replace(datos2$retiro_de_colegio,datos2$retiro_de_colegio==2, 0)
datos2$retiro_de_colegio<-replace(datos2$retiro_de_colegio,datos2$retiro_de_colegio==9, NA)
datos2<-na.omit(datos2)
#datos<-replace(datos$retiro_de_colegio[1],1)

model1=glm(retiro_de_colegio~Gasto_Total+mernoes_10_17_anios,family =binomial(link="probit"), data=datos)
summary(model1)
model2=glm(retiro_de_colegio~g_educación+g_alquiler+g_alimentos+g_salud+mernoes_10_17_anios,family =binomial(link="probit"), data=datos2)
summary(model2)