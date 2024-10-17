#Exploración de base de Datos para análisis de sobrevivencia

library(data.table)
library(dplyr)
library(haven)
library(ggplot2)
library(survival)
library(survminer)
library(tidyverse)
library(gtsummary)

base <- fread("./base_claudia.csv")

#Cambiar nombres de columnas del ID
setnames(base,
         old = "V1",
         new = "ID")

#Nos quedamos con la variables que nos interesan
base <- base[, c("ID", #ID
                 "WM7", #Resultado entrevista
                 "WB1Y", #Año del nacimiento de la mujer
                 "WB2", #Edad de la mujer
                 "CM1", #¿Ha tenido algún parto con un hijo nacido vivo=
                 "CM2Y", #Año del primer nacimiento
                 "SB1", #¿Qué edad tenía cuando mantuvo relaciones sexuales?
                 "SB2", #La primera vez que tuvo relaciones, usó condón?
                 "welevel", #Educación
                 "ethnicity", #Etnia jefe del hogar
                 "windex5", #Quintiles de riqueza
                 "HH6", #Urbano/Rural
                 "WM6Y")] #Año de la entrevista

#CONTADOR
#dt_n <- base[ethnicity ==5, .(n = .N), by = HH6]

#Saber cuántos NA hay en cada variable (primera visualización)
#Porcentaje
dt_n <- as.data.frame(apply(is.na(base), 2, mean)*100)
#Absoluto
dt_n <- as.data.frame(apply(is.na(base), 2, sum))

#Eliminar NA de las principales variables de interés
#AÑO NACIMIENTO DE LA MUJER
base <- base[!is.na(base$WB1Y),] #542 NAs
base <- base[WB1Y < 9000, ] #1obs no sabe

#MUJERES CON UN NACIDO VIVO
base <- base[CM1 < 8, ] #1obs con clave 9

#AÑO DEL PRIMER NACIMIENTO
base$CM2Y[is.na(base$CM2Y)] <- 0 #Convertir NAs en 0
base <- base[CM2Y != 9999, ] #4obs con clave 9

#AÑO DE LA PRIMERA RELACIÓN SEXUAL
base <- base[SB1 < 90 & SB1 > 0, ] #55obs con clave 9

#USO DE CONDON EN LA PRIMERA RELACIÓN SEXUAL
base$SB2[is.na(base$SB2)] <- 0 #Convertir NAs en 0 (683)
base <- base[SB2 < 8, ] #38obs no sabe

#NIVEL EDUCATIVO
base <- base[welevel < 8, ] #1obs con clave 9

#MUJERES INDÍGENAS
base <- base[ethnicity %in% c(2,3,4,5) ] #1obs con clave 9

#Sumar observaciones welevel (categorías 1 y 2)
base$welevel[base$welevel == 2] <- 1

# Construcción de variables de interés ------------------------------------
#Madre
setnames(base,
         old = "CM1",
         new = "madre")

base$madre[base$madre == 2] <- 0

#Año de la madre cuanto tuvo su primera relación sexual
base[, year_first_time := WB1Y + SB1 -1]

#Edad de la madre del primer hijo
base[, edad_primer_hijo := ifelse(madre == 1,
                             yes = CM2Y - WB1Y,
                             no = 0)]

#Tiempo entre primera relación y primer hijo
base[, TIEMPO := ifelse(madre == 1,
                        yes = edad_primer_hijo - SB1,
                        no = 0)]

base[, .(ID, edad_primer_hijo, SB1, TIEMPO)] #Aquí podemos ver que hay mala declaración de información
base <- base[TIEMPO >= 0, ] #144obs con información mal declarada

#Mujer que tuvo un hijo y la relación en el mismo año
base[, PRIMER_INTENTO := ifelse(madre == 1 & TIEMPO == 0,
                                yes = 1,
                                no = 0)]

#Sumar observaciones de 0 a 1 del tiempo
base$TIEMPO[base$PRIMER_INTENTO == 1] <- 1

#Tiempo entre primera relación y los que no han tenido hijxs
base[, TIEMPO := ifelse(MADRE == 0 & INTERCOURSE == 1,
                        yes = WB2 - SB1,
                        no = TIEMPO)]

#Mujer que tuvo su primera relación y fue entrevistada el mismo año
base[, SAME_YEAR := ifelse(WB2 == SB1,
                           yes = 1,
                           no = 0)]

#Sumar observaciones de 0 a 1 del tiempo
base$TIEMPO[base$SAME_YEAR == 1] <- 1

# #Agrupación de edad de primera relación sexual
# base[, EDAD_INTERCOURSE := ifelse(SB1 %in% c(15:19), #Entre 15 y 19 (73%)
#                                             yes = 2,
#                                             no = ifelse(SB1 %in% c(20:46), #Mayores de 20 (17%)
#                                                         yes = 3,
#                                                         no = 1))] #Menores de 15 años (10%)


#Tiempo de la censura
base[, exit := ifelse(madre == 1,
                           yes = CM2Y,
                           no = WM6Y)]

#Pasar la base a long
base.2 <- survSplit(Surv(year_first_time, exit, madre) ~., base,
                    cut=c(seq(1977,2016,1)), event = "hijo")


#Survival plot
#Sin variables
viz <- survfit(Surv(year_first_time, exit, hijo) 
               ~1, data = base.2)

class(viz)
ggsurvplot(viz, data = base.2,
           conf.int = T,
           size = 1,
           linetype = c(1),
           palette = c("#FF4040"),
           legend.title = "Surv Function",
           xlab = "Time",
           ylab = "Survival Probability",
           xlim = c(1977,2016),
           legend = "bottom",
           panel.background = element_rect(fill = "gray"))


#Modelo de riesgos proporcionales de Cox
coxph(Surv(year_first_time, exit, hijo)  ~ HH6 + ethnicity + windex5 + welevel + SB2 + SB1,
      data = base.2) %>% 
  tbl_regression(exp = TRUE)


#Diagnóstico del modelo
fit <- coxph(Surv(year_first_time, exit, hijo) ~ HH6 + ethnicity + windex5 + welevel + SB2 + SB1, 
             data = base.2)
cox.zph(fit)


#Interactuar con variables
coxph(Surv(year_first_time, exit, hijo) ~ HH6 + ethnicity + windex5 + welevel + SB2 + SB1 + SB2:year_first_time,
           data = base.2) %>%
        tbl_regression(exp = TRUE) 

