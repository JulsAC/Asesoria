#Exploración de base de Datos para análisis de sobrevivencia

library(data.table)
library(dplyr)
library(haven)
library(ggplot2)
library(survival)
library(survminer)
library(tidyverse)
library(gtsummary)

base_etiquetas <- as.data.frame(read_sav("./wm.sav"))
#write.csv(base_etiquetas, "base_claudia.csv")
base <- fread("./base_claudia.csv")

#Cambiar nombres de columnas del ID
setnames(base,
         old = "V1",
         new = "ID")

#Nos quedamos con la variables que nos interesan
base <- base[, c("ID", #ID
                 "MA9", #Cuántos años tenía cuando empezó a vivir con pareja?
                 "WM7", #Resultado entrevista
                 "WB1Y", #Año del nacimiento de la mujer
                 "WB2", #Edad de la mujer
                 "WB3", #Ha asistido a alguna institucion
                 "CM1", #¿Ha tenido algún parto con un hijo nacido vivo=
                 "WB4", #¿Cuál es el nivel más alto que ha asisitido?
                 "CM2Y", #Año del primer nacimiento
                 "DB1", #Cuando quedó embarazada de ___ ¿quería quedar embarazada?
                 "CP2A", #Alguna vez ha hecho o utilizado algo para demorar el embarazo?
                 "UN2", #¿Cuando quedó embarazada, quería quedar embarazada en ese momento?
                 "SB1", #¿Qué edad tenía cuando mantuvo relaciones sexuales?
                 "SB2", #La primera vez que tuvo relaciones, usó condón?
                 "MA2", #Años del esposo/concubino
                 "TA15", #¿Qué edad tenía cuando tomó su primer trago de alcohol?
                 "LS3", #Satisfacción vida familiar
                 "LS4", #Satisfacción amistades
                 "LS7", #Satisfacción con trabajo
                 "LS8", #Satisfacción salud
                 "LS9", #Satisfacción lugar donde vive
                 "LS12", #Satisfacción vida en general
                 "LS13", #Satisfacción ingresos
                 "welevel", #Educación
                 "ethnicity", #Etnia jefe del hogar
                 "windex5", #Quintiles de riqueza
                 "HH6", #Urbano/Rural
                 "WM6Y")] #Año de la entrevista

#CONTADOR
dt_n <- base[dif == 0, .(ID, SB1, WB2, PRIMER_HIJO, MADRE, exit, dif)]
dt_n <- base[ ,.(n = .N), by = dif]

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
base[SB1 == 95, .(ID, SB1, MA9)] #Aquí podemos ver que la opción 95 no puede ser reemplazada con valores de MA9
base <- base[SB1 < 90 & SB1 > 0, ] #55obs con clave 9

#USO DE CONDON EN LA PRIMERA RELACIÓN SEXUAL
base$SB2[is.na(base$SB2)] <- 0 #Convertir NAs en 0 (683)
base <- base[SB2 < 8, ] #38obs no sabe

#NIVEL EDUCATIVO
base <- base[welevel < 8, ] #1obs con clave 9

#MUJERES INDÍGENAS
base <- base[ethnicity %in% c(2,3,4,5) ] #1obs con clave 9


#CONSTRUCCIÓN VARIABLES DE INTERÉS
#¿Tuvo un hijo?
base[, MADRE := ifelse(CM1 == 1,
                       yes = 1,
                       no = 0)]

#¿Ha tenido relaciones sexuales?
base[, INTERCOURSE := ifelse(SB1 > 1,
                       yes = 1,
                       no = 0)]

#Edad de la madre del primer hijo
base[, PRIMER_HIJO := ifelse(MADRE == 1,
                             yes = CM2Y - WB1Y,
                             no = 0)]

#Tiempo entre primera relación y primer hijo
base[, TIEMPO := ifelse(MADRE == 1,
                        yes = PRIMER_HIJO - SB1,
                        no = 0)]

ver <- base[, .(ID, PRIMER_HIJO, SB1, TIEMPO)] #Aquí podemos ver que hay mala declaración de información
base <- base[TIEMPO >= 0, ] #144obs con información mal declarada

#Mujer que tuvo un hijo y la relación en la misma edad
base[, PRIMER_INTENTO := ifelse(MADRE == 1 & TIEMPO == 0,
                                yes = 1,
                                no = 0)]

#Sumar observaciones de 0 a 1 del tiempo
base$TIEMPO[base$PRIMER_INTENTO == 1] <- 1

#Tiempo entre primera relación y los que no han tenido hijxs
base[, TIEMPO := ifelse(MADRE == 0,
                        yes = WB2 - SB1,
                        no = TIEMPO)]

#Tiempo entre primera relación y los que no han tenido hijxs
base$SB1 <- ifelse(base$MADRE == 0 & base$dif == 0,
                        yes = base$SB1 - 1,
                        no = base$SB1)

#Tiempo entre primera relación y los que no han tenido hijxs
base$PRIMER_HIJO <- ifelse(base$MADRE == 1 & base$dif == 0,
                   yes = base$PRIMER_HIJO + 1,
                   no = base$PRIMER_HIJO)

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


#Sumar observaciones welevel (categorías 1 y 2)
base$welevel[base$welevel == 2] <- 1

#Tiempo de la censura
base[, exit := ifelse(MADRE == 1,
                      yes = PRIMER_HIJO,
                      no = WB2)]

base[, dif := exit - SB1]

#Sumar observaciones de 0 a 1 del tiempo
base$exit[base$dif == 0] <- 1

#Pasamos la base a CSV
write.csv(base, "primera_prueba.csv")
#Al final nos quedamos con 7067  obs.
#5266 sufrieron nuestro evento de interés (Ser madre)

#¿Cómo se comportan las observaciones (GRÁFICAMENTE)? 
hist(base$SB1, main ='Tiempo entre primera relación y nacimiento hijo', xlim = c(5,30), ylim = c(10, 1000)) #year of first sexual relationship



# Análisis de eventos -----------------------------------------------------


#Pasar a long la base
base.2 <- survSplit(Surv(SB1, exit , MADRE) ~., base,
                      cut=c(seq(8,49,1)), event = "HIJO")

#Survival plot
#Sin variables
viz <- survfit(Surv(SB1, exit, HIJO)
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
           legend = "bottom",
           xlim = c(12,49),
           panel.background = element_rect(fill = "gray"))

summary(viz)

#Urbano/rural
viz <- survfit(Surv(SB1, exit, HIJO)
               ~HH6, data = base.2)

class(viz)
ggsurvplot(viz, data = base.2,
           conf.int = FALSE,
           size = 1,
           linetype = c(1),
           palette = c("#FF4040", "#0000EE"),
           legend.title = "Surv Function",
           xlab = "Time",
           ylab = "Survival Probability",
           legend = "bottom",
           xlim = c(12,49),
           panel.background = element_rect(fill = "gray"))

viz <-  summary(viz)$table

#Etnicidad
viz <- survfit(Surv(SB1, exit, HIJO)
               ~ethnicity, data = base.2)

class(viz)
ggsurvplot(viz, data = base.2,
           conf.int = FALSE,
           size = 1,
           linetype = c(1),
           palette = c("#FF4040", "#0000EE", "#008B00", "#FFD700", "#87CEFA"),
           legend.title = "Surv Function",
           xlab = "Time",
           ylab = "Survival Probability",
           legend = "bottom",
           panel.background = element_rect(fill = "gray"))

#Quintiles de riqueza
viz <- survfit(Surv(TIEMPO, HIJO)
               ~windex5, data = base.2)

class(viz)
ggsurvplot(viz, data = base.2,
           conf.int = FALSE,
           size = 1,
           linetype = c(1),
           palette = c("#FF4040", "#0000EE", "#008B00", "#FFD700", "#87CEFA"),
           legend.title = "Surv Function",
           xlab = "Time",
           ylab = "Survival Probability",
           legend = "bottom",
           panel.background = element_rect(fill = "gray"))


#Educación
viz <- survfit(Surv(SB1, exit, HIJO)
               ~welevel, data = base.2)

class(viz)
ggsurvplot(viz, data = base.2,
           conf.int = FALSE,
           size = 1,
           linetype = c(1),
           palette = c("#FF4040", "#0000EE", "#008B00", "#FFD700", "#87CEFA"),
           legend.title = "Surv Function",
           xlab = "Time",
           ylab = "Survival Probability",
           legend = "bottom",
           panel.background = element_rect(fill = "gray"))

#Usaron condón en la primera relación
viz <- survfit(Surv(SB1, exit, HIJO)
               ~SB2, data = base.2)

class(viz)
ggsurvplot(viz, data = base.2,
           conf.int = FALSE,
           size = 1,
           linetype = c(1),
           palette = c("#FF4040", "#0000EE", "#008B00"),
           legend.title = "Surv Function",
           xlab = "Time",
           ylab = "Suvival Probability",
           legend = "bottom",
           panel.background = element_rect(fill = "gray"))


#Usaron condón en la primera relación
viz <- survfit(Surv(TIEMPO, HIJO)
               ~SB1, data = base.2)

class(viz)
ggsurvplot(viz, data = base.2,
           conf.int = FALSE,
           size = 1,
           pval = T,
           linetype = c(1),
           palette = c("#FF4040", "#0000EE", "#008B00"),
           legend.title = "Surv Function",
           xlab = "Time",
           ylab = "Survival Probability",
           legend = "bottom",
           panel.background = element_rect(fill = "gray"))

#Cambiar las variables a categóricas
base.2$welevel<- factor(base.2$welevel)
base.2$ethnicity <- factor(base.2$ethnicity)
base.2$windex5 <- factor(base.2$windex5)
base.2$HH6 <- factor(base.2$HH6)
base.2$SB2 <- factor(base.2$SB2)
base.2$EDAD_INTERCOURSE <- factor(base.2$EDAD_INTERCOURSE)
base.2$WB3 <- factor(base.2$WB3)

#Modelo de riesgos proporcionales de Cox
coxph(Surv(SB1, exit , HIJO) ~ HH6 + ethnicity + windex5 + welevel + SB2,
      data = base.2) %>% 
  tbl_regression(exp = TRUE) 


#Diagnóstico del Modelo
fit <- coxph(Surv(SB1, exit , HIJO) ~ HH6 + ethnicity + windex5 + welevel + SB2, 
             data = base.2)
cox.zph(fit)
ftest <- cox.zph(fit)
ftest
ggcoxzph(ftest)

#Interactuar con variables
coxph(Surv(SB1, exit , HIJO) ~ HH6 + ethnicity + windex5 + welevel + SB2 +
         ethnicity:SB1 + windex5:SB1 + welevel:SB1 + SB2:SB1,
      data = base.2) %>% 
  tbl_regression(exp = TRUE) 










