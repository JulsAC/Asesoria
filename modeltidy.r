#Data analisys for survival rates of pregnancy

library(data.table)
library(dplyr)
library(haven)
library(ggplot2)
library(survival)
library(survminer)
library(tidyverse)
library(gtsummary)
library (here)
library(foreign)
library(tidyr)
library(dplyr)
library(epiDisplay)
library(broom)

base_etiquetas <- as.data.frame(read_sav("./wm.sav"))
write.csv(base_etiquetas, "base_claudia.csv")
base <- fread("./base_claudia.csv")

#Set ID for women
setnames(base,
         old = "V1",
         new = "ID")

#Cleaning variables that we will not use
base <- base[, c("ID", #ID
                 "WB1Y", #Year of birth women
                 "WB2", #Age women at interview time
                 "CM1", #has given birth to alive child
                 "CM2Y", #Year FCBA
                 "SB1", #Age at first sexual relation (SR)
                 "SB2", #Use of condom in first SR
                 "welevel", #Educacation
                 "ethnicity", #ethnicity of home
                 "windex5", #wealth quintiles
                 "HH6", #Urban/Rural
                 "WM6Y", #Year of interview
                 "MA9",)] #Year of cohabitance with partner

#COUNTER
dt_n<- base %>%
  filter(MADRE == 1) %>%
  group_by(SB2) %>%
  summarize(n_f = n())


dt_n <- base[MADRE == 1, .(n_f = .N), by = SB2]

#Check NA's
#Porcentaje
dt_n <- as.data.frame(apply(is.na(base), 2, mean)*100)
#Absoluto
dt_n <- as.data.frame(apply(is.na(base), 2, sum))

#Clena NA's
#Year of birth woman
base <- base[!is.na(base$WB1Y),] #542 NAs down to 7310 obs
base <- base[WB1Y < 9000, ] #1obs no sabe 

#Women that had the event clean na's or didn't answear
base <- base[CM1 < 8, ] #1obs con clave 9 down to 7309 obs

#Year of fist alive birth
base$CM2Y[is.na(base$CM2Y)] <- 0 #Convertir NAs en 0 
base <- base[CM2Y != 9999, ] #4obs con clave 9 down to 7305

#Age of first seaxual relationship
base[SB1 == 95, .(ID, SB1, MA9)] #option 95 should be replaced with MA9 (first sexual realtionship was with moved in with first partnet) but there are missing values
base <- base[SB1 < 90, ] #clean missing down to 7250 obs

#Exclude observation that never had a sexual relationship because there is no hazard
base <- base[SB1 > 0] #down to 6567 obs


#Use of condon in first SR
base$SB2[is.na(base$SB2)] <- 0 #Convertir NAs en 0 (683)
base <- base[SB2 < 8, ] #clean 38obs with don-t know answer down to 6529 obs

#Educational level
base <- base[welevel < 8, ] #1NA down to 6528 obs

#Agreggate level 1 and 2 of education
base$welevel[base$welevel == 2] <- 1

#Construcction of variables
#¿Had a born alive?
base[, MADRE := ifelse(CM1 == 1,
                       yes = 1,
                       no = 0)]

#¿Had sexual relationship? just to check
base[, INTERCOURSE := ifelse(SB1 > 1,
                       yes = 1,
                       no = 0)]
table(base$INTERCOURSE) #all good 

#Age of mother at first child born
base[, PRIMER_HIJO := ifelse(MADRE == 1,
                             yes = CM2Y - WB1Y,
                             no = 0)]

#Years between first sexual relationship and first child born
base[, TIEMPO := ifelse(MADRE == 1,
                        yes = PRIMER_HIJO - SB1,
                        no = 0)]

base[, .(ID, PRIMER_HIJO, SB1, TIEMPO)] #There are some odd values as negative
base <- base[TIEMPO >= 0, ] #Clean odd values 144obs down to 6384 obs

#women who report SR in the same year of fcb -1 in age of first SR
base$SB1 <- ifelse(base$MADRE == 1 & base$TIEMPO == 0,
                        yes = base$SB1 - 1,
                        no = base$SB1)


###Time btw first sr and child for no mothers
base[, TIEMPO := ifelse(MADRE == 0 & INTERCOURSE == 1,
                        yes = WB2 - SB1,
                        no = TIEMPO)]

#Women that had their first SR and god interview that same year
base$SB1 <- ifelse(base$MADRE == 0 & base$TIEMPO == 0,
                   yes = base$SB1 + 1,
                   no = base$SB1)


#Summ  0 to 1 in time for that
base$TIEMPO[base$SAME_YEAR == 1] <- 1

#Truncate age of first Sexual relationship
base[, EDAD_INTERCOURSE := ifelse(SB1 %in% c(15:18), #Entre 15 y 19 (73%)
                                            yes = 2,
                                            no = ifelse(SB1 %in% c(19:46), #Mayores de 20 (17%)
                                                        yes = 3,
                                                        no = 1))] #Menores de 15 años (10%)


#Censored time
base[, exit := ifelse(MADRE == 1,
                      yes = PRIMER_HIJO,
                      no = WB2)]

base[, dif := exit - SB1]

#Write clean base to csv
write.csv(base, "baselimpia.csv")
#we end up with 6384 obs
#5266 had our event of interest 

#¿How do observations look? 
hist(base$SB1, main ='Age of first sexual relation', xlab = 'Age', xlim = c(5,30), ylim = c(10, 2000)) #year of first sexual relationship

#Making base to long
base.2 <- survSplit(Surv(SB1, exit , MADRE) ~., base,
                    cut=c(seq(8,49,1)), event = "HIJO")

#Survival plot

#fit model
fit.base2 <- survfit(data = base.2,
                   Surv(SB1, exit, HIJO) ~ 1)
str(fit.base2)

fit.base2$n.event

# Pull all this together in a data frame-------
lt.prep <- data.frame(
  age = fit.base2$time,
  nrisk = fit.base2$n.risk,
  nevent = fit.base2$n.event,
  survivor = fit.base2$surv) %>%
  mutate(hazard = nevent / nrisk
  )

# Plot the hazard-----------
ggplot(lt.prep, aes(x = age, y = hazard)) +
  geom_line() +
  theme(panel.grid = element_blank())

# Plot the survivor function
ggplot(lt.prep, aes(x = age, y = survivor)) +
  geom_hline(yintercept = 1, color = "white", linetype = 2) +
  geom_line() + 
  geom_vline(xintercept=12, linetype="dashed", color = "red") + 
  scale_y_continuous("Survival probability", breaks = c(0,.5,1), limits = c(0,1)) + 
  theme(panel.grid = element_blank())

fit.base2
#median time 12/5266 events

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
           xlim= c(8,45),
           xlab = "Age",
           ylab = "Survival Probability",
           legend = "bottom",
           panel.background = element_rect(fill = "gray"))


summary(viz)


#Urban/rural
viz <- survfit(Surv(SB1, exit, HIJO)
               ~HH6, data = base.2)

class(viz)
ggsurvplot(viz, data = base.2,
           conf.int = FALSE,
           size = 1,
           linetype = c(1),
           palette = c("#FF4040", "#0000EE"),
           legend.title = "Surv Function",
           xlim= c(8,45),
           xlab = "Age",
           legend.labs = c("Urban", "Rural"),
           ylab = "Survival Probability",
           legend = "bottom",
           panel.background = element_rect(fill = "gray"))


viz <-  summary(viz)$table


#Etnicidad
viz <- survfit(Surv(TIEMPO, HIJO)
               ~ethnicity, data = base.2)

class(viz)
ggsurvplot(viz, data = base.2,
           conf.int = FALSE,
           size = 1,
           linetype = c(1),
           palette = c("#FF4040", "#0000EE", "#008B00", "#FFD700", "#87CEFA"),
           legend.title = "Legend",
           legend.labs = c("Indigenous" ,"Guarani speaker", "Guarani and Spanish", "Only Spanish", "Other language"),
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
           legend.title = "Legend",
           legend.labs = c("More poor" ,"q2", "q3", "q4", "More rich"),
           xlab = "Time",
           ylab = "Survival Probability",
           legend = "bottom",
           panel.background = element_rect(fill = "gray"))


#Educación
viz <- survfit(Surv(TIEMPO, HIJO)
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
viz <- survfit(Surv(TIEMPO, HIJO)
               ~SB2, data = base.2)

class(viz)
ggsurvplot(viz, data = base.2,
           conf.int = FALSE,
           size = 1,
           linetype = c(1),
           palette = c("#FF4040", "#0000EE", "#008B00"),
           legend.title = "Surv Function",
           legend.labs = c("Yes" , "No"),
           xlab = "Time",
           ylab = "Suvival Probability",
           legend = "bottom",
           panel.background = element_rect(fill = "gray"))

# Linear model---------
linear.fit1 <- glm(HIJO ~ SB1 , family = binomial, data = base.2)
linear.fit2 <- glm(HIJO ~ EDAD_INTERCOURSE, family = binomial, data = base.2)
linear.fit3 <- glm(HIJO ~ EDAD_INTERCOURSE +welevel +windex5 + ethnicity + HH6 + SB2 , family = binomial, data = base.2)

# Nonlinear model
base.2$EDAD_INTERCOURSE <- as.numeric(as.character(base.2$EDAD_INTERCOURSE))
base.2$EDAD_INTERCOURSE.2 <- base.2$EDAD_INTERCOURSE^2

nonlinear.fit <- glm(HIJO ~ EDAD_INTERCOURSE + EDAD_INTERCOURSE.2, family = binomial, data = base.2)
summary(linear.fit1)
summary(linear.fit2)
summary(linear.fit3)
summary(nonlinear.fit)

# Test model fit
lrtest(linear.fit, nonlinear.fit)
#no improvement with non linear model

base.2$SB1 <- factor(base.2$SB1)
base.2$EDAD_INTERCOURSE <- factor(base.2$EDAD_INTERCOURSE)


is.factor(base.2$SB1)
is.factor(base.2$welevel)
is.factor(base.2$ethnicity)
is.factor(base.2$windex5)
is.factor(base.2$SB2)
is.factor(base.2$HH6)

rr1 <- glm(HIJO ~ EDAD_INTERCOURSE, family = binomial, data = base.2)
exp(coefficients(rr1))
logistic.display(rr1)


#Building model for cox
base.2$welevel<- factor(base.2$welevel)
base.2$ethnicity <- factor(base.2$ethnicity)
base.2$windex5 <- factor(base.2$windex5)
base.2$HH6 <- factor(base.2$HH6)
base.2$SB2 <- factor(base.2$SB2)
base.2$WB3 <- factor(base.2$WB3)


#Cox hazard model------
base.2$SB1 <- as.numeric(base.2$SB1)

coxph(Surv(SB1, exit , HIJO) ~ HH6 + ethnicity + windex5 + welevel + SB2,
      data = base.2) %>% 
  tbl_regression(exp = TRUE) 


#Diagnose of the model-----
fit <- coxph(Surv(SB1, exit , HIJO) ~ HH6 + ethnicity + windex5 + welevel + SB2 + TIEMPO,
             data = base.2)
cox.zph(fit)
ftest <- cox.zph(fit)
ftest
ggcoxzph(ftest)

#Interactuar time with variables
coxph(Surv(SB1,exit, HIJO) ~ HH6 + ethnicity + windex5 + welevel + SB2 + SB1 +
        HH6:SB1 + ethnicity:SB1 + windex5:SB1 + welevel:SB1 + SB2:SB1,
      data = base.2) %>% 
  tbl_regression(exp = TRUE) 

write.csv(base.2, "base_claudia_V2.csv")









