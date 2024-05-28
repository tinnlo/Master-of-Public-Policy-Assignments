# EXERCISE01
# Packages
library(ggplot2)
library(broom)
library(survminer)
library(survminer)

#Load Data
DATA01      <- readRDS("Exercise01.Rds")
DATA02      <-subset(DATA01, (DATA01$cntry=="DE"))

#Analysis I 
SURVIVAL01 <- survfit(Surv(DATA02$TIME, DATA02$EVENT) ~ 1)
plot(SURVIVAL01)
SURVIVAL02 <- survfit(Surv(DATA02$TIME, DATA02$EVENT) ~ DATA02$GENDER)
par(mfrow = c(1, 2))    
plot(SURVIVAL01)
plot(SURVIVAL02)

#Analysis II: by subgroups 
COHORT01      <-subset(DATA02, DATA02$COHORT=="1920-39")
COHORT02      <-subset(DATA02, DATA02$COHORT=="1940-59")
COHORT03      <-subset(DATA02, DATA02$COHORT=="1960-79")
COHORT04      <-subset(DATA02, DATA02$COHORT=="1980-00")

S_COHORT1    <- survfit(Surv(COHORT01$TIME, COHORT01$EVENT) ~ COHORT01$GENDER)
S_COHORT2    <- survfit(Surv(COHORT02$TIME, COHORT02$EVENT) ~ COHORT02$GENDER)
S_COHORT3    <- survfit(Surv(COHORT03$TIME, COHORT03$EVENT) ~ COHORT03$GENDER)
S_COHORT4    <- survfit(Surv(COHORT04$TIME, COHORT04$EVENT) ~ COHORT04$GENDER)

#BASE R
par(mfrow = c(2, 2)) 
plot(S_COHORT1,col = c("red", "blue"))
plot(S_COHORT2,col = c("red", "blue"))
plot(S_COHORT3,col = c("red", "blue"))
plot(S_COHORT4,col = c("red", "blue"))

#GGSURVPLOTS
ggsurvplot(S_COHORT1, data=COHORT01)
ggsurvplot(S_COHORT1, data=COHORT02)
ggsurvplot(S_COHORT1, data=COHORT03)
ggsurvplot(S_COHORT1, data=COHORT04)

#GGPLOT
T_COHORT1    <- tidy(S_COHORT1) 
T_COHORT2    <- tidy(S_COHORT2) 
T_COHORT3    <- tidy(S_COHORT3) 
T_COHORT4    <- tidy(S_COHORT4) 
par(mfrow = c(2, 2))

ggplot(T_COHORT1, aes(x=time, y=estimate, group=strata))+
  geom_step(aes(color=strata))+
  theme(legend.position="bottom")+
  scale_y_continuous(name="Survival probability") +
  scale_x_continuous(name="Age") +
  scale_color_manual(name="Gender", labels=c("female", "male"), values=c("red", "blue"))


