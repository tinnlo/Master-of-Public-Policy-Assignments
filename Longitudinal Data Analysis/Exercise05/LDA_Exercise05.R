library(gtsummary) 
library(table1) 
library(descr) 
library(sjPlot) 
library(dplyr) 
library(tidyr)
library(stargazer)


#### Step 1: Load data ####
rm(list=ls())

#Select sample: Only balanced panel 
DATA<-readRDS("assets/EXERCISE05.rds")
#New variable: SPELL01 
DATA <- DATA %>% arrange(ID, YEAR) %>% 
  group_by(ID) %>% mutate(SPELL01=row_number()) 

#New variable: SPELL_max01 
DATA<- DATA %>% arrange(ID, YEAR) %>% 
  group_by(ID) %>% mutate(SPELL_max01=max(SPELL01)) 

DATA$SPELL_max01<-as.factor(DATA$SPELL_max01)
DATA$SPELL01<-as.factor(DATA$SPELL01)
DATA$EMP<-as.factor(DATA$EMP)
DATA$SEX<-as.factor(DATA$SEX)
DATA$AGE100 <- DATA$AGE*1000

#Select only the last spell
DATA <- subset(DATA,DATA$SPELL_max01!=1)

table1(~AGE+FAM+YEAR+ SEX+MIG+EDU | EMP, data=DATA)

# we exclude the "2-In education" category in EMP
DATA01 <- subset(DATA,EMP!="2-In education")

#OLS-Regression 
MODEL01<-lm(SATIS ~ EMP + SEX + AGE100 + MIG + FAM, data=DATA01)
MODEL02<-lm(SATIS ~ EMP + SEX + AGE100 + MIG + FAM + EDU, data=DATA01) 
OUTPUT01<-tbl_regression(MODEL01) 
OUTPUT02<-tbl_regression(MODEL02) 
tbl_merge( tbls = list(OUTPUT01, OUTPUT02), tab_spanner = c("MODEL01", "MODEL02"))

# DiD

#Variables: FAM01 (numeric) 
DATA01$EMP01 <- -1 
DATA01$EMP01[DATA01$EMP=="1-Employed"] <- 0 
DATA01$EMP01[DATA01$EMP=="2-Unemployed"] <- 1 
DATA01$EMP01[DATA01$EMP=="3-Not working"] <- 3
DATA01$EMP01<-as.numeric(DATA01$EMP01)

#Variables: Lags 
DATA01 <- DATA01%>% group_by(ID) %>% 
  mutate(SATISLAG=lag(SATIS),EMPLAG=lag(EMP01)) %>% 
  ungroup() 

DATA01$DIFFSATIS <- DATA01$SATIS - DATA01$SATISLAG 
DATA01$DIFFEMP <- DATA01$EMP01 - DATA01$EMPLAG

DATA01$SATIS_NEW <- DATA01$SATIS
DATA01$EMP_NEW <- DATA01$EMP01

#Regression 
OUTPUT01<-lm(DIFFSATIS ~ DIFFEMP , data=DATA01) 
stargazer(OUTPUT01,type="text")

#Regression by gender 
SOEP01_WOMEN <- subset(DATA01,DATA01$SEX=="1-Women") 
SOEP01_MEN <- subset(DATA01,DATA01$SEX=="2-Men") 
OUTPUT01<-lm(DIFFSATIS ~ DIFFEMP , data=SOEP01_WOMEN) 
OUTPUT02<-lm(SATIS_NEW ~ EMP_NEW , data=SOEP01_MEN) 
stargazer(OUTPUT01,OUTPUT02,type="text")
