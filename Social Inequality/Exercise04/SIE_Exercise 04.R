library(haven)
library(descr)
library(stargazer)
library(table1)
library(gtsummary)


#### Step 1: data preparation ####
rm(list=ls())
#Read in data 
DATA01 <- read_dta("assets/PISA2018.dta") 
DATA01 <- subset(DATA01, DATA01$CNT=="ARG")


#### Step 2: variables construction ####

#VAR: Age 
DATA01$AGE <- as.factor(DATA01$AGE)

#VAR: Migration Background 
DATA01$MIG<-NA
DATA01$MIG[DATA01$IMMIG==1] <-"Native" 
DATA01$MIG[DATA01$IMMIG==2] <-"Second gen" 
DATA01$MIG[DATA01$IMMIG==3] <-"First gen" 
DATA01$MIG <-as.factor(DATA01$MIG) 
DATA01$MIG <-relevel(DATA01$MIG, ref="Native") #Change reference category

#VAR: Expected ISEI
DATA01$ISEI <- NA
DATA01$ISEI <- as.numeric(DATA01$BSMJ)

#VAR: Mother's ISEI
DATA01$MISEI <- NA
DATA01$MISEI <- as.numeric(DATA01$BMMJ1)

#VAR: Father's ISEI
DATA01$FISEI <- NA
DATA01$FISEI <- as.numeric(DATA01$BFMJ2)

#VAR: Gender
DATA01$GENDER<-NA
DATA01$GENDER[DATA01$ST004D01T==1]<-"1-Male"
DATA01$GENDER[DATA01$ST004D01T==2]<-"2-Female"
DATA01$GENDER<-as.factor(DATA01$GENDER)

#Sample Statistics
table1(~ FISEI+AGE+GENDER, data = DATA01)
table1(~ MISEI+FISEI+AGE+GENDER, data = DATA01)

#Split Sub-Dataset
GIRLS <- subset(DATA01,DATA01$ST004D01T == 2)
BOYS <- subset(DATA01,DATA01$ST004D01T == 1)


#### Step 3: Analysis ####

# Exercise 4.2
MODEL01 <- lm(ISEI ~ FISEI+AGE+GENDER, data = DATA01)
MODEL02 <- lm(ISEI ~ FISEI+MISEI+AGE+GENDER, data = DATA01)
stargazer(MODEL01,MODEL02,type="text") 

OUTPUT01 <- tbl_regression(MODEL01)
OUTPUT02 <- tbl_regression(MODEL02)
tbl_merge(tbls = list(OUTPUT01, OUTPUT02), tab_spanner = c("Model1", "Model2"))

# Exercise 4.3

MODEL03_G <- lm(ISEI ~ FISEI+MISEI+AGE, data = GIRLS)
MODEL03_B <- lm(ISEI ~ FISEI+MISEI+AGE, data = BOYS)
stargazer(MODEL03_G,MODEL03_B,type="text")

OUTPUT03_G <- tbl_regression(MODEL03_G)
OUTPUT03_B <- tbl_regression(MODEL03_B)

tbl_merge(tbls = list(OUTPUT03_G, OUTPUT03_B), tab_spanner = c("Girls", "Boys"))

# Exercise 4.4

freq(DATA01$MIG) # Argentina has few migrants
MODEL04 <- lm(ISEI ~ MIG+AGE+GENDER, data = DATA01)
MODEL05 <- lm(ISEI ~ MIG+AGE+GENDER+FISEI, data = DATA01)
stargazer(MODEL04,MODEL05,type="text")

OUTPUT04 <- tbl_regression(MODEL04)
OUTPUT05 <- tbl_regression(MODEL05)

tbl_merge(tbls = list(OUTPUT04, OUTPUT05), tab_spanner = c("Model4", "Model5"))
