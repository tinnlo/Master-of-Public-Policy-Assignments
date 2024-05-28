library(haven)
library(descr)
library(stargazer)
library(table1)
library(gtsummary)
library(ggplot2)
library(tidyr) 


#### Step 1: data preparation ####
rm(list=ls())
#Read in data 
DATA01 <- read_dta("assets/Exercise05.dta") 

# Filter the data in 1990
DATA <- subset(DATA01,DATA01$syear==1990)
# Filter the data for female
DATA <- subset(DATA,DATA$SEX==2)
# Filter the MIG variable
DATA <- subset(DATA,DATA$MIG==1 | DATA$MIG==2)


tbl_regression(MODEL01)

#### Step 2: variables construction ####


#Construct new variables 
DATA$LOG<-log(DATA$WHOUR) 
DATA$DUR2<-DATA$DUR*DATA$DUR 
DATA$AGE2<-(DATA$AGE*DATA$AGE)/1000 

#Change format 
DATA$MIG<-as.factor(DATA$MIG) 
DATA$EMP<-as.factor(DATA$EMP)

#### Step 3: Analysis ####

# Exercise 5.1
MODEL01 <- lm(LOG ~ AGE2+MIG+DUR+DUR2, data = DATA)
summary(MODEL01)

table1(~ AGE2+MIG+DUR+DUR2, data = DATA)


#Create data frame 
DUR <- (seq(0,30)) 
DATA01 = data.frame(DUR) 
DATA01$WAGE<- 1.5660860 + 0.0433363*DUR + (-0.0010315*DUR^2) 
#Plot the results 
ggplot(data=DATA01, aes(x=DUR, y=WAGE, group="1")) + 
  geom_line(color="red")+ 
  geom_point(color="red")+ 
  ylab("Wage")+ xlab("Duration")+ 
  geom_hline(yintercept = 0)

