library(haven)
library(descr)
library(survival)
library(survminer) 
library(lmtest)
library(table1)
library(gtsummary)


#### Step 1: Load data ####
rm(list=ls())

# All data from SOEP (http://companion.soep.de/)
# Meta Data (constant)
FIX <- read_dta("assets/FIX.dta") # 20862 obs.
FIX <- FIX[order(FIX$ID),]

# Personal Data (yearly TV)
VARYING <- read_dta("assets/VARYING.dta")  # 36833 obs.
VARYING <- VARYING[order(VARYING$ID,VARYING$SYEAR),]

# merge the two datasets by ID
SUBSET01 <- merge(VARYING,FIX, by=c("ID"))
SUBSET01 <- SUBSET01[order(SUBSET01$ID,SUBSET01$SYEAR),]

# Employment Biographies (monthly TV)
CALEN <- read_dta("assets/CALEN.dta")  # 241741 obs.
CALEN <- CALEN[order(CALEN$ID,CALEN$SYEAR,CALEN$BEGIN),]

# merge CALEN with SUBSET01 by ID and SYEAR
SUBSET02 <- merge(CALEN, SUBSET01, by=c("ID","SYEAR")) 
SUBSET02 <- SUBSET02[order(SUBSET02$ID,SUBSET02$SYEAR,SUBSET02$BEGIN),]


#### Step 2: Variable construction ####

#VAR: SEX 
SUBSET02$SEX<-"NA" 
SUBSET02$SEX[SUBSET02$SEX01==1]<-"Men"
SUBSET02$SEX[SUBSET02$SEX01==2]<-"Women" 
SUBSET02$SEX<-as.factor(SUBSET02$SEX) 

#VAR: AGECAT 
SUBSET02$AGE<-"4-Other" 
SUBSET02$AGE[SUBSET02$AGE01>=17]<-"17-24" 
SUBSET02$AGE[SUBSET02$AGE01>=25]<-"25-29" 
SUBSET02$AGE[SUBSET02$AGE01>=30]<-"30-39" 
SUBSET02$AGE[SUBSET02$AGE01>=40]<-"40-49" 
SUBSET02$AGE[SUBSET02$AGE01>=50]<-"50-64"
SUBSET02$AGE<-as.factor(SUBSET02$AGE) 

#VAR: MIG 
SUBSET02$MIG<-"NA"
SUBSET02$MIG[SUBSET02$MIGBACK==1]<-"Natives"
SUBSET02$MIG[SUBSET02$MIGBACK==2]<-"Migrants"
SUBSET02$MIG<-as.factor(SUBSET02$MIG)

#VAR: WORRY HEALTH 
SUBSET02$HEALTH<-"NA" 
SUBSET02$HEALTH[SUBSET02$HEALTH01==1]<-"1-Poor" 
SUBSET02$HEALTH[SUBSET02$HEALTH01==2]<-"2-Medium" 
SUBSET02$HEALTH[SUBSET02$HEALTH01==3]<-"3-Good" 
SUBSET02$HEALTH<-as.factor(SUBSET02$HEALTH) 

#VAR: EDU 
SUBSET02$EDU<-"NA" 
SUBSET02$EDU[SUBSET02$PGISCED97==1]<-"1-low" 
SUBSET02$EDU[SUBSET02$PGISCED97==2]<-"1-low" 
SUBSET02$EDU[SUBSET02$PGISCED97==3]<-"2-medium" 
SUBSET02$EDU[SUBSET02$PGISCED97==4]<-"2-medium" 
SUBSET02$EDU[SUBSET02$PGISCED97==5]<-"2-medium" 
SUBSET02$EDU[SUBSET02$PGISCED97==6]<-"3-high" 
SUBSET02$EDU<-as.factor(SUBSET02$EDU)

#VAR: FAMILY STATUS
SUBSET02$FAM<-"NA"
SUBSET02$FAM[SUBSET02$PGFAMSTD==1]<-"1-Married"
SUBSET02$FAM[SUBSET02$PGFAMSTD==2]<-"1-Married"
SUBSET02$FAM[SUBSET02$PGFAMSTD==3]<-"2-Single"
SUBSET02$FAM[SUBSET02$PGFAMSTD==4]<-"3-Divorced/Widowed"
SUBSET02$FAM[SUBSET02$PGFAMSTD==5]<-"3-Divorced/Widowed"
SUBSET02$FAM[SUBSET02$PGFAMSTD==6]<-"1-Married"
SUBSET02$FAM[SUBSET02$PGFAMSTD==7]<-"4-Same-Sex"
SUBSET02$FAM[SUBSET02$PGFAMSTD==8]<-"4-Same-Sex"
SUBSET02$FAM <- as.factor(SUBSET02$FAM)

# construct the event variable (employment)
SUBSET02$Surv <- with(SUBSET02, Surv(BEGIN, END, EVENT==1))

#Summary Statistics
table1(~ HEALTH + AGE + SEX + MIG | MIG, data = SUBSET02)


#### Step 3: Analysis ####

# fit the Weibull model with migration status
SURVIVAL01 <- survfit(SUBSET02$Surv ~ SUBSET02$MIGBACK)
plot(SURVIVAL01, col = c("blue", "red"), xlab="unemployment duration (in months)")
legend("topright", legend = c("Natives", "Migrants"), col = c("blue", "red"), lwd=2, cex = 1,)

# fit the Cox model with gender, migration status, age
MODEL01 <- coxph(SUBSET02$Surv ~ SEX+MIG+AGE, data=SUBSET02)
OUTPUT01 <- tbl_regression(MODEL01, exponentiate = TRUE)
tbl_merge(
  tbls = list(OUTPUT01),
  tab_spanner = c("Model01"))

# fit the Cox model with gender, migration status, age, education
MODEL02 <- coxph(SUBSET02$Surv ~ SEX+MIG+AGE+EDU, data=SUBSET02)
MODEL03 <- coxph(SUBSET02$Surv ~ SEX+MIG+AGE+EDU+HEALTH, data=SUBSET02) 
OUTPUT02 <- tbl_regression(MODEL02, exponentiate = TRUE) 
OUTPUT03 <- tbl_regression(MODEL03, exponentiate = TRUE) 
tbl_merge( tbls = list(OUTPUT01, OUTPUT02, OUTPUT03), 
           tab_spanner = c("Model01", "Model02", "Model03"))

# Conduct the likelihood ratio test
lrtest(MODEL01, MODEL02)
lrtest(MODEL02, MODEL03)

# Exercise 4.4

# Interaction effect between health and migration status
SUBSET02$INT <- interaction(SUBSET02$HEALTH, SUBSET02$MIG)
MODEL03_2 <- coxph(SUBSET02$Surv ~ INT+AGE+SEX+EDU, data=SUBSET02)
OUTPUT03_2 <- tbl_regression(MODEL03_2, exponentiate = TRUE)

# Use the survminer package to visualize the results
ggforest(MODEL03_2, data = NULL,fontsize = 1)

# Interaction effect betwwen gender and migration status
SUBSET02$INT2 <- interaction(SUBSET02$SEX, SUBSET02$MIG)
MODEL04 <- coxph(SUBSET02$Surv ~ INT2+AGE+EDU, data=SUBSET02)
OUTPUT04 <- tbl_regression(MODEL04, exponentiate = TRUE)
# Visualize the results
ggforest(MODEL04, data = NULL,fontsize = 1)

# Exercise 4.5

# Interaction effect between gender and family status
SUBSET02$INT3 <- interaction(SUBSET02$SEX, SUBSET02$FAM)
MODEL05 <- coxph(SUBSET02$Surv ~ INT3+EDU+MIG+AGE, data=SUBSET02)
OUTPUT05 <- tbl_regression(MODEL05, exponentiate = TRUE)

# Visualize the results
ggforest(MODEL05, data = NULL,fontsize = 1)
