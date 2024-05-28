library(haven)
library(descr)
library(questionr)
library(survival)
library(table1)
library(eha)
library(gtsummary)


#### Step 1: Load data ####
rm(list=ls())
DATA01 <- read_dta("assets/PKIR71FL_SMALL.DTA") # 13118 obs.

DATA02 <- subset(DATA01, !(is.na(DATA01$EVENT)))
DATA02 <- subset(DATA02, !(is.na(DATA02$TIME)))
# 11135 obs. left

# Update TIME=0 to TIME=0.1 because TIME=0 is not allowed in the Weibull model
# TIME=0 probably means that the event occurred at the same year of the second birth
DATA02$TIME <- ifelse(DATA02$TIME==0, 0.1, DATA02$TIME)

# Survival plot
SURVIVAL01 <- survfit(Surv(DATA02$TIME,DATA02$EVENT) ~ 1) 
plot(SURVIVAL01, xlab = "duration between 2nd and 3rd birth")


#### Step 2: Variable construction ####

#New Variable: GENKIDS01
DATA02$GENKIDS01 <- "NA" 
DATA02$GENKIDS01[DATA02$GENKIDS==1] <- "Boys" 
DATA02$GENKIDS01[DATA02$GENKIDS==2] <- "Girls" 
DATA02$GENKIDS01[DATA02$GENKIDS==3] <- "Mix" 
DATA02$GENKIDS01 <- as.factor(DATA02$GENKIDS01)

#New Variable: EDUCATION 
DATA02$EDU <- "NA" 
DATA02$EDU[DATA02$v106==0] <- "No education" 
DATA02$EDU[DATA02$v106==1] <- "Some education" 
DATA02$EDU[DATA02$v106==2] <- "Some education" 
DATA02$EDU[DATA02$v106==3] <- "Some education" 
DATA02$EDU <- as.factor(DATA02$EDU) 

#New Variable: URBAN 
DATA02$URBAN <- "NA" 
DATA02$URBAN[DATA02$v025==1] <- "Urban" 
DATA02$URBAN[DATA02$v025==2] <- "Rural" 
DATA02$URBAN <- as.factor(DATA02$URBAN)

#New Variable: COHORT
DATA02$COHORT <- "NA"
DATA02$COHORT[DATA02$v010 >= 1968 & DATA02$v010 <= 1983] <- "1968-1983"
DATA02$COHORT[DATA02$v010 >= 1984 & DATA02$v010 <= 2001] <- "1984-2001"
DATA02$COHORT <- as.factor(DATA02$COHORT)

#New Variable: AGEKID1
DATA02$AGEKID1 <- "NA"
DATA02$AGEKID1[DATA02$v212 < 20] <- "<20"
DATA02$AGEKID1[DATA02$v212 >= 20 & DATA02$v212 < 25] <- "20-24"
DATA02$AGEKID1[DATA02$v212 >= 25 & DATA02$v212 < 30] <- "25-29"
DATA02$AGEKID1[DATA02$v212 >= 30] <- ">=30"
DATA02$AGEKID1 <- as.factor(DATA02$AGEKID1)
DATA02$AGEKID1 <- factor(DATA02$AGEKID1, levels = c("<20", "20-24", "25-29", ">=30"))

# Urban and Rural separated datasets
DATA_URBAN <- subset(DATA02,subset = DATA02$v025==1)
DATA_RURAL <- subset(DATA02,subset = DATA02$v025==2)

# Sample Statistics
table1::label(DATA02$GENKIDS01) <- "Sex Composition"
table1::label(DATA02$EDU) <- "Education"
table1::label(DATA02$COHORT) <- "Cohort"
table1::label(DATA02$AGEKID1) <- "Age at First Birth"
table1(~ GENKIDS01 + COHORT + EDU + AGEKID1 | URBAN, data = DATA02)


#### Step 3: Analysis ####

# Model 1: GENKIDS01

# fit Cox model
coxph(Surv(TIME,EVENT) ~ GENKIDS01, data=DATA02)
MODEL01 <- coxph(Surv(TIME,EVENT) ~ GENKIDS01, data=DATA02)

# display regression model results
OUTPUT01 <- tbl_regression(MODEL01, exponentiate = TRUE)
tbl_merge(
  tbls = list(OUTPUT01),
  tab_spanner = c("Model01"))

# Model 2: GENKIDS01 + COHORT + EDU + AGEKID1 + URBAN
MODEL02 <- coxph(Surv(TIME,EVENT) ~ GENKIDS01 + COHORT + EDU + AGEKID1 + URBAN, data=DATA02)
OUTPUT02 <- tbl_regression(MODEL02, exponentiate = TRUE)

tbl_merge(
  tbls = list(OUTPUT02),
  tab_spanner = c("Model02"))


#### Step 4: Sub-datasets Analysis ####

# Model 3: GENKIDS01 + COHORT + EDU + AGEKID1
MODEL03_URBAN <- coxph(Surv(TIME,EVENT) ~ GENKIDS01 + COHORT + EDU + AGEKID1, data=DATA_URBAN)
OUTPUT03_URBAN <- tbl_regression(MODEL03_URBAN, exponentiate = TRUE)

MODEL03_RURAL <- coxph(Surv(TIME,EVENT) ~ GENKIDS01 + COHORT + EDU + AGEKID1, data=DATA_RURAL)
OUTPUT03_RURAL <- tbl_regression(MODEL03_RURAL, exponentiate = TRUE)

tbl_merge(
  tbls = list(OUTPUT03_URBAN, OUTPUT03_RURAL),
  tab_spanner = c("Model03_URBAN", "Model03_RURAL"))
