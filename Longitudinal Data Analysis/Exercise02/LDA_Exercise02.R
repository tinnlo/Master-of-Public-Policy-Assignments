library(haven)
library(descr)
library(questionr)
library(survival)
library(survminer)

#### Step 1: Load data ####
rm(list=ls())
DATA01 <- read_dta("assets/ESS9e03_2.dta")


#### Step 2: Select cases & variables ####
# create a subset of Italy with the variables we need 
DATA02 <- subset(DATA01, select=c(gndr, yrbrn, inwyys, cntry, eisced, evmar, maryr), DATA01$cntry == "IT")

# filter the female as the target group
DATA03 <- subset(DATA02, DATA02$gndr == 2)

# filter the people who birth year is between 1940 and 1999
DATA04 <- subset(DATA03, yrbrn >= 1940 & yrbrn <= 1999)
# filter the people who have valid 'eisced' values
DATA05 <- subset(DATA04, DATA04$eisced > 0 & DATA04$eisced < 8)
# filter the people who either live with parents or married between 1955 and 2019
DATA06 <- subset(DATA05, DATA05$evmar == 2 | (DATA05$maryr >= 1955 & DATA05$maryr < 2019))

# 1193 observations left


#### Step 3: Variable construction ####
# VAR: Event 
DATA06$EVENT <- NA
DATA06$EVENT[DATA06$evmar == 2] <- 0
DATA06$EVENT[DATA06$evmar == 1] <- 1 
DATA06$EVENT<-as.numeric(DATA06$EVENT)

# VAR: TIME
# if the oberservation already married, then 'TIME' equals the 'maryr' - 'yrbrn'
# if not, then 'the'TIME' equals 'inwyys' - 'yrbrn'
DATA06$TIME <- NA
DATA06$TIME[DATA06$evmar == 1] <- DATA06$maryr[DATA06$evmar == 1] - DATA06$yrbrn[DATA06$evmar == 1]
DATA06$TIME[DATA06$evmar == 2] <- DATA06$inwyys[DATA06$evmar == 2] - DATA06$yrbrn[DATA06$evmar == 2]
DATA06$TIME <- as.numeric(DATA06$TIME)

# VAR: COHORT classified as 2 categories
# 1040-1069: 1
# 1970-1999: 2
DATA06$COHORT <- NA
DATA06$COHORT[DATA06$yrbrn>=1940 & DATA06$yrbrn<=1969] <- "1940-69"
DATA06$COHORT[DATA06$yrbrn>=1970 & DATA06$yrbrn<=1999] <- "1970-99"
DATA06$COHORT<-factor(DATA06$COHORT)

# VAR: EDUCATION classified as 3 categories based on education level and sample size
DATA06$EDUCATION <- NA
DATA06$EDUCATION[DATA06$eisced==1 | DATA06$eisced==2] <- "1-Lower"
DATA06$EDUCATION[DATA06$eisced==3 | DATA06$eisced==4] <- "2-Upper"
DATA06$EDUCATION[DATA06$eisced==5 | DATA06$eisced==6 | DATA06$eisced==7] <- "3-Advanced"
DATA06$EDUCATION <- factor(DATA06$EDUCATION)

# Sample statistics for EDUCATION by COHORT
crosstab(DATA06$EDUCATION, DATA06$COHORT, prop.c = T)

#### Step 4: Analysis ####
# Analysis: Survival Function 
# survival function of first marriage group by 'COHORT'
SURVIVAL01 <- survfit(Surv(DATA06$TIME, DATA06$EVENT) ~ COHORT, data = DATA06)

ggsurvplot(SURVIVAL01, data = DATA06, surv.median.line = "hv", 
           # limited x axis to 0-40
           xlim = c(0, 40),
           xlab = "Age", ylab = "Survival Probability", 
           title = "First Marriage")

# median values of first marriage group by 'COHORT'
surv_median(SURVIVAL01)
# Log-rank test
survdiff(Surv(DATA06$TIME, DATA06$EVENT) ~ COHORT, data = DATA06)

# survival function of first marriage group by 'EDUCATION'
SURVIVAL02 <- survfit(Surv(DATA06$TIME, DATA06$EVENT) ~ EDUCATION, data = DATA06)

ggsurvplot(SURVIVAL02, data = DATA06, surv.median.line = "hv", 
           # limited x axis to 0-40
           xlim = c(0, 40),
           xlab = "Age", ylab = "Survival Probability", 
           title = "First Marriage")

# median values of first marriage group by 'COHORT'
surv_median(SURVIVAL02)
# Log-rank test
survdiff(Surv(DATA06$TIME, DATA06$EVENT) ~ EDUCATION, data = DATA06)

# survival function of first marriage group by 'COHORT' and 'EDUCATION'
ggsurvplot(
  survfit(Surv(DATA06$TIME, DATA06$EVENT) ~ EDUCATION, data = DATA06),
  data = DATA06,
  surv.median.line = "hv",
  facet.by = "COHORT",
  # limited x axis to 0-40
  xlim = c(0, 40),
  title = "Survival Curves by Cohort and Education"
)

# median values of first marriage group by 'COHORT' and 'EDUCATION'
SURVIVAL03 <- survfit(Surv(DATA06$TIME, DATA06$EVENT) ~ COHORT + EDUCATION, data = DATA06)
surv_median(SURVIVAL03)
# Log-rank test
survdiff(Surv(DATA06$TIME, DATA06$EVENT) ~ COHORT + EDUCATION, data = DATA06)
