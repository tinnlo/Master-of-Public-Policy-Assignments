library(haven)
library(descr)
library(stargazer)
library(table1)
library(gtsummary)
library(ggplot2) 
library(plm)


#### Step 1: data preparation ####
rm(list=ls())
#Read in data: pairfam | Germany | 2008-2018
DATA01 <- read_dta("assets/pairfam_Englishlabels.dta") 
# 103172 obs. 

# select variables of interest: id, wave, Number of all biological kids, Gender, Age, year of birth, Cohort, Migration status,
DATA02 <- subset(DATA01, select = c(id,wave, nkidsbio, SEX, age, doby_gen, cohort, migstatus,
                                    # General satisfaction with life, Self-assessment: Satisfaction with relationship, Satisfaction with family, 
                                    sat6, sat3, sat1i4,
                                    # balance between work and private life, Satisfaction with household's financial situation, 
                                    sat7, inc28,
                                    # Household net income, Net equivalence income (OECD), Monthly net household income,
                                    hhincnet, hhincoecd, inc13,
                                    # Highest level of education attained, ISCED, Parter's ISCED,
                                    sd27, isced, pisced, 
                                    # Highest vocational degree, Highest school degree,  Socio-Economic Index of Occupational Status
                                    vocat, school, isei,
                                    # Labor force status, Partner's labor force status, Current occupational status
                                    lfs, plfs, job2))

# drop DATA01 to save memory
rm(DATA01)

# filter the observations of the interest

# Age between 20-50 who could have children
DATA03 <- subset(DATA02, DATA02$age >= 20 & DATA02$age < 50)
# select respondents who are not in education
DATA03 <- subset(DATA03, DATA03$isced > 0)     # ISCED (Education): 0 currently enrolled
DATA03 <- subset(DATA03, DATA03$lfs > 0)       # Labor force status: 0 Education
DATA03 <- subset(DATA03, DATA03$cohort < 4)    # Cohort: drop the cohort 4: 2001-2003
# Explanatory Variables:
DATA03 <- subset(DATA03, !is.na(DATA03$SEX))   # Gender
DATA03 <- subset(DATA03, DATA03$nkidsbio >= 0) # Number of all biological kids
DATA03 <- subset(DATA03, DATA03$hhincoecd > 0) # Net equivalence income (OECD)
# Outcome Variable: 
DATA03 <- subset(DATA03, DATA03$sat6 >= 0)     # General satisfaction with life
# 62046 obs. 

# calculate the number of the respondents by distinct "id"
length(unique(DATA03$id))
# 12313 respondents


#### Step 2: Generate New Variables ####

#Var: Number of Children
DATA03$KIDS <- NA
DATA03$KIDS[DATA03$nkidsbio==0] <- "0-No children"
DATA03$KIDS[DATA03$nkidsbio==1] <- "1-1 child"
DATA03$KIDS[DATA03$nkidsbio==2] <- "2-2 children"
DATA03$KIDS[DATA03$nkidsbio>=3] <- "3-3 or more children"
DATA03$KIDS <- as.factor(DATA03$KIDS)

#Var: Satisfaction with life
DATA03$SAT <- as.numeric(DATA03$sat6)

#Var: Gender
DATA03$GENDER<- NA
DATA03$GENDER[DATA03$SEX==2]<-"1-Female" 
DATA03$GENDER[DATA03$SEX==1]<-"2-Male" 
DATA03$GENDER <- as.factor(DATA03$GENDER)

#Var: Cohort
DATA03$COHORT <- NA
DATA03$COHORT[DATA03$cohort==1] <- "1-1991-1993"
DATA03$COHORT[DATA03$cohort==2] <- "2-1981-1983"
DATA03$COHORT[DATA03$cohort==3] <- "3-1971-1973"
DATA03$COHORT <- as.factor(DATA03$COHORT)

#Var: ID & WAVE 
DATA03$ID   <- as.factor(DATA03$id) 
DATA03$WAVE <- as.factor(DATA03$wave) 

#Var: Continuous variables 
DATA03$AGE   <- as.numeric(DATA03$age) 
DATA03$OECDINC <- as.numeric(DATA03$hhincoecd)

#Var: International Standard Classification of Education (ISCED)
DATA03$EDU <- NA
DATA03$EDU[DATA03$isced==1] <- "1-Lower Secondary or below"
DATA03$EDU[DATA03$isced==2] <- "1-Lower Secondary or below"
DATA03$EDU[DATA03$isced==3] <- "1-Lower Secondary or below"
DATA03$EDU[DATA03$isced==4] <- "2-Upper and Post Secondary"
DATA03$EDU[DATA03$isced==5] <- "2-Upper and Post Secondary"
DATA03$EDU[DATA03$isced==6] <- "2-Upper and Post Secondary"
DATA03$EDU[DATA03$isced==7] <- "3-Tertiary Education"
DATA03$EDU[DATA03$isced==8] <- "3-Tertiary Education"
DATA03$EDU <- as.factor(DATA03$EDU)

#Var: Labor Force Status
DATA03$JOB <- NA
DATA03$JOB[DATA03$lfs==2] <- "2-Non-Working"
DATA03$JOB[DATA03$lfs==3] <- "2-Non-Working"
DATA03$JOB[DATA03$lfs==4] <- "2-Non-Working"
DATA03$JOB[DATA03$lfs==5] <- "2-Non-Working"
DATA03$JOB[DATA03$lfs==6] <- "2-Non-Working"
DATA03$JOB[DATA03$lfs==7] <- "2-Non-Working"
DATA03$JOB[DATA03$lfs==8]  <- "1-Working"
DATA03$JOB[DATA03$lfs==9]  <- "1-Working"
DATA03$JOB[DATA03$lfs==10] <- "1-Working"
DATA03$JOB[DATA03$lfs==11] <- "1-Working"
DATA03$JOB[DATA03$lfs==12] <- "1-Working"
DATA03$JOB[DATA03$lfs==13] <- "1-Working"
DATA03$JOB <- as.factor(DATA03$JOB)
# choose yes as reference
DATA03$JOB <- relevel(DATA03$JOB, ref = "1-Working")

# Sample Statistics
table1::label(DATA03$SAT)  <- "Satisfaction with life"
table1::label(DATA03$AGE)  <- "Age"
table1::label(DATA03$GENDER)  <- "Gender"
table1::label(DATA03$COHORT)  <- "Cohort"
table1::label(DATA03$JOB)     <- "Labor Force Status"
table1::label(DATA03$OECDINC) <- "Net equivalence income (OECD)"

table1::table1(~ SAT + KIDS + AGE + COHORT + GENDER + EDU + JOB + OECDINC | KIDS, data = DATA03)
mean(DATA03$SAT)       # 7.49 as the average of life satisfaction
mean(DATA03$nkidsbio)  # 1.12 children per respondent as the average

#### Step 3: analysis of the effect of having children on life satisfaction ####

TABLE1 <- table(DATA03$KIDS,DATA03$SAT)
TABLE2 <- prop.table(TABLE1,1)
TABLE3 <- as.data.frame(TABLE2) 
TABLE3$Percent <- TABLE3$Freq*100

# Plot the results
ggplot(TABLE3, aes(fill=Var1, x=Var2, y=Percent)) + 
  geom_bar(stat="identity", position=position_dodge(width=1)) + 
  ylab("%") + 
  xlab("Life Satisfaction") + 
  scale_fill_manual(name = "Number of Children", 
                    labels = c("No children", "1 child", "2 children", "3 or more children"), 
                    values = c("grey", "skyblue1", "skyblue2", "skyblue3"))

# Regression analysis
# AGE * 10 for better interpretation
DATA03$AGE10 <- DATA03$AGE/10
DATA03$OECDINC1000 <- DATA03$OECDINC/1000
OLS1 <- lm(SAT ~ KIDS + GENDER + AGE10 + COHORT + EDU + JOB + OECDINC1000 + WAVE, data=DATA03)

# Generate the Fixed Effects model
FE1 <- plm(SAT ~ KIDS + WAVE, data = DATA03, index = c("ID", "WAVE"), model = "within")

# View the summary of the Fixed Effects model
OUTPUT_OLS <- tbl_regression(OLS1)
OUTPUT_FE <- tbl_regression(FE1)
tbl_merge( tbls = list(OUTPUT_OLS, OUTPUT_FE), 
           tab_spanner = c("OLS", "Fixed Effects"))


# split the sample by GENDER
DATA_F <- subset(DATA03,DATA03$GENDER=="1-Female")
DATA_M <- subset(DATA03,DATA03$GENDER=="2-Male")

FE_F <- plm(SAT ~ KIDS+WAVE, data=DATA_F, index=c("ID","WAVE"), model = "within")
FE_M <- plm(SAT ~ KIDS+WAVE, data=DATA_M, index=c("ID","WAVE"), model = "within")

OUTPUT1 <- tbl_regression(FE_F)
OUTPUT2 <- tbl_regression(FE_M)
tbl_merge( tbls = list(OUTPUT1, OUTPUT2), 
           tab_spanner = c("Female", "Male"))

# split the sample by EDU
DATA_E1 <- subset(DATA03,DATA03$EDU=="1-Lower Secondary or below")
DATA_E2 <- subset(DATA03,DATA03$EDU=="2-Upper and Post Secondary")
DATA_E3 <- subset(DATA03,DATA03$EDU=="3-Tertiary Education")

FE_E1 <- plm(SAT ~ KIDS+WAVE, data=DATA_E1, index=c("ID","WAVE"), model = "within")
FE_E2 <- plm(SAT ~ KIDS+WAVE, data=DATA_E2, index=c("ID","WAVE"), model = "within")
FE_E3 <- plm(SAT ~ KIDS+WAVE, data=DATA_E3, index=c("ID","WAVE"), model = "within")

OUTPUT1 <- tbl_regression(FE_E1)
OUTPUT2 <- tbl_regression(FE_E2)
OUTPUT3 <- tbl_regression(FE_E3)
tbl_merge( tbls = list(OUTPUT1, OUTPUT2, OUTPUT3), 
           tab_spanner = c("Lower Secondary or below", "Upper and Post Secondary", "Tertiary Education"))

# split the sample by working status
DATA_J1 <- subset(DATA03,DATA03$JOB=="1-Working")
DATA_J2 <- subset(DATA03,DATA03$JOB=="2-Non-Working")

FE_J1 <- plm(SAT ~ KIDS+WAVE, data=DATA_J1, index=c("ID","WAVE"), model = "within")
FE_J2 <- plm(SAT ~ KIDS+WAVE, data=DATA_J2, index=c("ID","WAVE"), model = "within")

OUTPUT1 <- tbl_regression(FE_J1)
OUTPUT2 <- tbl_regression(FE_J2)
tbl_merge( tbls = list(OUTPUT1, OUTPUT2), 
           tab_spanner = c("Working", "Non-Working"))

# split the sample by OECDINC
DATA_I1 <- subset(DATA03,DATA03$OECDINC<1000)
DATA_I2 <- subset(DATA03,DATA03$OECDINC>=1000 & DATA03$OECDINC<2000)
DATA_I3 <- subset(DATA03,DATA03$OECDINC>=2000)

FE_I1 <- plm(SAT ~ KIDS+WAVE, data=DATA_I1, index=c("ID","WAVE"), model = "within")
FE_I2 <- plm(SAT ~ KIDS+WAVE, data=DATA_I2, index=c("ID","WAVE"), model = "within")
FE_I3 <- plm(SAT ~ KIDS+WAVE, data=DATA_I3, index=c("ID","WAVE"), model = "within")

OUTPUT1 <- tbl_regression(FE_I1)
OUTPUT2 <- tbl_regression(FE_I2)
OUTPUT3 <- tbl_regression(FE_I3)
tbl_merge( tbls = list(OUTPUT1, OUTPUT2, OUTPUT3), 
           tab_spanner = c("OECD Income < 1000", 
                           "OECD Income 1000-2000", 
                           "OECD Income > 2000"))
