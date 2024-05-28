library(haven)
library(descr)
library(stargazer)
library(sjPlot)
library(table1)
library(gtsummary)
library(ggplot2) 
library(plm)


#### Step 1: data preparation ####
rm(list=ls())

#Read in data: pairfam | Germany | 2008-2018
DATA01 <- read_dta("Exericse07.dta") 
# 103172 obs.


# filter the observations of the interest

# Age between 20-50 who could have children
DATA02 <- subset(DATA01, DATA01$TIME_KID >= -2 & DATA01$TIME_KID <= 2)
DATA02 <- subset(DATA02, !is.na(DATA02$TREAT_KID))
# delete the observations with missing values
DATA02 <- subset(DATA02, !is.na(DATA02$inc28)) # Satisfaction with household's financial situation

# Filter the missing value of Explanatory Variables: COLLEGE, migstqtus, EAST and SEX
DATA02 <- subset(DATA02, !is.na(DATA02$COLLEGE)) # Highest level of education attained
DATA02 <- subset(DATA02, !is.na(DATA02$migstatus)) # Migration status
DATA02 <- subset(DATA02, !is.na(DATA02$EAST)) # East Germany
DATA02 <- subset(DATA02, !is.na(DATA02$SEX))   # Gender
# 5711 obs. 

# calculate the number of the respondents by distinct "id"
length(unique(DATA02$id))
# 1537 respondents


#### Step 2: Generate New Variables ####


#Var: Satisfaction with life
DATA02$FINSAT <- as.numeric(DATA02$inc28)

#Var: Gender
DATA02$GENDER<- NA
DATA02$GENDER[DATA02$SEX==2]<-"1-Female" 
DATA02$GENDER[DATA02$SEX==1]<-"2-Male" 
DATA02$GENDER <- as.factor(DATA02$GENDER)

#Var: ID & WAVE 
DATA02$ID   <- as.factor(DATA02$id) 

#Var: Continuous variables 
DATA02$AGE   <- as.numeric(DATA02$age) 


#Var: COLLEGE
DATA02$EDU <- NA
DATA02$EDU[DATA02$COLLEGE==1] <- "1-Tertiary Degree"
DATA02$EDU[DATA02$COLLEGE==0] <- "2-No Tertiary Degree"
DATA02$EDU <- as.factor(DATA02$EDU)

#Var: Migration status
DATA02$MIGRATION <- NA
DATA02$MIGRATION[DATA02$migstatus==1] <- "1-Non-Migrant"
DATA02$MIGRATION[DATA02$migstatus==2] <- "2-Migrant"
DATA02$MIGRATION[DATA02$migstatus==3] <- "2-Migrant"
DATA02$MIGRATION <- as.factor(DATA02$MIGRATION)

#Var: EAST
DATA02$GE <- NA
DATA02$GE[DATA02$EAST==1] <- "1-East Germany"
DATA02$GE[DATA02$EAST==0] <- "2-West Germany"
DATA02$GE <- as.factor(DATA02$GE)

# Sample Statistics
table1::label(DATA02$FINSAT)  <- "Satisfaction with Financial Situation"
table1::label(DATA02$AGE)  <- "Age"
table1::label(DATA02$GENDER)  <- "Gender"
table1::label(DATA02$IMMIGRANT) <- "Immigrant Backgroud"
table1::label(DATA02$GE) <- "East or West"

table1::table1(~ FINSAT + GENDER + AGE + GE + EDU + MIGRATION, data = DATA02)


#### Step 3: analysis of the effect of having children on life satisfaction ####

MODEL01<-lm(FINSAT ~ as.factor(TREAT_KID), data=DATA02) 
MODEL02<-lm(FINSAT ~ as.factor(TREAT_KID)+GE+AGE+EDU+MIGRATION,data=DATA02) 
plot_model(MODEL01, type = "pred", terms = c("TREAT_KID")) 
plot_model(MODEL02, type = "pred", terms = c("TREAT_KID"))

# Generate the Fixed Effects model
library(plm) 

# Using Fixed Assets Model
DATA02 <- DATA02 %>%
          filter(!is.na(TREAT_KID, TIME_KID))
MODEL03 <- plm(inc28 ~ as.factor(TREAT_KID) + TIME_KID, 
               data = DATA02, 
               index = c("ID","TIME_KID"),
               model = "within") 
summary(MODEL03)
tbl_regression(MODEL03)

stargazer(MODEL03,MODEL04,type="text")



FE1 <- plm(SAT ~ KIDS + WAVE, data = DATA02, index = c("ID", "WAVE"), model = "within")

# View the summary of the Fixed Effects model
stargazer(OLS1, FE1, type = "text")

OUTPUT_OLS <- tbl_regression(OLS1)
OUTPUT_FE <- tbl_regression(FE1)
tbl_merge( tbls = list(OUTPUT_OLS, OUTPUT_FE), 
           tab_spanner = c("OLS", "Fixed Effects"))


# split the sample by GENDER
DATA_F <- subset(DATA02,DATA02$GENDER=="1-Female")
DATA_M <- subset(DATA02,DATA02$GENDER=="2-Male")

FE_F <- plm(SAT ~ KIDS+WAVE, data=DATA_F, index=c("ID","WAVE"), model = "within")
FE_M <- plm(SAT ~ KIDS+WAVE, data=DATA_M, index=c("ID","WAVE"), model = "within")

stargazer(FE_F, FE_M, type = "text")

OUTPUT1 <- tbl_regression(FE_F)
OUTPUT2 <- tbl_regression(FE_M)
tbl_merge( tbls = list(OUTPUT1, OUTPUT2), 
           tab_spanner = c("Female", "Male"))

# split the sample by EDU
DATA_E1 <- subset(DATA02,DATA02$EDU=="1-Lower Secondary or below")
DATA_E2 <- subset(DATA02,DATA02$EDU=="2-Upper and Post Secondary")
DATA_E3 <- subset(DATA02,DATA02$EDU=="3-Tertiary Education")

FE_E1 <- plm(SAT ~ KIDS+WAVE, data=DATA_E1, index=c("ID","WAVE"), model = "within")
FE_E2 <- plm(SAT ~ KIDS+WAVE, data=DATA_E2, index=c("ID","WAVE"), model = "within")
FE_E3 <- plm(SAT ~ KIDS+WAVE, data=DATA_E3, index=c("ID","WAVE"), model = "within")

stargazer(FE_E1, FE_E2, FE_E3, type = "text")

OUTPUT1 <- tbl_regression(FE_E1)
OUTPUT2 <- tbl_regression(FE_E2)
OUTPUT3 <- tbl_regression(FE_E3)
tbl_merge( tbls = list(OUTPUT1, OUTPUT2, OUTPUT3), 
           tab_spanner = c("Lower Secondary or below", "Upper and Post Secondary", "Tertiary Education"))

# split the sample by working status
DATA_J1 <- subset(DATA02,DATA02$JOB=="1-Working")
DATA_J2 <- subset(DATA02,DATA02$JOB=="2-Non-Working")

FE_J1 <- plm(SAT ~ KIDS+WAVE, data=DATA_J1, index=c("ID","WAVE"), model = "within")
FE_J2 <- plm(SAT ~ KIDS+WAVE, data=DATA_J2, index=c("ID","WAVE"), model = "within")

stargazer(FE_J1, FE_J2, type = "text")

OUTPUT1 <- tbl_regression(FE_J1)
OUTPUT2 <- tbl_regression(FE_J2)
tbl_merge( tbls = list(OUTPUT1, OUTPUT2), 
           tab_spanner = c("Working", "Non-Working"))

# split the sample by OECDINC
DATA_I1 <- subset(DATA02,DATA02$OECDINC<1000)
DATA_I2 <- subset(DATA02,DATA02$OECDINC>=1000 & DATA02$OECDINC<2000)
DATA_I3 <- subset(DATA02,DATA02$OECDINC>=2000)

FE_I1 <- plm(SAT ~ KIDS+WAVE, data=DATA_I1, index=c("ID","WAVE"), model = "within")
FE_I2 <- plm(SAT ~ KIDS+WAVE, data=DATA_I2, index=c("ID","WAVE"), model = "within")
FE_I3 <- plm(SAT ~ KIDS+WAVE, data=DATA_I3, index=c("ID","WAVE"), model = "within")

stargazer(FE_I1, FE_I2, FE_I3, type = "text")

OUTPUT1 <- tbl_regression(FE_I1)
OUTPUT2 <- tbl_regression(FE_I2)
OUTPUT3 <- tbl_regression(FE_I3)
tbl_merge( tbls = list(OUTPUT1, OUTPUT2, OUTPUT3), 
           tab_spanner = c("OECD Income < 1000", 
                           "OECD Income 1000-2000", 
                           "OECD Income > 2000"))

# # split the sample by COHORT
# DATA_C1 <- subset(DATA02,DATA02$COHORT=="1-1991-1993")
# DATA_C2 <- subset(DATA02,DATA02$COHORT=="2-1981-1983")
# DATA_C3 <- subset(DATA02,DATA02$COHORT=="3-1971-1973")
# 
# FE_C1 <- plm(SAT ~ KIDS+WAVE, data=DATA_C1, index=c("ID","WAVE"), model = "within")
# FE_C2 <- plm(SAT ~ KIDS+WAVE, data=DATA_C2, index=c("ID","WAVE"), model = "within")
# FE_C3 <- plm(SAT ~ KIDS+WAVE, data=DATA_C3, index=c("ID","WAVE"), model = "within")
# 
# stargazer(FE_C1, FE_C2, FE_C3, type = "text")
# 
# OUTPUT1 <- tbl_regression(FE_C1)
# OUTPUT2 <- tbl_regression(FE_C2)
# OUTPUT3 <- tbl_regression(FE_C3)
# tbl_merge( tbls = list(OUTPUT1, OUTPUT2, OUTPUT3), 
#            tab_spanner = c("1991-1993", "1981-1983", "1971-1973"))
#Read in data: pairfam | Germany | 2008-2018
DATA01 <- read_dta("assets/pairfam_Englishlabels.dta") 


# easySHARE data
DATA <- read_dta("assets/easySHARE_rel8-0-0.dta")

# DATA01: Reduce the sample to Austria, Germany, Sweden, Netherlands, 
# Spain, Italy, France, Denmark, Greece, Switzerland, Belgium 
DATA01 <- subset(DATA,DATA$country>10 & DATA$country<25)
# Reduce the sample to respondents aged 50-79
DATA02 <- subset(DATA01,DATA01$age>=50 & DATA01$age<=79)
# Reduce the sample to cases with valid information on smoking status 
DATA02 <- subset(DATA02,DATA02$smoking==1 | DATA02$smoking==5)  # 1=Yes, 5=No, wave 3 has no information on smoking
# Reduce the sample to cases with valid information on the dependent variable: Depression Index
DATA04 <- subset(DATA02, DATA02$eurod>=0) 
# Reduce the sample to cases with valid information on the Sports Paticipation 
DATA05 <- subset(DATA04, DATA04$br015_>0)
# Reduce the sample to cases with valid information on the Job Situation
DATA06 <- subset(DATA05, DATA05$ep005_>0 & DATA05$ep005_<10)

#### Step 2: Generate New Variables ####

#Var: Smoking
DATA06$SMOKING <- NA
DATA06$SMOKING[DATA06$smoking==1] <- "1-Yes"
DATA06$SMOKING[DATA06$smoking==5] <- "2-No"
DATA06$SMOKING <- as.factor(DATA06$SMOKING)
# choose yes as reference
DATA06$SMOKING <- relevel(DATA06$SMOKING, ref = "2-No")

#Var: Id & WAVE 
DATA06$ID   <- as.factor(DATA06$mergeid) 
DATA06$WAVE <- as.factor(DATA06$wave) 

#Var: Continuous variables 
DATA06$EUROD <- as.numeric(DATA06$eurod) 
DATA06$AGE   <- as.numeric(DATA06$age) 

#Var: Gender 
DATA06$GENDER<- "NA"
DATA06$GENDER[DATA06$female==1]<-"1-Female" 
DATA06$GENDER[DATA06$female==0]<-"2-Male" 
DATA06$GENDER<-as.factor(DATA06$GENDER)

#Var: Country 
DATA06$COUNTRY<-"NA" 
DATA06$COUNTRY[DATA06$country==11]<-"Austria" 
DATA06$COUNTRY[DATA06$country==12]<-"Germany" 
DATA06$COUNTRY[DATA06$country==13]<-"Sweden" 
DATA06$COUNTRY[DATA06$country==14]<-"Netherlands" 
DATA06$COUNTRY[DATA06$country==15]<-"Spain" 
DATA06$COUNTRY[DATA06$country==16]<-"Italy" 
DATA06$COUNTRY[DATA06$country==17]<-"France" 
DATA06$COUNTRY[DATA06$country==18]<-"Denmark" 
DATA06$COUNTRY[DATA06$country==19]<-"Greece" 
DATA06$COUNTRY[DATA06$country==20]<-"Switzerland" 
DATA06$COUNTRY[DATA06$country==23]<-"Belgium" 
DATA06$COUNTRY<-as.factor(DATA06$COUNTRY)

#Var: Sports Participation
DATA06$SPORTS <- 'NA'
DATA06$SPORTS[DATA06$br015_==1] <- "1-More than once per week"
DATA06$SPORTS[DATA06$br015_==2] <- "2-One to Four times a month"
DATA06$SPORTS[DATA06$br015_==3] <- "2-One to Four times a month"
DATA06$SPORTS[DATA06$br015_==4] <- "3-Hardly ever"
DATA06$SPORTS <- as.factor(DATA06$SPORTS)

#Var: Job Situation
DATA06$JOB <- 'NA'
DATA06$JOB[DATA06$ep005_==1] <- "2-Retired"
DATA06$JOB[DATA06$ep005_==2] <- "1-Employed"
DATA06$JOB[DATA06$ep005_==3] <- "3-Unemployed"
DATA06$JOB[DATA06$ep005_==4] <- "3-Unemployed"
DATA06$JOB[DATA06$ep005_==5] <- "3-Unemployed"
DATA06$JOB <- as.factor(DATA06$JOB)
# choose yes as reference
DATA06$JOB <- relevel(DATA06$JOB, ref = "1-Employed")

# Sample Statistics
table1::label(DATA06$EUROD) <- "DEPRESSION"
table1::label(DATA06$SMOKING) <- "SMOKING"
table1::label(DATA06$GENDER) <- "Gender" 
table1::label(DATA06$AGE) <- "Age of respondent" 
table1::label(DATA06$SPORTS) <- "Sports Frequency"
table1::label(DATA06$JOB) <- "Job Situation"

table1::table1(~ EUROD + SMOKING + GENDER + AGE | SMOKING, data = DATA06)

#### Step 3: analysis of the effect of smoking on depression ####

TABLE1 <- table(DATA06$SMOKING,DATA06$EUROD)
TABLE2 <- prop.table(TABLE1,1)
TABLE3 <- as.data.frame(TABLE2) 
TABLE3$Percent <- TABLE3$Freq*100
  
ggplot(TABLE3, aes(fill=Var1, x=Var2, y=Percent)) + 
  geom_bar(stat="identity", position=position_dodge(width=1)) + 
  ylab("%") + 
  xlab("Depression (EURO_D)") + 
  scale_fill_manual(name = "Smoking status", labels = c("No smoking", "Smoking"), values = c("blue1", "skyblue1"))

# Regression analysis
# AGE * 100 for better interpretation
DATA06$AGE100 <- DATA06$AGE/100
OLS1 <- lm(EUROD ~ SMOKING+GENDER+AGE100+SPORTS+JOB+COUNTRY+WAVE, data=DATA06)

# Fixed Effects on
FE1 <- plm(EUROD ~ SMOKING+WAVE, data=DATA06, index=c("ID","WAVE"), model = "within")

OUTPUT_OLS <- tbl_regression(OLS1)
OUTPUT_FE <- tbl_regression(FE1)
tbl_merge( tbls = list(OUTPUT_OLS, OUTPUT_FE), 
           tab_spanner = c("OLS", "Fixed Effects"))

# split the sample by GENDER
DATA_F <- subset(DATA06,DATA06$GENDER=="1-Female")
DATA_M <- subset(DATA06,DATA06$GENDER=="2-Male")

FE_F <- plm(EUROD ~ SMOKING+WAVE, data=DATA_F, index=c("ID","WAVE"), model = "within")
FE_M <- plm(EUROD ~ SMOKING+WAVE, data=DATA_M, index=c("ID","WAVE"), model = "within")

OUTPUT1 <- tbl_regression(FE_F)
OUTPUT2 <- tbl_regression(FE_M)
tbl_merge( tbls = list(OUTPUT1, OUTPUT2), 
           tab_spanner = c("Female", "Male"))
