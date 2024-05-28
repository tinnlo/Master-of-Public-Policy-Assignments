library(haven)
library(descr) 
library(table1)
library(gtsummary) 
library(ggplot2) 
library(plm)


#### Step 1: Load data ####
rm(list=ls())

# easySHARE data
DATA <- read_dta("assets/easySHARE_rel8-0-0.dta")

# DATA01: Reduce the sample to Austria, Germany, Sweden, Netherlands, 
# Spain, Italy, France, Denmark, Greece, Switzerland, Belgium 
DATA01 <- subset(DATA,DATA$country>10 & DATA$country<25)
# Reduce the sample to respondents aged 50-79
DATA02 <- subset(DATA01,DATA01$age>=50 & DATA01$age<=79)
# Reduce the sample to cases with valid information on smoking status 
DATA03 <- subset(DATA02,DATA02$smoking==1 | DATA02$smoking==5)  # 1=Yes, 5=No, wave 3 has no information on smoking
# Reduce the sample to cases with valid information on the dependent variable: Depression Index
DATA04 <- subset(DATA03, DATA03$eurod>=0) 
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
