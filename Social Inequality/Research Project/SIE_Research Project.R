library(haven)
library(descr)
library(stargazer)
library(table1)
library(gtsummary)
library(ggplot2)


#### Step 1: data preparation ####
rm(list=ls())
#Read in data 
DATA01 <- read_dta("assets/HUNGARY_SWEDEN.dta") 
#10636 obs. HUN & SWE

# select variables of interest: Country, Gender, Age, Birth Year, Immigration, language speak at home,
DATA02 <- DATA01[,c("CNT","ST004D01T","AGE","ST003D03T", "IMMIG", "ST022Q01TA",      
                    #Math, Text Structure, Reading, Science, 
                    "PV1MATH","PV1RTML","PV1READ","PV1SCIE",
                    #Socio-Economic Status Index, Expected ISEI, Parent's Highest ISEI, Mother's ISEI, Father's ISEI, 
                    "ESCS","BSMJ","HISEI","BMMJ1","BFMJ2",
                    #Policies for Parental Involvement, Emotional Support, Home Possessions, Number of Books
                    "PASCHPOL", "EMOSUPS", "HOMEPOS","ST013Q01TA",  
                    #Parent's Education in Year, Parents Highest Education, Mother's HiEdu, Father's HiEdu.
                    "PARED","HISCED","MISCED","FISCED")]

# filter variables having missing values
DATA02 <- subset(DATA02, !is.na(DATA02$PV1MATH)) # Math Scores
DATA02 <- subset(DATA02, !is.na(DATA02$PV1RTML)) # Text Structure Scores
DATA02 <- subset(DATA02, !is.na(DATA02$MISCED))  # Mother's Education
DATA02 <- subset(DATA02, !is.na(DATA02$FISCED))  # Father's Education
DATA02 <- subset(DATA02, !is.na(DATA02$ST013Q01TA))  # Number of Books
DATA02 <- subset(DATA02, !is.na(DATA02$EMOSUPS))     # Parental Emotional Support
DATA02 <- subset(DATA02, !is.na(DATA02$IMMIG))       # Migration Background
DATA02 <- subset(DATA02, !is.na(DATA02$ST022Q01TA))  # Language spoken at home
# 8847 obs. HUN & SWE


#### Step 2: variables construction ####

# Students' Characteristics
#VAR: Gender
DATA02$GENDER<-NA
DATA02$GENDER[DATA02$ST004D01T==2]<-"1-Male"
DATA02$GENDER[DATA02$ST004D01T==1]<-"2-Female"
DATA02$GENDER<-as.factor(DATA02$GENDER)

#VAR: Age 
DATA02$AGE <- as.numeric(DATA02$AGE)

#VAR: Age by Year
DATA02$AGEBY <- as.factor(2018- as.numeric((DATA02$ST003D03T)))

# Educational Outcomes:
#VAR: Math Scores
DATA02$MATH <- as.numeric(DATA02$PV1MATH)

#VAR: Reading Scores
DATA02$TS <- as.numeric(DATA02$PV1RTML)

# Students' family background

# Parens' Education

#VAR: Highest Parental Education
DATA02$HISCEDC <- NA
DATA02$HISCEDC[DATA02$HISCED==0] <- "1-Secondary or below"
DATA02$HISCEDC[DATA02$HISCED==1] <- "1-Secondary or below"
DATA02$HISCEDC[DATA02$HISCED==2] <- "1-Secondary or below"
DATA02$HISCEDC[DATA02$HISCED==3] <- "1-Secondary or below"
DATA02$HISCEDC[DATA02$HISCED==4] <- "2-Post-Secondary"
DATA02$HISCEDC[DATA02$HISCED==5] <- "2-Post-Secondary"
DATA02$HISCEDC[DATA02$HISCED==6] <- "3-Tertiary Education"
DATA02$HISCEDC <- as.factor(DATA02$HISCEDC)

#VAR: Mother's Education
DATA02$MISCEDC <- NA
DATA02$MISCEDC[DATA02$MISCED==0] <- "1-Secondary or below"
DATA02$MISCEDC[DATA02$MISCED==1] <- "1-Secondary or below"
DATA02$MISCEDC[DATA02$MISCED==2] <- "1-Secondary or below"
DATA02$MISCEDC[DATA02$MISCED==3] <- "1-Secondary or below"
DATA02$MISCEDC[DATA02$MISCED==4] <- "2-Post-Secondary"
DATA02$MISCEDC[DATA02$MISCED==5] <- "2-Post-Secondary"
DATA02$MISCEDC[DATA02$MISCED==6] <- "3-Tertiary Education"
DATA02$MISCEDC <- as.factor(DATA02$MISCEDC)

#VAR: Father's Education
DATA02$FISCEDC <- NA
DATA02$FISCEDC[DATA02$FISCED==0] <- "1-Secondary or below"
DATA02$FISCEDC[DATA02$FISCED==1] <- "1-Secondary or below"
DATA02$FISCEDC[DATA02$FISCED==2] <- "1-Secondary or below"
DATA02$FISCEDC[DATA02$FISCED==3] <- "1-Secondary or below"
DATA02$FISCEDC[DATA02$FISCED==4] <- "2-Post-Secondary"
DATA02$FISCEDC[DATA02$FISCED==5] <- "2-Post-Secondary"
DATA02$FISCEDC[DATA02$FISCED==6] <- "3-Tertiary Education"
DATA02$FISCEDC <- as.factor(DATA02$FISCEDC)

#VAR: Number of Books
DATA02$BOOKSG <- NA
DATA02$BOOKSG[DATA02$ST013Q01TA==1] <- "1.0-10"
DATA02$BOOKSG[DATA02$ST013Q01TA==2] <- "2.11-25"
DATA02$BOOKSG[DATA02$ST013Q01TA==3] <- "3.26-100"
DATA02$BOOKSG[DATA02$ST013Q01TA==4] <- "4.101-200"
DATA02$BOOKSG[DATA02$ST013Q01TA==5] <- "5.201-500"
DATA02$BOOKSG[DATA02$ST013Q01TA==6] <- "6.500+"
DATA02$BOOKSG <- as.factor(DATA02$BOOKSG)

#VAR: Parental Emotional Support
DATA02$EMOSUPS <- as.numeric(DATA02$EMOSUPS)

#VAR: Migration Background 
DATA02$MIG<-NA
DATA02$MIG[DATA02$IMMIG==1] <-"Native" 
DATA02$MIG[DATA02$IMMIG==2] <-"Immigrant" 
DATA02$MIG[DATA02$IMMIG==3] <-"Immigrant"
DATA02$MIG <-as.factor(DATA02$MIG) 
DATA02$MIG <-relevel(DATA02$MIG, ref="Native") #Change reference category

#VAR: Language spoken at home
DATA02$FLANG <- NA
DATA02$FLANG[DATA02$ST022Q01TA==1] <- "1-Language of the test"
DATA02$FLANG[DATA02$ST022Q01TA==2] <- "2-Other language"
DATA02$FLANG <- as.factor(DATA02$FLANG)

# Sample Statistics
table1::label(DATA02$MISCEDC) <- "Mother's Education"
table1::label(DATA02$FISCEDC) <- "Father's Education"
table1::label(DATA02$BOOKSG)  <- "Number of Books"
table1::label(DATA02$EMOSUPS) <- "Parental Emotional Support"
table1::label(DATA02$MIG)     <- "Migration Background"
table1::label(DATA02$FLANG)   <- "Language Spoken at Home"
table1::label(DATA02$CNT)     <- "Country"
table1::label(DATA02$GENDER)  <- "Gender"
table1::label(DATA02$AGEBY)   <- "Age"

# Meternal and Paternal Education Comparison
table1(~ MISCEDC+FISCEDC+EMOSUPS+BOOKSG+MIG+FLANG+CNT+GENDER+AGEBY | CNT, data = DATA02)
# Immigration Perspective
#table1(~ HISCEDC+MIG+FLANG+EMOSUPS+BOOKSG+CNT+GENDER, data = DATA02)

# Distribution of Math Scores, the x-axis is start from 200 to 800
ggplot(DATA02, aes(x = MATH, fill = CNT)) +
  geom_histogram(binwidth = 50, position = "dodge") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Math Scores by Country", x = "Math Scores", y = "Count", fill = "Country") +
  xlim(200, 800) + ylim(0, 1000)

# Distribution of Reading Scores
ggplot(DATA02, aes(x = TS, fill = CNT)) +
  geom_histogram(binwidth = 50, position = "dodge") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Reading Scores by Country", x = "Reading Scores", y = "Count", fill = "Country") +
  xlim(200, 800) + ylim(0, 1000)

# Mother's Education Distribution
ggplot(DATA02, aes(x = MISCEDC, fill = CNT)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Mother's Education by Country", x = "Mother's Education", y = "Count") +
  ylim(0, 2300)
  
# Father's Education Distribution
ggplot(DATA02, aes(x = FISCEDC, fill = CNT)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Father's Education by Country", x = "Father's Education", y = "Count") +
  ylim(0, 2300)

#Split Sub-Dataset by Country
HUN <- subset(DATA02,DATA02$CNT == "HUN")
SWE <- subset(DATA02,DATA02$CNT == "SWE")

#### Step 3: Data Analysis ####

# 1. Linear Regression with whole dataset

# 1.1 Math Scores as Outcome Variable

# Mother's Education, Gender
MODEL01 <- lm(MATH ~ MISCEDC+GENDER, data = DATA02)
# Mother's Education, Emotional Support, Gender
MODEL02 <- lm(MATH ~ MISCEDC+EMOSUPS+GENDER, data = DATA02)
# Mother's Education, Emotional Support, the Number of Books and Gender
MODEL03 <- lm(MATH ~ MISCEDC+EMOSUPS+BOOKSG+GENDER, data = DATA02)

OUTPUT01 <- tbl_regression(MODEL01)
OUTPUT02 <- tbl_regression(MODEL02)
OUTPUT03 <- tbl_regression(MODEL03)
tbl_merge(tbls = list(OUTPUT01, OUTPUT02, OUTPUT03), tab_spanner = c("Model1", "Model2", "Model3"))

# Check Correlation between Variables
corrplot::corrplot(round(cor(DATA02[,c("MISCED","EMOSUPS","ST013Q01TA")], use = "complete.obs"),2))

Model3c <- lm(MATH ~ (MISCEDC+EMOSUPS)*BOOKSG, data = DATA02)
summary(Model3c)
# It shows that the interaction term is not significant, so we can remove it from the model.

# 1.2 Reading Scores as Outcome Variable

MODEL04 <- lm(TS ~  MISCEDC+EMOSUPS+BOOKSG+GENDER, data = DATA02)
stargazer(MODEL03, MODEL04,type="text")

# Compare Math and Reading Scores
OUTPUT04 <- tbl_regression(MODEL04)
tbl_merge(tbls = list(OUTPUT03, OUTPUT04), tab_spanner = c("Math", "Read"))


# 2. Linear Regression with country-specific models
MODEL03_HUN <- lm(MATH ~ MISCEDC+EMOSUPS+BOOKSG+GENDER, data = HUN)
MODEL03_SWE <- lm(MATH ~ MISCEDC+EMOSUPS+BOOKSG+GENDER, data = SWE)
MODEL04_HUN <- lm(TS ~ MISCEDC+EMOSUPS+BOOKSG+GENDER, data = HUN)
MODEL04_SWE <- lm(TS ~ MISCEDC+EMOSUPS+BOOKSG+GENDER, data = SWE)
stargazer(MODEL03_HUN, MODEL04_HUN, MODEL03_SWE, MODEL04_SWE, type="text")

OUTPUT03_HUN <- tbl_regression(MODEL03_HUN)
OUTPUT04_HUN <- tbl_regression(MODEL04_HUN)
OUTPUT03_SWE <- tbl_regression(MODEL03_SWE)
OUTPUT04_SWE <- tbl_regression(MODEL04_SWE)

tbl_merge(tbls = list(OUTPUT03_HUN, OUTPUT04_HUN, OUTPUT03_SWE, OUTPUT04_SWE),
          tab_spanner = c("Hungary Math", "Hungary Read", "Sweden Math", "Sweden Read"))


# 3. Analyze country-specific models with paternal education and maternal education comparison
# Math Scores
MODEL05_HUN <- lm(MATH ~ MISCEDC+EMOSUPS+BOOKSG+GENDER, data = HUN)
MODEL05_SWE <- lm(MATH ~ MISCEDC+EMOSUPS+BOOKSG+GENDER, data = SWE)
MODEL06_HUN <- lm(MATH ~ FISCEDC+EMOSUPS+BOOKSG+GENDER, data = HUN)
MODEL06_SWE <- lm(MATH ~ FISCEDC+EMOSUPS+BOOKSG+GENDER, data = SWE)
# Reading Scores
MODEL07_HUN <- lm(TS ~ MISCEDC+EMOSUPS+BOOKSG+GENDER, data = HUN)
MODEL07_SWE <- lm(TS ~ MISCEDC+EMOSUPS+BOOKSG+GENDER, data = SWE)
MODEL08_HUN <- lm(TS ~ FISCEDC+EMOSUPS+BOOKSG+GENDER, data = HUN)
MODEL08_SWE <- lm(TS ~ FISCEDC+EMOSUPS+BOOKSG+GENDER, data = SWE)

stargazer(MODEL05_HUN, MODEL06_HUN, MODEL07_HUN, MODEL08_HUN,type="text")
stargazer(MODEL05_SWE, MODEL06_SWE, MODEL07_SWE, MODEL08_SWE,type="text")

OUTPUT05_HUN <- tbl_regression(MODEL05_HUN)
OUTPUT05_SWE <- tbl_regression(MODEL05_SWE)
OUTPUT06_HUN <- tbl_regression(MODEL06_HUN)
OUTPUT06_SWE <- tbl_regression(MODEL06_SWE)
OUTPUT07_HUN <- tbl_regression(MODEL07_HUN)
OUTPUT07_SWE <- tbl_regression(MODEL07_SWE)
OUTPUT08_HUN <- tbl_regression(MODEL08_HUN)
OUTPUT08_SWE <- tbl_regression(MODEL08_SWE)

# Math Comparison
tbl_merge(tbls = list(OUTPUT05_HUN, OUTPUT06_HUN, OUTPUT07_HUN, OUTPUT08_HUN),
          tab_spanner = c("Hungary Math", "Hungary Math", "Hungary Read", "Hungary Read"))

# Reading Comparison
tbl_merge(tbls = list(OUTPUT05_SWE, OUTPUT06_SWE, OUTPUT07_SWE, OUTPUT08_SWE),
          tab_spanner = c("Sweden Math", "Sweden Math", "Sweden Read", "Sweden Read"))


# 4. Analyze Migrant Background
freq(HUN$MIG) #has 2.25% migrants
freq(SWE$MIG) #has 16.80% migrants

# Math Scores
MODEL07_HUN <- lm(MATH ~ MISCEDC+MIG+FLANG+EMOSUPS+BOOKSG+GENDER, data = HUN)
MODEL07_SWE <- lm(MATH ~ MISCEDC+MIG+FLANG+EMOSUPS+BOOKSG+GENDER, data = SWE)

# Reading Scores
MODEL08_HUN <- lm(TS ~ MISCEDC+MIG+FLANG+EMOSUPS+BOOKSG+GENDER, data = HUN)
MODEL08_SWE <- lm(TS ~ MISCEDC+MIG+FLANG+EMOSUPS+BOOKSG+GENDER, data = SWE)
stargazer(MODEL07_HUN,MODEL08_HUN,MODEL07_SWE,MODEL08_SWE,type="text")

OUTPUT07_HUN <- tbl_regression(MODEL07_HUN)
OUTPUT08_HUN <- tbl_regression(MODEL08_HUN)
OUTPUT07_SWE <- tbl_regression(MODEL07_SWE)
OUTPUT08_SWE <- tbl_regression(MODEL08_SWE)

tbl_merge(tbls = list(OUTPUT07_HUN, OUTPUT08_HUN, OUTPUT07_SWE, OUTPUT08_SWE),
          tab_spanner = c("Hungary Math", "Hungary Read", "Sweden Math", "Sweden Read"))
