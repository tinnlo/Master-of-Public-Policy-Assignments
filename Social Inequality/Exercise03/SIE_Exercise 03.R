library(haven)
library(descr)
library(table1)
library(gmodels)
library(corrplot)
library(gtsummary)
library(sjPlot)

#### Step 1: data preparation ####
rm(list=ls())
# Read in data & select the analytical sample 
ISSP2019 <- read_dta("assets/ISSP2019_EX3.dta") # 44975 obs.
# Select Taiwan and relevant variables
DATA01 <- subset(ISSP2019, ISSP2019$country == 158, 
                 select = c(CLASS, CLASS_SPOUSE, AGE, SEX, v53, v61)) # 1926 obs. from Taiwan
# remove ISSP2019 from memory
rm(ISSP2019)

# Remove missing values
DATA02 <- subset(DATA01, !(is.na(DATA01$CLASS))) # 1844 obs.
DATA03 <- subset(DATA02, DATA02$v53 > 0)  # 1837 obs.
DATA04 <- subset(DATA03, DATA03$v61 > 0)  # 1803 obs.
DATA05 <- subset(DATA04, !(is.na(DATA04$CLASS_SPOUSE))) # 990 obs.

#### Step 2: variables construction ####
#Construct Variables 
#Var: GENDER 
DATA05$GENDER<-NA 
DATA05$GENDER[DATA05$SEX==1]<-"1-Men"
DATA05$GENDER[DATA05$SEX==2]<-"2-Women"
DATA05$GENDER<-as.factor(DATA05$GENDER)

#Var: SOCIAL CLASS
DATA05$CLASS01<-NA 
DATA05$CLASS01[DATA05$CLASS==1]<-"1-unskilled workers" 
DATA05$CLASS01[DATA05$CLASS==2]<-"2-skilled workers"
DATA05$CLASS01[DATA05$CLASS==3]<-"3-lower grade service" 
DATA05$CLASS01[DATA05$CLASS==4]<-"4-Higher grade service" 
DATA05$CLASS01<-as.factor(DATA05$CLASS01)

#Var: Spouse's Social Class
DATA05$CLASS02<-NA
DATA05$CLASS02[DATA05$CLASS_SPOUSE==1]<-"1-unskilled workers"
DATA05$CLASS02[DATA05$CLASS_SPOUSE==2]<-"2-skilled workers"
DATA05$CLASS02[DATA05$CLASS_SPOUSE==3]<-"3-lower grade service"
DATA05$CLASS02[DATA05$CLASS_SPOUSE==4]<-"4-Higher grade service"
DATA05$CLASS02<-as.factor(DATA05$CLASS02)

#Var: DEP
DATA05$DEP<-NA
DATA05$DEP[DATA05$v53<=2] <- 1
DATA05$DEP[DATA05$v53>=3] <- 0
DATA05$DEP <- as.numeric(DATA05$DEP)

#FINAL DATA SETS 
ALL <- DATA05
FEMALE <- subset(DATA05,DATA05$SEX==2) 
MALE <- subset(DATA05,DATA05$SEX==1)

#Summmary Statistics 
table1::label(ALL$CLASS01) <- "CLASS"
table1::label(ALL$CLASS02) <- "CLASS_SPOUSE"
table1::label(ALL$v61) <- "PCLASS"
table1(~ CLASS01 + CLASS02 + v61 + DEP + GENDER | GENDER, data = ALL)


#### Step 3: Analysis ####

# Exercise 3.2
# Correlation between CLASS and PCLASS
round(cor(MALE$CLASS, MALE$v61, method = "spearman"), 2) # 0.45
round(cor(FEMALE$CLASS, FEMALE$v61, method = "spearman"), 2) # 0.33

# Correlation between CLASS_SPOUSE and PCLASS
round(cor(MALE$CLASS_SPOUSE, MALE$v61, method = "spearman"), 2) # 0.32
round(cor(FEMALE$CLASS_SPOUSE, FEMALE$v61, method = "spearman"), 2) # 0.27

# Correlogram 
CORR_M<-subset(MALE,select=c(CLASS,v61,CLASS_SPOUSE)) 
corrplot(corr=cor(CORR_M, use="complete.obs", method = "spearman"), method="number", tl.srt = 45)
title_1 <- "Male"
mtext(title_1, side = 2, cex = 1.5, col = "black")

CORR_F<-subset(FEMALE,select=c(CLASS,v61,CLASS_SPOUSE))
corrplot(corr=cor(CORR_F, use="complete.obs", method = "spearman"), method="number", tl.srt = 45)
title_2 <- "Female"
mtext(title_2, side = 2, cex = 1.5, col = "black")

# Exercise 3.3
# Generate a contingency table with social class by social class of partner
table(ALL$CLASS, ALL$CLASS_SPOUSE)

# Calculate the fraction of class homogeneous couples
round(sum(ALL$CLASS == ALL$CLASS_SPOUSE)/nrow(ALL), 2) # 0.43

# Calculate the fraction of couples where the man belongs to a higher social class than the woman
round((sum(MALE$CLASS > MALE$CLASS_SPOUSE) + sum(FEMALE$CLASS < FEMALE$CLASS_SPOUSE)) / nrow(ALL), 2) # 0.40

# Calculate Spearman’s rank correlation coefficient between CLASS and CLASS_SPOUSE
round(cor(ALL$CLASS, ALL$CLASS_SPOUSE, method = "spearman"), 2) # 0.32

# Calculate the fraction of couples where the women belongs to a higher social class than the men
round((sum(MALE$CLASS < MALE$CLASS_SPOUSE) + sum(FEMALE$CLASS > FEMALE$CLASS_SPOUSE)) / nrow(ALL), 2) # 0.17

round(cor(FEMALE$CLASS_SPOUSE, FEMALE$v61, method = "spearman"), 2) # 0.27

# Exercise 3.4
#Linear Regression
MODEL01 <- lm(DEP ~ CLASS01 + GENDER + AGE, data = ALL)
# display the regression results
tbl_regression(MODEL01)

MODEL02 <- lm(DEP ~ CLASS01 + GENDER + AGE + CLASS02, data = ALL)
OUTPUT01 <-tbl_regression(MODEL01) 
OUTPUT02 <-tbl_regression(MODEL02) 
tbl_merge(tbls = list(OUTPUT01, OUTPUT02), tab_spanner = c("Model1", "Model2"))

plot_model(MODEL02, type = "pred", terms = c("CLASS01"))

# Exercise 3.5
# Conduct the analysis by gender
MODEL03F_A <- lm(DEP ~ CLASS01 + AGE, data = FEMALE)
MODEL03F_B <- lm(DEP ~ CLASS01 + AGE + CLASS02, data = MALE)

MODEL03M_A <- lm(DEP ~ CLASS01 + AGE, data = MALE)
MODEL03M_B <- lm(DEP ~ CLASS01 + AGE + CLASS02, data = MALE)

OUTPUT03F_A <-tbl_regression(MODEL03F_A)
OUTPUT03M_A <-tbl_regression(MODEL03M_A)

OUTPUT03F_B <-tbl_regression(MODEL03F_B)
OUTPUT03M_B <-tbl_regression(MODEL03M_B)

tbl_merge(tbls = list(OUTPUT03F_A, OUTPUT03F_B), tab_spanner = c("Own Class(Female)", "With Spouse's Class(Female)"))
tbl_merge(tbls = list(OUTPUT03M_A, OUTPUT03M_B), tab_spanner = c("Own Class(Male)", "With Spouse's Class(Male)"))
