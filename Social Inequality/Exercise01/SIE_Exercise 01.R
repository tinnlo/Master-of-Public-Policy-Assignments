library(haven)
library(descr)
library(questionr)

#### Exercise 1.1: data preparation ####
rm(list=ls())
DATA00 <- read_dta("assets/ZA7600_v3-0-0.dta", encoding='latin1')

# Select the data from Italy(380)
DATA01 <- subset(DATA00, DATA00$country == 380)

# Select the variables of interest
DATA02 <- subset(DATA01, select = c(IT_INC, HHADULT, HOMPOP, AGE, URBRURAL))

# IT_INC: Country specific household income
# Select the data with positive regular income
DATA03 <- subset(DATA02, DATA02$IT_INC >= 0 & IT_INC < 5001)

# HOMPOP: How many persons in household
# HHADULT: How many adults in household
# AGE: Age of the respondent
# Delete all cases with missing values on the key variables of interest
DATA03 <- subset(DATA03, DATA03$HOMPOP > 0 & DATA03$HHADULT > 0) 
DATA03 <- subset(DATA03, DATA03$HOMPOP >= DATA03$HHADULT) 
DATA03 <- subset(DATA03, DATA03$AGE >= 18)

# Generate the new variables that required for the investigation
# Var: Household Size 
DATA03$TOTAL <- DATA03$HOMPOP
DATA03$TOTAL <- as.numeric(DATA03$HOMPOP)

DATA03$ADULT <- DATA03$HHADULT
DATA03$ADULT <- as.numeric(DATA03$ADULT) 

DATA03$OTHER <- DATA03$ADULT-1
DATA03$OTHER <- as.numeric(DATA03$OTHER)

DATA03$KIDS <- DATA03$TOTAL - DATA03$ADULT
DATA03$KIDS <- as.numeric(DATA03$KIDS)

# Var: Age(categorical) 
DATA03$AGE_C <- "NA"
DATA03$AGE_C[DATA03$AGE>=18] <- "18-39" 
DATA03$AGE_C[DATA03$AGE>=40] <- "40-59" 
DATA03$AGE_C[DATA03$AGE>=60] <- "60+" 
DATA03$AGE_C <- as.factor(DATA03$AGE_C)

# Var: Household Income 
DATA03$INCOME01 = DATA03$IT_INC
DATA03$INCOME02 = DATA03$IT_INC/(1+DATA03$OTHER*0.5+DATA03$KIDS*0.3)

# Var: AROP 
DATA03$POOR <- "Not poor"
DATA03$POOR[DATA03$INCOME02 < (median(DATA03$INCOME02)*0.6)] <- "Poor" 
DATA03$POOR <- as.factor(DATA03$POOR)

# the share of older people in the population
crosstab(DATA03$AGE_C, DATA03$POOR, prop.r = T)
# 37.6% of the population is 60+ years old
# Poor: 21.6%, nor poor: 78.4%

# how large is the household size on average
mean(DATA03$TOTAL)
# the average household size is 2.95

#### Exercise 1.2 ####

# Calculate the equalized median income (based on modified OECD-scale). 
median(DATA03$INCOME02) * 0.6

# How large is the share of persons at risk of poverty
# (defined as 60 % of the median net equalized household income)?

# Poverty Ratio 
freq(DATA03$POOR)
# 22% of the population is at risk of poverty

#### Exercise 1.3 ####

# Do you think that age matters for poverty in your country of choice? 
# Formulate a testable hypothesis.

# Testable hypothesis:
# H0: Age does not matter for poverty in Italy
# H1: Age matters for poverty in Italy

# Conduct a chi-square test to test the hypothesis
chisq.test(DATA03$POOR, DATA03$AGE_C)
# With a p-value of 0.9782, the null hypothesis cannot be rejected.
# In other words, there is no evidence to suggest that age matters for poverty in Italy.

# Calculate the at-risk-of-poverty ratio by age. 
# Distinguish the following age groups: 20-39, 40-59, 60 and older.
crosstab(DATA03$POOR, DATA03$AGE_C, prop.c = T)

#### Exercise 1.4 ####

# distinguish those without children, with one or two and those with three and more children as a factor variable
DATA03$KIDS_C <- "NA"
DATA03$KIDS_C[DATA03$KIDS==0] <- "No kids"
DATA03$KIDS_C[DATA03$KIDS==1 | DATA03$KIDS==2] <- "1-2 kids"
DATA03$KIDS_C[DATA03$KIDS>=3] <- "3+ kids"
DATA03$KIDS_C <- as.factor(DATA03$KIDS_C)

# change the order of the factor levels as the number of children increases
DATA03$KIDS_C <- factor(DATA03$KIDS_C, levels = c("No kids", "1-2 kids", "3+ kids"))

# at-risk-of-poverty ratio by number of children in households
crosstab(DATA03$POOR, DATA03$KIDS_C, prop.c = T)

# Chi-square test
chisq.test(DATA03$POOR, DATA03$KIDS_C)
# With a p-value much less than 0.05, the null hypothesis can be rejected.
# In other words, the number of children in the household matters for poverty in Italy.


# (b) 
# There is a debate of whether national or sub-national units should be used to define poverty. 
# Is this a relevant consideration for your country of choice?
# How do patterns differ if you use different levels for rural and urban areas? 

# distinguish only urban and rural places as a factor variable
DATA03$URBRURAL_C <- "NA"
DATA03$URBRURAL_C[DATA03$URBRURAL <= 3] <- "Urban"
DATA03$URBRURAL_C[DATA03$URBRURAL > 3] <- "Rural"
DATA03$URBRURAL_C <- as.factor(DATA03$URBRURAL_C)

# At-risk-of-poverty ratio by urban and rural areas
crosstab(DATA03$POOR, DATA03$URBRURAL_C, prop.c = T)

# Chi-square test
chisq.test(DATA03$POOR, DATA03$URBRURAL_C)
# With a p-value of 0.1348, the null hypothesis cannot be rejected.
# In other words, there is no evidence to suggest that the level of urbanization matters for poverty in Italy.
