library(haven)
library(descr)
library(ineq)


#### Step 1: data preparation ####
rm(list=ls())
# Read in data & select analytical sample 
ISSP2009 <- read_dta("assets/ISSP2009.dta") #35281 obs.
ISSP2019 <- read_dta("assets/ISSP2019.dta") #40589 obs.
# Filter the data for Philippines(608)
ISSP2009 <- subset(ISSP2009, ISSP2009$country == 608) #1200 obs.
ISSP2019 <- subset(ISSP2019, ISSP2019$country == 608) #4250 obs.


#### Step 2: variables construction ####
# year 2009
# Var: WEIGHT 
ISSP2009$WEIGHT <- NA 
# HOMPOP: How many people in household
# filter out the missing values and the outliers
ISSP2009$WEIGHT[ISSP2009$HOMPOP > 0 & ISSP2009$HOMPOP < 99] <- ISSP2009$HOMPOP[ISSP2009$HOMPOP > 0 & ISSP2009$HOMPOP < 99] 
ISSP2009$WEIGHT <- as.numeric(ISSP2009$WEIGHT)

# Var: Income01 
ISSP2009$INCOME01 <- NA
ISSP2009$INCOME01[ISSP2009$PH_INC > 0 & ISSP2009$PH_INC < 999999] <- ISSP2009$PH_INC[ISSP2009$PH_INC > 0 & ISSP2009$PH_INC < 999999]
ISSP2009$INCOME01 <- as.numeric(ISSP2009$INCOME01)

# Var: Income2, Equalized Household income (1 for each person in hh) 
ISSP2009$INCOME02 <- NA 
ISSP2009$INCOME02 <- ISSP2009$INCOME01/ISSP2009$WEIGHT
ISSP2009$INCOME02 <- as.numeric(ISSP2009$INCOME02)

# Var: Income3, Deflated Equalized Household income (Index Consumer price index 2010 = 100) 
ISSP2009$INCOME03 <- NA 
ISSP2009$INCOME03 <- ISSP2009$INCOME02/0.963485477
ISSP2009$INCOME03 <- as.numeric(ISSP2009$INCOME03)

# year 2019 
# Var: WEIGHT 
ISSP2019$WEIGHT <- NA 
ISSP2019$WEIGHT[ISSP2019$HOMPOP>0 & ISSP2019$HOMPOP<99] <-ISSP2019$HOMPOP[ISSP2019$HOMPOP>0 & ISSP2019$HOMPOP<99] 
ISSP2019$WEIGHT <- as.numeric(ISSP2019$WEIGHT) 

# VAR: INCOME 
ISSP2019$INCOME01 <- NA 
ISSP2019$INCOME01[ISSP2019$PH_INC>0 & ISSP2019$PH_INC<999999] <-ISSP2019$PH_INC[ISSP2019$PH_INC>0 & ISSP2019$PH_INC<999999] 
ISSP2019$INCOME01 <-as.numeric(ISSP2019$INCOME01) 

ISSP2019$INCOME02<-NA 
ISSP2019$INCOME02 <-ISSP2019$INCOME01/ISSP2019$WEIGHT 
ISSP2019$INCOME02 <-as.numeric(ISSP2019$INCOME02) 

ISSP2019$INCOME03<-NA 
ISSP2019$INCOME03 <-ISSP2019$INCOME02/1.29621996
ISSP2019$INCOME03 <-as.numeric(ISSP2019$INCOME02)


#### Step 3: Analysis ####

#Analysis 2009 
mean(ISSP2009$INCOME03, na.rm=TRUE)
median(ISSP2009$INCOME03, na.rm=TRUE) 
Gini(ISSP2009$INCOME03)
Lc(ISSP2009$INCOME03, plot = TRUE) 
     
#Analysis 2019 
mean(ISSP2019$INCOME03, na.rm=TRUE)
median(ISSP2019$INCOME03, na.rm=TRUE) 
Gini(ISSP2019$INCOME03)
Lc(ISSP2009$INCOME03, plot = TRUE) 

#Lorenz curves in one figure (red=2009) (blue=2019) 
LORENZ09 <- Lc(ISSP2009$INCOME03) 
LORENZ19 <- Lc(ISSP2019$INCOME03) 
# 2009
plot(LORENZ09, col="red", lwd=2, 
     xlab="Cumulative share of population", 
     ylab="Cumulative share of income", 
     main="Lorenz curves 2009 and 2019")
# 2019
lines(LORENZ19, col="blue", lwd=2)
legend("topleft", legend=c("2009", "2019"), col=c("red", "blue"), lty=1:1, cex=0.8)
