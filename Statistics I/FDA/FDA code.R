### Xingting Luo 238672 Final Data Analysis 2023

# Load packages
library(dplyr)
library(psych)
library(ggplot2)
library(corrplot) 
library(GGally)
library(stargazer)
library(effects)
library(lattice)
library(car)  # vif test for MC 
library(lmtest)  # BP test, coeftest


######## Section 0 - Initiate Data Set ########

## 1. Load data: 
ESS <- read.csv("ESS10.csv") 

## 2. Trim data set to only include the set of variables you can choose from: 
ESS <- ESS[ , c("cntry", "agea", "gndr", "eisced", "brncntr", "ppltrst", "nwspol", 
                "trstprl", "trstep", "stflife", "imbgeco", "hhmmb", "respc19")] 

## 3. Rename the variables to make them easier to work with: 
ESS <- rename(ESS, country=cntry, age=agea, gender=gndr, education=eisced, 
              bornincountry=brncntr, soctrust=ppltrst, news=nwspol, 
              parltrust=trstprl, eptrust=trstep, lifesat=stflife, 
              immig=imbgeco, household=hhmmb, covid=respc19) 

## 4. Recode NA values. 
# All NA values are by default coded as values in ESS. 
# With the following lines of code, you tell R which values reflect NAs: 
ESS[, c(2,4,6:12)][ESS[, c(2,4,6:12)] > 50] <- NA 
ESS[, c(3,5,13)][ESS[, c(3,5,13)] > 5] <- NA 

## 5. Recode dummy variables and define them as factors. 
# The dummy variables are by default coded as 1,2 instead of 0,1 in ESS.  
# Further, they are not automatically read as factors (i.e. as categorical variables). 
# The following lines of code will fix this: 

# Gender: 
ESS$gender <- ifelse(ESS$gender == 2, 1, 0) # value == 2 modify to 1, if else, modify to 0
ESS$gender <- as.factor(ESS$gender) 
# "Female" is now 1, "Male" is 0. 

# Born in country: 
ESS$bornincountry <- ifelse(ESS$bornincountry == 2, 0, 1)  # value == 2 modify to 0, if else, modify to 1
ESS$bornincountry <- as.factor(ESS$bornincountry) 
# "Yes" is now 1, "No" is 0. 

# Covid: 
ESS$covid <- ifelse(ESS$covid==1, 1, 0)  # value == 1 modify to 1 (no change), if else, modify to 0
ESS$covid <- as.factor(ESS$covid) 
# "Yes" is now 1, "No" is 0.


######## Section 1 - Descriptive statistics ########

# 1.1 Data cleansing and preparation

# observe the NA distribution
summary(ESS)
# observe the data type of columns
str(ESS)

# age, news, covid variables have plenty Null values
# Three factor varibles: gender, bornincountry, covid
# country is a chr variable


# data cleansing exclude age, news, covid
ess.clean <- ESS[!is.na(ESS$education) & !is.na(ESS$bornincountry)
                 & !is.na(ESS$soctrust) & !is.na(ESS$parltrust) 
                 & !is.na(ESS$eptrust) & !is.na(ESS$lifesat) 
                 & !is.na(ESS$immig) & !is.na(ESS$household), ] 
# 37611 -> 33684 observations
describe(ess.clean)


# data cleansing for age variable
ess.clean2 <- ess.clean[!is.na(ess.clean$age), ]
# 33684 -> 16231:  lost half of observations


# data cleansing for covid variable
ess.clean3 <- ess.clean2[!is.na(ess.clean2$covid), ]
# 16231 -> 13202
plot(ess.clean3$covid, ess.clean2$eptrust) # no obvious differences.


# data cleasing for news variable
ess.clean4 <- ess.clean[!is.na(ess.clean$news), ] 
# 33684 -> 13202: lost 2/3 of observations


# 1.2 Descriptive statistics for IVs and DV

# observe the distribution of interval variables
stargazer(ESS, type = "text", out = "summary_stats.txt", 
          summary.stat = c("n","mean", "median", "sd", "min", "max", "p25", "p75"), 
          title = "Summary Statistics",
          digits = 1)


######## Section 2 - Descriptive statistics: Visual analysis ########

# 2.1 Histograms for interval variables
# eptrust distribution
hist(ess.clean$eptrust,
     col = "orange",
     breaks = 10,
     xlab = "Trust of European Parliament",
     main = "the dependent variable")
table(ess.clean$eptrust)

# immig distribution
hist(ess.clean$immig,
     col = "orange",
     breaks = 10,
     xlab = "the economic role of immigrants",
     main = "the main independent variable")
table(ess.clean$immig)

# basically normal distributed but more 0 scores.

# 2.2 Boxplots for dummy variables

# In our regression model, we want to include a bornincountry interaction term, so it
# makes sense to look at the descriptive measures within each category of
table(ess.clean$bornincountry)  # No: 2623; Yes: 31061
# bornincountry plot
plot(ess.clean$bornincountry, ess.clean$eptrust, 
     horizontal = T, xlab = "EP trust", ylab = "bornincountry") # nearly identical on a general level

# gender
plot(ess.clean$gender, ess.clean$eptrust, 
     horizontal = T, xlab = "EP trust", ylab = "gender") # almost the same in the overall level

# covid
plot(ess.clean2$covid, ess.clean2$eptrust, horizontal = T) # nearly identical on a broad scale

# country
ess.clean$country <- factor(ess.clean$country)
plot(ess.clean$country, ess.clean$eptrust) # Nothing obvious

# 2.3 Scatter plots for coefficients

# age
scatterplot(eptrust ~ age, data=ess.clean2, pch=".")  # negative

# education
scatterplot(eptrust ~ education, data=ess.clean, pch=".")  # positive

# household
scatterplot(eptrust ~ household, data=ess.clean, pch=".")  # non-linear

# news
scatterplot(eptrust ~ news, data=ess.clean4, pch=".")  # positive, non-linear

# age
scatterplot(eptrust ~ age, data=ess.clean3, pch=".")  # negative


# 2.4 Correlations
# check the correlation between variables
round(cor(ess.clean3[, c(2, 4, 6, 8:12)], use="complete.obs"),2)

corrplot(round(cor(ess.clean3[, c(2, 4, 6, 8:12)]),2))

ggpairs(ess.clean[, c("eptrust", "parltrust", "soctrust", "immig", "lifesat")])
# the eptrust has moderate correlations with parltrust and soctrust, 
# lower correlation between immig and lifesat.


######## Section 3 - Model estimation and interpretation ########

# Theory: European residents' identity characteristics and 
# attitudes towards the economic role of immigrants 
# will affect their evaluation of the EP trust in this dataset. 

# Research Question: How does whether or not being native-born will affect 
# how EU residents' rating results for eptrust depend on immig?  
# H1: The effect of the attitude towards the economic role of immigrants on EP trust rating 
# is different for native residents and immigrant residents.
# H0: The effect of the attitude towards the economic role of immigrants on EP trust rating 
# is the same for native residents and immigrant residents.


# 3.1 Models 

# Cross Tabulation
xtabs(~ bornincountry + immig, data = ess.clean)

# compute cell means to compare between native and immigrated people.
Tapply(eptrust ~ bornincountry + immig, mean, data = ess.clean)
# Native people gave more extreme eptrust scores on either side.


# We estimate three models in step-wise fashion:
# Without interaction, without control (naive model):
reg_ess_1 <- lm(eptrust ~ bornincountry + immig, data = ess.clean)

# Without interaction, with control:
reg_ess_2 <- lm(eptrust ~ bornincountry + immig + education + lifesat, data = ess.clean)  
vif(reg_ess_2) # No Multicollinearity

# With interaction, with control:
reg_ess_3 <- lm(eptrust ~ bornincountry + immig + bornincountry*immig + education + lifesat, data = ess.clean)  

# Comparation
stargazer(reg_ess_1, reg_ess_2, reg_ess_3, type = "text",
          out = "summary_regressions.txt", 
          title = "Summary Statistics for regressions")


# The effect plot for model reg_ess_3, which includes the bornincountry * immig
plot( Effect( c("bornincountry", "immig"), reg_ess_3), 
      main    = "Interaction Model (a)", 
      confint = list(style = "bars"),
      lines   = list(multiline = TRUE), 
      ylim    = c (3, 6.5)) 

plot( Effect( c("bornincountry", "immig"), reg_ess_2), 
      main    = "Main-Effects Model (b)",
      confint = list(style = "bars"),
      lines   = list(multiline = TRUE),
      ylim    = c (3, 6.5))

plot(predictorEffects(reg_ess_3), lines = list(multiline = TRUE))
# bornincountry * immig interaction shows the low immig scores affected by bornincountry status

# With more control:
reg_ess_4 <- lm(eptrust ~ bornincountry + immig + bornincountry*immig + education + lifesat + household, data = ess.clean)  
reg_ess_5 <- lm(eptrust ~ bornincountry + immig + bornincountry*immig + education + lifesat + soctrust, data = ess.clean)  
reg_ess_6 <- lm(eptrust ~ bornincountry + immig + bornincountry*immig + education + lifesat + parltrust, data = ess.clean) 

stargazer(reg_ess_3, reg_ess_4, reg_ess_5, reg_ess_6, type = "text", 
          out = "summary_regressions 2.txt", 
          title = "Summary Statistics for regressions")

summary(reg_ess_4) # household can be added to the model.


######## Section 4 - OLS assumptions and regression diagnostics ########

## Specify 1 as the second argument to examine whether appear to have a linear relationship: 
plot(reg_ess_4, 1)

# We aim for a horizontal red line. This line looked a bit like a parabola, 
# it would be worth testing a quadratic relationship instead.


## Specify 2 as the second argument to examine whether the residuals are normally 
# distributed: 
plot(reg_ess_4, 2)
# Alternatively, plot a histogram of the residuals:
hist(reg_ess_4$residuals)


## Specify 3 as the second argument to examine whether the residuals are homoskedastic:
plot(reg_ess_4, 3)

# Again, we aim for a horizontal red line, implying no systematic pattern in the 
# residuals of the model.

bptest(reg_ess_4) # reject null hypothesis of homoscedasticity

reg_ess_4_robust <- coeftest(reg_ess_4, vcov = vcovHC(reg_ess_4, type = "HC1"))
reg_ess_4_robust


# diagnostics for lm
residualPlots(reg_ess_4)  
# the variables have *** marks on p-value, suggesting a transformation.

# Create residual plots with lifesat variable
residualPlots(reg_ess_4, ~ lifesat, fitted = FALSE, id = list(n = 3))
# Create residual plots with education variable
residualPlots(reg_ess_4, ~ education, fitted = FALSE, id = list(n = 3))


# use quadratic regression for education and lifesat variable
reg_ess_final <- lm(eptrust ~ bornincountry + immig + bornincountry*immig 
                    + poly(education, 2, raw = TRUE)  
                    + poly(lifesat, 2, raw = TRUE)
                    + poly(household, 2, raw = TRUE), 
                    data = ess.clean)
summary(reg_ess_final)

# test again
residualPlots(reg_ess_final)  # better fitting

# marginalModelPlots for Marginal-Model Plots
marginalModelPlots(reg_ess_final) # no obvious problem.

Anova(reg_ess_final) # Type II tests: analysis-of-variance tables for model **objects**


######## Section 5 - Outliers ########

# Studentized Residuals

# The generic qqPlot() function in the car package has a method for linear models, 
# plotting Studentized residuals against the corresponding quantiles of t(n − k − 2).
qqPlot(reg_ess_final, id = list(n = 3))

# A test based on the largest (absolute) Studentized residual, 
# using the outlierTest () function in the car package, 
outlierTest(reg_ess_final)
# No Studentized residuals with Bonferroni p < 0.05


# Leverage: Hat-Values

# function from car package
# includes index plots of Studentized residuals, 
# the corresponding Bonferroni p-values for outlier testing, 
# the hat-values, and Cook’s distances.
influenceIndexPlot(reg_ess_final, id=list (n=3))
influencePlot(reg_ess_final, id=list (n=3))


# dfbetas plots for immig:
dfbetasPlots(reg_ess_final, ~immig,  id.n = 5, labels = rownames(dfbetas(reg_ess_final)))
# dfbetas plots for all variables:
dfbetasPlots(reg_ess_final,  id.n = 5, labels = rownames(dfbetas(reg_ess_final)))
# All |dfbetas| < 1 so no outliers!

# try remove 5 observations have biggest determination.
ess.clean.out <- ess.clean[-c(9661, 12609, 12087, 26202, 30123), ] 

# Re-run regression, and report both results: 
reg_ess_final_out <- lm(eptrust ~ bornincountry + immig + bornincountry*immig 
                    + poly(education, 2, raw = TRUE)  
                    + poly(lifesat, 2, raw = TRUE)
                    + poly(household, 2, raw = TRUE), 
                    data = ess.clean.out)
stargazer(reg_ess_final, reg_ess_final_out, type = "text")
# We notice that the coefficients have hardly changed (as expected), 
# as the outliers don't have massive influence

S(reg_ess_final_out)
plot(predictorEffects(reg_ess_final_out), lines = list(multiline = TRUE))
