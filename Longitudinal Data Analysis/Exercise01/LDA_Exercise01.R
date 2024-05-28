library(survival)
library(survminer)
library(descr)

# Load the data of UK
DATA01 <- readRDS("assets/Exercise01.Rds")
DATA02 <- subset(DATA01,(DATA01$cntry=="GB"))

# Using the survfit() function to fit the survival curve
SURVIVAL01 <- survfit(Surv(DATA02$TIME,DATA02$EVENT) ~ 1) # ~1 means no covariate

# show the summary statistics of the survival curve
summary(SURVIVAL01)
surv_median(SURVIVAL01) # Median survival time is 20
ggsurvplot(SURVIVAL01, data=DATA02, surv.median.line = "hv")

# Add Gender as a covariate
SURVIVAL02 <- survfit(Surv(DATA02$TIME,DATA02$EVENT) ~ DATA02$GENDER)
summary(SURVIVAL01)
surv_median(SURVIVAL02) # Median survival time is 20 in both genders
ggsurvplot(SURVIVAL02, data=DATA02, surv.median.line = "hv")

# Add Cohort as a Covariate
SURVIVAL03 <- survfit(Surv(DATA02$TIME,DATA02$EVENT) ~ DATA02$GENDER + DATA02$COHORT)
surv_median(SURVIVAL03)
ggsurvplot(SURVIVAL03, data=DATA02, surv.median.line = "hv")

# Generate survival curves separately for each cohort with gender comparison on each plot
ggsurvplot(
  survfit(Surv(DATA02$TIME, DATA02$EVENT) ~ GENDER, data = DATA02),
  data = DATA02,
  surv.median.line = "hv",
  facet.by = "COHORT",
  facet.wrap.nrow = 2,  # Adjust the number of rows in the facet grid as needed
  title = "Survival Curves by Cohort and Gender"
)