######### Section 4: Inferential statistics (Slide XXX) #########

rm(list=ls())

## Install (if you haven't before) and load packages/libraries 
# install.packages(c("AER", "stargazer", "ggplot2")) 
library(car)
library(carData)
library(AER)
library(stargazer)
library(ggplot2)


## Load data
load("indicators.Rda")
world.clean <-world[!is.na(world$corrupt)&!is.na(world$polstab)&!is.na(world$homicide),] 

## Bivariate Regression

# We start by checking the correlation between our main variables of interest: 
cor(world.clean$polstab, world.clean$corrupt)

# Strong, positive correlation.

# Then, we run the regression:
reg1 <- lm(corrupt ~ polstab, data = world.clean)

# Before the tilde is the dependent variable. 
# After the tilde is the independent variable(s)

# The summary command gives us the relevant information from our regression model:
summary(reg1)


## Multivariate Regression

# You add more independent variables to the regression equation with a plus:
reg2 <- lm(corrupt ~ polstab + homicide, data = world.clean)

summary(reg2)


## We use stargazer to show the results of our regressions. 

# The first few arguments are used to specify which regression models should be 
# included in the output:
stargazer(reg1, reg2, title = "Regression results", type = "text", out = "mul_reg.txt")

# Or to make the output look a little nicer:
stargazer(reg1, reg2, title="Regression Results",
          digits = 2, # Edit the number of decimal places
          covariate.labels = c("Political Stability", "Homicide rate", "Constant"), # Edit variable names
          dep.var.labels=c("Level of Corruption"), # Edit name of dependent variable
          type = "text",
          notes = "This is a note", # Include notes
          notes.align = "l", # Edit the alignment of the note's text
          out="mul_reg.txt")

# These are just examples.
# See the cheatsheet for ways to make output even fancier: 
# (https://www.jakeruss.com/cheatsheets/stargazer/)

# The output is saved in your working directory.

# You can also save the regression table as a word document
stargazer(reg1, reg2, title = "Regression results", type = "text", out = "mul_reg.doc")




######### Section 5: Checking assumptions (Slide XXX) #########

## Now, let's run some regression diagnostics for our bivariate model using 
# the plot() function.


## Specify 1 as the second argument to examine whether corruption and political 
# stability appear to have a linear relationship: 
plot(reg1,1)

# We aim for a horizontal red line. If the line looked more like e.g. a parabola, 
# it would be worth testing a quadratic relationship instead.


## Specify 2 as the second argument to examine whether the residuals are normally 
# distributed: 
plot(reg1,2)

# We aim for the points to follow the diagonal as closely as possible.

# Alternatively, plot a histogram of the residuals:
hist(reg1$residuals)


## Specify 3 as the second argument to examine whether the residuals are homoskedastic:
plot(reg1,3)

# Again, we aim for a horizontal red line, implying no systematic pattern in the 
# residuals of the model.


## How about the zero mean independence (omitted variable bias) and multicollinearity
# assumptions?
round(cor(world.clean[, -c(1,2,3,13)], use="complete.obs"),2)

# Apparently, "goveff" and "law" are correlated with both "polstab" (the X) and 
# "corrupt" (the Y) 

# What would be the effect if we add these to the model?
reg3 <- lm(corrupt ~ polstab + homicide + goveff + law, data = world.clean)

summary(reg3)

vif(reg3)

# The effect of political stability becomes smaller - but both "goveff" and "law" 
# have very high VIF values which indicates multicollinearity! 


## Let's remove "law" and see what happens now:
reg4 <- lm(corrupt ~ polstab + homicide + goveff, data = world.clean)

vif(reg4)

# Now vif values are more acceptable again. How about our model estimates?
summary(reg4)

stargazer(reg1, reg2, reg3, reg4, title = "Regression results", type = "text", out = "mul_reg.txt")


######### Visualisation (slide 28) ####

## If you use the normal plot() function for data visualisation, you can add a regression 
# line using abline():
plot(world.clean$polstab, world.clean$corrupt)

abline(reg1)

# Alternatively, we can use the ggplot2 library.
library(ggplot2)

# The basic idea is that we will specify layer upon layer of the things that we want
# to graph (don't forget the "+" signs!!)

# Let’s start with a simple graph
ggplot(world.clean, aes(x = polstab, y = corrupt)) +  # Specify dataset, and the x and y axes
  geom_point(color="blue") # Plot a scatterplot, type in any colour you would like

# Now we expand:
ggplot(world.clean, aes(x = polstab, y = corrupt)) +  
  geom_point(color="blue") + 
  geom_smooth(method = "lm") + # add the regression line
  theme_minimal() + # add a theme (visual)
  labs(x="Political Stability",
       y="Level of Corruption",
       title = "Effect of Political Stability on Corruption")




######### Group assignment ####

# Pick one variable as your dependent variable
# Then pick two other variables as your independent variables
world.clean2 <- world[!is.na(world$growth)&!is.na(world$goveff)&!is.na(world$politydem),] 
reg5 <- lm(growth ~ goveff + politydem, data = world.clean2)
summary(reg5)

# Check for all the important assumptions:
      # 1) Linearity
plot(reg5,1)
      # 2) Mean independence of errors
residuals <- resid(reg5)
mean_residuals <- mean(residuals) # Calculate the mean of residuals
round(mean_residuals,4)
plot(reg5, which = 1, main = "Residuals vs Fitted Values", col = "blue", pch = 16)
      # 3) Homoscedasticity
plot(reg5,3)
      # 4) Normal distribution of errors
plot(reg5,2)
hist(reg5$residuals)
      # 5) No multicollinearity
vif(reg5)
