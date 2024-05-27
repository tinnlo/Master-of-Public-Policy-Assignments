#########################
# R review session
# Stats I
# November 2023
#########################

## 1. Clean your global environment 
rm(list=ls())
options(scipen=999) # Remove scientific notation


## 2. Set your working directory 
setwd("/Users/lxt/Documents/Data Science/Statistics/MPP Statistics 1 Lab/Workshop 20231118") # sets my working directory

getwd() # shows me my working directory


## 3. Install (if you haven't before) and load packages/libraries 
# install.packages("dplyr")
library(dplyr)

## 4.  Load your data 
# Today we will work with a data set that you already know: 'indicators.Rda'.
load("indicators.Rda")




######### Section 2: Preparing your data (Slide 8) #########

## Section 2.1 - Vectors and factors
x <- c(10.4, 5.6, 3.1, 6.4, 21.7) # With the c() function, I can create a vector
x

l <- c(10.4, "green", "blue", "yellow", 6.4, 21.7) # A list, is a generalized form of a vector
l


# Creating a vector with characters:
approval <- c("strongly approve","approve","disapprove", "strongly disapprove","approve", "approve",
              "disapprove","strongly approve","disapprove","disapprove") 

mode(approval)
table(approval)

approval

# Factors
factor.approval <- as.factor(approval) # Transforming it into a factor 

factor.approval  # What changed? Now we see that it gives us the levels (i.e. the 
                 # categories)

levels(factor.approval) # But, levels in alphabetical order, not in the order that 
                        # makes sense

# Let's change that:
approve.ord <- factor(approval, levels = c("strongly disapprove", "disapprove", "approve", 
                                                  "strongly approve"), ordered = TRUE)
levels(approve.ord) # levels are in the desired order!

approve.num <- as.numeric(approve.ord)

## Section 2.2 - Dealing with missing values

# Now, we go back to our data "world" that we already uploaded:
View(world)
head(world)     # Shows the first few rows
str(world)      # Shows the structure of the data
names(world)    # Shows the names of the columns
nrow(world)     # Shows the number of rows

# is.na() helps us remove missing values from our dataframe
table(is.na(world$corrupt)) # This counts the number of NAs (missings)in a variable

# Today, we will be working with corruption levels as our dependent variable. 
# Let's start by cleaning up:
world.clean <- world[!is.na(world$corrupt),] # Remove rows with NAs for variable 
                                             # corrupt.

# Our main independent variable will be political stability. 
# We will also be testing homicide rates as a potential control variable:
world.clean <- world.clean[!is.na(world.clean$polstab),] # Main IV
world.clean <- world.clean[!is.na(world.clean$homicide),] # Potential control

# Assume you would want to do this for different variables in one line of code:
world.clean2 <-world[!is.na(world$corrupt)&!is.na(world$polstab)&!is.na(world$homicide),] 

# Notice in your global environment that world.clean and world.clean2 are the same.




## Section 2.3 - Recategorizing and subsetting data

# You can transform interval level variables into categorical ones using the cut() 
# function.

# Suppose there are three relevant categories for political stability (an index 
# from 0 to 100):
#     Very unstable if the index value is <= 35
#     Unstable if the index value is > 35 and <= 65 
#     Stable if the index value is  > 65
world.clean$polstab_cat <- cut(world.clean$polstab, 
                              breaks = c(0, 35, 65, 100),
                              labels = c("Very unstable", "Unstable", "Stable"), 
                              ordered_result = TRUE)

# a) The first argument of the cut function is the variable we want to recode from 
# interval level to ordinal level. 
# b) With the 'breaks' argument you specify the cut-off points for the categories. 
# The SECOND number in the 'breaks' argument defines the first cut-off point.
# c) The labels argument assigns labels to the categories.
# d) The option ordered_result = T ensures that we get an ordinal variable, not a 
# nominal one.

# This is also a super easy way to add a single new variable to the dataset!

# Now find the mode of our newly created categorical variable:
mean(world.clean$polstab)

table(world.clean$polstab_cat)


## Subsetting data: Extracting elements from a dataframe

# Let’s create this simple dataframe to get a sense on how subsetting by rows and 
# columns works:
my_df <- data.frame(a = runif(10), 
                    b = rnorm(10, 10), 
                    c = letters[1:10], 
                    d = LETTERS[1:2])

my_df

my_df[1:3, ] # will subset my_df and return the first three rows as a data frame
my_df[, 1:3] # will subset my_df and return the first three columns as a data frame
my_df[1:3] #  will do the same

# The following will extract column “a” as a vector:
v <- my_df[, 1]
v2 <- my_df$a


## Now let’s use the world database

# What if I wanted to include all variables, but only Asian countries?
world.clean_asia <- world.clean[world.clean$continent == "Asia", ]

# Second, I want to include all observations, but only information about their continent,
# human development index, and democratic status:
world.clean2 <- world.clean[ ,c("continent","hdi","demo") ]

# Finally, we can combine this and e.g. subset only Asian countries and their 
# human development indices:
world.clean3 <- world.clean[world.clean$continent == "Asia", c("country", "hdi")]


## OPTIONAL: Subsetting with dplyr 

# Use filter() to subset rows: 
# Include only European countries that are classified as politically stable:
world.clean_europe <- filter(world.clean, continent == "Europe", polstab_cat == "Stable")

# Use select() to subset columns: 
# Here, I subset country name, homicide rate, and political stability category for 
# all countries in the data set:
world.clean4 <- select(world.clean, country, homicide, polstab_cat)




######### Section 3: Descriptive statistics (Slide 19) #########

## The quickest way to get a description of your data is with the summary() function:
summary(world.clean$corrupt) # A variable of interest
summary(world.clean) # The entire dataset

library(psych)

describe(world.clean) # the * meaning: the variable deviates from normality

# Notice differences in the summary for interval and categorical variables.


## For categorical variables, the summary output is the same as when creating a table
# for one variable:
table(world.clean$demo)

# If I want to explore how two categorical variables are related in my data, 
# I can add an additional argument to the table() function. 
# E.g how many countries on a specific continent are democratic/non-democratic:
table(world.clean$demo, world.clean$continent)

# If I rather want to know the proportion of countries are democratic/non-democratic, 
# I use prop.table(), which takes a 'normal' table as its argument:
t1 <- table(world.clean$demo, world.clean$continent)
prop.table(t1) # cell percentages
prop.table(t1, 1) # row percentages: Out of all the democratic countries, 10.9% are in Asia.
prop.table(t1, 2) # column percentages: Out of all the Asian countries, 25% are democratic.


## For interval level data, you can explore a single variable graphically with a histogram:
# To create a histogram for the political stability index, use the function hist():
hist(world.clean$polstab)

# You can edit this graph in different ways. 
# In histograms, you can specify the number of bins.
# The argument 'breaks' adjusts the number of observations grouped together in each bin. 
# However, the choice of break points can significantly change how the histogram looks. 
# Badly chosen, this can cause misrepresentation of the characteristics of your data.
hist(world.clean$polstab,
     col = "orange",
     breaks = 2,
     xlab = "Political stability index",
     main = "My main independent variable")

# With 2 bins, it's very uninformative.

# Better:
hist(world.clean$polstab,
     col = "orange",
     breaks = 15,
     xlab = "Political stability index",
     main = "My main independent variable")

# Instead, you can also use a density plot. 
    # Step 1: Compute the density estimates with the density() function.
    # Step 2: Use plot() to display them in a graph.
d <- density(world.clean$polstab)
plot(d) # just between 0 and 100, the thread is show based on the calculation method.

# Smooth line: Nothing can be tweaked. 


## Finally, we can use the plot() function to display the relationship between 
# two variables in our data.
# The plot() function is a generic function that tries to predict the best way to
# graphically represent the data that we provide as input. If we provide two interval
# level variables, it will produce a scatterplot. 

# Plot political stability against corruption:
plot(world.clean$polstab, world.clean$corrupt)

# If the variable plotted on the x-axis is categorical, the plot() function will
# automatically produce boxplots. For example:
plot(world.clean$continent, world.clean$corrupt)

# You can make the plot nicer and easier to understand by adding additional arguments:
plot(world.clean$continent, world.clean$corrupt,
     xlab = "Continent",
     ylab = "Level of corruption",
     main = "Corruption by continent")




######### Group assignment ####

# Choose one of the variables in the "indicators" dataset and
# 1) Remove observations with missing values for this variable
load("indicators.Rda")
world.clean_t <- world[!is.na(world$politydem),] # Main IV
world.clean_t <- world.clean_t[!is.na(world.clean_t$corrupt),]

# 2) Turn this variable into a categorical one (whether ordered or nominal)
world.clean_t$politydemcat <- cut(world.clean_t$politydem, 
                               breaks = c(0, 4, 7, 10),
                               labels = c("Non-Democratic", "Partly Democratic", "Highly Democratic"), 
                               ordered_result = TRUE)

# 3) Subset the dataset: remove observations belonging to one of the categories 
#    from the dataset
world.clean_t2 <- filter(world.clean_t, politydemcat != "Highly Democratic")

# 4) Make a plot showing the distribution of one other numerical variable for the 
#    different categories
plot(world.clean_t2$politydemcat, world.clean_t2$corrupt)
