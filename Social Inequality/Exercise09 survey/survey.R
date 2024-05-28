#Packages 
library(descr)
library(table1) 

#Load data 
DATA<-read.csv("Survey.csv", sep=";")

#Frequencies
freq(DATA$Q1)
freq(DATA$Q2) 
freq(DATA$Q3)
freq(DATA$Q4)

#Table 
table1(~ Q1+Q2+Q3+Q4, data= DATA)
