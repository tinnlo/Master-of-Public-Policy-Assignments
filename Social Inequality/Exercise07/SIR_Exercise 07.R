# Session 07
# Packages
library(haven)
library(descr)
library(ggplot2)
library(table1)
library(gtsummary)
library(sjPlot)

#Read in data
DATA00<-read_dta("Peru.dta")  # 23888 obs.
DATA01<-subset(DATA00, !(is.na(DATA00$d107))) 
DATA01<-subset(DATA01, !(is.na(DATA01$v131))) 
DATA01<-subset(DATA01, !(is.na(DATA01$v106)))

#New Variable: IPV
DATA01$IPV<-"NA"
DATA01$IPV[DATA01$d107==0]<-"No IPV"
DATA01$IPV[DATA01$d107==1]<-"IPV"
DATA01$IPV<- as.factor(DATA01$IPV)

#New Variable: DEP
DATA01$DEP<-"NA"
DATA01$DEP[DATA01$d107==0]<-0
DATA01$DEP[DATA01$d107==1]<-1
DATA01$DEP<- as.numeric(DATA01$DEP)

#New Variable: ETHNIC
# 1=Spanish, 2=Quechua, 3=Aymara and indigenous, 4=Other, 5=Foreign language
attributes(DATA01$v131)
# drop the ethnic group "foreign language" (6 obs.)
DATA01 <- subset(DATA01, v131 < 5)  # 13477 obs.

DATA01$ETHNIC<-"NA"
DATA01$ETHNIC[DATA01$v131==1]<-"Spanish"
DATA01$ETHNIC[DATA01$v131==2]<-"Quechua"
DATA01$ETHNIC[DATA01$v131==3]<-"Aymara and indigenous"
DATA01$ETHNIC[DATA01$v131==4]<-"Aymara and indigenous"
DATA01$ETHNIC<- as.factor(DATA01$ETHNIC)
DATA01$ETHNIC<- relevel(DATA01$ETHNIC, ref = "Spanish")

#New Variable: EDUCATION
DATA01$CLASS<-"NA"
DATA01$CLASS[DATA01$v106==0]<-"Low"
DATA01$CLASS[DATA01$v106==1]<-"Low"
DATA01$CLASS[DATA01$v106==2]<-"Medium"
DATA01$CLASS[DATA01$v106==3]<-"Top"
DATA01$CLASS <- as.factor(DATA01$CLASS)

#New Variable: AGE
DATA01$AGE<-DATA01$v007-DATA01$v010

#Frequencies
freq(DATA01$IPV)

#Column
crosstab(DATA01$IPV,DATA01$ETHNIC, prop.c=TRUE, xlab = "Ethnic", ylab = "IPV")
crosstab(DATA01$IPV,DATA01$CLASS, prop.c=TRUE, xlab = "Class", ylab = "IPV")

#Bar Chart: 2 Dimensional
# Ethnic
TABLE1 <- table(DATA01$IPV,DATA01$ETHNIC)
TABLE2 <- prop.table(TABLE1,2)
TABLE3 <- as.data.frame(TABLE2)
TABLE3$Percent <- TABLE3$Freq*100
ggplot(TABLE3, aes(fill=Var1, x=Var2, y=Percent))+
  geom_bar(stat="identity",position="stack")+
  ylab("%")+
  xlab("Ethnicity")+
  scale_fill_manual(name = "IPV",
                    labels = c("IPV", "No IPV"),
                    values = c("blue1", "skyblue1"))

# Class
TABLE4 <- table(DATA01$IPV,DATA01$CLASS)
TABLE5 <- prop.table(TABLE4,2)
TABLE6 <- as.data.frame(TABLE5)
TABLE6$Percent <- TABLE6$Freq*100
ggplot(TABLE6, aes(fill=Var1, x=Var2, y=Percent))+
  geom_bar(stat="identity",position="stack")+
  ylab("%")+
  xlab("Class")+
  scale_fill_manual(name = "IPV",
                    labels = c("IPV", "No IPV"),
                    values = c("blue1", "skyblue1"))

# Intersectionality
DATA01$INT=DATA01$ETHNIC:DATA01$CLASS
crosstab(DATA01$IPV, DATA01$INT, prop.c=TRUE, xlab = "Intersectionality: Ethnic and Class", ylab = "IPV")

#Bar Chart: 3 Dimensional (nicer)
TABLE1 <- table(DATA01$IPV,DATA01$INT)
TABLE2 <- prop.table(TABLE1,2)
TABLE3 <- as.data.frame(TABLE2)
TABLE3$Percent <- TABLE3$Freq*100
ggplot(TABLE3, aes(fill=Var1, x=Var2, y=Percent))+
  geom_bar(stat="identity",position="stack")+
  ylab("%")+
  xlab("Ethnic Origin")+
  scale_fill_manual(name = "IPV",
                    labels = c("IPV", "No IPV"),
                    values = c("blue1", "skyblue1")) +
  theme(axis.text.x = element_text(angle = 90))


# Regression
table1::label(DATA01$AGE) <- "AGE"
table1(~DEP+ETHNIC+CLASS+AGE | IPV, data = DATA01)

MODEL1 <- glm(DEP ~ CLASS+ETHNIC+AGE, data=DATA01, family=binomial())
tbl_regression(MODEL1, exponentiate = TRUE)

MODEL2 <- glm(DEP ~ CLASS:ETHNIC+AGE, data=DATA01, family=binomial())
tbl_regression(MODEL2, exponentiate = TRUE)
plot_model(MODEL2, type = "pred", terms = c("CLASS","ETHNIC"))
