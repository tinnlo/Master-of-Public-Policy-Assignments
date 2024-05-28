library(TraMineR) 
library(reshape2) 
library(cluster) 
library(WeightedCluster) 
library(haven) 
library(ggplot2) 
library(descr)


#### Sequence ####
rm(list=ls())

#Read in data: pairfam | Germany | 2008-2018
DATA01 <- read_dta("Exericse07.dta") 
# 103172 obs.

ID <-as.numeric(c(1,1,1,1,1,1,1,1,1,1,1,1,1)) 
TIME <-as.numeric(c(8,9,10,11,12,13,14,15,16,17,18,19,20)) 
EMP <-as.factor(c("SLEEP","EAT","WORK","WORK","EAT","WORK","WORK","WORK","WORK","WORK","WORK","SPORT","EAT")) 
DATA01 = data.frame(ID,TIME,EMP)

# reshape the data
DATA02<-reshape(DATA01,idvar="ID",timevar="TIME",direction = "wide")

# create the sequence object
DATA03 <- seqdef(DATA02[,2:14])

# par() function is used to set or query graphical parameters.
par(mfrow=c(2,1))

# plot the sequence object
cpal(DATA03) <- c("orange","grey", "green", "lightblue")
# remove the x-axis and the legend
seqIplot(DATA03, xaxis=FALSE, withlegend=FALSE, ) 
# nicer x-axis
axis(1, at=c(0,13), labels=c("8 a.m.","8 p.m."))
# legend with bty="n" removes the box around the legend
seqlegend(DATA03, bty="n")

#### INDEL ####

#Insert Data
ID <-as.numeric(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6))
TIME01 <-as.numeric(c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5))
TIME<-as.factor(c("CDU","SPD","GREEN","SPD","SPD","CDU","SPD","GREEN","GREEN", "GREEN",
                  "CDU","SPD","GREEN", "SPD","GREEN","CDU","SPD","SPD","SPD","SPD","SPD","SPD","GREEN","GREEN",
                  "SPD", "CDU","SPD","GREEN", "SPD","SPD"))
DATA01 = data.frame(ID,TIME01,TIME)
DATA02<-reshape(DATA01,idvar="ID",timevar="TIME01",direction="wide")

# Declare the data as a sequence file
DATA03 <- seqdef(DATA02[,2:6])
#Plot the sequence index
cpal(DATA03) <- c("black","green", "red")
seqIplot(DATA03, border=T, with.legend="right")
#Plot the state distribution
seqdplot(DATA03, border=T, with.legend="right")
# Generate and display (print) the distance matrix
DISTANCE <- seqdist(DATA03, method = "LCS")
print(DISTANCE)
# Cluster the data (hclust)
CLUSTER <-hclust(as.dist(DISTANCE), method="ward.D")
plot(CLUSTER)
# Group the data into 2 clusters
TREE <- cutree(CLUSTER, k = 2)
# Display the sequence index plots of the two clusters
seqIplot(DATA03, border=T, with.legend="right", group = TREE)
# Label the two clusters in an appropriate manner
LABELS <- c("SPD changers", "Green Transitioners")
TREE01<- factor(TREE, levels = 1:2, labels = LABELS)
seqIplot(DATA03, border=T, with.legend="right", group = TREE01)



ID <-as.numeric(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)) 
TIME01 <-as.numeric(c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)) 
TIME<-as.factor(c("CDU", "SPD", "GREEN", "SPD","SPD","CDU","SPD","GREEN", "GREEN", "GREEN", "CDU","SPD","GREEN", "SPD","GREEN")) 
DATA01 = data.frame(ID,TIME01,TIME) 
DATA02<-reshape(DATA01, idvar = "ID", timevar = "TIME01", direction = "wide") 
DATA03 <- seqdef(DATA02[,2:6])

# distance
DISTANCE <- seqdist(DATA03, method = "OM",sm = "CONSTANT") 
print(DISTANCE)

#### Practice 7 ####
rm(list=ls())
#Read in data 
DATA <- read_dta("assets/SELF.dta") 

#Define sequence object 
DATA01 <- seqdef(DATA[,2:6]) 

# Legend 
cpal(DATA01) <- c("black","green", "gray", "blue", "red") 
seqlegend(DATA01, bty="n")

# Draw sequence index plot
seqIplot(DATA01, xaxis=FALSE, with.legend=F) 
axis(1, at=c(0,5), labels=c("20","25"))

# Draw distribution plot
seqdplot(DATA01, xaxis=FALSE, with.legend=F) 
axis(1, at=c(0,5), labels=c("20","25"))

# Distance
DISTANCE <- seqdist(DATA01, method = "LCS") 

# Clusters
CLUSTER <-hclust(as.dist(DISTANCE), method="ward.D") 
plot(CLUSTER)

WARDRANGE <-as.clustrange(CLUSTER, diss = DISTANCE, ncluster = 10) 
plot(WARDRANGE, stat=c("ASW", "ASWw"))


# Typology
LABELS <- c("Group 1", "Group 2", "Group 3", "Group 4")
TREE <- cutree(CLUSTER, k = 4) 
TREE01 <- factor(TREE, levels = 1:4, labels = LABELS)
seqIplot(DATA01, with.legend="right", group = TREE01)

#Who is in which cluster?
DATA$CLUSTERS <- TREE 
crosstab(DATA$CLUSTERS, DATA$GENDER, prop.c = T)

library(ggplot2) 
TABLE1 <- table(DATA$CLUSTERS, DATA$GENDER) 
TABLE2 <- prop.table(TABLE1,2) 
TABLE3 <- as.data.frame(TABLE2) 
ggplot(TABLE3, aes(fill=Var1, x=Var2, y=Freq))+ 
  geom_bar(stat="identity")+ 
  ylim(0,1)+ 
  ggtitle("")+ 
  ylab("%")+ xlab("")

# Describe cluster membership by suitable covariates
seqdplot(DATA01, xaxis=FALSE, group =DATA$GENDER) 
axis(1, at=c(0,5), labels=c("20","25"))