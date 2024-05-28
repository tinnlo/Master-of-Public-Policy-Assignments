library(haven)
library(TraMineR)
library(tidyverse)
library(reshape2)
library(cluster)
library(WeightedCluster)
library(janitor)
library(descr)
library(kableExtra)
library(ggseqplot)


# data --------------------------------------------------------------------

DATA <- read_dta("data/SELF.dta")

tabyl(DATA$SELF2020)


# prepare -----------------------------------------------------------------


# VAR: EMP
DATA01 <- DATA |> 
  mutate(SELF2016_new = case_when(
    SELF2016 == "0-Self-employed" ~ "0-Self-employed",
    SELF2016 == "1-Employed full time" | SELF2016 == "2-Employed part time" ~ "1-Employed",
    SELF2016 == "3-Education" | SELF2016 == "4-Not working/unemployed" ~ "3-Not Working",
    TRUE ~ NA_character_)
  ) |> 
  mutate(SELF2016_new = factor(SELF2016_new, levels = c("0-Self-employed", "1-Employed", "3-Not Working")))

# VAR: EMP
DATA01 <- DATA01 |> 
  mutate(SELF2017_new = case_when(
    SELF2017 == "0-Self-employed" ~ "0-Self-employed",
    SELF2017 == "1-Employed full time" | SELF2017 == "2-Employed part time" ~ "1-Employed",
    SELF2017 == "3-Education" | SELF2017 == "4-Not working/unemployed" ~ "3-Not Working",
    TRUE ~ NA_character_)
  ) |> 
  mutate(SELF2017_new = factor(SELF2017_new, levels = c("0-Self-employed", "1-Employed", "3-Not Working")))

# VAR: EMP
DATA01 <- DATA01 |> 
  mutate(SELF2018_new = case_when(
    SELF2018 == "0-Self-employed" ~ "0-Self-employed",
    SELF2018 == "1-Employed full time" | SELF2018 == "2-Employed part time" ~ "1-Employed",
    SELF2018 == "3-Education" | SELF2018 == "4-Not working/unemployed" ~ "3-Not Working",
    TRUE ~ NA_character_)
  ) |> 
  mutate(SELF2018_new = factor(SELF2018_new, levels = c("0-Self-employed", "1-Employed", "3-Not Working")))

# VAR: EMP
DATA01 <- DATA01 |> 
  mutate(SELF2019_new = case_when(
    SELF2019 == "0-Self-employed" ~ "0-Self-employed",
    SELF2019 == "1-Employed full time" | SELF2019 == "2-Employed part time" ~ "1-Employed",
    SELF2019 == "3-Education" | SELF2019 == "4-Not working/unemployed" ~ "3-Not Working",
    TRUE ~ NA_character_)
  ) |> 
  mutate(SELF2019_new = factor(SELF2019_new, levels = c("0-Self-employed", "1-Employed", "3-Not Working")))

# VAR: EMP
DATA01 <- DATA01 |> 
  mutate(SELF2020_new = case_when(
    SELF2020 == "0-Self-employed" ~ "0-Self-employed",
    SELF2020 == "1-Employed full time" | SELF2020 == "2-Employed part time" ~ "1-Employed",
    SELF2020 == "3-Education" | SELF2020 == "4-Not working/unemployed" ~ "3-Not Working",
    TRUE ~ NA_character_)
  ) |> 
  mutate(SELF2020_new = factor(SELF2020_new, levels = c("0-Self-employed", "1-Employed", "3-Not Working")))

DATA02 <- DATA01 |> 
  select(-SELF2016, -SELF2017, -SELF2018, -SELF2019, -SELF2020)


# analysis ----------------------------------------------------------------

shortlab <- c("SEMP", "EMP", "UNEMP")
longlab <- c("Self-employment" , "Employment" , "Unemployment")

# create sequence
DATA02 <- seqdef(DATA02[,7:11], states = shortlab, labels = longlab)

# colors
cpal(DATA02) <- c("#99CFE0","#ef6900", "#800000")

# first two plots 
seqIplot(DATA02, with.legend = "right")
seqdplot(DATA02, border = F, with.legend = "right")

# formated plot 1
seqIplot(DATA02,xt = c(1:5),xlab = "Time point", ylab = " # sequences (n = 400)", with.legend = "right") 

# formated plot 2
seqdplot(DATA02,xt = c(1:5),xlab = "Time point", ylab = "Rel. Frequency", with.legend = "right") 


# next part ---------------------------------------------------------------

# cluster
DISTANCE <- seqdist(DATA02, method = "LCS")

CLUSTER <- hclust(as.dist(DISTANCE), method = "ward.D")

WARDRANGE <- as.clustrange(CLUSTER, diss = DISTANCE, ncluster = 10)

plot(WARDRANGE, stat = c("ASW", "ASWw"))

# plot 
# TREE <- cutree(CLUSTER, k = 4)
# LABELS <- c("Lone Wolves", "Switchers", "Security Seekers", "Vulnerables")
# TREE1 <- factor(TREE, levels = 1:4, labels = LABELS)
TREE <- cutree(CLUSTER, k = 2)
LABELS <- c("Lone Wolves", "Security Seekers")
TREE1 <- factor(TREE, levels = 1:2, labels = LABELS)

# 1 - seqi
seqiplot(DATA02, group = TREE1)
seqiplot(DATA02, group = TREE1, xt = c("2016", "2017", "2018", "2019", "2020"), xlab = "Time point", ylab = "# sequences", with.legend = "right", border = FALSE)

# 2 - seqd
seqdplot(DATA02, xt = c(1:5),xlab = "Time point",  ylab = "Relative Frequency", with.legend = "right", group = TREE1)

# 3 - mean time spent plot
seqmtplot(DATA02, group = TREE1, ylim = c(0, 5), with.legend = "right")

# percentage in each cluster ----------------------------------------------
# distributions --------------------------------------------------------

DATA$CLUSTERS <- TREE
crosstab(DATA$CLUSTERS, DATA$GENDER, prop.c = T)


# prop table
percentages <- round(prop.table(table(DATA$CLUSTERS)) * 100, 2)
result_df <- data.frame(Cluster = names(percentages), Percentage = paste(percentages, "%", sep = ""))

result_df <- kable(result_df, format = "html") %>%
  kable_styling()



# plot by gender
TABLE1 <- table(DATA$CLUSTERS, DATA$GENDER)
TABLE2 <- prop.table(TABLE1,2)
TABLE3 <- as.data.frame(TABLE2)

custom_colors <- c("#99CFE0", "#ef6900", "#FFD700", "#800000")

ggplot(TABLE3, aes(fill = Var1, x = Var2, y = Freq)) + 
  geom_bar(stat = "identity") + ylim(0,1)+ ggtitle("") + 
  ylab("Share") + 
  ggtitle("Cluster Affiliation by Gender") +
  xlab("") +
  scale_fill_manual(values = custom_colors, name = "") +  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 20, t = 10))  # Make the title bold
  ) 

# education ---------------------------------------------------------------

# plot by group
TABLE1 <- table(DATA$CLUSTERS, DATA$EDUCAT)
TABLE2 <- prop.table(TABLE1,2)
TABLE3 <- as.data.frame(TABLE2)

custom_colors <- c("#99CFE0", "#ef6900", "#FFD700", "#800000")

ggplot(TABLE3, aes(fill = Var1, x = Var2, y = Freq)) + 
  geom_bar(stat = "identity")+ ylim(0,1)+ ggtitle("") + 
  ylab("Share") + 
  ggtitle("Cluster Affiliation by Highest Level of Education") +
  xlab("") +
  scale_fill_manual(values = custom_colors, name = "") +  
  scale_x_discrete(labels = c("In Education", "Low Education", "Vocational T.", "Tertiary Education")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 20, t = 10))  # Make the title bold
  ) 


# changes -------------------------------------------------------------------
# x-axes 
# calculate the percentage in each group 
# sequence index and distribution plot


# logit -------------------------------------------------------------------

TREE <- cutree(CLUSTER, k = 2) 
LABELS <- c("Commited Self-Employed", "Switchers") 
TREE01 <- factor(TREE, levels = 1:2, labels = LABELS) 
seqIplot(DATA02,  border = T, with.legend = "right", group = TREE01)

# plot by goups
TABLE1 <- table(DATA$CLUSTERS, DATA$REGION)
TABLE2 <- prop.table(TABLE1,2)
TABLE3 <- as.data.frame(TABLE2)

custom_colors <- c("#99CFE0", "#ef6900")

ggplot(TABLE3, aes(fill = Var1, x = Var2, y = Freq)) + 
  geom_bar(stat = "identity")+ ylim(0,1)+ ggtitle("") + 
  ylab("Share") + 
  ggtitle("Cluster Affiliation by Region") +
  xlab("") +
  scale_fill_manual(values = custom_colors, name = "") +  
  scale_x_discrete(labels = c("OST", "WEST")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", margin = margin(b = 20, t = 10))  # Make the title bold
  )

