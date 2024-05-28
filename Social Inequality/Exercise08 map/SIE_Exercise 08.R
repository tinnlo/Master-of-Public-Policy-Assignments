library(descr) 
library(haven) 
library(RColorBrewer) 
library(sf)

#### Step 1: data preparation ####
rm(list=ls())

#Read in data 
DATA_MAP <-read_sf("NUTS_RG_01M_2021_3035.shp")

# LEVL_CODE == 0 means country level
DATA_MAP <- subset(DATA_MAP, LEVL_CODE==0) 
# select columns
DATA_MAP <- subset(DATA_MAP, select=c(CNTR_CODE, geometry))

#plot(DATA_MAP)


# share of foreign-born
DATA_MAP$SFB <- -1
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "BE"] <- 8  # Belgium
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "CZ"] <- 2  # Czechia
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "DK"] <- 5  # Denmark
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "DE"] <- 9  # Germany
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "IE"] <- 7  # Ireland
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "ES"] <- 4  # Spain
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "CY"] <- 9  # Cyprus (assuming "CY" is the code)
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "LV"] <- 24 # Latvia
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "LT"] <- 1  # Lithuania
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "LU"] <- 38 # Luxembourg
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "HU"] <- 1  # Hungary
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "MT"] <- 2  # Malta
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "NL"] <- 4  # Netherlands
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "AT"] <- 9  # Austria
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "PL"] <- 0  # Poland
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "PT"] <- 2  # Portugal
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "SI"] <- 2  # Slovenia
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "FI"] <- 2  # Finland
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "SE"] <- 5  # Sweden
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "IS"] <- 3  # Iceland (assuming "IS" is the code)
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "NO"] <- 4  # Norway
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "CH"] <- 20 # Switzerland

DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "IT"] <- 2  # Italy
DATA_MAP$SFB[DATA_MAP$CNTR_CODE == "FR"] <- 6  # France


# Note: This approach is less efficient for large datasets. Consider vectorization for better performance.

DATA_MAP <- subset(DATA_MAP, select = -CNTR_CODE)

# delete the obersevation with missing values in SFB
#DATA_MAP <- subset(DATA_MAP, DATA_MAP$SFB>=0)

#Plot 
COLOR <- c("white", "blue1", "cadetblue", "red")
BREAKS <- c(-1,0, 5, 10) 
COLOR01 <- COLOR[findInterval(DATA_MAP$SFB, vec = BREAKS)] 
plot(DATA_MAP, col = COLOR01, main = "Share of Foreign Born Map 2002")

# Add legend 
legend(x="topright", legend =c("missing", "0-5%","5-10%","10+"), fill = COLOR, cex = 0.5)
