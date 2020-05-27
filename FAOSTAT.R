setwd("C:/Users/Valentin/Desktop/THESIS_VALENTIN")

library(readxl)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(xlsx)

# Import FAOSTAT data

FAOSTAT <- read_excel("FAOSTAT.xlsx")
FAOSTAT <- subset(FAOSTAT, COUNTRY != "Africa")
FAOSTAT <- subset(FAOSTAT, GROUP != "TOTAL")
FAOSTAT$COUNTRY <- factor(FAOSTAT$COUNTRY)
FAOSTAT$GROUP <- factor(FAOSTAT$GROUP)

ggplot(FAOSTAT, aes(x = YEAR, y = CONSUMPTION, fill = GROUP)) + 
  geom_bar(position = "dodge", stat = "identity")

FAOSTAT_RESULTS <- subset(FAOSTAT, COUNTRY =="Ghana"|
                         COUNTRY=="Nigeria"|
                         COUNTRY=="Senegal"|
                         COUNTRY=="Togo"|
                         COUNTRY=="Tunisia"|
                         COUNTRY=="Africa")

ggplot(FAOSTAT, aes(x = YEAR, y = CONSUMPTION, fill = COUNTRY)) + 
  geom_bar(position = "dodge", stat = "identity")+
  labs(y="CONSUMPTION (kg/capita/year)")

# The CONSUMPTION rate seems more or less stable over time, for this reason we will use only the last year (2013)
FAOSTAT_REDUCED <- subset(FAOSTAT, YEAR == "2013")

# Import METAL_CONTAMINATION_SORTED data
METAL_CONTAMINATION_AFRICA <- read_excel("METAL_CONTAMINATION_AFRICA.xlsx", 
                                         sheet = "SORTED")
METAL_CONTAMINATION_AFRICA$METAL <- factor(METAL_CONTAMINATION_AFRICA$METAL)
METAL_CONTAMINATION_AFRICA$COUNTRY <- factor(METAL_CONTAMINATION_AFRICA$COUNTRY)

##############################################################
# Merge dataset
METAL_CONTAMINATION_AFRICA_REDUCED <- subset(METAL_CONTAMINATION_AFRICA, YEAR > 2000)

DATA <- merge.data.frame(FAOSTAT, METAL_CONTAMINATION_AFRICA, by = c("COUNTRY","GROUP","YEAR"))
attach(DATA)
DATABIS <- merge(FAOSTAT_REDUCED, METAL_CONTAMINATION_AFRICA_REDUCED, by=c("COUNTRY", "GROUP"))
attach(DATABIS)

##############################################################
# Non temporal analysis (DATABIS - 2013 consumption value)

THQ_data2013 <- do.call(data.frame, aggregate(DATABIS[,"VALUE"],list(DATABIS$COUNTRY, DATABIS$GROUP, DATABIS$METAL, DATABIS$CONSUMPTION), function(x) c(median=median(x),mean=mean(x),sd=sd(x),min=min(x),max=max(x))))

colnames(THQ_data2013)[colnames(THQ_data2013)=="Group.1"] <- "COUNTRY"
colnames(THQ_data2013)[colnames(THQ_data2013)=="Group.2"] <- "GROUP"
colnames(THQ_data2013)[colnames(THQ_data2013)=="Group.3"] <- "METAL"
colnames(THQ_data2013)[colnames(THQ_data2013)=="Group.4"] <- "CONSUMPTION"
colnames(THQ_data2013)[colnames(THQ_data2013)=="x.median"] <- "MEDIAN"
colnames(THQ_data2013)[colnames(THQ_data2013)=="x.mean"] <- "MEAN_CONT"
colnames(THQ_data2013)[colnames(THQ_data2013)=="x.sd"] <- "SD"
colnames(THQ_data2013)[colnames(THQ_data2013)=="x.min"] <- "MIN"
colnames(THQ_data2013)[colnames(THQ_data2013)=="x.max"] <- "MAX"
THQ_data2013$COUNTRY <- factor(THQ_data2013$COUNTRY)
THQ_data2013$GROUP <- factor(THQ_data2013$GROUP)
THQ_data2013$METAL <- factor(THQ_data2013$METAL)

# Calculate THQ

# Create EF (Exposure Frequency) -> 365
THQ_data2013$EF <- 365

# Create ED (Exposure Duration) -> average life span per country (data from WHO 2016)
THQ_data2013$EDm <- 0
THQ_data2013$EDf <- 0

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Algeria"] <- 75.5
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Algeria"] <- 77.9

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Cameroon"] <- 57.7
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Cameroon"] <- 60.2

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Democratic Republic of the Congo"] <- 58.9
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Democratic Republic of the Congo"] <- 61.9

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Egypt"] <- 69.6
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Egypt"] <- 74.2

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Ethiopia"] <- 64.4
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Ethiopia"] <- 68.2

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Ghana"] <- 62.7
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Ghana"] <- 64.9

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Ivory Coast"] <- 56.3
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Ivory Coast"] <- 58.7

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Kenya"] <- 64
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Kenya"] <- 68.7

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Lesotho"] <- 50.6
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Lesotho"] <- 57

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Mauritania"] <- 63.1
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Mauritania"] <- 66.3

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Mauritius"] <- 71.5
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Mauritius"] <- 78.4

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Morocco"] <- 75.2
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Morocco"] <- 77.7

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Mozambique"] <- 57.1
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Mozambique"] <- 63

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Nigeria"] <- 53.5
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Nigeria"] <- 55.2

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Reunion"] <- 79.54
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Reunion"] <- 82.9

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Senegal"] <- 66.8
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Senegal"] <- 66.8

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Seychelles"] <- 73.3
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Seychelles"] <- 73.3

THQ_data2013$EDm[THQ_data2013$COUNTRY == "South Africa"] <- 65.5
THQ_data2013$EDf[THQ_data2013$COUNTRY == "South Africa"] <- 69.6

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Tanzania"] <- 63.2
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Tanzania"] <- 66.8

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Togo"] <- 59.9
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Togo"] <- 61.6

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Tunisia"] <- 75.7
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Tunisia"] <- 77.4

THQ_data2013$EDm[THQ_data2013$COUNTRY == "Zimbabwe"] <- 59.5
THQ_data2013$EDf[THQ_data2013$COUNTRY == "Zimbabwe"] <- 62.6

# Create FIR (Fresh Food Ingestion) -> convert CONSUMPTION (kg/per/year) to FIR (g/per/day)
THQ_data2013$FIR <- (THQ_data2013$CONSUMPTION/365)*1000

# Create RfD (Reference Oral Dose) -> from WHO/FAO/UE
THQ_data2013$RfD[THQ_data2013$METAL == "As"] <- 0.0003
THQ_data2013$RfD[THQ_data2013$METAL == "Cd"] <- 0.001
THQ_data2013$RfD[THQ_data2013$METAL == "Co"] <- NA
THQ_data2013$RfD[THQ_data2013$METAL == "Cr"] <- 0.003
THQ_data2013$RfD[THQ_data2013$METAL == "Cu"] <- 0.04
THQ_data2013$RfD[THQ_data2013$METAL == "Hg"] <- 0.0001
THQ_data2013$RfD[THQ_data2013$METAL == "Ni"] <- 0.02
THQ_data2013$RfD[THQ_data2013$METAL == "Pb"] <- 0.004
THQ_data2013$RfD[THQ_data2013$METAL == "Zn"] <- 0.3

# Create W (average body weight in africa) -> 60.7kg
THQ_data2013$W <- 60.7

# Create Ta (average exposure time) -> EF*ED
THQ_data2013$Tam <- THQ_data2013$EF*THQ_data2013$EDm
THQ_data2013$Taf <- THQ_data2013$EF*THQ_data2013$EDf

# Calculate THQ for male (THQm=(EF*ED*FIR*MEAN_CONT)/(RfD*W*Ta))
THQ_data2013$THQm <-((THQ_data2013$EF*THQ_data2013$EDm*THQ_data2013$FIR*THQ_data2013$MEAN_CONT)/(THQ_data2013$RfD*THQ_data2013$W*THQ_data2013$Tam))*10^-3

# Calculate THQ for female (THQf=(EF*ED*FIR*MEAN_CONT)/(RfD*W*Ta))
THQ_data2013$THQf <-((THQ_data2013$EF*THQ_data2013$EDf*THQ_data2013$FIR*THQ_data2013$MEAN_CONT)/(THQ_data2013$RfD*THQ_data2013$W*THQ_data2013$Taf))*10^-3

# Calculate TTHQm2013
(TTHQm2013 <- aggregate(THQ_data2013[,"THQm"], list(THQ_data2013$COUNTRY), na.rm=TRUE, sum))
as.data.frame(TTHQm2013)
colnames(TTHQm2013)[colnames(TTHQm2013)=="Group.1"] <- "COUNTRY"
colnames(TTHQm2013)[colnames(TTHQm2013)=="x"] <- "TTHQm2013"
attach(TTHQm2013)

# Calculate TTHQf2013
(TTHQf2013 <- aggregate(THQ_data2013[,"THQf"], list(THQ_data2013$COUNTRY), na.rm=TRUE, sum))
as.data.frame(TTHQf2013)
colnames(TTHQf2013)[colnames(TTHQf2013)=="Group.1"] <- "COUNTRY"
colnames(TTHQf2013)[colnames(TTHQf2013)=="x"] <- "TTHQf2013"
attach(TTHQf2013)

# Export data frame
write.xlsx(THQ_data2013, file = "FAOSTAT_RESULTS_2013.xlsx", 
           sheetName = "RESULTS",
           col.names = TRUE, 
           row.names = FALSE, 
           append = FALSE)

write.xlsx(TTHQm2013, file = "TTHQm2013.xlsx", 
           sheetName = "TTHQm2013",
           col.names = TRUE, 
           row.names = FALSE, 
           append = FALSE)

write.xlsx(TTHQf2013, file = "TTHQf2013.xlsx", 
           sheetName = "TTHQf2013",
           col.names = TRUE, 
           row.names = FALSE, 
           append = FALSE)

##############################################################
# Plot

# TTHQm
TTHQ_m_2013 <- aggregate(THQ_data2013[,"THQm"], list(THQ_data2013$COUNTRY, THQ_data2013$METAL), na.rm=TRUE, sum)

ggplot(TTHQ_m_2013, aes(x=Group.1, y=x, fill = Group.2)) + 
  geom_bar(stat="identity", position = "identity", color="black")+
  coord_flip()+
  ggtitle("THQs value of metals for male from different African country in 2013")+
  labs(y="THQ", x="COUNTRY", fill="Metal")

ggplot(TTHQm2013, aes(x=COUNTRY, y=TTHQm2013)) + 
  geom_bar(stat = "identity", color="black") + 
  coord_flip() +
  ggtitle("Total THQs value per African country in 2013 for male")+
  labs(y="Total Target Hazard Quotient")+
  geom_hline(yintercept=1, linetype="dashed", color="red")
  
# TTHQf
TTHQ_f_2013 <- aggregate(THQ_data2013[,"THQf"], list(THQ_data2013$COUNTRY, THQ_data2013$METAL), na.rm=TRUE, sum)

ggplot(TTHQ_f_2013, aes(x=Group.1, y=x, fill = Group.2)) + 
  geom_bar(stat="identity", color="black")+
  coord_flip()+
  ggtitle("THQs value of metals for female from different African country in 2013")+
  labs(y="THQ", x="COUNTRY", fill="Metal")

ggplot(TTHQf2013, aes(x=COUNTRY, y=TTHQf2013)) + 
  geom_bar(stat = "identity", color="black") + 
  coord_flip() +
  ggtitle("Total THQs value per African country in 2013 for female")+
  labs(y="Total Target Hazard Quotient")+
  geom_hline(yintercept=1, linetype="dashed", color="red")

##############################################################
# Test for difference between male and female

# Test normality
ggqqplot(my_data$len)

shapiro.test(THQ_data2013$THQm)
# p-value ... 0.05 the distribution of the data is significantly similar/different from the normal distribution
shapiro.test(THQ_data2013$THQf)
# p-value ... 0.05 the distribution of the data is significantly similar/different from the normal distribution

# Test homoscedasticity
bartlett.test(THQ_data2013$THQm, THQ_data2013$THQf)

##############################################################
##############################################################
##############################################################
# Temporal analysis (DATA)
THQ_data <- aggregate(DATA[,"VALUE"], list(DATA$COUNTRY, DATA$GROUP, DATA$YEAR, DATA$METAL, DATA$CONSUMPTION), median)
as.data.frame(THQ_data)

colnames(THQ_data)[colnames(THQ_data)=="Group.1"] <- "COUNTRY"
colnames(THQ_data)[colnames(THQ_data)=="Group.2"] <- "GROUP"
colnames(THQ_data)[colnames(THQ_data)=="Group.3"] <- "YEAR"
colnames(THQ_data)[colnames(THQ_data)=="Group.4"] <- "METAL"
colnames(THQ_data)[colnames(THQ_data)=="Group.5"] <- "CONSUMPTION"
colnames(THQ_data)[colnames(THQ_data)=="x"] <- "MEAN_CONT"
THQ_data$COUNTRY <- factor(THQ_data$COUNTRY)
THQ_data$GROUP <- factor(THQ_data$GROUP)
THQ_data$METAL <- factor(THQ_data$METAL)

# Calculate THQ

# Create EF (Exposure Frequency) -> 365
THQ_data$EF <- 365

# Create ED (Exposure Duration) -> average life span per country (data from WHO 2016)
THQ_data$ED <- 0
THQ_data$ED[THQ_data$COUNTRY == "Algeria" & THQ_data$YEAR < 2014] <- 75.27
THQ_data$ED[THQ_data$COUNTRY == "Algeria" & THQ_data$YEAR < 2011] <- 73.88
THQ_data$ED[THQ_data$COUNTRY == "Algeria" & THQ_data$YEAR < 2006] <- 71.50

THQ_data$ED[THQ_data$COUNTRY == "Egypt" & THQ_data$YEAR < 2014] <- 70.84
THQ_data$ED[THQ_data$COUNTRY == "Egypt" & THQ_data$YEAR < 2011] <- 69.87
THQ_data$ED[THQ_data$COUNTRY == "Egypt" & THQ_data$YEAR < 2006] <- 68.99

THQ_data$ED[THQ_data$COUNTRY == "Ethiopia" & THQ_data$YEAR < 2014] <- 63.69
THQ_data$ED[THQ_data$COUNTRY == "Ethiopia" & THQ_data$YEAR < 2011] <- 59.08
THQ_data$ED[THQ_data$COUNTRY == "Ethiopia" & THQ_data$YEAR < 2006] <- 53.61

THQ_data$ED[THQ_data$COUNTRY == "Ghana" & THQ_data$YEAR < 2014] <- 61.68
THQ_data$ED[THQ_data$COUNTRY == "Ghana" & THQ_data$YEAR < 2011] <- 60.03
THQ_data$ED[THQ_data$COUNTRY == "Ghana" & THQ_data$YEAR < 2006] <- 57.49

THQ_data$ED[THQ_data$COUNTRY == "Kenya" & THQ_data$YEAR < 2014] <- 65.40 
THQ_data$ED[THQ_data$COUNTRY == "Kenya" & THQ_data$YEAR < 2011] <- 59.72
THQ_data$ED[THQ_data$COUNTRY == "Kenya" & THQ_data$YEAR < 2006] <- 52.73

THQ_data$ED[THQ_data$COUNTRY == "Mauritania" & THQ_data$YEAR < 2014] <- 62.64
THQ_data$ED[THQ_data$COUNTRY == "Mauritania" & THQ_data$YEAR < 2011] <- 61.32
THQ_data$ED[THQ_data$COUNTRY == "Mauritania" & THQ_data$YEAR < 2006] <- 60.27

THQ_data$ED[THQ_data$COUNTRY == "Morocco" & THQ_data$YEAR < 2014] <- 74.87
THQ_data$ED[THQ_data$COUNTRY == "Morocco" & THQ_data$YEAR < 2011] <- 72.88
THQ_data$ED[THQ_data$COUNTRY == "Morocco" & THQ_data$YEAR < 2006] <- 69.95

THQ_data$ED[THQ_data$COUNTRY == "Mozambique" & THQ_data$YEAR < 2014] <- 56.08
THQ_data$ED[THQ_data$COUNTRY == "Mozambique" & THQ_data$YEAR < 2011] <- 53.24
THQ_data$ED[THQ_data$COUNTRY == "Mozambique" & THQ_data$YEAR < 2006] <- 49.56

THQ_data$ED[THQ_data$COUNTRY == "Nigeria" & THQ_data$YEAR < 2014] <- 51.88
THQ_data$ED[THQ_data$COUNTRY == "Nigeria" & THQ_data$YEAR < 2011] <- 49.75
THQ_data$ED[THQ_data$COUNTRY == "Nigeria" & THQ_data$YEAR < 2006] <- 46.94

THQ_data$ED[THQ_data$COUNTRY == "Senegal" & THQ_data$YEAR < 2014] <- 65.71
THQ_data$ED[THQ_data$COUNTRY == "Senegal" & THQ_data$YEAR < 2011] <- 62.41
THQ_data$ED[THQ_data$COUNTRY == "Senegal" & THQ_data$YEAR < 2006] <- 58.93

THQ_data$ED[THQ_data$COUNTRY == "Tanzania" & THQ_data$YEAR < 2014] <- 62.78
THQ_data$ED[THQ_data$COUNTRY == "Tanzania" & THQ_data$YEAR < 2011] <- 58.82
THQ_data$ED[THQ_data$COUNTRY == "Tanzania" & THQ_data$YEAR < 2006] <- 53.65

THQ_data$ED[THQ_data$COUNTRY == "Togo" & THQ_data$YEAR < 2014] <- 59.07
THQ_data$ED[THQ_data$COUNTRY == "Togo" & THQ_data$YEAR < 2011] <- 55.80
THQ_data$ED[THQ_data$COUNTRY == "Togo" & THQ_data$YEAR < 2006] <- 53.88

THQ_data$ED[THQ_data$COUNTRY == "Tunisia" & THQ_data$YEAR < 2014] <- 75.04
THQ_data$ED[THQ_data$COUNTRY == "Tunisia" & THQ_data$YEAR < 2011] <- 74.56
THQ_data$ED[THQ_data$COUNTRY == "Tunisia" & THQ_data$YEAR < 2006] <- 73.70

# Create FIR (Fresh Food Ingestion) -> convert CONSUMPTION (kg/per/year) to FIR (g/per/day)
THQ_data$FIR <- (THQ_data$CONSUMPTION/365)*1000

# Create RfD (Reference Oral Dose) -> from WHO/FAO/UE
THQ_data$RfD[THQ_data$METAL == "As"] <- 0.0003
THQ_data$RfD[THQ_data$METAL == "Cd"] <- 0.001
THQ_data$RfD[THQ_data$METAL == "Co"] <- NA
THQ_data$RfD[THQ_data$METAL == "Cr"] <- 0.003
THQ_data$RfD[THQ_data$METAL == "Cu"] <- NA
THQ_data$RfD[THQ_data$METAL == "Hg"] <- NA
THQ_data$RfD[THQ_data$METAL == "Ni"] <- 0.02
THQ_data$RfD[THQ_data$METAL == "Pb"] <- NA
THQ_data$RfD[THQ_data$METAL == "Zn"] <- 0.3

# Create W (average body weight in africa) -> 60.7kg
THQ_data$W <- 60.7

# Create Ta (average exposure time) -> EF*ED
THQ_data$Ta <- THQ_data$EF*THQ_data$ED

# Calculate THQ (THQ=(EF*ED*FIR*MEAN_CONT)/(RfD*W*Ta))
THQ_data$THQ <-((THQ_data$EF*THQ_data$ED*THQ_data$FIR*THQ_data$MEAN_CONT)/(THQ_data$RfD*THQ_data$W*THQ_data$Ta))*10^-3

# Calculate TTHQ
(TTHQ <- aggregate(THQ_data[,"THQ"], list(THQ_data$COUNTRY, THQ_data$YEAR), na.rm=TRUE, sum))
as.data.frame(TTHQ)
colnames(TTHQ)[colnames(TTHQ)=="Group.1"] <- "COUNTRY"
colnames(TTHQ)[colnames(TTHQ)=="Group.2"] <- "YEAR"
colnames(TTHQ)[colnames(TTHQ)=="x"] <- "TTHQ"

# Export data frame
write.xlsx(THQ_data, file = "FAOSTAT_RESULTS.xlsx", 
           sheetName = "RESULTS",
           col.names = TRUE, 
           row.names = FALSE, 
           append = FALSE)

write.xlsx(TTHQ, file = "TTHQ.xlsx", 
           sheetName = "TTHQ",
           col.names = TRUE, 
           row.names = FALSE, 
           append = FALSE)

##############################################################
# Plot

# TTHQ
TTHQ <- subset(TTHQ, COUNTRY!="Togo" & YEAR!="2011")

ggplot(TTHQ, aes(x=YEAR, y=TTHQ, group = COUNTRY)) + 
  geom_line(aes(color=COUNTRY))+
  geom_point(aes(color=COUNTRY))+
  labs(title="Evolution of the TTHQs over years for metal in different African countries")

ggplot(TTHQ, aes(x=YEAR, y=TTHQ, fill=COUNTRY)) + 
  geom_bar(stat = "identity", width=0.5, position = "dodge", color="black") + 
  ggtitle("TTHQ in African countries from 2000 to 2013")+
  labs(y="TTHQ")+
  geom_hline(yintercept=1, linetype="dashed", color="black")
