rm(list=ls())

setwd("C:/Users/Valentin/Desktop/THESIS_VALENTIN")

library(corrplot)
library(devtools)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(graphics)
library(ggmap)
library(reshape2)
library(readr)
library(readxl)
library(tidyverse)
library(viridis)

##############################################################
# Import data
META_L_ANALYSIS <- read_excel("METAL_CONTAMINATION_AFRICA.xlsx", sheet = "RAW",
                              col_types = c("text", "text", "numeric", 
                                            "numeric", "numeric", "text", "text", "text", 
                                            "text", "text", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "text", "text", "text"))

##############################################################
# Rearrenge data
METAL_ANALYSIS <- melt(META_L_ANALYSIS, id=c('COUNTRY',"LOCATION",'LATITUDE','LONGITUDE','YEAR','CLASS','ORDER','FAMILY','SPECIES',"GROUP",'UNIT','REF','REFERENCE'), na.rm=FALSE)
METAL_ANALYSIS$UNIT <- as.factor(METAL_ANALYSIS$UNIT)

  # MEAN
MEAN <- subset(METAL_ANALYSIS, 
              variable == "As_MEAN"|
              variable == 'Cd_MEAN'|
              variable == 'Co_MEAN'|
              variable == 'Cr_MEAN'|
              variable == 'Cu_MEAN'|
              variable == 'Hg_MEAN'|
              variable == 'Ni_MEAN'|
              variable == 'Pb_MEAN'|
              variable == 'Zn_MEAN')
names(MEAN)[names(MEAN) == 'variable'] <- 'METAL'
names(MEAN)[names(MEAN) == 'value'] <- 'MEAN'
levels(MEAN$METAL)[levels(MEAN$METAL)=="As_MEAN"] <- "As"
levels(MEAN$METAL)[levels(MEAN$METAL)=="Cd_MEAN"] <- "Cd"
levels(MEAN$METAL)[levels(MEAN$METAL)=="Co_MEAN"] <- "Co"
levels(MEAN$METAL)[levels(MEAN$METAL)=="Cr_MEAN"] <- "Cr"
levels(MEAN$METAL)[levels(MEAN$METAL)=="Cu_MEAN"] <- "Cu"
levels(MEAN$METAL)[levels(MEAN$METAL)=="Hg_MEAN"] <- "Hg"
levels(MEAN$METAL)[levels(MEAN$METAL)=="Ni_MEAN"] <- "Ni"
levels(MEAN$METAL)[levels(MEAN$METAL)=="Pb_MEAN"] <- "Pb"
levels(MEAN$METAL)[levels(MEAN$METAL)=="Zn_MEAN"] <- "Zn"

  # SD
SD <- subset(METAL_ANALYSIS, 
               variable == "As_SD"|
                 variable == 'Cd_SD'|
                 variable == 'Co_SD'|
                 variable == 'Cr_SD'|
                 variable == 'Cu_SD'|
                 variable == 'Hg_SD'|
                 variable == 'Ni_SD'|
                 variable == 'Pb_SD'|
                 variable == 'Zn_SD')
names(SD)[names(SD) == 'variable'] <- 'METAL'
names(SD)[names(SD) == 'value'] <- 'SD'
levels(SD$METAL)[levels(SD$METAL)=="As_SD"] <- "As"
levels(SD$METAL)[levels(SD$METAL)=="Cd_SD"] <- "Cd"
levels(SD$METAL)[levels(SD$METAL)=="Co_SD"] <- "Co"
levels(SD$METAL)[levels(SD$METAL)=="Cr_SD"] <- "Cr"
levels(SD$METAL)[levels(SD$METAL)=="Cu_SD"] <- "Cu"
levels(SD$METAL)[levels(SD$METAL)=="Hg_SD"] <- "Hg"
levels(SD$METAL)[levels(SD$METAL)=="Ni_SD"] <- "Ni"
levels(SD$METAL)[levels(SD$METAL)=="Pb_SD"] <- "Pb"
levels(SD$METAL)[levels(SD$METAL)=="Zn_SD"] <- "Zn"

  # MIN
MIN <- subset(METAL_ANALYSIS, 
               variable == "As_MIN"|
                 variable == 'Cd_MIN'|
                 variable == 'Co_MIN'|
                 variable == 'Cr_MIN'|
                 variable == 'Cu_MIN'|
                 variable == 'Hg_MIN'|
                 variable == 'Ni_MIN'|
                 variable == 'Pb_MIN'|
                 variable == 'Zn_MIN')
names(MIN)[names(MIN) == 'variable'] <- 'METAL'
names(MIN)[names(MIN) == 'value'] <- 'MIN'
levels(MIN$METAL)[levels(MIN$METAL)=="As_MIN"] <- "As"
levels(MIN$METAL)[levels(MIN$METAL)=="Cd_MIN"] <- "Cd"
levels(MIN$METAL)[levels(MIN$METAL)=="Co_MIN"] <- "Co"
levels(MIN$METAL)[levels(MIN$METAL)=="Cr_MIN"] <- "Cr"
levels(MIN$METAL)[levels(MIN$METAL)=="Cu_MIN"] <- "Cu"
levels(MIN$METAL)[levels(MIN$METAL)=="Hg_MIN"] <- "Hg"
levels(MIN$METAL)[levels(MIN$METAL)=="Ni_MIN"] <- "Ni"
levels(MIN$METAL)[levels(MIN$METAL)=="Pb_MIN"] <- "Pb"
levels(MIN$METAL)[levels(MIN$METAL)=="Zn_MIN"] <- "Zn"

  # MAX
MAX <- subset(METAL_ANALYSIS, 
               variable == "As_MAX"|
                 variable == 'Cd_MAX'|
                 variable == 'Co_MAX'|
                 variable == 'Cr_MAX'|
                 variable == 'Cu_MAX'|
                 variable == 'Hg_MAX'|
                 variable == 'Ni_MAX'|
                 variable == 'Pb_MAX'|
                 variable == 'Zn_MAX')
names(MAX)[names(MAX) == 'variable'] <- 'METAL'
names(MAX)[names(MAX) == 'value'] <- 'MAX'
levels(MAX$METAL)[levels(MAX$METAL)=="As_MAX"] <- "As"
levels(MAX$METAL)[levels(MAX$METAL)=="Cd_MAX"] <- "Cd"
levels(MAX$METAL)[levels(MAX$METAL)=="Co_MAX"] <- "Co"
levels(MAX$METAL)[levels(MAX$METAL)=="Cr_MAX"] <- "Cr"
levels(MAX$METAL)[levels(MAX$METAL)=="Cu_MAX"] <- "Cu"
levels(MAX$METAL)[levels(MAX$METAL)=="Hg_MAX"] <- "Hg"
levels(MAX$METAL)[levels(MAX$METAL)=="Ni_MAX"] <- "Ni"
levels(MAX$METAL)[levels(MAX$METAL)=="Pb_MAX"] <- "Pb"
levels(MAX$METAL)[levels(MAX$METAL)=="Zn_MAX"] <- "Zn"

##############################################################
# Merge data
META_ANALYSIS <- MEAN
META_ANALYSIS$SD <- SD$SD
META_ANALYSIS$MIN <- MIN$MIN
META_ANALYSIS$MAX <- MAX$MAX

# Data clean up
META_ANALYSIS<- META_ANALYSIS[!is.na(META_ANALYSIS$MEAN)|
                        !is.na(META_ANALYSIS$SD)|
                        !is.na(META_ANALYSIS$MIN)|
                        !is.na(META_ANALYSIS$MAX), ]

META_ANALYSIS$MEAN <-as.numeric(as.character(META_ANALYSIS$MEAN))
META_ANALYSIS$SD <-as.numeric(as.character(META_ANALYSIS$SD))
META_ANALYSIS$MIN <-as.numeric(as.character(META_ANALYSIS$MIN))
META_ANALYSIS$MAX <-as.numeric(as.character(META_ANALYSIS$MAX))

# Rearrange data (create a column VALUE which is the highest value per raw)
META_ANALYSIS$VALUE <-ifelse(is.na(META_ANALYSIS$MAX), META_ANALYSIS$MEAN, META_ANALYSIS$MAX)
META_ANALYSIS$VALUE <-as.numeric(as.character(META_ANALYSIS$VALUE))
attach(META_ANALYSIS)
META_ANALYSIS$METAL <- factor(META_ANALYSIS$METAL)
META_ANALYSIS$CLASS <- factor(META_ANALYSIS$CLASS)
META_ANALYSIS <- subset(META_ANALYSIS, GROUP!="PLANTS")
META_ANALYSIS$GROUP <- factor(META_ANALYSIS$GROUP)

##############################################################
# Proportion & table
ggplot(META_ANALYSIS, aes(x = METAL))+
  geom_histogram(stat = "count")
table(META_ANALYSIS$METAL)
prop.table(table(META_ANALYSIS$METAL))*100 # in %

##############################################################
# Explore relation METAL~GROUP
prop.table(table(META_ANALYSIS$METAL, META_ANALYSIS$GROUP)*100)

(tab8<-table(META_ANALYSIS$METAL, META_ANALYSIS$GROUP))
dt8 <- as.table(as.matrix(tab8))
balloonplot(t(dt8), main="METAL", label = FALSE, show.margins = FALSE)

ggplot(META_ANALYSIS, aes(x = GROUP, fill = METAL))+
  geom_histogram(stat = "count")+
  ggtitle("Proportion of metal contaminant per group")+
  coord_flip()

ggplot(META_ANALYSIS, aes(x = GROUP, fill = METAL)) + 
  geom_bar(position = "fill")+
  ggtitle("Relative proportion of metal contaminant per group")

ggplot(META_ANALYSIS, aes(x=YEAR, y=VALUE, color=GROUP)) +
  geom_jitter(stat="identity", position=position_jitter(0.8)) +
  ylab("Metal concentration (mg/kg)")+
  xlab('Year')+
  ggtitle("Trace metal contamination in different group from 1974 to 2017") + 
  facet_wrap(~ METAL)

ggplot(META_ANALYSIS, aes(x=YEAR, y=VALUE, color=METAL)) +
  geom_jitter(stat="identity", position=position_jitter(0.8)) +
  scale_color_manual(values=c("#151515","#1C1C1C","#2E2E2E","#424242","#585858","#6E6E6E","#848484","#A4A4A4","#BDBDBD"))+
  scale_y_sqrt() +
  ylab("Metal concentration (mg/g w.w)")+
  xlab('Year')+
  geom_hline(yintercept=0.3, linetype="dashed", color="#A4A4A4")+
  geom_hline(yintercept=0.5, linetype="dotdash", color ="#6E6E6E")+
  geom_hline(yintercept=0.05, linetype="longdash", color ="#1C1C1C")+
  geom_hline(yintercept=0.3, linetype="solid", color ="#151515")

ggplot(META_ANALYSIS, aes(x=YEAR, y=VALUE, color=METAL, log="y")) +
  geom_jitter(stat="identity") +
  ylab("Metal concentration (mg/kg)")+
  xlab('Year')+
  ggtitle("Trace metal contamination in different group from 1974 to 2017")+
  facet_wrap(~ GROUP)+
  scale_y_sqrt()

(chisq8<- chisq.test(META_ANALYSIS$GROUP, META_ANALYSIS$METAL)) 
# p-value < 0.05 so the two variables are significantly associated
# The amount off contamination for each metal might be linked with the organism group

chisq8$observed
chisq8$expected
chisq8$residuals
contrib8 <- 100*chisq8$residuals^2/chisq8$statistic
round(contrib8, 3)
corrplot(chisq8$residuals, is.cor = FALSE, method = "color", insig = "blank")
corrplot(chisq8$residuals, is.cor = FALSE, method = "number", insig = "blank")
corrplot(contrib8, method = "color", is.cor = FALSE)
# shows which couple have the highest absolute contribution to the strenght of the relationship between METAL and FAMILY

##############################################################
# Explore relation METAL~CLASS
prop.table(table(META_ANALYSIS$METAL, META_ANALYSIS$CLASS)*100)

(tab3<-table(META_ANALYSIS$METAL, META_ANALYSIS$CLASS))
dt3 <- as.table(as.matrix(tab3))
balloonplot(t(dt3), main="METAL", label = FALSE, show.margins = FALSE)

ggplot(META_ANALYSIS, aes(x = CLASS, fill = METAL))+
  geom_histogram(stat = "count")+
  ggtitle("Real proportion of metal contaminant per class")+
  coord_flip()

ggplot(META_ANALYSIS, aes(x = CLASS, fill = METAL)) + 
  geom_bar(position = "fill")+
  ggtitle("Relative proportion of metal contaminant per class")

(chisq3<- chisq.test(META_ANALYSIS$CLASS, META_ANALYSIS$METAL)) 
# p-value < 0.05 so the two variables are significantly associated
# The amount off contamination for each metal might be linked with the CLASS

chisq3$observed
chisq3$expected
chisq3$residuals
contrib3 <- 100*chisq3$residuals^2/chisq3$statistic
round(contrib3, 3)
corrplot(chisq3$residuals, is.cor = FALSE, method = "color", insig = "blank")
corrplot(chisq3$residuals, is.cor = FALSE, method = "number", insig = "blank")
corrplot(contrib3, is.cor = FALSE)
# shows which couple have the highest absolute contribution to the strenght of the relationship between METAL and FAMILY

ggplot(META_ANALYSIS, aes(x = CLASS, y = VALUE, color = METAL))+
  geom_jitter(alpha=0.5)+
  coord_flip()

ggplot(META_ANALYSIS, aes(x = CLASS, y = VALUE)) + 
  geom_boxplot()+
  coord_flip()

##############################################################
# Export sorted data
##############################################################
# Export data frame
write.xlsx(META_ANALYSIS, file = "METAL_CONTAMINATION_SORTED.xlsx", 
           sheetName = "SORTED",
           col.names = TRUE, 
           row.names = FALSE, 
           append = FALSE)

##############################################################
# Susbet data for total map
METAL_ANALYSIS_TOTAL <- subset(META_ANALYSIS, COUNTRY!="Saudi Arabia")
METAL_ANALYSIS_TOTAL <- subset(METAL_ANALYSIS_TOTAL, COUNTRY!="Oman")
METAL_ANALYSIS_TOTAL <- subset(METAL_ANALYSIS_TOTAL, COUNTRY!="Qatar")
METAL_ANALYSIS_TOTAL <- subset(METAL_ANALYSIS_TOTAL, COUNTRY!="Portugal")
METAL_ANALYSIS_TOTAL$COUNTRY <- factor(METAL_ANALYSIS_TOTAL$COUNTRY)

# Subset data ACTINOPTERYGII
METAL_ANALYSIS_REDUCED <- subset(META_ANALYSIS, CLASS=="ACTINOPTERYGII")

# Convert DW value into WW 
METAL_ANALYSIS_REDUCED$MEAN <- ifelse(METAL_ANALYSIS_REDUCED$UNIT == "DW", METAL_ANALYSIS_REDUCED$MEAN*0.25, METAL_ANALYSIS_REDUCED$MEAN)
METAL_ANALYSIS_REDUCED$SD <- ifelse(METAL_ANALYSIS_REDUCED$UNIT == "DW", METAL_ANALYSIS_REDUCED$SD*0.25, METAL_ANALYSIS_REDUCED$SD)
METAL_ANALYSIS_REDUCED$MIN <- ifelse(METAL_ANALYSIS_REDUCED$UNIT == "DW", METAL_ANALYSIS_REDUCED$MIN*0.25, METAL_ANALYSIS_REDUCED$MIN)
METAL_ANALYSIS_REDUCED$MAX <- ifelse(METAL_ANALYSIS_REDUCED$UNIT == "DW", METAL_ANALYSIS_REDUCED$MAX*0.25, METAL_ANALYSIS_REDUCED$MAX)
METAL_ANALYSIS_REDUCED$VALUE <- ifelse(METAL_ANALYSIS_REDUCED$UNIT == "DW", METAL_ANALYSIS_REDUCED$VALUE*0.25, METAL_ANALYSIS_REDUCED$VALUE)

METAL_ANALYSIS_REDUCED$UNIT <- ifelse(METAL_ANALYSIS_REDUCED$UNIT == "DW", METAL_ANALYSIS_REDUCED$UNIT <- "WW", METAL_ANALYSIS_REDUCED$UNIT)
METAL_ANALYSIS_REDUCED$METAL <- factor(METAL_ANALYSIS_REDUCED$METAL)
METAL_ANALYSIS_REDUCED$COUNTRY <- factor(METAL_ANALYSIS_REDUCED$COUNTRY)

# Subset data for the map
METAL_ANALYSIS_REDUCED <- subset(METAL_ANALYSIS_REDUCED, YEAR > 1989)
METAL_ANALYSIS_REDUCED <- subset(METAL_ANALYSIS_REDUCED, METAL=='As' |
                                   METAL=='Cd' |
                                   METAL=='Co' |
                                   METAL=='Cu' | 
                                   METAL=='Pb' |
                                   METAL=='Hg')
METAL_ANALYSIS_REDUCED <- subset(METAL_ANALYSIS_REDUCED, COUNTRY!="Saudi Arabia")
METAL_ANALYSIS_REDUCED <- subset(METAL_ANALYSIS_REDUCED, COUNTRY!="Oman")

# Check for missing value in LONGITUDE, LATITUDE, VALUE, METAL
which(is.na(METAL_ANALYSIS_REDUCED$LONGITUDE))
which(is.na(METAL_ANALYSIS_REDUCED$LATITUDE))
which(is.na(METAL_ANALYSIS_REDUCED$VALUE))
which(is.na(METAL_ANALYSIS_REDUCED$METAL))

# As
METAL_ANALYSIS_REDUCED_As <- subset(METAL_ANALYSIS_REDUCED, METAL=="As")

# Cd
METAL_ANALYSIS_REDUCED_Cd <- subset(METAL_ANALYSIS_REDUCED, METAL=="Cd")

# Co
METAL_ANALYSIS_REDUCED_Co <- subset(METAL_ANALYSIS_REDUCED, METAL=="Co")

# Cu
METAL_ANALYSIS_REDUCED_Cu <- subset(METAL_ANALYSIS_REDUCED, METAL=="Cu")

# Hg
METAL_ANALYSIS_REDUCED_Hg <- subset(METAL_ANALYSIS_REDUCED, METAL=="Hg")

# Pb
METAL_ANALYSIS_REDUCED_Pb <- subset(METAL_ANALYSIS_REDUCED, METAL=="Pb")

##############################################################
# Plot by latitude & longitude TOTAL_MAP
ggplot(METAL_ANALYSIS_TOTAL, aes(x=LONGITUDE, y=LATITUDE, color=METAL, size=VALUE)) +
  geom_point(alpha = 0.3)+
  ggtitle("Metal contamination in different aquatic species in Africa")+
  labs(size = "Metal concentration (mg/kg)", METAL="Metal")


# Plot by latitude & longitude
ggplot(METAL_ANALYSIS_REDUCED, aes(x=LONGITUDE, y=LATITUDE, color=METAL, size=VALUE)) +
  geom_point(alpha = 0.3)+
  ggtitle("Metal contamination in Actinopterigii from Africa")+
  labs(size = "Metal concentration (mg/kg wet weight)")

# Plot a map
### Google map
register_google(key = 'AIzaSyD_NBrzwJW2T5QBF0Wu4YYJXK1XmDv4SsI', write = TRUE)

map1 <- get_map(location = 'Democratic Republic of the Congo', zoom = 3, key = 'AIzaSyD_NBrzwJW2T5QBF0Wu4YYJXK1XmDv4SsI')
map2 <- get_map(location = 'Africa', zoom = 2, key = 'AIzaSyD_NBrzwJW2T5QBF0Wu4YYJXK1XmDv4SsI')
map3 <- get_map(location = 'Ghana', zoom = 5, key = 'AIzaSyD_NBrzwJW2T5QBF0Wu4YYJXK1XmDv4SsI')
map4 <- get_map(location = 'Morocco', zoom = 5, key = 'AIzaSyD_NBrzwJW2T5QBF0Wu4YYJXK1XmDv4SsI')
map5 <- get_map(location = 'Senegal', zoom = 5, key ='AIzaSyD_NBrzwJW2T5QBF0Wu4YYJXK1XmDv4SsI')
map6 <- get_map(location = "Egypt", zoom = 5, key = 'AIzaSyD_NBrzwJW2T5QBF0Wu4YYJXK1XmDv4SsI')
map7 <- get_map(location = "Kenya", zoom = 5, key = 'AIzaSyD_NBrzwJW2T5QBF0Wu4YYJXK1XmDv4SsI')
map8 <- get_map(location = "South Africa", zoom = 5, key = 'AIzaSyD_NBrzwJW2T5QBF0Wu4YYJXK1XmDv4SsI')

# Maps
As_map <- ggmap(map1)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED_As, alpha=0.7, aes(x = LONGITUDE, y = LATITUDE, size = VALUE), color = "#1b9e77")+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Arsenic contamination")

Cd_map <- ggmap(map1)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED_Cd, alpha=0.7, aes(x = LONGITUDE, y = LATITUDE, size = VALUE), color="#d95f02")+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Cadmium contamination")

Co_map <- ggmap(map1)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED_Co, alpha=0.7, aes(x = LONGITUDE, y = LATITUDE, size = VALUE), color="#7570b3")+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Cobalt contamination")

Cu_map <- ggmap(map1)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED_Cu, alpha=0.7, aes(x = LONGITUDE, y = LATITUDE, size = VALUE), color="#e7298a")+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Copper contamination")

Hg_map <- ggmap(map1)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED_Hg, alpha=0.7, aes(x = LONGITUDE, y = LATITUDE, size = VALUE), color="#66a61e")+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Mercury contamination")

Pb_map <- ggmap(map1)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED_Pb, alpha=0.7, aes(x = LONGITUDE, y = LATITUDE, size = VALUE), color="#e6ab02")+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Lead contamination")

ggarrange(As_map, Cd_map, Co_map, Cu_map, Pb_map, Hg_map, ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")

ggmap(map1)+ 
  geom_point(data = METAL_ANALYSIS_TOTAL, alpha=0.3, aes(x = LONGITUDE, y = LATITUDE, size = VALUE, color=METAL))+
  labs(size = "Metal concentration (mg/kg)")+
  ggtitle("Metal contamination in different aquatic species in Africa")

ggmap(map1)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED, alpha=0.5, aes(x = LONGITUDE, y = LATITUDE, size = VALUE, color=METAL))+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Metal contamination in Actinopterygii from Africa")

ggmap(map2)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED, alpha=0.3, aes(x = LONGITUDE, y = LATITUDE, size = VALUE, color=METAL))+
  ggtitle("Metal contamination in Actinopterygii from Africa")+
  labs(size = "Metal concentration (mg/kg wet weight)")

ggmap(map3)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED, alpha=0.6, aes(x = LONGITUDE, y = LATITUDE, size = VALUE, color=METAL))+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Metal contamination in Actinopterygii from Guinean Gulf")

ggmap(map4)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED, alpha=0.6, aes(x = LONGITUDE, y = LATITUDE, size = VALUE, color=METAL))+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Metal contamination in Actinopterygii from North West Africa")

ggmap(map5)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED, alpha=0.6, aes(x = LONGITUDE, y = LATITUDE, size = VALUE, color=METAL))+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Metal contamination in Actinopterygii from West Africa")

ggmap(map6)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED, alpha=0.6, aes(x = LONGITUDE, y = LATITUDE, size = VALUE, color=METAL))+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Metal contamination in Actinopterygii from North East Africa")

ggmap(map7)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED, alpha=0.6, aes(x = LONGITUDE, y = LATITUDE, size = VALUE, color=METAL))+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Metal contamination in Actinopterygii from East Africa")

ggmap(map8)+ 
  geom_point(data = METAL_ANALYSIS_REDUCED, alpha=0.6, aes(x = LONGITUDE, y = LATITUDE, size = VALUE, color=METAL))+
  labs(size = "Metal concentration (mg/kg wet weight)")+
  ggtitle("Metal contamination in Actinopterygii from South Africa")

##############################################################
# Explore relation METAL~ORDER in ACTINOPTERYGII
prop.table(table(METAL_ANALYSIS_REDUCED$METAL, METAL_ANALYSIS_REDUCED$ORDER)*100)

(tab1<-table(METAL_ANALYSIS_REDUCED$METAL, METAL_ANALYSIS_REDUCED$ORDER))
dt1 <- as.table(as.matrix(tab1))
balloonplot(t(dt1), main="METAL", label = FALSE, show.margins = FALSE)

ggplot(METAL_ANALYSIS_REDUCED, aes(x = ORDER, fill = METAL))+
  geom_histogram(stat = "count")+
  ggtitle("Proportion of metal contaminant per actinopterygii order")+
  coord_flip()

ggplot(META_ANALYSIS, aes(x = ORDER, fill = METAL))+
  geom_histogram(stat = "count")+
  ggtitle("Proportion of metal contaminant per order")+
  coord_flip()

ggplot(METAL_ANALYSIS_REDUCED, aes(x = METAL, fill = ORDER)) + 
  geom_bar(position = "fill")+
  ggtitle("Relative proportion of metal contaminant per actinopterygii order")

(chisq1<- chisq.test(METAL_ANALYSIS_REDUCED$ORDER, METAL_ANALYSIS_REDUCED$METAL))
(chisq1<- chisq.test(META_ANALYSIS$ORDER, META_ANALYSIS$METAL)) 
# p-value < 0.05 so the two variables are significantly associated
# The amount off contamination for each metal might be linked with the actinopterygii order

chisq1$observed
chisq1$expected
chisq1$residuals
contrib1 <- 100*chisq1$residuals^2/chisq1$statistic
round(contrib1, 3)
corrplot(chisq1$residuals, is.cor = FALSE, method = "color", insig = "blank")
corrplot(chisq1$residuals, is.cor = FALSE, method = "number", insig = "blank")

corrplot(contrib1, method = "color", is.cor = FALSE)
# shows which couple have the highest absolute contribution to the strenght of the relationship between METAL and FAMILY

ggplot(METAL_ANALYSIS_REDUCED, aes(x = ORDER, fill = METAL))+
  geom_histogram(stat = "count")+
  coord_flip()

ggplot(METAL_ANALYSIS_REDUCED, aes(x = ORDER, fill = METAL))+
  geom_bar(position = "fill")+
  coord_flip()

##############################################################
# Explore relation METAL~FAMILY
prop.table(table(METAL_ANALYSIS_REDUCED$METAL, METAL_ANALYSIS_REDUCED$FAMILY)*100)

(tab2<-table(METAL_ANALYSIS_REDUCED$METAL, METAL_ANALYSIS_REDUCED$FAMILY))
dt2 <- as.table(as.matrix(tab2))
balloonplot(t(dt2), main="METAL", label = FALSE, show.margins = FALSE)

ggplot(METAL_ANALYSIS_REDUCED, aes(x = METAL, fill = FAMILY))+
  geom_histogram(stat = "count")+
  ggtitle("Real proportion of metal contaminant per actinopterygii family")

ggplot(METAL_ANALYSIS_REDUCED, aes(x = METAL, fill = FAMILY)) + 
  geom_bar(position = "fill")+
  ggtitle("Relative proportion of metal contaminant per actinopterygii family")

(chisq2<- chisq.test(METAL_ANALYSIS_REDUCED$FAMILY, METAL_ANALYSIS_REDUCED$METAL)) 
(chisq2<-chisq.test(META_ANALYSIS$FAMILY, META_ANALYSIS$METAL))
# p-value < 0.05 so the two variables are significantly associated
# The amount off contamination for each metal might be linked with the actinopterygii FAMILY

chisq2$observed
chisq2$expected
chisq2$residuals # extract the Pearson residuals (r) -> the Chi-square statistic for each cell
corrplot(chisq2$residuals, is.cor = FALSE, method = "color", insig = "blank") 
corrplot(chisq2$residuals, is.cor = FALSE, method = "number", insig = "blank") 
contrib2 <- 100*chisq2$residuals^2/chisq2$statistic # calculate the contribution in %
corrplot(contrib2, method="color", is.cor = FALSE)
# shows which couple have the highest absolute contribution to the strenght of the relationship between METAL and FAMILY

ggplot(METAL_ANALYSIS_REDUCED, aes(x = FAMILY, y = VALUE)) + 
  geom_boxplot()+
  coord_flip()

ggplot(METAL_ANALYSIS_REDUCED, aes(x = FAMILY, fill = METAL))+
  geom_histogram(stat = "count")+
  coord_flip()

ggplot(METAL_ANALYSIS_REDUCED, aes(x = FAMILY, fill = METAL))+
  geom_bar(position = "fill")+
  coord_flip()

##############################################################
# Explore relation METAL~SPECIES
prop.table(table(METAL_ANALYSIS_REDUCED$METAL, METAL_ANALYSIS_REDUCED$SPECIES)*100)

(tab2<-table(METAL_ANALYSIS_REDUCED$METAL, METAL_ANALYSIS_REDUCED$SPECIES))
dt2 <- as.table(as.matrix(tab2))
balloonplot(t(dt2), main="METAL", label = FALSE, show.margins = FALSE)

ggplot(METAL_ANALYSIS_REDUCED, aes(x = METAL, fill = SPECIES))+
  geom_histogram(stat = "count")+
  ggtitle("Real proportion of metal contaminant per actinopterygii SPECIES")

ggplot(METAL_ANALYSIS_REDUCED, aes(x = METAL, fill = SPECIES)) + 
  geom_bar(position = "fill")+
  ggtitle("Relative proportion of metal contaminant per actinopterygii SPECIES")

(chisq7<- chisq.test(METAL_ANALYSIS_REDUCED$SPECIES, METAL_ANALYSIS_REDUCED$METAL)) 
(chisq7<-chisq.test(META_ANALYSIS$SPECIES, META_ANALYSIS$METAL))
# p-value < 0.05 so the two variables are significantly associated
# The amount off contamination for each metal might be linked with the actinopterygii SPECIES

chisq7$observed
chisq7$expected
chisq7$residuals # extract the Pearson residuals (r) -> the Chi-square statistic for each cell
corrplot(chisq7$residuals, is.cor = FALSE, method = "color", insig = "blank") 
corrplot(chisq7$residuals, is.cor = FALSE, method = "number", insig = "blank") 
contrib7 <- 100*chisq7$residuals^2/chisq7$statistic # calculate the contribution in %
corrplot(contrib7, method="color", is.cor = FALSE)
# shows which couple have the highest absolute contribution to the strenght of the relationship between METAL and SPECIES

ggplot(METAL_ANALYSIS_REDUCED, aes(x = SPECIES, y = VALUE)) + 
  geom_boxplot()+
  coord_flip()

ggplot(METAL_ANALYSIS_REDUCED, aes(x = SPECIES, fill = METAL))+
  geom_histogram(stat = "count")+
  coord_flip()

ggplot(METAL_ANALYSIS_REDUCED, aes(x = SPECIES, fill = METAL))+
  geom_bar(position = "fill")+
  coord_flip()
##############################################################
##############################################################
##############################################################
# Subset data by metal
As <- META_ANALYSIS[ which(META_ANALYSIS$METAL=='As'), ]
Cd <- META_ANALYSIS[ which(META_ANALYSIS$METAL=='Cd'), ]
Co <- META_ANALYSIS[ which(META_ANALYSIS$METAL=='Co'), ]
Cr <- META_ANALYSIS[ which(META_ANALYSIS$METAL=='Cr'), ]
Cu <- META_ANALYSIS[ which(META_ANALYSIS$METAL=='Cu'), ]
Hg <- META_ANALYSIS[ which(META_ANALYSIS$METAL=='Hg'), ]
Ni <- META_ANALYSIS[ which(META_ANALYSIS$METAL=='Ni'), ]
Pb <- META_ANALYSIS[ which(META_ANALYSIS$METAL=='Pb'), ]
Zn <- META_ANALYSIS[ which(META_ANALYSIS$METAL=='Zn'), ]

# Calculate Maximum Limits (ML) proportions

mean(As$CLASS == "ACTINOPTERYGII" & As$VALUE > 0.1) # Proportion of value above ML for As = 0.1 mg/kg

mean(Cd$CLASS == "ACTINOPTERYGII" & Cd$VALUE > 0.05) # Proportion of value above ML for Cd = 0.05 mg/kg
mean(Cd$FAMILY == "SCOMBRIDAE" & Cd$VALUE > 0.1) # Proportion of value above ML for SCOMBRIDAE, Cd = 0.1 mg/kg  
mean(Cd$SPECIES == "Katsuwonus pelaris" & Cd$VALUE>0.1)
mean(Cd$FAMILY == "ENGRAULIDAE" & Cd$VALUE > 0.25) # Proportion of value above ML for ENGRAULIDAE, Cd = 0.25 mg/kg
mean(Cd$FAMILY == "XIPHIIDAE" & Cd$VALUE > 0.25) # Proportion of value above ML for XIPHIIDAE, Cd = 0.25 mg/kg
mean(Cd$FAMILY == "CLUPEIDAE" & Cd$VALUE > 0.25) # Proportion of value above ML for CLUPEIDAE, Cd = 0.25 mg/kg
mean(Cd$CLASS == "MALACOSTRACA" & Cd$VALUE > 0.5) # Proportion of value above ML for MALACOSTRACA, Cd = 0.5 mg/kg
mean(Cd$CLASS == "BIVALVIA" & Cd$VALUE > 1) # Proportion of value above ML for BIVALVIA, Cd = 1.0 mg/kg
mean(Cd$CLASS == "BIVALVIA" & Cd$VALUE > 2)
mean(Cd$CLASS == "CEPHALOPODS" & Cd$VALUE > 1) # Proportion of value above ML for CEPHALOPODS, Cd = 1.0 mg/kg
mean(Cd$CLASS == "CEPHALOPODS" & Cd$VALUE > 2)

mean(Hg$CLASS == "ACTINOPTERYGII" & Hg$VALUE > 0.5) # Proportion of value above ML for Hg = 0.5 mg/kg
mean(Hg$FAMILY == "SCOMBRIDAE" & Hg$VALUE > 1)
mean(Hg$FAMILY == "ANGUILLIDAE" & Hg$VALUE > 1)
mean(Hg$FAMILY == "MULLIDAE" & Hg$VALUE > 1)
mean(Hg$FAMILY == "RAJIDAE" & Hg$VALUE > 1)
mean(Hg$FAMILY == "TRICHURIDAE" & Hg$VALUE > 1)
mean(Hg$FAMILY == "SPARIDAE" & Hg$VALUE > 1)
mean(Hg$FAMILY == "XIPHIIDAE" & Hg$VALUE > 1)
mean(Hg$FAMILY == "SCOMBRIDAE" & Hg$VALUE > 1)

mean(Pb$CLASS == "ACTINOPTERYGII" & Pb$VALUE > 0.3) # Proportion of value above ML for Pb = 0.3 mg/kg
mean(Pb$CLASS == "CEPHALOPODS" & Pb$VALUE > 0.3)
mean(Pb$CLASS == "MALACOSTRACA" & Pb$VALUE > 0.5)
mean(Pb$CLASS == "BIVALVIA" & Pb$VALUE > 1.5)

##############################################################
# Subset data ACTINOPTERYGII per METAL
As_ACTINOPTERYGII <- subset(As, CLASS=="ACTINOPTERYGII")
Cd_ACTINOPTERYGII <- subset(Cd, CLASS=="ACTINOPTERYGII")
Cu_ACTINOPTERYGII <- subset(Cu, CLASS=="ACTINOPTERYGII")
Hg_ACTINOPTERYGII <- subset(Hg, CLASS=="ACTINOPTERYGII")
Pb_ACTINOPTERYGII <- subset(Pb, CLASS=="ACTINOPTERYGII")

# Convert DW value into WW 
As_ACTINOPTERYGII$MEAN <- ifelse(As_ACTINOPTERYGII$UNIT == "DW", As_ACTINOPTERYGII$MEAN*0.25, As_ACTINOPTERYGII$MEAN)
As_ACTINOPTERYGII$SD <- ifelse(As_ACTINOPTERYGII$UNIT == "DW", As_ACTINOPTERYGII$SD*0.25, As_ACTINOPTERYGII$SD)
As_ACTINOPTERYGII$MIN <- ifelse(As_ACTINOPTERYGII$UNIT == "DW", As_ACTINOPTERYGII$MIN*0.25, As_ACTINOPTERYGII$MIN)
As_ACTINOPTERYGII$MAX <- ifelse(As_ACTINOPTERYGII$UNIT == "DW", As_ACTINOPTERYGII$MAX*0.25, As_ACTINOPTERYGII$MAX)
As_ACTINOPTERYGII$UNIT <- ifelse(As_ACTINOPTERYGII$UNIT == "DW", As_ACTINOPTERYGII$UNIT <- "WW", As_ACTINOPTERYGII$UNIT)

Cd_ACTINOPTERYGII$MEAN <- ifelse(Cd_ACTINOPTERYGII$UNIT == "DW", Cd_ACTINOPTERYGII$MEAN*0.25, Cd_ACTINOPTERYGII$MEAN)
Cd_ACTINOPTERYGII$SD <- ifelse(Cd_ACTINOPTERYGII$UNIT == "DW", Cd_ACTINOPTERYGII$SD*0.25, Cd_ACTINOPTERYGII$SD)
Cd_ACTINOPTERYGII$MIN <- ifelse(Cd_ACTINOPTERYGII$UNIT == "DW", Cd_ACTINOPTERYGII$MIN*0.25, Cd_ACTINOPTERYGII$MIN)
Cd_ACTINOPTERYGII$MAX <- ifelse(Cd_ACTINOPTERYGII$UNIT == "DW", Cd_ACTINOPTERYGII$MAX*0.25, Cd_ACTINOPTERYGII$MAX)
Cd_ACTINOPTERYGII$UNIT <- ifelse(Cd_ACTINOPTERYGII$UNIT == "DW", Cd_ACTINOPTERYGII$UNIT <- "WW", Cd_ACTINOPTERYGII$UNIT)

Cu_ACTINOPTERYGII$MEAN <- ifelse(Cu_ACTINOPTERYGII$UNIT == "DW", Cu_ACTINOPTERYGII$MEAN*0.25, Cu_ACTINOPTERYGII$MEAN)
Cu_ACTINOPTERYGII$SD <- ifelse(Cu_ACTINOPTERYGII$UNIT == "DW", Cu_ACTINOPTERYGII$SD*0.25, Cu_ACTINOPTERYGII$SD)
Cu_ACTINOPTERYGII$MIN <- ifelse(Cu_ACTINOPTERYGII$UNIT == "DW", Cu_ACTINOPTERYGII$MIN*0.25, Cu_ACTINOPTERYGII$MIN)
Cu_ACTINOPTERYGII$MAX <- ifelse(Cu_ACTINOPTERYGII$UNIT == "DW", Cu_ACTINOPTERYGII$MAX*0.25, Cu_ACTINOPTERYGII$MAX)
Cu_ACTINOPTERYGII$UNIT <- ifelse(Cu_ACTINOPTERYGII$UNIT == "DW", Cu_ACTINOPTERYGII$UNIT <- "WW", Cu_ACTINOPTERYGII$UNIT)

Hg_ACTINOPTERYGII$MEAN <- ifelse(Hg_ACTINOPTERYGII$UNIT == "DW", Hg_ACTINOPTERYGII$MEAN*0.25, Hg_ACTINOPTERYGII$MEAN)
Hg_ACTINOPTERYGII$SD <- ifelse(Hg_ACTINOPTERYGII$UNIT == "DW", Hg_ACTINOPTERYGII$SD*0.25, Hg_ACTINOPTERYGII$SD)
Hg_ACTINOPTERYGII$MIN <- ifelse(Hg_ACTINOPTERYGII$UNIT == "DW", Hg_ACTINOPTERYGII$MIN*0.25, Hg_ACTINOPTERYGII$MIN)
Hg_ACTINOPTERYGII$MAX <- ifelse(Hg_ACTINOPTERYGII$UNIT == "DW", Hg_ACTINOPTERYGII$MAX*0.25, Hg_ACTINOPTERYGII$MAX)
Hg_ACTINOPTERYGII$UNIT <- ifelse(Hg_ACTINOPTERYGII$UNIT == "DW", Hg_ACTINOPTERYGII$UNIT <- "WW", Hg_ACTINOPTERYGII$UNIT)

Pb_ACTINOPTERYGII$MEAN <- ifelse(Pb_ACTINOPTERYGII$UNIT == "DW", Pb_ACTINOPTERYGII$MEAN*0.25, Pb_ACTINOPTERYGII$MEAN)
Pb_ACTINOPTERYGII$SD <- ifelse(Pb_ACTINOPTERYGII$UNIT == "DW", Pb_ACTINOPTERYGII$SD*0.25, Pb_ACTINOPTERYGII$SD)
Pb_ACTINOPTERYGII$MIN <- ifelse(Pb_ACTINOPTERYGII$UNIT == "DW", Pb_ACTINOPTERYGII$MIN*0.25, Pb_ACTINOPTERYGII$MIN)
Pb_ACTINOPTERYGII$MAX <- ifelse(Pb_ACTINOPTERYGII$UNIT == "DW", Pb_ACTINOPTERYGII$MAX*0.25, Pb_ACTINOPTERYGII$MAX)
Pb_ACTINOPTERYGII$UNIT <- ifelse(Pb_ACTINOPTERYGII$UNIT == "DW", Pb_ACTINOPTERYGII$UNIT <- "WW", Pb_ACTINOPTERYGII$UNIT)

# Plot per METAL with treshold
As_plot <- ggplot(As, aes(x=YEAR, y=VALUE, color=ORDER)) +
  geom_jitter(stat="identity", position=position_jitter(0.8)) +
  ylab("Metal concentration (mg/g w.w)")+
  xlab('Year')+
  geom_hline(yintercept=0.1, linetype="dashed", color="black")+
  ggtitle("As")

Cd_plot <- ggplot(Cd_ACTINOPTERYGII, aes(x=YEAR, y=VALUE, color=ORDER)) +
  geom_jitter(stat="identity", position=position_jitter(0.8)) +
  ylab("Metal concentration (mg/g w.w)")+
  xlab('Year')+
  geom_hline(yintercept=0.05, linetype="dashed", color="black")+
  ggtitle("Cd")

Cu_plot <- ggplot(Cu_ACTINOPTERYGII, aes(x=YEAR, y=VALUE, color=ORDER)) +
  geom_jitter(stat="identity", position=position_jitter(0.8)) +
  ylab("Metal concentration (mg/g w.w)")+
  xlab('Year')+
  geom_hline(yintercept=0.5, linetype="dashed", color="black")+
  ggtitle("Cu")

Pb_plot <- ggplot(Pb_ACTINOPTERYGII, aes(x=YEAR, y=VALUE, color=ORDER)) +
  geom_jitter(stat="identity", position=position_jitter(0.8)) +
  ylab("Metal concentration (mg/g w.w)")+
  xlab('Year')+
  geom_hline(yintercept=0.3, linetype="dashed", color="black")+
  ggtitle("Pb")

Hg_plot <- ggplot(Hg_ACTINOPTERYGII, aes(x=YEAR, y=VALUE, color=ORDER)) +
  geom_jitter(stat="identity", position=position_jitter(0.8)) +
  ylab("Metal concentration (mg/g w.w)")+
  xlab('Year')+
  geom_hline(yintercept=0.5, linetype="dashed", color="black")+
  ggtitle("Hg")

ggarrange(As_plot, Cu_plot, Pb_plot, Hg_plot, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

##############################################################
# Subset data ACTINOPTERYGII
ACTINOPTERYGII <- subset(META_ANALYSIS, CLASS=="ACTINOPTERYGII")

# Convert DW value into WW 
ACTINOPTERYGII$MEAN <- ifelse(ACTINOPTERYGII$UNIT == "DW", ACTINOPTERYGII$MEAN*0.25, ACTINOPTERYGII$MEAN)
ACTINOPTERYGII$SD <- ifelse(ACTINOPTERYGII$UNIT == "DW", ACTINOPTERYGII$SD*0.25, ACTINOPTERYGII$SD)
ACTINOPTERYGII$MIN <- ifelse(ACTINOPTERYGII$UNIT == "DW", ACTINOPTERYGII$MIN*0.25, ACTINOPTERYGII$MIN)
ACTINOPTERYGII$MAX <- ifelse(ACTINOPTERYGII$UNIT == "DW", ACTINOPTERYGII$MAX*0.25, ACTINOPTERYGII$MAX)
ACTINOPTERYGII$UNIT <- ifelse(ACTINOPTERYGII$UNIT == "DW", ACTINOPTERYGII$UNIT <- "WW", ACTINOPTERYGII$UNIT)

# Rearrange data (create a column VALUE which is the highest value per raw)
ACTINOPTERYGII$VALUE <-ifelse(is.na(ACTINOPTERYGII$MAX), ACTINOPTERYGII$MEAN, ACTINOPTERYGII$MAX)
ACTINOPTERYGII$VALUE <-as.numeric(as.character(ACTINOPTERYGII$VALUE))

# Plot per ORDER
ggplot(ACTINOPTERYGII, aes(x=YEAR, y=VALUE, color=ORDER)) +
  geom_jitter(stat="identity", position=position_jitter(0.8)) +
  ylab("Metal concentration (mg/g w.w)")+
  xlab('Year')+
  ggtitle("Trace metal contamination in different African order of Actinopterygii from 1974 to 2017") + 
  facet_wrap(~ METAL)

ggplot(ACTINOPTERYGII, aes(x=YEAR, y=VALUE, color=METAL)) +
  geom_jitter(stat="identity", position=position_jitter(0.8)) +
  scale_color_manual(values=c("#151515","#1C1C1C","#2E2E2E","#424242","#585858","#6E6E6E","#848484","#A4A4A4","#BDBDBD"))+
  ylab("Metal concentration (mg/g w.w)")+
  xlab('Year')+
  geom_hline(yintercept=0.3, linetype="dashed", color="#A4A4A4")+
  geom_hline(yintercept=0.5, linetype="dotdash", color ="#6E6E6E")+
  geom_hline(yintercept=0.05, linetype="longdash", color ="#1C1C1C")+
  geom_hline(yintercept=0.3, linetype="solid", color ="#151515")

# Plot per FAMILY
ggplot(ACTINOPTERYGII, aes(x=YEAR, y=VALUE, color=FAMILY)) +
  geom_jitter(stat="identity", position=position_jitter(0.8)) +
  ylab("Metal concentration (mg/g w.w)")+
  xlab('Year')+
  ggtitle("Trace metal contamination in different African family of Actinopterygii from 1974 to 2017")+
  facet_wrap(~ METAL)

# Plot per METAL
ggplot(ACTINOPTERYGII, aes(x=YEAR, y=VALUE, color=METAL)) +
  geom_jitter(stat="identity") +
  ylab("Metal concentration (mg/g w.w)")+
  xlab('Year')+
  ggtitle("Trace metal contamination in different African actinopterygii from 1974 to 2017")+
  facet_wrap(~ ORDER)

##############################################################
# PCA
REDUCED_PCA<- prcomp(METAL_ANALYSIS_REDUCED[,c(3,4,5,18)], center = TRUE, scale = TRUE)
summary(REDUCED_PCA)
ggbiplot(REDUCED_PCA, obs.scale = 1, var.scale = 1,
         groups = METAL_ANALYSIS_REDUCED$METAL, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

ACTINO_PCA <- prcomp(ACTINOPTERYGII[,c(3,4,5,18)], center = TRUE, scale = TRUE)
summary(ACTINO_PCA)
str(ACTINO_PCA)
biplot(princomp(ACTINO_PCA))
ggbiplot(ACTINO_PCA)
ggbiplot(ACTINO_PCA, obs.scale = 1, var.scale = 1,
         groups = ACTINOPTERYGII$METAL, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

# or

plot(ACTINO_PCA$x[,1],ACTINO_PCA$x[,2])
ACTINO_PCA_VAR <- ACTINO_PCA$sdev^2
ACTINO_PCA_VAR_PER <- round(ACTINO_PCA_VAR/sum(ACTINO_PCA_VAR)*100,1)
barplot(ACTINO_PCA_VAR_PER, main="Scree plot", xlab = "Principal component", ylab = "Percent variation")

ACTINO_PCA_data <- data.frame(Sample=rownames(ACTINO_PCA$x),
                       X=ACTINO_PCA$x[,1],
                       Y=ACTINO_PCA$x[,2])
ggplot(data=ACTINO_PCA_data, aes(x=X, y=Y, label= ACTINOPTERYGII$METAL)) +
  geom_text() +
  xlab(paste("PC1 : ", ACTINO_PCA_VAR_PER[1], "%", sep="")) +
  ylab(paste("PC2 : ", ACTINO_PCA_VAR_PER[2], "%", sep="")) +
  theme_bw() +
  ggtitle("PCA trace metal contamination Africa")

##############################################################
# nMDS



##############################################################
# Save the work space
save.image(file = "META_ANALYSIS.RData")

##############################################################
# Citation
### R
citation()

### ggmap
citation("ggmap")
