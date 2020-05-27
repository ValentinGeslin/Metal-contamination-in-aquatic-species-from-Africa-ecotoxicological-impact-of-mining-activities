setwd("E:/THESIS_VALENTIN")
setwd("F:/THESIS_VALENTIN")
setwd("~/Desktop/THESIS_VALENTIN")
setwd("C:/Users/Valentin/Desktop/THESIS_VALENTIN")

library(readxl)
library(ggplot2)
library(GGally)
library(CCA)
library(PairedData)
library(FactoMineR)
library(factoextra)
library(plyr)
library(corrplot)
library(xlsx)
library(ggpubr)
library(gplots)

##############################################################
# Import dataset
NN_JOIN_CONT_MINING <- read_excel("F:/THESIS_VALENTIN/QGIS/DISTANCE_MATRIX/NN_JOIN_CONT_MINING.xlsx", 
                                  col_types = c("text", "text", "numeric", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "text", "text", "text", 
                                                "text", "text", "text", "text", "numeric", 
                                                "text", "numeric"))
attach(NN_JOIN_CONT_MINING)
NN_JOIN_CONT_MINING$distance<-NN_JOIN_CONT_MINING$distance*111

NN_JOIN_MINING_CONT <- read_excel("F:/THESIS_VALENTIN/QGIS/DISTANCE_MATRIX/NN_JOIN_MINING_CONT.xlsx", 
                                  col_types = c("text", "text", "text", 
                                                "text", "text", "text", "text", "numeric", 
                                                "text", "text", "text", "numeric", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric"))
attach(NN_JOIN_MINING_CONT)
NN_JOIN_MINING_CONT$distance <- NN_JOIN_MINING_CONT$distance*111
NN_JOIN_MINING_CONT <- subset(NN_JOIN_MINING_CONT, distance < 200)
NN_JOIN_MINING_CONT_REDUCED <- subset(NN_JOIN_MINING_CONT, MINERAL!="petroleum")
NN_JOIN_MINING_CONT_REDUCED <- subset(NN_JOIN_MINING_CONT_REDUCED, MINERAL!="diamond")
NN_JOIN_MINING_CONT_REDUCED <- subset(NN_JOIN_MINING_CONT_REDUCED, MINERAL!="natural gas")

DISTANCE_METAL_TO_MINERAL <- read_excel("DISTANCE_METAL_TO_MINERAL.xlsx", sheet = "DISTANCE_METAL_TO_MINERAL")
attach(DISTANCE_METAL_TO_MINERAL)
DISTANCE_METAL_TO_MINERAL$COUNTRY <- factor(DISTANCE_METAL_TO_MINERAL$COUNTRY)
DISTANCE_METAL_TO_MINERAL$CLASS <- factor(DISTANCE_METAL_TO_MINERAL$CLASS)
DISTANCE_METAL_TO_MINERAL$ORDER <- factor(DISTANCE_METAL_TO_MINERAL$ORDER)
DISTANCE_METAL_TO_MINERAL$FAMILY <- factor(DISTANCE_METAL_TO_MINERAL$FAMILY)
DISTANCE_METAL_TO_MINERAL$GROUP <- factor(DISTANCE_METAL_TO_MINERAL$GROUP)
DISTANCE_METAL_TO_MINERAL$METAL <- factor(DISTANCE_METAL_TO_MINERAL$METAL)
DISTANCE_METAL_TO_MINERAL$MINERAL <- factor(DISTANCE_METAL_TO_MINERAL$MINERAL)

##############################################################
##############################################################
##############################################################
# EXPLORE THE RELATION METAL~MINERAL

##############################################################
# Plot
ggplot(data = DISTANCE_METAL_TO_MINERAL, aes(x=METAL, y=MINERAL, color = METAL, size=VALUE))+ 
  geom_jitter(alpha=0.3) + 
  ylab("MINING ACTIVITY")

##############################################################
# Proportion & table
# Prop of mining activity
ggplot(DISTANCE_METAL_TO_MINERAL, aes(x = METAL))+
  geom_histogram(stat = "count")
table(DISTANCE_METAL_TO_MINERAL$METAL)
prop.table(table(DISTANCE_METAL_TO_MINERAL$METAL))*100 # in %

# Prop of metal contaminant
ggplot(DISTANCE_METAL_TO_MINERAL, aes(x = MINERAL))+
  geom_histogram(stat = "count")+
  coord_flip()
table(DISTANCE_METAL_TO_MINERAL$MINERAL)
prop.table(table(DISTANCE_METAL_TO_MINERAL$MINERAL))*100

# Prop table of metal contaminant & mining activity
(tab1<-table(DISTANCE_METAL_TO_MINERAL$METAL, DISTANCE_METAL_TO_MINERAL$MINERAL))
(prop.tab1 <-prop.table(table(DISTANCE_METAL_TO_MINERAL$METAL, DISTANCE_METAL_TO_MINERAL$MINERAL)))
(tab2<-table(DISTANCE_METAL_TO_MINERAL$MINERAL, DISTANCE_METAL_TO_MINERAL$METAL))
(prop.tab2<-prop.table(table(DISTANCE_METAL_TO_MINERAL$MINERAL, DISTANCE_METAL_TO_MINERAL$METAL))*100)
prop.tab2 <- as.data.frame(prop.tab2)

dt <- as.table(as.matrix(tab2))
balloonplot(t(dt), main="METAL", label = FALSE, show.margins = FALSE)

ggplot(DISTANCE_METAL_TO_MINERAL, aes(x = METAL, fill = MINERAL)) + 
  geom_histogram(stat = "count")+
  ggtitle("Proportion of nearest mining activity per metal contaminant in different African aquatic species")

ggplot(DISTANCE_METAL_TO_MINERAL, aes(x = METAL, fill = MINERAL)) + 
  geom_bar(position = "fill")+
  ggtitle("Relative proportion of nearest mining activity per metal contaminant")

write.xlsx(prop.tab2, "F:/THESIS_VALENTIN/PROP.TABLE.xlsx")

# Chi square test (to see if METAL and MINERAL are independant or not)
(chisq <- chisq.test(DISTANCE_METAL_TO_MINERAL$MINERAL, DISTANCE_METAL_TO_MINERAL$METAL))
# p-value < 0.05 so the two variables are not independent (metal contamination might be linked with mining activity)
chisq.test(DISTANCE_MINERAL_TO_METAL$metal, DISTANCE_MINERAL_TO_METAL$MINERAL)
# p-value < 0.05 so the two variables are not independent (metal contamination might be linked with mining activity)

chisq$observed
chisq$expected
chisq$residuals
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(chisq$residuals, is.cor = FALSE, method = "color", insig = "blank")
corrplot(contrib, is.cor = FALSE, method = "color")

##############################################################
##############################################################
##############################################################
# EXPLORE THE RELATION VALUE~DISTANCE
DISTANCE_METAL_TO_MINERAL$VALUE<-as.numeric(DISTANCE_METAL_TO_MINERAL$VALUE)
DISTANCE_METAL_TO_MINERAL<-subset(DISTANCE_METAL_TO_MINERAL, DISTANCE < 200) # DISTANCE > 200km are considered as outsiders
DISTANCE_METAL_TO_MINERAL_REDUCED <- subset(DISTANCE_METAL_TO_MINERAL, MINERAL!="petroleum")
DISTANCE_METAL_TO_MINERAL_REDUCED <- subset(DISTANCE_METAL_TO_MINERAL_REDUCED, MINERAL!="diamond")
DISTANCE_METAL_TO_MINERAL_REDUCED <- subset(DISTANCE_METAL_TO_MINERAL_REDUCED, MINERAL!="natural gas")

# For a question of unit homogeneity we will use DISTANCE_METAL_TO_MINERAL_REDUCED (without petroleum, diamond, natural gas)
ggplot(data = DISTANCE_METAL_TO_MINERAL_REDUCED, aes(x=DISTANCE, y=MINERAL, size=VALUE, fill=METAL)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  ylab("Mining activity") +
  xlab("Distance from the mine")

##############################################################
# Subset data by metal
As_JJ <- DISTANCE_METAL_TO_MINERAL_REDUCED[ which(DISTANCE_METAL_TO_MINERAL_REDUCED$METAL=='As'), ]
Cd_JJ <- DISTANCE_METAL_TO_MINERAL_REDUCED[ which(DISTANCE_METAL_TO_MINERAL_REDUCED$METAL=='Cd'), ]
Co_JJ <- DISTANCE_METAL_TO_MINERAL_REDUCED[ which(DISTANCE_METAL_TO_MINERAL_REDUCED$METAL=='Co'), ]
Cr_JJ <- DISTANCE_METAL_TO_MINERAL_REDUCED[ which(DISTANCE_METAL_TO_MINERAL_REDUCED$METAL=='Cr'), ]
Cu_JJ <- DISTANCE_METAL_TO_MINERAL_REDUCED[ which(DISTANCE_METAL_TO_MINERAL_REDUCED$METAL=='Cu'), ]
Hg_JJ <- DISTANCE_METAL_TO_MINERAL_REDUCED[ which(DISTANCE_METAL_TO_MINERAL_REDUCED$METAL=='Hg'), ]
Ni_JJ <- DISTANCE_METAL_TO_MINERAL_REDUCED[ which(DISTANCE_METAL_TO_MINERAL_REDUCED$METAL=='Ni'), ]
Pb_JJ <- DISTANCE_METAL_TO_MINERAL_REDUCED[ which(DISTANCE_METAL_TO_MINERAL_REDUCED$METAL=='Pb'), ]
Zn_JJ <- DISTANCE_METAL_TO_MINERAL_REDUCED[ which(DISTANCE_METAL_TO_MINERAL_REDUCED$METAL=='Zn'), ]

# Plot metal contamination related to mining activities along years
As_JJ_plot <- ggplot(data = As_JJ, aes(x=YEAR, y=MINERAL, size=VALUE)) +
  geom_point(alpha=0.5, shape=21, color="#CC0000")+ 
  ylab("MINING ACTIVITY")

Cd_JJ_plot <- ggplot(data = Cd_JJ, aes(x=YEAR, y=MINERAL, size=VALUE)) +
  geom_point(alpha=0.5, shape=21, color="#66CC00")+ 
  ylab("MINING ACTIVITY")

Co_JJ_plot <- ggplot(data = Co_JJ, aes(x=YEAR, y=MINERAL, size=VALUE)) +
  geom_point(alpha=0.5, shape=21, color="black")+ 
  ylab("MINING ACTIVITY")

Cr_JJ_plot <- ggplot(data = Cr_JJ, aes(x=YEAR, y=MINERAL, size=VALUE)) +
  geom_point(alpha=0.5, shape=21, color="black")+ 
  ylab("MINING ACTIVITY")

Cu_JJ_plot <- ggplot(data = Cu_JJ, aes(x=YEAR, y=MINERAL, size=VALUE)) +
  geom_point(alpha=0.5, shape=21, color="black")+ 
  ylab("MINING ACTIVITY")

Hg_JJ_plot <- ggplot(data = Hg_JJ, aes(x=YEAR, y=MINERAL, size=VALUE)) +
  geom_point(alpha=0.5, shape=21, color="#3399FF")+ 
  ylab("MINING ACTIVITY")

Ni_JJ_plot <- ggplot(data = Ni_JJ, aes(x=YEAR, y=MINERAL, size=VALUE)) +
  geom_point(alpha=0.5, shape=21, color="black")+ 
  ylab("MINING ACTIVITY")

Pb_JJ_plot <- ggplot(data = Pb_JJ, aes(x=YEAR, y=MINERAL, size=VALUE)) +
  geom_point(alpha=0.5, shape=21, color="#9900CC")+ 
  ylab("MINING ACTIVITY")

Zn_JJ_plot <- ggplot(data = Zn_JJ, aes(x=YEAR, y=MINERAL, size=VALUE)) +
  geom_point(alpha=0.5, shape=21, color="black")+ 
  ylab("MINING ACTIVITY")

ggarrange(As_JJ_plot, Cd_JJ_plot, Pb_JJ_plot, Hg_JJ_plot, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

##############################################################
# Plot relation between distance and value
ggplot(DISTANCE_METAL_TO_MINERAL_REDUCED, aes(DISTANCE, VALUE))+
  geom_point(aes(color=METAL), alpha = 0.3)+
  stat_smooth()+
  scale_x_sqrt()+
  scale_y_sqrt()+
  ggtitle("Relation between metal concentration and distance to mine")

ggplot(As_JJ, aes(VALUE, DISTANCE))+
  geom_point(aes(color=MINERAL), alpha = 0.3)+
  stat_smooth()+
  geom_hline(yintercept=0.3, linetype="dashed", color="#A4A4A4")+
  ggtitle("Relation between As concentration and distance to mine")

ggplot(Cd_JJ, aes(VALUE, DISTANCE))+
  geom_point(aes(color=MINERAL), alpha = 0.3)+
  stat_smooth()+
  geom_hline(yintercept=0.5, linetype="dashed", color ="#6E6E6E")+
  ggtitle("Relation between Cd concentration and distance to mine")

ggplot(Co_JJ, aes(VALUE, DISTANCE))+
  geom_point(aes(color=MINERAL), alpha = 0.3)+
  stat_smooth()+
  ggtitle("Relation between Co concentration and distance to mine")

ggplot(Cr_JJ, aes(VALUE, DISTANCE))+
  geom_point(aes(color=MINERAL), alpha = 0.3)+
  stat_smooth()+
  ggtitle("Relation between Cr concentration and distance to mine")

ggplot(Cu_JJ, aes(VALUE, DISTANCE))+
  geom_point(aes(color=MINERAL), alpha = 0.3)+
  stat_smooth()+
  ggtitle("Relation between Cu concentration and distance to mine")

ggplot(Hg_JJ, aes(VALUE, DISTANCE))+
  geom_point(aes(color=MINERAL), alpha = 0.3)+
  stat_smooth()+
  geom_hline(yintercept=0.05, linetype="dashed", color ="#1C1C1C")+
  ggtitle("Relation between Hg concentration and distance to mine")

ggplot(Ni_JJ, aes(VALUE, DISTANCE))+
  geom_point(aes(color=MINERAL), alpha = 0.3)+
  stat_smooth()+
  ggtitle("Relation between Ni concentration and distance to mine")

ggplot(Pb_JJ, aes(VALUE, DISTANCE))+
  geom_point(aes(color=MINERAL), alpha = 0.3)+
  stat_smooth()+
  geom_hline(yintercept=0.3, linetype="dashed", color ="#151515")+
  ggtitle("Relation between Pb concentration and distance to mine")

ggplot(Zn_JJ, aes(VALUE, DISTANCE))+
  geom_point(aes(color=MINERAL), alpha = 0.3)+
  stat_smooth()+
  ggtitle("Relation between Zn concentration and distance to mine")

##############################################################
# Correlation
cor.test(DISTANCE_METAL_TO_MINERAL_REDUCED$VALUE, DISTANCE_METAL_TO_MINERAL_REDUCED$DISTANCE, method = "pearson")
cor.test(DISTANCE_METAL_TO_MINERAL$VALUE, DISTANCE_METAL_TO_MINERAL$DISTANCE, method = "pearson")

# p-value < 0.05 so the correlation is significant
# cor = 0.14 indicate a weak relationship btw VALUE and DISTANCE, suggesting that most of the variation observed in VALUE are not explained by DISTANCE

cor.test(As_JJ$VALUE, As_JJ$DISTANCE, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Cd_JJ$VALUE, Cd_JJ$DISTANCE, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Co_JJ$VALUE, Co_JJ$DISTANCE, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Cr_JJ$VALUE, Cr_JJ$DISTANCE, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Cu_JJ$VALUE, Cu_JJ$DISTANCE, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Hg_JJ$VALUE, Hg_JJ$DISTANCE, method = "pearson") # p-value < 0.05 so the correlation is significant but cor=-0.33 so the relationship is weak
cor.test(Ni_JJ$VALUE, Ni_JJ$DISTANCE, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Pb_JJ$VALUE, Pb_JJ$DISTANCE, method = "pearson") # p-value < 0.05 so the correlation is significant but cor=0.24 so the relationship is weak
cor.test(Zn_JJ$VALUE, Zn_JJ$DISTANCE, method = "pearson") # p-value < 0.05 so the correlation is significant but cor=0.32 so the relationship is weak

##############################################################
# Linear model
model1 <- lm(VALUE~DISTANCE, DISTANCE_METAL_TO_MINERAL_REDUCED)
summary(model1)
# p-value < 0.05 so the model is signficant
# R-squared = 0.02 so the relationship VALUE~DISTANCE explain only 2% of the total variation

##############################################################
##############################################################
##############################################################
# EXPLORE THE RELATION VALUE~CAPACITY

NN_JOIN_CONT_MINING <- subset(NN_JOIN_CONT_MINING, distance < 200)
NN_JOIN_CONT_MINING_REDUCED <- subset(NN_JOIN_CONT_MINING, join_MINERAL!="petroleum")
NN_JOIN_CONT_MINING_REDUCED <- subset(NN_JOIN_CONT_MINING_REDUCED, join_MINERAL!="diamond")
NN_JOIN_CONT_MINING_REDUCED <- subset(NN_JOIN_CONT_MINING_REDUCED, join_MINERAL!="natural gas")

ggplot(data = NN_JOIN_CONT_MINING_REDUCED, aes(x=join_Capacity, y=join_MINERAL, size=VALUE, fill=METAL)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  ylab("Mining activity") +
  xlab("Mining capacity (t/year)")

##############################################################
# Subset data by metal
As_NN <- NN_JOIN_CONT_MINING_REDUCED[ which(NN_JOIN_CONT_MINING_REDUCED$METAL=='As'), ]
Cd_NN <- NN_JOIN_CONT_MINING_REDUCED[ which(NN_JOIN_CONT_MINING_REDUCED$METAL=='Cd'), ]
Co_NN <- NN_JOIN_CONT_MINING_REDUCED[ which(NN_JOIN_CONT_MINING_REDUCED$METAL=='Co'), ]
Cr_NN <- NN_JOIN_CONT_MINING_REDUCED[ which(NN_JOIN_CONT_MINING_REDUCED$METAL=='Cr'), ]
Cu_NN <- NN_JOIN_CONT_MINING_REDUCED[ which(NN_JOIN_CONT_MINING_REDUCED$METAL=='Cu'), ]
Hg_NN <- NN_JOIN_CONT_MINING_REDUCED[ which(NN_JOIN_CONT_MINING_REDUCED$METAL=='Hg'), ]
Ni_NN <- NN_JOIN_CONT_MINING_REDUCED[ which(NN_JOIN_CONT_MINING_REDUCED$METAL=='Ni'), ]
Pb_NN <- NN_JOIN_CONT_MINING_REDUCED[ which(NN_JOIN_CONT_MINING_REDUCED$METAL=='Pb'), ]
Zn_NN <- NN_JOIN_CONT_MINING_REDUCED[ which(NN_JOIN_CONT_MINING_REDUCED$METAL=='Zn'), ]

##############################################################
# Plot relation between value and capacity
ggplot(NN_JOIN_CONT_MINING_REDUCED, aes(VALUE, join_Capacity))+
  geom_point(aes(color=METAL), alpha = 0.3)+
  stat_smooth()+
  ggtitle("Relation between metal concentration and mining capacity")+
  ylab("CAPACITY")

ggplot(As_NN, aes(VALUE, join_Capacity))+
  geom_point(aes(color=join_MINERAL), alpha = 0.3)+
  stat_smooth()+
  geom_hline(yintercept=0.3, linetype="dashed", color="#A4A4A4")+
  ggtitle("Relation between As concentration and mining capacity")+
  ylab("CAPACITY")

ggplot(Cd_NN, aes(VALUE, join_Capacity))+
  geom_point(aes(color=join_MINERAL), alpha = 0.3)+
  stat_smooth()+
  geom_hline(yintercept=0.5, linetype="dashed", color ="#6E6E6E")+
  ggtitle("Relation between Cd concentration and mining capacity")+
  ylab("CAPACITY")

ggplot(Co_NN, aes(VALUE, join_Capacity))+
  geom_point(aes(color=join_MINERAL), alpha = 0.3)+
  stat_smooth()+
  ggtitle("Relation between Co concentration and mining capacity")+
  ylab("CAPACITY")

ggplot(Cr_NN, aes(VALUE, join_Capacity))+
  geom_point(aes(color=join_MINERAL), alpha = 0.3)+
  stat_smooth()+
  ggtitle("Relation between Cr concentration and mining capacity")+
  ylab("CAPACITY")

ggplot(Cu_NN, aes(VALUE, join_Capacity))+
  geom_point(aes(color=join_MINERAL), alpha = 0.3)+
  stat_smooth()+
  ggtitle("Relation between Cu concentration and mining capacity")+
  ylab("CAPACITY")

ggplot(Hg_NN, aes(VALUE, join_Capacity))+
  geom_point(aes(color=join_MINERAL), alpha = 0.3)+
  stat_smooth()+
  geom_hline(yintercept=0.05, linetype="dashed", color ="#1C1C1C")+
  ggtitle("Relation between Hg concentration and mining capacity")+
  ylab("CAPACITY")

ggplot(Ni_NN, aes(VALUE, join_Capacity))+
  geom_point(aes(color=join_MINERAL), alpha = 0.3)+
  stat_smooth()+
  ggtitle("Relation between Ni concentration and mining capacity")+
  ylab("CAPACITY")

ggplot(Pb_NN, aes(VALUE, join_Capacity))+
  geom_point(aes(color=join_MINERAL), alpha = 0.3)+
  stat_smooth()+
  geom_hline(yintercept=0.3, linetype="dashed", color ="#151515")+
  ggtitle("Relation between Pb concentration and mining capacity")+
  ylab("CAPACITY")

ggplot(Zn_NN, aes(VALUE, join_Capacity))+
  geom_point(aes(color=join_MINERAL), alpha = 0.3)+
  stat_smooth()+
  ggtitle("Relation between Zn concentration and mining capacity")+
  ylab("CAPACITY")

##############################################################
# Correlation
cor.test(NN_JOIN_CONT_MINING_REDUCED$VALUE, NN_JOIN_CONT_MINING_REDUCED$join_Capacity, method = "pearson")
# p-value > 0.05 so the correlation is not significant

cor.test(As_NN$VALUE, As_NN$join_Capacity, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Cd_NN$VALUE, Cd_NN$join_Capacity, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Co_NN$VALUE, Co_NN$join_Capacity, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Cr_NN$VALUE, Cr_NN$join_Capacity, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Cu_NN$VALUE, Cu_NN$join_Capacity, method = "pearson") # p-value < 0.05 so the correlation is significant, showing a relationship btw Cu contamination and mining capacity
cor.test(Hg_NN$VALUE, Hg_NN$join_Capacity, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Ni_NN$VALUE, Ni_NN$join_Capacity, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Pb_NN$VALUE, Pb_NN$join_Capacity, method = "pearson") # p-value > 0.05 so the correlation is not significant
cor.test(Zn_NN$VALUE, Zn_NN$join_Capacity, method = "pearson") # p-value > 0.05 so the correlation is not significant

##############################################################
# Linear model

model2 <- lm(VALUE~join_Capacity, data = NN_JOIN_CONT_MINING_REDUCED)
summary(model2)
# p-value > 0.05 so the model is not signficant

##############################################################
##############################################################
##############################################################
# COMPARISON WITH RANDOM DATA
DISTANCE_METAL_TO_MINERAL <- read_excel("F:/THESIS_VALENTIN/QGIS/DISTANCE_METAL_TO_MINERAL.xlsx", 
                                        col_types = c("text", "text", "numeric", 
                                                      "text", "text", "text", "text", "text", 
                                                      "text", "text", "text", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "text", "numeric"))
attach(DISTANCE_METAL_TO_MINERAL)
colnames(DISTANCE_METAL_TO_MINERAL)[17] <- "MINERAL"
colnames(DISTANCE_METAL_TO_MINERAL)[18] <- "DISTANCE"

DISTANCE_RANDOM_METAL_TO_MINERAL <- read_excel("F:/THESIS_VALENTIN/QGIS/DISTANCE_RANDOM_METAL_TO_MINERAL.xlsx", 
                                               col_types = c("text", "text", "numeric"))
attach(DISTANCE_RANDOM_METAL_TO_MINERAL)
colnames(DISTANCE_RANDOM_METAL_TO_MINERAL)[2] <- "mineral"
colnames(DISTANCE_RANDOM_METAL_TO_MINERAL)[3] <- "distance"

##############################################################
# Test normality and homoscedasticity
qqnorm(DISTANCE_RANDOM_METAL_TO_MINERAL$distance)
qqline(DISTANCE_RANDOM_METAL_TO_MINERAL$distance)
shapiro.test(DISTANCE_RANDOM_METAL_TO_MINERAL$distance) # p-value < 0.05 so distance is not normaly distributed

qqnorm(DISTANCE_METAL_TO_MINERAL$DISTANCE)
qqline(DISTANCE_METAL_TO_MINERAL$DISTANCE)
shapiro.test(DISTANCE_METAL_TO_MINERAL$DISTANCE) # p-value < 0.05 so DISTANCE is not normaly distributed

# NEED TO USE NON-PARAMETRIC TEST

##############################################################
# Kolmogorov test

ks.test(DISTANCE_RANDOM_METAL_TO_MINERAL$distance, DISTANCE_METAL_TO_MINERAL$DISTANCE)

# !!!INTERPRET THE RESLUTS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

##############################################################
# nMDS
library(vegan)
nmds_data <- NN_JOIN_CONT_MINING_REDUCED[,c("VALUE","join_Capacity","distance")]

nmds <- metaMDS(nmds_data, distance = "bray", k=2)
plot(nmds, color = METAL)
ordispider(nmds, groups = METAL, label = TRUE)

##############################################################
# PCA
library(ggbiplot)
NN_JOIN_PCA <- prcomp(NN_JOIN_CONT_MINING_REDUCED[,c("VALUE","join_Capacity","distance")], center = TRUE, scale = TRUE)
summary(NN_JOIN_PCA)
ggbiplot(NN_JOIN_PCA, obs.scale = 1, var.scale = 1,
         groups = DISTANCE_METAL_TO_MINERAL_REDUCED$METAL, ellipse = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
