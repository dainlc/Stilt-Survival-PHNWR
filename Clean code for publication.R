# Clean R script for publication
library(readr)
library(readxl)
library(plyr)
library(dplyr)
library(effects)
library(car)
library(psych)

#	All analyses were performed in the program R 3.5.1. (R core team, 2013). Packages included plyr (v1.8.4; Wickham, 2011),... 
# ...dplyr (v0.8.3; Wickham et al. 2019), Rmisc (v1.5; Hope 2013), ggplot2 (v3.2.1; Wickham 2016), and car (v3.0.3; Fox & Weisberg 2019). 

citation(package = "base")
citation(package = "plyr")
citation(package = "dplyr")
citation(package = "Rmisc")
citation(package = "ggplot2")
citation(package = "car")

# Download seperate data sheets.------------------------------------------------------------------------------------------------
x2019_Hatching_Summary <- read_csv("Hawaiian Stilt project/R code for processing data/2019_Camera trap data - Hatching Summary.csv")
View(x2019_Hatching_Summary)
colnames(x2019_Hatching_Summary)[6] <- "prop.hat"
colnames(x2019_Hatching_Summary)[4] <- "eggs"
colnames(x2019_Hatching_Summary)[5] <- "hatched"
x2019_Hatching_Summary$`Ratio Hatched/Layed` = NULL # This is removing the ratio colunm which was never a helpful metric.


##Starting with 2019 Leeward sites only
Nestid = x2019_Hatching_Summary$`Nest Id`
Site = x2019_Hatching_Summary$Site
Site = as.factor(Site)
eggs = x2019_Hatching_Summary$`eggs`
hatched = x2019_Hatching_Summary$hatched
Time = x2019_Hatching_Summary$`Time in nesting area (hrs)`
prop.hat =x2019_Hatching_Summary$prop.hat
fate = x2019_Hatching_Summary$`Nest Fate (Hatch, Partial Hatch, Abandoned, Depredated, Unknown)`
fate = as.factor(fate)

library("Rmisc")

hat.sum <- summarySE(x2019_Hatching_Summary, measurevar = "prop.hat", groupvars = "Site")# Variables must be in quotations
hat.sum

Eggs.sum <- summarySE(x2019_Hatching_Summary, measurevar = "eggs", groupvars = "Site")# Variables must be in quotations
Eggs.sum

hatched.sum <- summarySE(x2019_Hatching_Summary, measurevar = "hatched", groupvars = "Site")# Variables must be in quotations
hatched.sum


library(ggplot2)

ggplot(data = hat.sum, aes(x = Site, y = prop.hat)) + 
  geom_bar(stat = "identity", fill = "lightgrey", width = 0.5) +
  theme_classic() +
  geom_errorbar(aes(ymin = I(prop.hat-se), ymax = I(prop.hat+se), width = .2)) + # The Capital I is telling r that the function perc.hat + or - se is a function and that you are not trying to call something
  labs(y ="Proportion of Hatched Eggs/Nest") +
  ylim(0,1)+
  theme(text = element_text(size = 20), axis.text = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=90, vjust=3)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(x = NULL)


ggplot(Eggs.sum, aes(x = Site, y = eggs)) + 
  geom_bar(stat = "identity", fill = "lightgrey", width = 0.5) +
  ylim(0,5) +
  theme_classic() +
  geom_errorbar(aes(ymin = I(eggs-se), ymax = I(eggs+se), width = .2)) + 
  labs(y ="# of Eggs Layed/Nest") +
  theme(text = element_text(size = 20), axis.text = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=90, vjust=3)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(x = NULL)


ggplot(hatched.sum, aes(x = Site, y = hatched)) + 
  geom_bar(stat = "identity", fill = "lightgrey", width = 0.5) +
  ylim(0,5) +
  theme_classic() +
  geom_errorbar(aes(ymin = I(hatched-se), ymax = I(hatched+se), width = .2)) + 
  labs(y ="# of Eggs Hatched/Nest")+
  theme(text = element_text(size = 20), axis.text = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=90, vjust=3)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(x = NULL)

ggplot(x2019_Hatching_Summary, aes(x = Site, y = frequency(fate), fill = fate)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_classic()+
  theme(text = element_text(size = 20), axis.text = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=90, vjust=3)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(x = NULL)


#Consolidated table for cleaner black and white graph

library(readr)
X2019_Camera_trap_data_Hatching_Summary_consolidated_fates_version_1_xlsb <- read_csv("Hawaiian Stilt project/R code for processing data/2019_Camera trap data - Hatching Summary consolidated fates(version 1).xlsb.csv")
View(X2019_Camera_trap_data_Hatching_Summary_consolidated_fates_version_1_xlsb)

x2019_Hatching_Summary_CF <-X2019_Camera_trap_data_Hatching_Summary_consolidated_fates_version_1_xlsb
Fate = x2019_Hatching_Summary_CF$`Nest Fate (Full Clutch Hatch, Partial Clutch Hatch, Other, Depredated)`
Fate = as.factor(Fate)
Site = x2019_Hatching_Summary_CF$Site


ggplot(x2019_Hatching_Summary_CF, aes(x = Site, y = frequency(Fate), fill = Fate)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_grey(start = 0, end = 0.9)+ 
  theme_classic()+
  theme(text = element_text(size = 30), axis.text = element_text(size = 20)) +
  theme(axis.title.y=element_text(angle=90, vjust=4)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(x = NULL, y = "Frequency")

summary(Fate)



# SE
sub.w = subset(prop.hat, Site == "Waiawa")#Here I can switch variables out like Time, prop.hat, # of eggs etc. out to get the SE.
sub.w

mean(sub.w, na.rm = TRUE)
w.sd = sd(sub.w, na.rm = TRUE)
w.se = w.sd/sqrt(21)
w.se

sub.h = subset(prop.hat, Site == "Honouliuli")
mean(sub.h, na.rm = TRUE)
h.sd = sd(sub.h, na.rm = TRUE)
h.se = h.sd/sqrt(9)
h.se

#code prints a Welch's Two Sample t-test which is a modified Student's t-test but assumes the two populations have unequal variances.
t.test(eggs ~ Site)
t.test(hatched ~ Site)
t.test(prop.hat ~ Site)


##paired t.test for eggs laid to hatched by site
Eggs.h = subset(eggs, Site == "Honouliuli")
hat.h = subset(hatched, Site =="Honouliuli")
t.test(Eggs.h, hat.h, paired = TRUE)


Eggs.w = subset(eggs, Site == "Waiawa")
hat.w = subset(hatched, Site =="Waiawa")
t.test(Eggs.w, hat.w, paired = TRUE)


##Time spent at the nest after the 1st chick hatched until the last chick leaves the nesting area.
hist(Time, main = 'Time spent at the nest', xlab = "Hours")
boxplot(Time ~ Site, main = 'Time spent at the nest', ylab = 'Hours')
t.test(Time ~ Site)
mean(Time, na.rm = TRUE)
summary(Time)
sd(Time, na.rm = TRUE)


