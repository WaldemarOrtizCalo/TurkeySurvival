#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2020-12-20 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

##  Library                                                                 ####
library(tidyverse)

##  Functions                                                               ####

##  Data                                                                    ####
female_annual   <- read_csv("1.Data/Processed/annual_f.csv")
female_seasonal <- read_csv("1.Data/Processed/seasonal_f.csv")
female_monthly  <- read_csv("1.Data/Processed/monthly_f.csv")
male_annual     <- read_csv("1.Data/Processed/annual_m.csv")
male_seasonal   <- read_csv("1.Data/Processed/seasonal_m.csv")
male_monthly    <- read_csv("1.Data/Processed/monthly_m.csv")

###############################################################################
#  Female [Monthly]                                                         ####

# Changing the name column to a factor
female_monthly$name <- as.factor(female_monthly$name)
female_monthly$Month <- as.factor(female_monthly$Month)

# Variable Extraction
DeathProb_Adult           <- subset(female_monthly,female_monthly$name == "deathP_a")
DeathProb_Juevenile       <- subset(female_monthly,female_monthly$name == "deathP_j")
SurvivalProb_Adult        <- subset(female_monthly,female_monthly$name == "survivalP_a")
SurvivalProb_Juevenile    <- subset(female_monthly,female_monthly$name == "survivalP_j")
NaturalMortProb_Adult     <- subset(female_monthly,female_monthly$name == "naturalMortP_a")
NaturalMortProb_Juevenile <- subset(female_monthly,female_monthly$name == "naturalMortP_j")
HarvestProb_Adult         <- subset(female_monthly,female_monthly$name == "harvestP_a")
HarvestProb_Juevenile     <- subset(female_monthly,female_monthly$name == "harvestP_j")

##  GGPLOT 1 - Probability of Death (Adult)                                 ####

ggplot(DeathProb_Adult, aes(Month,value)) +
  geom_boxplot()

##  GGPLOT 2 - Probability of Harvest (Adult)                               ####

ggplot(HarvestProb_Adult, aes(Month,value)) +
  geom_boxplot()+
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9)) 




