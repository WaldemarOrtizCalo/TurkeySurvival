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
###############################################################################
###############################################################################
#                           Male [Annual]                                   ####

# Changing the name column to a factor
male_annual$name <- as.factor(male_annual$name)
male_annual$Year <- as.factor(male_annual$Year)

# Variable Extraction
SurvivalProb <- subset(male_annual,male_annual$name == "annual_survival")

##  GGPLOT 1 - Probability of Survival (Adult)                              ####

ggplot(SurvivalProb, aes(Year,value)) +
  geom_point()+
  ggtitle("Survival Probability")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))+
  facet_wrap(~Age)


###############################################################################
#                           Male [Seasonal]                                 ####

# Changing the seasonal names
male_seasonal$name[male_seasonal$name == "seasonal_survival_1"] <- 'seasonal_survival'
male_seasonal$name[male_seasonal$name == "seasonal_survival_2"] <- 'seasonal_survival'
male_seasonal$name[male_seasonal$name == "seasonal_survival_3"] <- 'seasonal_survival'
male_seasonal$name[male_seasonal$name == "seasonal_survival_4"] <- 'seasonal_survival'
male_seasonal$name[male_seasonal$name == "seasonal_survival_u"] <- 'seasonal_survival'
male_seasonal$name[male_seasonal$name == "seasonal_harvestP_1"] <- 'seasonal_harvest'
male_seasonal$name[male_seasonal$name == "seasonal_harvestP_2"] <- 'seasonal_harvest'
male_seasonal$name[male_seasonal$name == "seasonal_harvestP_3"] <- 'seasonal_harvest'
male_seasonal$name[male_seasonal$name == "seasonal_harvestP_4"] <- 'seasonal_harvest'
male_seasonal$name[male_seasonal$name == "seasonal_harvestP_u"] <- 'seasonal_harvest'
male_seasonal$name[male_seasonal$name == "seasonal_mortality_1"] <- 'seasonal_mortality'
male_seasonal$name[male_seasonal$name == "seasonal_mortality_2"] <- 'seasonal_mortality'
male_seasonal$name[male_seasonal$name == "seasonal_mortality_3"] <- 'seasonal_mortality'
male_seasonal$name[male_seasonal$name == "seasonal_mortality_4"] <- 'seasonal_mortality'
male_seasonal$name[male_seasonal$name == "seasonal_mortality_u"] <- 'seasonal_mortality'
male_seasonal$name[male_seasonal$name == "seasonal_natural_mortality_1"] <- 'seasonal_natural_mortality'
male_seasonal$name[male_seasonal$name == "seasonal_natural_mortality_2"] <- 'seasonal_natural_mortality'
male_seasonal$name[male_seasonal$name == "seasonal_natural_mortality_3"] <- 'seasonal_natural_mortality'
male_seasonal$name[male_seasonal$name == "seasonal_natural_mortality_4"] <- 'seasonal_natural_mortality'
male_seasonal$name[male_seasonal$name == "seasonal_natural_mortality_u"] <- 'seasonal_natural_mortality'
male_seasonal$name[male_seasonal$name == "seasonal_natural_survival_1"] <- 'seasonal_natural_survival'
male_seasonal$name[male_seasonal$name == "seasonal_natural_survival_2"] <- 'seasonal_natural_survival'
male_seasonal$name[male_seasonal$name == "seasonal_natural_survival_3"] <- 'seasonal_natural_survival'
male_seasonal$name[male_seasonal$name == "seasonal_natural_survival_4"] <- 'seasonal_natural_survival'
male_seasonal$name[male_seasonal$name == "seasonal_natural_survival_u"] <- 'seasonal_natural_survival'

# Changing the name column to a factor
male_seasonal$name <- as.factor(male_seasonal$name)
male_seasonal$Season <- as.factor(male_seasonal$Season)
levels(male_seasonal$Season) <- c("Spring","Summer","Fall","Winter")

# Variable Extraction
SurvivalProb      <- subset(male_seasonal,male_seasonal$name == "seasonal_survival")
MortalityProb     <- subset(male_seasonal,male_seasonal$name == "seasonal_mortality")
NaturalMortProb   <- subset(male_seasonal,male_seasonal$name == "seasonal_natural_mortality")
NaturalSurvProb   <- subset(male_seasonal,male_seasonal$name == "seasonal_natural_survival")
HarvestProb       <- subset(male_seasonal,male_seasonal$name == "seasonal_harvest")

##  GGPLOT 1  - Probability of Survival                                     ####

ggplot(SurvivalProb, aes(Season,value)) +
  geom_point()+
  ggtitle("Survival Probability")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))+
  facet_wrap(~Age)


##  GGPLOT 2  - Probability of Mortality                                    ####

ggplot(MortalityProb, aes(Season,value)) +
  geom_point()+
  ggtitle("Mortality Probability")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))+
  facet_wrap(~Age)

##  GGPLOT 3  - Probability of Natural Mortality                            ####

ggplot(NaturalMortProb, aes(Season,value)) +
  geom_point()+
  ggtitle("Natural Mortality Probability")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))+
  facet_wrap(~Age)

##  GGPLOT 4  - Probability of Natural Survival                             ####

ggplot(NaturalSurvProb, aes(Season,value)) +
  geom_point()+
  ggtitle("Natural Survival Probability")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))+
  facet_wrap(~Age)

##  GGPLOT 5  - Probability of Harvest                                      ####

ggplot(HarvestProb, aes(Season,value)) +
  geom_point()+
  ggtitle("Harvest Probability")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))+
  facet_wrap(~Age)

###############################################################################
#                            Male [Monthly]                                 ####

# Changing the seasonal names
male_monthly$name[male_monthly$name == "survivalP_1"] <- 'monthly_survival'
male_monthly$name[male_monthly$name == "survivalP_2"] <- 'monthly_survival'
male_monthly$name[male_monthly$name == "survivalP_3"] <- 'monthly_survival'
male_monthly$name[male_monthly$name == "survivalP_4"] <- 'monthly_survival'
male_monthly$name[male_monthly$name == "survivalP_u"] <- 'monthly_survival'
male_monthly$name[male_monthly$name == "deathP_1"] <- 'monthly_death'
male_monthly$name[male_monthly$name == "deathP_2"] <- 'monthly_death'
male_monthly$name[male_monthly$name == "deathP_3"] <- 'monthly_death'
male_monthly$name[male_monthly$name == "deathP_4"] <- 'monthly_death'
male_monthly$name[male_monthly$name == "deathP_u"] <- 'monthly_death'
male_monthly$name[male_monthly$name == "naturalMortP_1"] <- 'monthly_natural_mortality'
male_monthly$name[male_monthly$name == "naturalMortP_2"] <- 'monthly_natural_mortality'
male_monthly$name[male_monthly$name == "naturalMortP_3"] <- 'monthly_natural_mortality'
male_monthly$name[male_monthly$name == "naturalMortP_4"] <- 'monthly_natural_mortality'
male_monthly$name[male_monthly$name == "naturalMortP_u"] <- 'monthly_natural_mortality'
male_monthly$name[male_monthly$name == "harvestP_1"] <- 'monthly_harvest'
male_monthly$name[male_monthly$name == "harvestP_2"] <- 'monthly_harvest'
male_monthly$name[male_monthly$name == "harvestP_3"] <- 'monthly_harvest'
male_monthly$name[male_monthly$name == "harvestP_4"] <- 'monthly_harvest'
male_monthly$name[male_monthly$name == "harvestP_u"] <- 'monthly_harvest'

# Changing the name column to a factor
male_monthly$name <- as.factor(male_monthly$name)
male_monthly$Month <- as.factor(male_monthly$Month)
levels(male_monthly$Month) <- c("Mar","Apr","May",
                                  "June","July","Aug",
                                  "Sep","Oct","Nov",
                                  "Dec","Jan","Feb")

# Variable Extraction
SurvivalProb       <- subset(male_monthly,male_monthly$name == "monthly_survival")
DeathProb           <- subset(male_monthly,male_monthly$name == "monthly_death")
NaturalMortProb    <- subset(male_monthly,male_monthly$name == "monthly_natural_mortality")
HarvestProb        <- subset(male_monthly,male_monthly$name == "monthly_harvest")

##  GGPLOT 1 - Probability of Survival                                      ####

ggplot(SurvivalProb, aes(Month,value)) +
  geom_point()+
  ggtitle("Survival Probability")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))+
  facet_wrap(~Age)

##  GGPLOT 2 - Probability of Death                                         ####

ggplot(DeathProb, aes(Month,value)) +
  geom_point()+
  ggtitle("Death Probability")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))+
  facet_wrap(~Age)

##  GGPLOT 3 - Probability of Natural Mortality                             ####

ggplot(NaturalMortProb, aes(Month,value)) +
  geom_point()+
  ggtitle("Natural Mortality Probability")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))+
  facet_wrap(~Age)

##  GGPLOT 4 - Probability of Harvest                                       ####

ggplot(HarvestProb, aes(Month,value)) +
  geom_point()+
  ggtitle("Harvest Probability")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))+
  facet_wrap(~Age)

###############################################################################
###############################################################################
###############################################################################
#                            Female [Annual]                                ####

# Changing the name column to a factor
female_annual$name <- as.factor(female_annual$name)
female_annual$Year <- as.factor(female_annual$Year)

# Variable Extraction
SurvivalProb_Adult    <- subset(female_annual,female_annual$name == "annual_survival_a")
SurvivalProb_Juvenile <- subset(female_annual,female_annual$name == "annual_survival_j")

##  GGPLOT 1 - Probability of Survival (Adult)                              ####

ggplot(SurvivalProb_Adult, aes(Year,value)) +
  geom_point()+
  ggtitle("Survival Probability (Adult)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))


##  GGPLOT 2 - Probability of Survival (Juvenile)                           ####

ggplot(SurvivalProb_Juvenile, aes(Year,value)) +
  geom_point()+
  ggtitle("Survival Probability (Juvenile)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

###############################################################################
#                            Female [Seasonal]                              ####

# Changing the name column to a factor
female_seasonal$name <- as.factor(female_seasonal$name)
female_seasonal$Season <- as.factor(female_seasonal$Season)
levels(female_seasonal$Season) <- c("Spring","Summer","Fall","Winter")

# Variable Extraction
SurvivalProb_Adult        <- subset(female_seasonal,female_seasonal$name == "seasonal_survival_a")
SurvivalProb_Juvenile     <- subset(female_seasonal,female_seasonal$name == "seasonal_survival_j")
MortalityProb_Adult       <- subset(female_seasonal,female_seasonal$name == "seasonal_mortality_a")
MortalityProb_Juvenile    <- subset(female_seasonal,female_seasonal$name == "seasonal_mortality_j")
NaturalMortProb_Adult     <- subset(female_seasonal,female_seasonal$name == "seasonal_natural_mortality_a")
NaturalMortProb_Juvenile  <- subset(female_seasonal,female_seasonal$name == "seasonal_natural_mortality_j")
NaturalSurvProb_Adult     <- subset(female_seasonal,female_seasonal$name == "seasonal_natural_survival_a")
NaturalSurvProb_Juvenile  <- subset(female_seasonal,female_seasonal$name == "seasonal_natural_survival_j")
HarvestProb_Adult         <- subset(female_seasonal,female_seasonal$name == "seasonal_harvestP_a")
HarvestProb_Juvenile      <- subset(female_seasonal,female_seasonal$name == "seasonal_harvestP_j")

##  GGPLOT 1  - Probability of Survival (Adult)                             ####

ggplot(SurvivalProb_Adult, aes(Season,value)) +
  geom_point()+
  ggtitle("Survival Probability (Adult)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))


##  GGPLOT 2  - Probability of Survival (Juvenile)                          ####

ggplot(SurvivalProb_Juvenile, aes(Season,value)) +
  geom_point()+
  ggtitle("Survival Probability (Juvenile)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

##  GGPLOT 3  - Probability of Mortality (Adult)                            ####

ggplot(MortalityProb_Adult, aes(Season,value)) +
  geom_point()+
  ggtitle("Mortality Probability (Adult)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

##  GGPLOT 4  - Probability of Mortality (Juvenile)                         ####

ggplot(MortalityProb_Juvenile, aes(Season,value)) +
  geom_point()+
  ggtitle("Mortality Probability (Juvenile)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

##  GGPLOT 5  - Probability of Natural Mortality (Adult)                    ####

ggplot(NaturalMortProb_Adult, aes(Season,value)) +
  geom_point()+
  ggtitle("Natural Mortality Probability (Adult)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

##  GGPLOT 6  - Probability of Natural Mortality (Juvenile)                 ####

ggplot(NaturalMortProb_Juvenile, aes(Season,value)) +
  geom_point()+
  ggtitle("Natural Mortality Probability (Juvenile)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

##  GGPLOT 7  - Probability of Natural Survival (Adult)                     ####

ggplot(NaturalSurvProb_Adult, aes(Season,value)) +
  geom_point()+
  ggtitle("Natural Survival Probability (Adult)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))


##  GGPLOT 4 - Probability of Survival (Juvenile)   
##  GGPLOT 8  - Probability of Natural Survival (Juvenile)                  ####

ggplot(NaturalSurvProb_Juvenile, aes(Season,value)) +
  geom_point()+
  ggtitle("Natural Survival Probability (Juvenile)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))


##  GGPLOT 9  - Probability of Harvest (Adult)                              ####

ggplot(HarvestProb_Adult, aes(Season,value)) +
  geom_point()+
  ggtitle("Harvest Probability (Adult)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

##  GGPLOT 10 - Probability of Harvest (Juvenile)                           ####

ggplot(HarvestProb_Juvenile, aes(Season,value)) +
  geom_point()+
  ggtitle("Harvest Probability (Juvenile)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))


###############################################################################
#                            Female [Monthly]                               ####

# Changing the name column to a factor
female_monthly$name <- as.factor(female_monthly$name)
female_monthly$Month <- as.factor(female_monthly$Month)
levels(female_monthly$Month) <- c("Mar","Apr","May",
                                  "June","July","Aug",
                                  "Sep","Oct","Nov",
                                  "Dec","Jan","Feb")

# Variable Extraction
DeathProb_Adult           <- subset(female_monthly,female_monthly$name == "deathP_a")
DeathProb_Juvenile       <- subset(female_monthly,female_monthly$name == "deathP_j")
SurvivalProb_Adult        <- subset(female_monthly,female_monthly$name == "survivalP_a")
SurvivalProb_Juvenile    <- subset(female_monthly,female_monthly$name == "survivalP_j")
NaturalMortProb_Adult     <- subset(female_monthly,female_monthly$name == "naturalMortP_a")
NaturalMortProb_Juvenile <- subset(female_monthly,female_monthly$name == "naturalMortP_j")
HarvestProb_Adult         <- subset(female_monthly,female_monthly$name == "harvestP_a")
HarvestProb_Juvenile     <- subset(female_monthly,female_monthly$name == "harvestP_j")

##  GGPLOT 1 - Probability of Death (Adult)                                 ####

ggplot(DeathProb_Adult, aes(Month,value)) +
  geom_point()+
  ggtitle("Death Probability (Adult)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

##  GGPLOT 2 - Probability of Death (Juvenile)                              ####

ggplot(DeathProb_Juvenile, aes(Month,value)) +
  geom_point()+
  ggtitle("Death Probability (Juvenile)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

##  GGPLOT 3 - Probability of Survival (Adult)                              ####

ggplot(SurvivalProb_Adult, aes(Month,value)) +
  geom_point()+
  ggtitle("Survival Probability (Adult)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))


##  GGPLOT 4 - Probability of Survival (Juvenile)                           ####

ggplot(SurvivalProb_Juvenile, aes(Month,value)) +
  geom_point()+
  ggtitle("Survival Probability (Juvenile)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

##  GGPLOT 5 - Probability of Natural Mortality (Adult)                     ####

ggplot(NaturalMortProb_Adult, aes(Month,value)) +
  geom_point()+
  ggtitle("Natural Mortality Probability (Adult)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

##  GGPLOT 6 - Probability of Natural Mortality (Juvenile)                  ####

ggplot(NaturalMortProb_Juvenile, aes(Month,value)) +
  geom_point()+
  ggtitle("Natural Mortality Probability (Juvenile)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

##  GGPLOT 7 - Probability of Harvest (Adult)                               ####

ggplot(HarvestProb_Adult, aes(Month,value)) +
  geom_point()+
  ggtitle("Harvest Probability (Adult)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

##  GGPLOT 8 - Probability of Harvest (Juvenile)                            ####

ggplot(HarvestProb_Juvenile, aes(Month,value)) +
  geom_point()+
  ggtitle("Harvest Probability (Juvenile)")+
  ylab("Probability")+
  geom_errorbar(aes(ymin = value - std.dev, ymax = value + std.dev))

