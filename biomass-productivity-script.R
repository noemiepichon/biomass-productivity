# Biodiversity-productivity script - 2025
# Noémie A. Pichon, Anu Eskelinen et al.
# data available for review and upon acceptance on Dryad

# load libraries
library(lme4)
library(nlme)
# setwd("~/")


rm(list = ls())
#



#### Statistics main models ####


## Productivity 
# live and dead biomass
# without negative values

productivity_main <- read.table("productivity_main.txt", header = T)

lm_productivity = lme(Productivity ~ Exclosure*NPK, random = ~1|Site/Block, productivity_main)
anova(lm_productivity)
round(coef(summary(lm_productivity))[,c(1:2)], 3)
#


## Offtake
# 3 outliers removed
herbivoreofftake_main <- read.table("herbivoreofftake_main.txt", header = T)

lm_herbivoreofftake = lme(HerbivoreOfftake ~ Exclosure*NPK, random = ~1|Site/Block, 
                          subset(herbivoreofftake_main, HerbivoreOfftake < 1000 & HerbivoreOfftake > -500))
anova(lm_herbivoreofftake)
round(coef(summary(lm_herbivoreofftake))[,c(1:2)], 3)



## Biomass
# live and dead biomass

biomass_main <- read.table("biomass_main.txt", header = T)

lm_biomass = lme(Biomass ~ Exclosure*NPK, random = ~1|Site/Block, biomass_main)
anova(lm_biomass)
round(coef(summary(lm_biomass))[,c(1:2)], 3)
#



## Belowground biomass

belowgroundbiomass_main <- read.table("belowgroundbiomass_main.txt", header = T)

lm_belowgroundbiomass = lme(log(BelowgroundBiomass) ~ Exclosure*NPK, random = ~1|Site/Block, belowgroundbiomass_main)
anova(lm_belowgroundbiomass)

# untransformed coefficients
lm_belowgroundbiomass_untransformed = lme((BelowgroundBiomass) ~ Exclosure*NPK, random = ~1|Site/Block, belowgroundbiomass_main)
round(coef(summary(lm_belowgroundbiomass_untransformed))[,c(1:2)], 3)
#


## Above-belowground productivity ratio

abovebelowground_main <- read.table("abovebelowground_main.txt", header = T)

lm_abovebelowground = lme(log(AboveBelowground_Productivity) ~ Exclosure*NPK, random = ~1|Site/Block, abovebelowground_main)
anova(lm_abovebelowground)
round(coef(summary(lm_abovebelowground))[,c(1:2)], 3)
#







## Biomass predicts productivity

biomass_productivity_main <- read.table("biomass_productivity_main.txt", header = T)

lm_productivity_biomass = lme(sqrt(Productivity+2) ~ Biomass*(Exclosure*NPK), random = ~1|Site/Block/Plot, biomass_productivity_main)
anova(lm_productivity_biomass)
round(coef(summary(lm_productivity_biomass))[,c(1:2)], 3)
#




## Productivity with regrowth

regrowth_main <- read.table("regrowth_main.txt", header = T)

lm_regrowth = lme(Productivity ~ Regrowth, random = ~1|Site, regrowth_main)
anova(lm_regrowth)
round(coef(summary(lm_regrowth))[,c(1:2)], 3)



