# Biodiversity-productivity script - 2025
# Noémie A. Pichon, Anu Eskelinen et al.
# data available for review and upon acceptance on Dryad

# load libraries
library(lme4)
library(nlme)
library(ggplot2)
library(cowplot)

# rm(list = ls())

source("summarySE.R")
pd = position_dodge(0.2)
# setwd("~/")


#

biomass_productivity_main_ds <- read.table("biomass_productivity_main_ds.txt", header = T)
biomass_productivity_main_ds$Treatment = paste(biomass_productivity_main_ds$NPK, biomass_productivity_main_ds$Exclosure, sep = "_")
regrowth_main <- read.table("regrowth_main.txt", header = T)
#

#### Statistics main models ####


## Productivity 
# live and dead biomass
# without negative values

productivity_main <- biomass_productivity_main_ds[!is.na(biomass_productivity_main_ds$Productivity),]
lm_productivity = lme(Productivity ~ Exclosure*NPK, random = ~1|Site/Block, productivity_main)
anova(lm_productivity)
round(coef(summary(lm_productivity))[,c(1:2)], 3)
#


## Offtake
# 3 outliers removed
herbivoreofftake_main <- biomass_productivity_main_ds[!is.na(biomass_productivity_main_ds$HerbivoreOfftake),]
lm_herbivoreofftake = lme(HerbivoreOfftake ~ Exclosure*NPK, random = ~1|Site/Block, 
                          subset(herbivoreofftake_main, HerbivoreOfftake < 1000 & HerbivoreOfftake > -500))
anova(lm_herbivoreofftake)
round(coef(summary(lm_herbivoreofftake))[,c(1:2)], 3)



## Biomass
# live and dead biomass

biomass_main <- biomass_productivity_main_ds[!is.na(biomass_productivity_main_ds$Biomass),]
lm_biomass = lme(Biomass ~ Exclosure*NPK, random = ~1|Site/Block, biomass_main)
anova(lm_biomass)
round(coef(summary(lm_biomass))[,c(1:2)], 3)
#



## Belowground biomass

belowgroundbiomass_main <- biomass_productivity_main_ds[!is.na(biomass_productivity_main_ds$BelowgroundBiomass),]
lm_belowgroundbiomass = lme(log(BelowgroundBiomass) ~ Exclosure*NPK, random = ~1|Site/Block, belowgroundbiomass_main)
anova(lm_belowgroundbiomass)

# untransformed coefficients
lm_belowgroundbiomass_untransformed = lme((BelowgroundBiomass) ~ Exclosure*NPK, random = ~1|Site/Block, belowgroundbiomass_main)
round(coef(summary(lm_belowgroundbiomass_untransformed))[,c(1:2)], 3)
#


## Above-belowground productivity ratio
abovebelowground_main <- biomass_productivity_main_ds[!is.na(biomass_productivity_main_ds$AboveBelowground_Productivity),]
lm_abovebelowground = lme(log(AboveBelowground_Productivity) ~ Exclosure*NPK, random = ~1|Site/Block, abovebelowground_main)
anova(lm_abovebelowground)
round(coef(summary(lm_abovebelowground))[,c(1:2)], 3)
#







## Biomass predicts productivity

biomass_productivity_main <- biomass_productivity_main_ds[!is.na(biomass_productivity_main_ds$Biomass),]
lm_productivity_biomass = lme(sqrt(Productivity+2) ~ Biomass*(Exclosure*NPK), random = ~1|Site/Block/Plot, biomass_productivity_main)
anova(lm_productivity_biomass)
round(coef(summary(lm_productivity_biomass))[,c(1:2)], 3)
#




## Productivity with regrowth

lm_regrowth = lme(Productivity ~ Regrowth, random = ~1|Site, regrowth_main)
anova(lm_regrowth)
round(coef(summary(lm_regrowth))[,c(1:2)], 3)

#



#### Plots in manuscript ####


# Productivity

prod_sum <- summarySE(productivity_main, measurevar = "Productivity", groupvars = c("Treatment"))
prod = ggplot(prod_sum, aes(x=Treatment, y=Productivity, color = Treatment)) +
  geom_errorbar(aes(ymin=Productivity-se, ymax=Productivity+se), width=.1, position = pd, size = 1.5) +
  geom_line(position = pd, size = 1.5, alpha = 1) +
  geom_point(position = pd, size = 4, alpha = 1) +
  geom_hline(yintercept=0) +
  theme_cowplot() +
  theme(legend.position = "none") +
  ylab(label = expression ("Aboveground productivity"~~(g/m^2/y))) +
  scale_x_discrete(limits= c("0_0", "0_1", "1_0", "1_1"),
                   labels = c("Control", "Exclosure", "NPK", "NPK & Excl"), name = element_blank()) +
  ylim(c(0,600)) +
  scale_colour_manual(values = c("#704357", "#fdbf68", "#78cdd0", "#afd472"))
prod
#


# Herbivory

herb_sum <- summarySE(subset(herbivoreofftake_main, HerbivoreOfftake < 1000 & HerbivoreOfftake > -500), measurevar = "HerbivoreOfftake", groupvars = c("Treatment"))
herb <- ggplot(herb_sum, aes(x=Treatment, y=HerbivoreOfftake, color = Treatment)) +
  geom_errorbar(aes(ymin=HerbivoreOfftake-se, ymax=HerbivoreOfftake+se), width=.1, position = pd, size =1.5) + 
  geom_line(position = pd, size = 1.5, alpha = 1) +
  geom_point(position = pd, size = 4, alpha = 1) +
  geom_hline(yintercept=0) +
  theme_cowplot() +
  ylim(c(-10,600)) +
  theme(legend.position = "none") +
  ylab(label = expression ("Herbivore offtake"~~(g/m^2/y))) +
  scale_x_discrete(limits= c("0_0", "0_1", "1_0", "1_1"), 
                   labels = c("Control", "Exclosure", "NPK", "NPK & Excl"), name = element_blank()) +
  scale_colour_manual(values = c("#704357", "#fdbf68", "#78cdd0", "#afd472"))
herb
#


# Biomass

bio_sum <- summarySE(biomass_main, measurevar = "Biomass", groupvars = c("Treatment"))
bio <- ggplot(bio_sum, aes(x=Treatment, y=Biomass, color = Treatment)) +
  geom_errorbar(aes(ymin=Biomass-se, ymax=Biomass+se), width=.1, position = pd, size = 1.5) +
  geom_line(position = pd, size = 1.5, alpha = 1) +
  geom_point(position = pd, size = 4, alpha = 1) +
  geom_hline(yintercept=0) +
  theme_cowplot() +
  ylim(c(0,600)) +
  theme(legend.position = "none") +
  ylab(label = expression ("Aboveground biomass"~~(g/m^2))) +
  scale_x_discrete(limits= c("0_0", "0_1", "1_0", "1_1"), 
                   labels = c("Control", "Exclosure", "NPK", "NPK & Excl"), name = element_blank()) +
  scale_colour_manual(values = c("#704357", "#fdbf68", "#78cdd0", "#afd472"))
bio
#


# Belowground productivity

root_sum <- summarySE(belowgroundbiomass_main, measurevar = "BelowgroundBiomass", groupvars = c("Treatment"))
root = ggplot(root_sum, aes(x=Treatment, y=BelowgroundBiomass, color = Treatment)) +
  geom_errorbar(aes(ymin=BelowgroundBiomass-se, ymax=BelowgroundBiomass+se), width=.1, position = pd, size = 1.5) +
  geom_line(position = pd, size = 1.5, alpha = 1) +
  geom_point(position = pd, size = 4, alpha = 1) +
  geom_hline(yintercept=0) +
  theme_cowplot() +
  theme(legend.position = "none")+
  ylab(label = expression ("Belowground productivity"~~(g/y))) +
  scale_x_discrete(limits= c("0_0", "0_1", "1_0", "1_1"), 
                   labels = c("Control", "Exclosure", "NPK", "NPK & Excl"), name = element_blank()) +
  scale_colour_manual(values = c("#704357", "#fdbf68", "#78cdd0", "#afd472"))
root
#



# Above/belowground ratio, log transformed

abovebelowground_main$AboveBelowground_Productivity_log = log(abovebelowground_main$AboveBelowground_Productivity)

ratiorootlog_sum <- summarySE(abovebelowground_main, measurevar = "AboveBelowground_Productivity_log", groupvars = c("Treatment"))
ratiorootlog = ggplot(ratiorootlog_sum, aes(x=Treatment, y=AboveBelowground_Productivity_log, color = Treatment)) +
  geom_errorbar(aes(ymin=AboveBelowground_Productivity_log-se, ymax=AboveBelowground_Productivity_log+se), width=.1, position = pd, size = 1.5) +
  geom_line(position = pd, size = 1.5, alpha = 1) +
  geom_point(position = pd, size = 4, alpha = 1) +
  theme_cowplot() +
  theme(legend.position = "none") +
  ylab(label = "Aboveground-belowground\nproductivity ratio (log scale)") +
  scale_x_discrete(limits= c("0_0", "0_1", "1_0", "1_1"), 
                   labels = c("Control", "Exclosure", "NPK", "NPK & Excl"), name = element_blank()) +
  scale_colour_manual(values = c("#704357", "#fdbf68", "#78cdd0", "#afd472"))
ratiorootlog
#




# Plot Figure 2

plot_grid(prod, herb, bio, root, ratiorootlog,
          ncol = 2, nrow = 3, align = "v", labels = c("a)", "b)", "c)", "d)", "e)"))
# ggsave("Figure 2.png", width = 8.8, height = 11, units = "in")









# Biomass-productivity relationship

# compute standard errors of model prediction
lm_bio_prod = lme(Productivity ~ Biomass*(Exclosure*NPK), random = ~1|Site/Block, biomass_productivity_main)

newdat <- expand.grid(
  Exclosure = unique(biomass_productivity_main$Exclosure),
  NPK = unique(biomass_productivity_main$NPK),
  Biomass = c(min(biomass_productivity_main$Biomass),
                            max(biomass_productivity_main$Biomass)),
  Site = "potrok.ar",
  Block = 3,
  Productivity = 0
)

newdat$Productivity <- predict(lm_bio_prod, newdat, re.form=NA) # NULL or NA?
mm <- model.matrix(terms(lm_bio_prod), newdat)
pvar1 <- diag(mm %*% tcrossprod(vcov(lm_bio_prod), mm))
cmult <- 1.96

newdat <- data.frame(
  newdat
  , plo = newdat$Productivity-cmult*sqrt(pvar1)
  , phi = newdat$Productivity+cmult*sqrt(pvar1)
)

ggplot(data = newdat, aes(Biomass, Productivity, color=interaction(Exclosure, NPK))) +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  geom_smooth(method = lm, se = F, size = 1.5) +
  geom_ribbon(aes(ymin = plo, ymax = phi), linetype = 0, alpha = 0.05) +
  geom_line(aes(y=plo), size = 0.1, alpha = 0.7) +
  geom_line(aes(y=phi), size = 0.1, alpha = 0.7) +
  geom_point(data = biomass_productivity_main, aes(x=Biomass, y=Productivity, colour=interaction(Exclosure, NPK)),
             size = 3.5, alpha = 0.7) +
  scale_colour_manual(values = c("#704357", "#fdbf68", "#78cdd0", "#afd472"), 
                      labels = c("Control", "Exclosure", "NPK", "NPK & Exclosure")) +
  theme_cowplot() +
  theme(legend.title = element_blank()) +
  labs(y = expression("Aboveground productivity"~~(g/m^2/y)), x = expression("Aboveground biomass"~~(g/m^2)))
#

# ggsave("Figure 3.png", width = 6.2, height = 5, units = "in", bg = "white")
#




# Aboveground productivity with and without regrowth

regr_sum <- summarySE(regrowth_main, measurevar = "Productivity", groupvars = c("Regrowth"))
ggplot(regr_sum, aes(x=Regrowth, y=Productivity, color = Regrowth)) +
  geom_errorbar(aes(ymin=Productivity-se, ymax=Productivity+se), width=.1, position = pd, size = 1.5) +
  geom_line(position = pd, size = 1.5, alpha = 1) +
  geom_point(position = pd, size = 4, alpha = 1) +
  scale_color_manual(values = c("#66c2a4", "#006d2c")) +
  theme_cowplot() +
  theme(legend.position = "none")+
  scale_x_discrete(limits= c("no_regrowth", "regrowth"), 
                   labels = c("Without regrowth", "With regrowth"), name = element_blank())+
  labs(y = expression("Aboveground productivity"~~(g/m^2/y)))
#

# ggsave("Figure 4.png", width = 5.5, height = 4, units = "in", bg = "white")
#

