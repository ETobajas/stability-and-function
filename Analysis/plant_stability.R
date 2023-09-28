

# stability of pollinator visitation rate influences 
# the stability of the plant reproductive success


#load packages
library(dplyr)
library(lme4)
library(DHARMa)



# data of visitation rate stability, richness and synchrony 
# as well as stability of plant reproductive success (fruit and seed)
data_plant<- read.csv("Analysis/data/data_stability.csv")



# we analyze whether the stability of fruit set is affect 
# by stability of visitation rate
mod1_sta= lmer(cv_1_fruit ~ cv_1_visitation + (1|Plant_gen_sp)+(1|Site_ID), data = data_plant)
summary(mod1_sta)


simulationOutput_mod1 <- simulateResiduals(fittedModel = mod1_sta, plot = F)
plot(simulationOutput_mod1)



# we analyze whether the stability of seed set is affect 
# by stability of visitation rate
mod2_sta= lmer(cv_1_seed ~ cv_1_visitation + (1|Plant_gen_sp)+(1|Site_ID), data = data_plant)
summary(mod2_sta)


simulationOutput_mod2 <- simulateResiduals(fittedModel = mod2_sta, plot = F)
plot(simulationOutput_mod2)
