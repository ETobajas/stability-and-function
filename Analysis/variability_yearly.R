
# analysis of the variability in the yearly effect of pollinator richness 
# and visitation rate on plant reproductive success (fruit and seed)


#load packages
library(dplyr)
library(lme4)
library(DHARMa)
library(car)


# data of 12 plant species in five years: 
# fruits, seed number, visitation rate and richness.
pollinator_plant<- read.csv("Analysis/data/data_yearly.csv")
str(pollinator_plant)
pollinator_plant$Year= as.factor(pollinator_plant$Year)



# we analyze whether the fruit proportion is affected by richness,year and its interaction
mod1_fruit= glmer(cbind(fruit_formado, Fruit_No) ~ S_total * Year + (1|Site_ID/Plant_gen_sp),family=binomial,data=pollinator_plant)
summary(mod1_fruit)

Anova(mod1_fruit,type="III")

simulationOutput_mod1_fruit <- simulateResiduals(fittedModel = mod1_fruit, plot = F)
plot(simulationOutput_mod1_fruit)



# and visitation rate, year and its interaction
mod2_fruit= glmer(cbind(fruit_formado, Fruit_No) ~ visitatio_rate * Year + (1|Site_ID/Plant_gen_sp),family=binomial,data=pollinator_plant)
summary(mod2_fruit)

Anova(mod2_fruit,type="III")

simulationOutput_mod2_fruit <- simulateResiduals(fittedModel = mod2_fruit, plot = F)
plot(simulationOutput_mod2_fruit)




# we analyze whether the seed number is affected by richness,year and its interaction 
# scale seed number variable
pollinator_plant=pollinator_plant %>% 
  group_by(Plant_gen_sp) %>%
  mutate(seed_sc=scale(Seeds, center = T, scale = T))



mod1_seed= lmer(seed_sc ~S_total * Year + (1|Site_ID/Plant_gen_sp),data=pollinator_plant)
summary(mod1_seed)

Anova(mod1_seed,type="III")


simulationOutput_mod1_seed <- simulateResiduals(fittedModel = mod1_seed, plot = F)
plot(simulationOutput_mod1_seed)


# and visitation rate, year and its interaction
mod2_seed= lmer(seed_sc ~ visitatio_rate*Year + (1|Site_ID/Plant_gen_sp),data=pollinator_plant)
summary(mod2_seed)

Anova(mod2_seed, type="III")

simulationOutput_mod2_seed <- simulateResiduals(fittedModel = mod2_seed, plot = F)
plot(simulationOutput_mod2_seed)
