

# stability of pollinator visitation rate influences 
# the stability of the plant reproductive success


#load packages
library(dplyr)
library(lme4)
library(DHARMa)
library(effects)
library(ggplot2)



# data of visitation rate stability, richness and synchrony 
# as well as stability of plant reproductive success (fruit and seed)
data_plant<- read.csv("Analysis/data/data_stability.csv")



# we analyze whether the stability of fruit set is affect 
# by stability of visitation rate
mod1_sta= lmer(cv_1_fruit ~ cv_1_visitation + (1|Plant_gen_sp)+(1|Site_ID), data = data_plant)
summary(mod1_sta)


simulationOutput_mod1 <- simulateResiduals(fittedModel = mod1_sta, plot = F)
plot(simulationOutput_mod1)


# plot model
effe_mod1_sta <-data.frame( effect("cv_1_visitation", mod1_sta, se = TRUE))
ggplot(effe_mod1_sta, aes(cv_1_visitation, fit)) + 
  geom_line(size=1,linetype = "dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.1)+
  coord_cartesian(ylim=c(-10,120))+
  geom_point(data = data_plant, aes(x =  cv_1_visitation, y = cv_1_fruit, color=Plant_gen_sp), size=3)+
  scale_colour_brewer(palette = "Set1")+
  theme_bw ()+
  labs(y = "Stability of fruit proportion",x="Stability of visitation rate")





# we analyze whether the stability of seed set is affect 
# by stability of visitation rate
mod2_sta= lmer(cv_1_seed ~ cv_1_visitation + (1|Plant_gen_sp)+(1|Site_ID), data = data_plant)
summary(mod2_sta)


simulationOutput_mod2 <- simulateResiduals(fittedModel = mod2_sta, plot = F)
plot(simulationOutput_mod2)




effe_mod2_sta <-data.frame( effect("cv_1_visitation", mod2_sta, se = TRUE))
ggplot(effe_mod2_sta, aes(cv_1_visitation, fit)) + 
  geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2)+
  coord_cartesian(ylim=c(-5,60))+
  geom_point(data = data_plant, aes(x =  cv_1_visitation, y = cv_1_seed, color=Plant_gen_sp), size=3)+
  scale_colour_brewer(palette = "Set1")+
  theme_bw ()+
  labs(y = "Stability of seed number",x="Stability of visitation rate")
