
# Analysis of the drivers of visitation rate stability 
# (pollinator richness and asynchrony)


#load packages
library(dplyr)
library(lme4)
library(DHARMa)
library(broom)



# data of visitation rate stability, richness and synchrony 
# as well as stability of plant reproductive success (fruit and seed)
data_plant<- read.csv("Analysis/data/data_stability.csv")



# We express asynchrony as 1 - φ, 
#where φ is Loreau and de Mazancourt’s (2008)
data_plant$asyn_LM= 1-data_plant$syncLM



# we analyze whether the stability of visitation rate is affect 
# by total richness and asynchrony (L & M index) 

# stability of visitation rate ~ pollinator richness
glm.nb1_sta= glmer.nb (cv_1_visitation ~ S_total + (1|Site_ID) + (1|Plant_gen_sp), data = data_plant)
summary(glm.nb1_sta)


simulationOutput1 <- simulateResiduals(fittedModel = glm.nb1_sta, plot = F)
plot(simulationOutput1)



# stability of visitation rate ~ asynchrony
glm.nb2_sta= glmer.nb (cv_1_visitation ~ asyn_LM + (1|Site_ID) + (1|Plant_gen_sp), data = data_plant)
summary(glm.nb2_sta)

simulationOutput2 <- simulateResiduals(fittedModel = glm.nb2_sta, plot = F)
plot(simulationOutput2)


# influence of pollinator richness on the asynchrony 
mod_rich_asyn= lmer (asyn_LM ~ S_total +  (1|Site_ID),data = data_plant)
summary(mod_rich_asyn)

simulationOutput3 <- simulateResiduals(fittedModel = mod_rich_asyn, plot = F)
plot(simulationOutput3)





# we also analysed the effect of richness and synchrony on the stability of visitation rate 
# per plant species separately

sta_plant_richness<-data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()



sta_plant_asynchrony<-data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~asyn_LM,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()





