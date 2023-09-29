
# Analysis of the drivers of visitation rate stability 
# (pollinator richness and asynchrony)


#load packages
library(dplyr)
library(lme4)
library(DHARMa)
library(effects)
library(ggplot2)
library(broom)



# data of visitation rate stability, richness and synchrony 
# as well as stability of plant reproductive success (fruit and seed)
data_plant<- read.csv("Analysis/data/data_stability.csv")



# We express asynchrony as 1 - φ, 
#where φ is Loreau and de Mazancourt’s (2008)
data_plant$asyn_LM= 1-data_plant$syncLM



# we analyze whether the stability of visitation rate is affect 
# by total richness and asynchrony (L & M index) 


# stability of visitation rate ~ asynchrony
glm.nb1_sta= glmer.nb (cv_1_visitation ~ asyn_LM + (1|Site_ID) + (1|Plant_gen_sp), data = data_plant)
summary(glm.nb1_sta)

simulationOutput1 <- simulateResiduals(fittedModel = glm.nb1_sta, plot = F)
plot(simulationOutput1)


# plot model 
effe_mod_sta <-data.frame( effect("asyn_LM", glm.nb1_sta, se = TRUE))
ggplot(effe_mod_sta, aes(asyn_LM, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper),alpha=0.2)+
  labs(color='Plant species')+
  coord_cartesian(ylim=c(-0.5,9.5))+
  geom_point(data = data_plant, aes(x =  asyn_LM, y = cv_1_visitation, color=Plant_gen_sp), alpha=0.7,size=3)+
  scale_colour_brewer(palette = "Set1")+
  theme_bw ()+
  labs(x = "Pollinator asynchrony",y="Stability of visitation rate")


# stability of visitation rate ~ pollinator richness
glm.nb2_sta= glmer.nb (cv_1_visitation ~ S_total + (1|Site_ID) + (1|Plant_gen_sp), data = data_plant)
summary(glm.nb2_sta)


simulationOutput2 <- simulateResiduals(fittedModel = glm.nb2_sta, plot = F)
plot(simulationOutput2)


# plot model
effe2_mod_sta <-data.frame( effect("S_total", glm.nb2_sta, se = TRUE))
ggplot(effe2_mod_sta, aes(S_total, fit)) + geom_line(size=1,linetype = "dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.1)+
  labs(color='Plant species')+
  scale_y_continuous(limits = c(-0.5, 9.5))+
  geom_point(data = data_plant, aes(x =  S_total, y = cv_1_visitation, color=Plant_gen_sp), size=3)+
  scale_colour_brewer(palette = "Set1")+
  theme_bw ()+
  labs(x = "Pollinator richness",y="Stability of visitation rate")




# influence of pollinator richness on the asynchrony 
mod_rich_asyn= lmer (asyn_LM ~ S_total +  (1|Site_ID),data = data_plant)
summary(mod_rich_asyn)

simulationOutput3 <- simulateResiduals(fittedModel = mod_rich_asyn, plot = F)
plot(simulationOutput3)


#plot model 
effe_rich_mod_sta <-data.frame( effect("S_total", mod_rich_asyn, se = TRUE))
ggplot(effe_rich_mod_sta, aes(S_total, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2)+
  labs(color='Plant species')+
  coord_cartesian(ylim=c(0,1))+
  geom_point(data = data_plant, aes(x =  S_total, y = asyn_LM, color=Plant_gen_sp),size=3)+
  scale_colour_brewer(palette = "Set1")+
  theme_bw ()+
  labs(x = "Pollinator richness",y="Pollinator asynchronhy")




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





