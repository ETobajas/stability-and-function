
library(tidyverse)
library(lme4)
library(effects)
library(patchwork)


Pollinator_Plant<- read.csv("Data/Pollinator_Plant.csv") #400 observaciones
Pollinator_Plant<-Pollinator_Plant[,-1]
Pollinator_Plant$Year= as.factor(Pollinator_Plant$Year)
Pollinator_Plant$Site_ID= as.factor(Pollinator_Plant$Site_ID)
Pollinator_Plant$Plant_gen_sp= as.factor(Pollinator_Plant$Plant_gen_sp)


# species out
#plant species that for each year occur only at one site
spp_out <- Pollinator_Plant %>%
  group_by(Plant_gen_sp, Year) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id == 1) %>%
  select(Plant_gen_sp, Year)


Pollinator_Plant= anti_join(Pollinator_Plant, spp_out)

# #scale seed number variable
Pollinator_Plant=Pollinator_Plant %>% 
  group_by(Plant_gen_sp) %>%
  mutate(seed_sc=scale(Seeds, center = T, scale = T))

#model3
mod3_to= lmer(seed_sc ~S_total * Year + (1|Site_ID/Plant_gen_sp),data=Pollinator_Plant)

#getting effects for the two variables
ee_mod3 <- as.data.frame(Effect(c("S_total","Year"),mod = mod3_to))

p3=ggplot(ee_mod3, aes(S_total,fit))+
  geom_line(size=0.8)+ 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data= Pollinator_Plant, aes(x = S_total , y = seed_sc),shape=21, fill="grey")+
  labs(x = "Pollinator richness",y="Seed number")+
  ylim(-3,3)+
  facet_wrap(~Year, nrow = 1) + 
  theme_bw ()+
  theme(legend.position = "none")

#model4
mod4_to= lmer(seed_sc ~ visitatio_rate*Year + (1|Site_ID/Plant_gen_sp),data=Pollinator_Plant)


#getting effects for the two variables
ee_mod4 <- as.data.frame(Effect(c("visitatio_rate","Year"),mod = mod4_to))

#plot

p4 = ggplot(ee_mod4, aes(visitatio_rate,fit))+
  geom_line(size=0.8)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data= Pollinator_Plant, aes(x =visitatio_rate , y = seed_sc),shape=21, fill="grey")+
  labs(x = "Visitation rate",y="Seed number")+
  facet_wrap(~Year, nrow = 1) + 
  coord_cartesian(xlim=c(0, quantile(Pollinator_Plant$visitatio_rate,0.95)),ylim = c(-3,3))+
  theme_bw ()+
  theme(legend.position="none")

p3/p4
