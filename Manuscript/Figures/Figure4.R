
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



# model 1
mod1_to= glmer(cbind(fruit_formado, Fruit_No) ~ S_total * Year + (1|Site_ID/Plant_gen_sp),family=binomial,data=Pollinator_Plant)

#getting effects for the two variables
ee_mod1 <- as.data.frame(Effect(c("S_total","Year"),mod = mod1_to))

p1=ggplot(ee_mod1, aes(S_total,fit))+
  geom_line(size=0.8)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data= Pollinator_Plant, aes(x = S_total , y = fruit_proportion),shape=21, fill="grey")+
  labs(x = "Pollinator richness",y="Fruit proportion")+
  facet_wrap(~Year, nrow = 1) + 
  theme_bw ()+
  theme(legend.position = "none")



# model 2
mod2_to= glmer(cbind(fruit_formado, Fruit_No) ~ visitatio_rate * Year + (1|Site_ID/Plant_gen_sp),family=binomial,data=Pollinator_Plant)

#getting effects for the two variables
ee_mod2 <- as.data.frame(Effect(c("visitatio_rate","Year"),mod = mod2_to))

p2=ggplot(ee_mod2, aes(visitatio_rate,fit))+
  geom_line(size=0.8)+  
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data= Pollinator_Plant, aes(x =visitatio_rate , y = fruit_proportion),shape=21, fill="grey")+
  labs(x = "Visitation rate",y="Fruit proportion")+
  facet_wrap(~Year, nrow = 1) + 
  coord_cartesian(xlim=c(0, quantile(Pollinator_Plant$visitatio_rate,0.95)))+
  theme_bw ()+
  theme(legend.position = "none")


p1/p2

