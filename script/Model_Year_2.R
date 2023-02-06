library(tidyverse)
library(broom.mixed)
library(broom)
library(lmerTest)
library(dplyr)
library(patchwork)
library(ggcorrplot)
library(ggpubr)
library (lme4)
library(sjPlot)
library(car)
library(effects)
library(RColorBrewer)
library(piecewiseSEM)
library(multcomp)


Pollinator_Plant<- read.csv("Data/Pollinator_Plant.csv") #400 observaciones
Pollinator_Plant<-Pollinator_Plant[,-1]
Pollinator_Plant$Year= as.factor(Pollinator_Plant$Year)
Pollinator_Plant$Site_ID= as.factor(Pollinator_Plant$Site_ID)
Pollinator_Plant$Plant_gen_sp= as.factor(Pollinator_Plant$Plant_gen_sp)

str(Pollinator_Plant)



# species out
#plant species that for each year occur only at one site
spp_out <- Pollinator_Plant %>%
  group_by(Plant_gen_sp, Year) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id == 1) %>%
  dplyr::select(Plant_gen_sp, Year)


Pollinator_Plant= anti_join(Pollinator_Plant, spp_out)

levels(factor(Pollinator_Plant$Plant_gen_sp))


#we analyze whether the fruit proportion is affected by richness, 
#year and its interaction for all data 
#with plant species nested within site as random effect
mod1_to= glmer(cbind(fruit_formado, Fruit_No) ~ S_total * Year + (1|Site_ID/Plant_gen_sp),family=binomial,data=Pollinator_Plant)
summary(mod1_to)
car::Anova(mod1_to,type="III")


plot_model(mod1_to, type = "int")
plot_model(mod1_to, type = "pred")

summary(glht(mod1_to, mcp(Year="Tukey")))


#getting effects for the two variables
ee_mod1 <- as.data.frame(Effect(c("S_total","Year"),mod = mod1_to))

p1=ggplot(ee_mod1, aes(S_total,fit))+
  geom_line(size=0.8)+ 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data= Pollinator_Plant, aes(x = S_total , y = fruit_proportion),shape=21, fill="grey")+
  labs(x = "richness",y="fruit proportion")+
  ylim(0,1)+
  facet_wrap(~Year, nrow = 1) + 
  theme_bw ()+
  theme(legend.position = "none")




# and visitation rate (in one minute), year and its interaction
mod2_to= glmer(cbind(fruit_formado, Fruit_No) ~ visitatio_rate * Year + (1|Site_ID/Plant_gen_sp),family=binomial,data=Pollinator_Plant)
summary(mod2_to)
car::Anova(mod2_to,type="III")


plot_model(mod2_to, type = "int")
plot_model(mod2_to, type = "pred")


#getting effects for the two variables
ee_mod2 <- as.data.frame(Effect(c("visitatio_rate","Year"),mod = mod2_to))

p2=ggplot(ee_mod2, aes(visitatio_rate,fit))+
  geom_line(size=0.8)+  
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data= Pollinator_Plant, aes(x =visitatio_rate , y = fruit_proportion),shape=21, fill="grey")+
  labs(x = "Visitation rate",y="fruit proportion")+
  facet_wrap(~Year, nrow = 1) + 
  coord_cartesian(xlim=c(0, quantile(Pollinator_Plant$visitatio_rate,0.95)))+
  theme_bw ()+
  theme(legend.position = "none")



p1/p2


#we also analyze whether the seed number is affected by richness, 
#year and its interaction for all data 
#with plant species nested within site as random effect

# #scale seed number variable
Pollinator_Plant=Pollinator_Plant %>% 
  group_by(Plant_gen_sp) %>%
  mutate(seed_sc=scale(Seeds, center = T, scale = T))


#model
mod3_to= lmer(seed_sc ~S_total * Year + (1|Site_ID/Plant_gen_sp),data=Pollinator_Plant)
summary(mod3_to)
car::Anova(mod3_to,type="III")

plot_model(mod3_to, type = "int", show.data = F)
plot_model(mod3_to, type="pred")

#getting effects for the two variables
ee_mod3 <- as.data.frame(Effect(c("S_total","Year"),mod = mod3_to))

p3=ggplot(ee_mod3, aes(S_total,fit))+
  geom_line(size=0.8)+ 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data= Pollinator_Plant, aes(x = S_total , y = seed_sc),shape=21, fill="grey")+
  labs(x = "richness",y="Seed")+
  ylim(-3,3)+
  facet_wrap(~Year, nrow = 1) + 
  theme_bw ()+
  theme(legend.position = "none")



#model

mod4_to= lmer(seed_sc ~ visitatio_rate*Year + (1|Site_ID/Plant_gen_sp),data=Pollinator_Plant)
summary(mod4_to)
car::Anova(mod4_to, type="III")

plot_model(mod4_to, type = "int", show.data = F)
plot_model(mod4_to, type="pred")


#getting effects for the two variables
ee_mod4 <- as.data.frame(Effect(c("visitatio_rate","Year"),mod = mod4_to))

#plot

p4 = ggplot(ee_mod4, aes(visitatio_rate,fit))+
  geom_line(size=0.8)+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data= Pollinator_Plant, aes(x =visitatio_rate , y = seed_sc),shape=21, fill="grey")+
  labs(x = "Visitation rate",y="Seed")+
  facet_wrap(~Year, nrow = 1) + 
  coord_cartesian(xlim=c(0, quantile(Pollinator_Plant$visitatio_rate,0.95)),ylim = c(-3,3))+
  theme_bw ()+
  theme(legend.position="none")



p3/p4




