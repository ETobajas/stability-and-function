

library(tidyverse)
#install.packages("broom.mixed")
library(broom.mixed)
library(broom)
#install.packages("lmerTest")
library(lmerTest)



Pollinator_Plant<- read.csv("Data/Pollinator_Plant.csv") #402 observaciones
str(Pollinator_Plant)
Pollinator_Plant<-Pollinator_Plant[,-1]



#Species out
spp_out <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id == 1) %>%
  select(Plant_gen_sp) %>% pull(Plant_gen_sp)


#Data with species in more than 1 site
Pollinator_Plant = Pollinator_Plant %>% filter(!Plant_gen_sp %in% spp_out)


#Models _ Seed numbers 
# richness
ta_seed<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~richness+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()



# visistation rate
ta_seed2<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~visitatio_rate+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


#Models _ Seed weight 
# richnes
ta_weight<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~richness+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

# visistation rate
ta_weight2<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~visitatio_rate+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


#Models _ fruit proportion 
# richnes
ta_f<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~richness + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

# visistation rate
ta_f2<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~visitatio_rate + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()







