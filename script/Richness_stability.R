library(tidyverse)
library(broom.mixed)
library(broom)
library(lmerTest)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(ggeffects)
library(patchwork)
library(ggcorrplot)
library(ggpubr)
library(qqplotr)


Pollinator_Plant<- read.csv("Data/Pollinator_Plant.csv") 
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


# datos sin plant_id (mean de las variables porque hay plantas con 2 indv, otras 3, etc)
# nos quedamos con sitio - año - plant species
pol_plant2=Pollinator_Plant %>%
  group_by(Site_ID,Year, Plant_gen_sp) %>%
  summarise_if(is.numeric, mean)

levels(factor(pol_plant2$Plant_gen_sp)) #12 plant species 



## Stability of richness ##

cv_1richness<-pol_plant2 %>%
  group_by(Plant_gen_sp,Site_ID) %>%
  summarise_at(vars(S_total),cv_1 <- function(x, ...) {
    ( mean(x, ...)/sd(x, ...) )
  })%>%
  ungroup()


# Stability of fruit proportion
cv_1Fruit2<-pol_plant2 %>%
  group_by(Plant_gen_sp,Site_ID) %>%
  summarise_at(vars(fruit_proportion),cv_1 <- function(x, ...) {
    ( mean(x, ...)/sd(x, ...) )
  })%>%
  ungroup()




# seed number stability 
cv_1Seed2<-pol_plant2 %>%
  group_by(Plant_gen_sp,Site_ID) %>%
  summarise_at(vars(Seeds),cv_1 <- function(x, ...) {
    ( mean(x, ...)/sd(x, ...) )
  })%>%
  ungroup()



# join tables (sin plant_id)
stab=full_join(cv_1Fruit2, cv_1Seed2, by = c('Plant_gen_sp', 'Site_ID'))%>%
  full_join (., cv_1richness, by=c('Plant_gen_sp', 'Site_ID')) %>%
  rename(cv_1_fruit=fruit_proportion, cv_1_seed=Seeds, cv_1_richness=S_total)



# Analysis #

# eliminar plant species 
sp_out2=stab %>%
  group_by(Plant_gen_sp) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id <= 4) %>%
  select(Plant_gen_sp) %>% pull(Plant_gen_sp)

#Data with species in at least 5 sites
stab = stab %>% filter(!Plant_gen_sp %in% sp_out2)

levels(factor(stab$Plant_gen_sp))


stab=stab %>%
  filter(!Plant_gen_sp=="Salvia rosmarinus") %>%
  filter(!Plant_gen_sp=="Halimium calycinum")%>%
  filter(!Plant_gen_sp=="Lavandula stoechas")


## Replace Inf and -Inf with NA
stab[is.na(stab) | stab == "Inf"] <- NA 
stab[is.na(stab) | stab == "-Inf"] <- NA 


# we analyze whether the stability of fruit set is affect 
#by stability of richness
#incorporating site and plant species as random effects

mod_staR= lmer(cv_1_fruit ~ cv_1_richness + (1|Plant_gen_sp)+(1|Site_ID), data = stab)
summary(mod_staR)


sjPlot::plot_model(mod_staR, type="pred",  show.data = T)



# we analyze whether the stability of seed set is affect 
#by stability of richness
#incorporating site and plant species as random effects 

mod2_staR= lmer(cv_1_seed ~ cv_1_richness + (1|Plant_gen_sp)+(1|Site_ID), data = stab)
summary(mod2_staR)


sjPlot::plot_model(mod2_staR, type="pred",  show.data = T)
