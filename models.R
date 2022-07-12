

library(tidyverse)
#install.packages("broom.mixed")
library(broom.mixed)
library(broom)
#install.packages("lmerTest")
library(lmerTest)
library(dplyr)



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


# correlation
a<-Pollinator_Plant %>% select(Plant_gen_sp,fruit_proportion, Seeds,Seed_weight,richness,visitatio_rate,Frequency)
a <- mutate_all(a, ~replace(., is.na(.), 0))

#by(a[,2:7], a$Plant_gen_sp, cor)

a_cor <- a %>%
  group_by(Plant_gen_sp) %>%
  do(cormat = cor(select(., -matches("Plant_gen_sp")))) 

a_cor[[2]]

list_1 <- a_cor[[2]]

a_cor[[1]]

v <- a_cor[[1]]
str(v)
names(list_1) <- v

str(list_1)
data2 <- do.call(rbind.data.frame, list_1)

data2 %>%
mutate(newcolum = rownames(data2))%>%
separate(newcolum, into=c('species', 'rows'), extra = "merge", sep = "\\.")%>%
select(species, rows,everything())  %>%
remove_rownames() %>%
  mutate(species = replace(species, duplicated(species), ""))




#Models _ Seed numbers 

require("lattice")
xyplot(Seeds ~ visitatio_rate|Plant_gen_sp, data = Pollinator_Plant, type = c("p", "r"))
xyplot(Seeds ~ richness|Plant_gen_sp, data = Pollinator_Plant, type = c("p", "r"))

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

require("lattice")
xyplot(Seed_weight ~ visitatio_rate|Plant_gen_sp, data = Pollinator_Plant, type = c("p", "r"))
xyplot(Seed_weight ~ richness|Plant_gen_sp, data = Pollinator_Plant, type = c("p", "r"))

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

require("lattice")
xyplot(fruit_proportion ~ visitatio_rate|Plant_gen_sp, data = Pollinator_Plant, type = c("p", "r"))
xyplot(fruit_proportion ~ richness|Plant_gen_sp, data = Pollinator_Plant, type = c("p", "r"))



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







