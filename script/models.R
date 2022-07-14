

library(tidyverse)
#install.packages("broom.mixed")
library(broom.mixed)
library(broom)
#install.packages("lmerTest")
library(lmerTest)
library(dplyr)


Pollinator_Plant<- read.csv("Data/Pollinator_Plant.csv") #400 observaciones
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

corre<-data2 %>%
mutate(newcolum = rownames(data2))%>%
separate(newcolum, into=c('species', 'rows'), extra = "merge", sep = "\\.")%>%
select(species, rows,everything())  %>%
remove_rownames() %>%
  mutate(species = replace(species, duplicated(species), ""))


#corrplot 
a %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ corrplot::corrplot(cor(select(.x, where(is.numeric)),
  use = "complete.obs"),method="number",type="upper",
  tl.cex=0.8,title = first(.x$Plant_gen_sp)), .keep = TRUE)
   

library(patchwork)
library(ggcorrplot)

df_with_plots <- a %>%
  group_by(Plant_gen_sp) %>%
  nest() %>%
  mutate(plot = map(data, function(.x) {
    .x  %>%
      cor() %>%
      ggcorrplot::ggcorrplot(show.diag = F, type="lower",lab=TRUE)
  }))

plots1 <- map2(df_with_plots$plot, df_with_plots$Plant_gen_sp, ~(.x + labs(title = .y)))

install.packages("ggpubr")
library(ggpubr)

plots1[[1]] + plots1[[2]] + plots1[[3]] + plots1[[4]]+plots1[[5]]+plots1[[6]]+plots1[[7]]+
  plots1[[8]]+plots1[[9]]+plots1[[10]]+plots1[[11]]+plots1[[12]]


ggarrange(plots1[[1]], plots1[[2]], plots1[[3]], plots1[[4]], ncol=2, nrow=2, common.legend = TRUE, legend="bottom")


#df_with_plots$plot %>% wrap_plots(ncol = 2)

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

# Frequency
ta_seed3<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Frequency+(1|Site_ID),data))) %>%
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

# frequency
ta_weight3<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Frequency+(1|Site_ID),data))) %>%
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


# frequency
ta_f3<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Frequency + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


#########
# Lavandula species together (L.peduncalata (49 obs) and L.stoechas (24 obs))

lavan_tog=Pollinator_Plant %>%
  mutate(Plant_gen_sp = recode(Plant_gen_sp, "Lavandula pedunculata" = "Lavandula","Lavandula stoechas" = "Lavandula"))

#seed number
# richness
ta_seed_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~richness+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


# visistation rate
ta_seed2_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~visitatio_rate+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

# Frequency
ta_seed3_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Frequency+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


## seed weight
# richnes
ta_weight_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~richness+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

# visistation rate
ta_weight2_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~visitatio_rate+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

# frequency
ta_weight3_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Frequency+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


## Fruit proportion

# richnes
ta_f_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~richness + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()



# visistation rate
ta_f2_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~visitatio_rate + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


# frequency
ta_f3_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Frequency + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()
