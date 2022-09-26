

library(tidyverse)
#install.packages("broom.mixed")
library(broom.mixed)
library(broom)
#install.packages("lmerTest")
library(lmerTest)
library(dplyr)
library(patchwork)
library(ggcorrplot)
library(ggpubr)


Pollinator_Plant<- read.csv("Data/Pollinator_Plant.csv") #400 observaciones
str(Pollinator_Plant)
Pollinator_Plant<-Pollinator_Plant[,-1]
head(Pollinator_Plant)

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


#######
# correlacion por sitios en lugar de por plant species
b<-Pollinator_Plant %>% select(Site_ID,fruit_proportion, Seeds,Seed_weight,richness,visitatio_rate,Frequency)
b <- mutate_all(b, ~replace(., is.na(.), 0))

b_cor <- b %>%
  group_by(Site_ID) %>%
  do(cormat = cor(select(., -matches("Site_ID")))) 

b_cor[[2]]

list_1 <- b_cor[[2]]

b_cor[[1]]

v <- b_cor[[1]]
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


#####
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
   



df_with_plots <- a %>%
  group_by(Plant_gen_sp) %>%
  nest() %>%
  mutate(plot = map(data, function(.x) {
    .x  %>%
      cor() %>%
      ggcorrplot::ggcorrplot(show.diag = F, type="lower",lab=TRUE)
  }))

plots1 <- map2(df_with_plots$plot, df_with_plots$Plant_gen_sp, ~(.x + labs(title = .y)))




plots1[[1]] + plots1[[2]] + plots1[[3]] + plots1[[4]]+plots1[[5]]+plots1[[6]]+plots1[[7]]+
  plots1[[8]]+plots1[[9]]+plots1[[10]]+plots1[[11]]+plots1[[12]]


ggarrange(plots1[[1]], plots1[[2]], plots1[[3]], plots1[[4]], ncol=2, nrow=2, common.legend = TRUE, legend="bottom")


#df_with_plots$plot %>% wrap_plots(ncol = 2)


# replace NAs by 0
#hay especies de plantas que presentan datos de fruit y seed pero no de polinizador
#(durante el tiempo observado no se ve ningun polinizador )
Pollinator_Plant <- mutate_all(Pollinator_Plant, ~replace(., is.na(.), 0))


#Models _ Seed numbers---- 

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

# hymenoptera visitation
ta_seed4<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Hym_visit+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


#Models _ Seed weight---- 

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


# Hymenoptera visitation
ta_weight4<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Hym_visit+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


#Models _ fruit proportion ----

require("lattice")
xyplot(fruit_proportion ~ visitatio_rate|Plant_gen_sp, data = Pollinator_Plant, type = c("p", "r"))
xyplot(fruit_proportion ~ richness|Plant_gen_sp, data = Pollinator_Plant, type = c("p", "r"))


# richnes

#plot
plots_fruitproportion <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=richness, y=fruit_proportion) + 
   geom_point() + ggtitle(.y[[1]]))

plots_fruitproportion[[1]]+plots_fruitproportion[[2]]+plots_fruitproportion[[3]]+plots_fruitproportion[[4]]
plots_fruitproportion[[5]]+plots_fruitproportion[[6]]+plots_fruitproportion[[7]]+plots_fruitproportion[[8]] 
plots_fruitproportion[[9]]+plots_fruitproportion[[10]]+plots_fruitproportion[[11]]+plots_fruitproportion[[12]]


# model 
mod_fruitproportion<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~richness + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


mod_fruitproportion_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~richness + (1|Site_ID),family=binomial,data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_fruitproportion_trial<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~richness + (1|Site_ID),family=binomial,data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_fruitproportion_trial$plots[[1]])
plot(mod_fruitproportion_trial$plots[[2]])
plot(mod_fruitproportion_trial$plots[[3]])
plot(mod_fruitproportion_trial$plots[[4]])
plot(mod_fruitproportion_trial$plots[[5]])
plot(mod_fruitproportion_trial$plots[[6]])
plot(mod_fruitproportion_trial$plots[[7]])
plot(mod_fruitproportion_trial$plots[[8]])
plot(mod_fruitproportion_trial$plots[[9]])
plot(mod_fruitproportion_trial$plots[[10]])
plot(mod_fruitproportion_trial$plots[[11]])
plot(mod_fruitproportion_trial$plots[[12]])


# model plot 
mod_fruitproportion.1 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~richness + (1|Site_ID),family=binomial,data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "richness"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_fruitproportion.1$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  richness, y = fruit_proportion),shape=20)+ 
  labs(x = "richness",y="fruit proportion",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.1$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  richness, y = fruit_proportion),shape=20)+ 
  labs(x = "richness",y="fruit proportion",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.1$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  richness, y = fruit_proportion),shape=20)+ 
  labs(x = "richness",y="fruit proportion",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.1$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  richness, y = fruit_proportion),shape=20)+ 
  labs(x = "richness",y="fruit proportion",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.1$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  richness, y = fruit_proportion),shape=20)+ 
  labs(x = "richness",y="fruit proportion",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.1$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  richness, y = fruit_proportion),shape=20)+ 
  labs(x = "richness",y="fruit proportion",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.1$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  richness, y = fruit_proportion),shape=20)+ 
  labs(x = "richness",y="fruit proportion",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.1$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  richness, y = fruit_proportion),shape=20)+ 
  labs(x = "richness",y="fruit proportion",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.1$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  richness, y = fruit_proportion),shape=20)+ 
  labs(x = "richness",y="fruit proportion",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.1$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  richness, y = fruit_proportion),shape=20)+ 
  labs(x = "richness",y="fruit proportion",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.1$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  richness, y = fruit_proportion),shape=20)+ 
  labs(x = "richness",y="fruit proportion",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.1$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  richness, y = fruit_proportion),shape=20)+ 
  labs(x = "richness",y="fruit proportion",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))




# visistation rate

#plot
plots_fruitproportion_2 <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=visitatio_rate, y=fruit_proportion) + 
              geom_point() + ggtitle(.y[[1]]))

plots_fruitproportion_2[[1]]+plots_fruitproportion_2[[2]]+plots_fruitproportion_2[[3]]+plots_fruitproportion_2[[4]]
plots_fruitproportion_2[[5]]+plots_fruitproportion_2[[6]]+plots_fruitproportion_2[[7]]+plots_fruitproportion_2[[8]] 
plots_fruitproportion_2[[9]]+plots_fruitproportion_2[[10]]+plots_fruitproportion_2[[11]]+plots_fruitproportion_2[[12]]

#model
mod_fruitproportion_2<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~visitatio_rate + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


mod_fruitproportion_2_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~visitatio_rate + (1|Site_ID),family=binomial,data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_fruitproportion_trial2<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~visitatio_rate + (1|Site_ID),family=binomial,data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_fruitproportion_trial2$plots[[1]])
plot(mod_fruitproportion_trial2$plots[[2]])
plot(mod_fruitproportion_trial2$plots[[3]])
plot(mod_fruitproportion_trial2$plots[[4]])
plot(mod_fruitproportion_trial2$plots[[5]])
plot(mod_fruitproportion_trial2$plots[[6]])
plot(mod_fruitproportion_trial2$plots[[7]])
plot(mod_fruitproportion_trial2$plots[[8]])
plot(mod_fruitproportion_trial2$plots[[9]])
plot(mod_fruitproportion_trial2$plots[[10]])
plot(mod_fruitproportion_trial2$plots[[11]])
plot(mod_fruitproportion_trial2$plots[[12]])

# model plot 
mod_fruitproportion.2 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~visitatio_rate + (1|Site_ID),family=binomial,data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "visitatio_rate"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_fruitproportion.2$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  visitatio_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate",y="fruit proportion",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.2$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  visitatio_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate",y="fruit proportion",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.2$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  visitatio_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate",y="fruit proportion",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.2$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  visitatio_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate",y="fruit proportion",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.2$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  visitatio_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate",y="fruit proportion",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.2$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  visitatio_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate",y="fruit proportion",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.2$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  visitatio_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate",y="fruit proportion",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.2$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  visitatio_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate",y="fruit proportion",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.2$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  visitatio_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate",y="fruit proportion",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.2$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  visitatio_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate",y="fruit proportion",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.2$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  visitatio_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate",y="fruit proportion",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.2$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  visitatio_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate",y="fruit proportion",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))



# frequency
ta_f3<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Frequency + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

# hymenoptera visitation
ta_f4<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Hym_visit + (1|Site_ID),family=binomial,data)))%>%
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

# hymenoptera visitation
ta_seed4_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Hym_visit+(1|Site_ID),data))) %>%
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

# hymenoptera visitation
ta_weight4_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Hym_visit+(1|Site_ID),data))) %>%
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


# hymenoptera visitation
ta_f4_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Hym_visit + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


# frequency
ta_f3_la<-lavan_tog %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Frequency + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()
