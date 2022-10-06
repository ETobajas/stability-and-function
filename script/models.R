

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
library (lme4)


Pollinator_Plant<- read.csv("Data/Pollinator_Plant_Apis_separada.csv") #400 observaciones
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


# correlation ----
a<-Pollinator_Plant %>% select(Plant_gen_sp,fruit_proportion, Seeds,Seed_weight,richness,visitatio_rate,Frequency)
a <- mutate_all(a, ~replace(., is.na(.), 0))

a_cor <- a %>%
  group_by(Plant_gen_sp) %>%
  do(cormat = cor(select(., -matches("Plant_gen_sp")))) 


#+++++++++++++++++++++
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


#+++++++++++++++++++++++++++++++++
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




# replace NAs by 0
#hay especies de plantas que presentan datos de fruit y seed pero no de polinizador
#(durante el tiempo observado no se ve ningun polinizador )
Pollinator_Plant <- mutate_all(Pollinator_Plant, ~replace(., is.na(.), 0))

#correlacion general, no por especie de plantas 
cor_general<-Pollinator_Plant %>% 
  select(fruit_proportion, Seeds,Seed_weight,richness,visitatio_rate,Frequency)

corr <- round(cor(cor_general), 4)
p.mat <- cor_pmat(cor_general)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)



# Models _ Seed numbers---- 

require("lattice")
xyplot(Seeds ~ visitatio_rate|Plant_gen_sp, data = Pollinator_Plant, type = c("p", "r"))
xyplot(Seeds ~ richness|Plant_gen_sp, data = Pollinator_Plant, type = c("p", "r"))

# richness
#plot
plots_seed <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=richness, y=Seeds) + 
              geom_point() + ggtitle(.y[[1]]))

plots_seed[[1]]+plots_seed[[2]]+plots_seed[[3]]+plots_seed[[4]]
plots_seed[[5]]+plots_seed[[6]]+plots_seed[[7]]+plots_seed[[8]] 
plots_seed[[9]]+plots_seed[[10]]+plots_seed[[11]]+plots_seed[[12]]

#model
mod_seed<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~richness+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_seed_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~richness+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_seed_trial<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~richness+(1|Site_ID),data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_seed_trial$plots[[1]]) #Asphodelus fistulosus
plot(mod_seed_trial$plots[[2]]) #Cistus crispus
plot(mod_seed_trial$plots[[3]]) #Cistus ladanifer
plot(mod_seed_trial$plots[[4]]) #Cistus libanotis
plot(mod_seed_trial$plots[[5]]) #Cistus monspeliensis
plot(mod_seed_trial$plots[[6]]) #Cistus salviifolius
plot(mod_seed_trial$plots[[7]]) #Halimium calycinum
plot(mod_seed_trial$plots[[8]]) #Halimium halimifolium
plot(mod_seed_trial$plots[[9]]) #Lavandula pedunculata
plot(mod_seed_trial$plots[[10]]) #Lavandula stoechas
plot(mod_seed_trial$plots[[11]]) #Salvia rosmarinus
plot(mod_seed_trial$plots[[12]]) #Teucrium fruticans


# model plot 
mod_seed.1 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~richness+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "richness"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed.1$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  richness, y = Seeds),shape=20)+ 
  labs(x = "richness",y="seed_number",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  richness, y = Seeds),shape=20)+ 
  labs(x = "richness",y="seed_number",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  richness, y = Seeds),shape=20)+ 
  labs(x = "richness",y="seed_number",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  richness, y = Seeds),shape=20)+ 
  labs(x = "richness",y="seed_number",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  richness, y = Seeds),shape=20)+ 
  labs(x = "richness",y="seed_number",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  richness, y = Seeds),shape=20)+ 
  labs(x = "richness",y="seed_number",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  richness, y = Seeds),shape=20)+ 
  labs(x = "richness",y="seed_number",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  richness, y = Seeds),shape=20)+ 
  labs(x = "richness",y="seed_number",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  richness, y = Seeds),shape=20)+ 
  labs(x = "richness",y="seed_number",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  richness, y = Seeds),shape=20)+ 
  labs(x = "richness",y="seed_number",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  richness, y = Seeds),shape=20)+ 
  labs(x = "richness",y="seed_number",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  richness, y = Seeds),shape=20)+ 
  labs(x = "richness",y="seed_number",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))



# visistation rate

#plot
plots_seed_2 <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=visitatio_rate, y=Seeds) + 
              geom_point() + ggtitle(.y[[1]]))

plots_seed_2[[1]]+plots_seed_2[[2]]+plots_seed_2[[3]]+plots_seed_2[[4]]
plots_seed_2[[5]]+plots_seed_2[[6]]+plots_seed_2[[7]]+plots_seed_2[[8]] 
plots_seed_2[[9]]+plots_seed_2[[10]]+plots_seed_2[[11]]+plots_seed_2[[12]]

#model
mod_seed2<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~visitatio_rate+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_seed2_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~visitatio_rate+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_seed2_trial<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~visitatio_rate+(1|Site_ID),data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_seed2_trial$plots[[1]]) #Asphodelus fistulosus
plot(mod_seed2_trial$plots[[2]]) #Cistus crispus
plot(mod_seed2_trial$plots[[3]]) #Cistus ladanifer
plot(mod_seed2_trial$plots[[4]]) #Cistus libanotis
plot(mod_seed2_trial$plots[[5]]) #Cistus monspeliensis
plot(mod_seed2_trial$plots[[6]]) #Cistus salviifolius
plot(mod_seed2_trial$plots[[7]]) #Halimium calycinum
plot(mod_seed2_trial$plots[[8]]) #Halimium halimifolium
plot(mod_seed2_trial$plots[[9]]) #Lavandula pedunculata
plot(mod_seed2_trial$plots[[10]]) #Lavandula stoechas
plot(mod_seed2_trial$plots[[11]]) #Salvia rosmarinus
plot(mod_seed2_trial$plots[[12]]) #Teucrium fruticans

# model plot 
mod_seed.2 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~visitatio_rate+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "visitatio_rate"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed.2$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  visitatio_rate, y = Seeds),shape=20)+ 
  labs(x = "visitation_rate",y="seed_number",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.2$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  visitatio_rate, y = Seeds),shape=20)+ 
  labs(x = "visitation_rate",y="seed_number",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.2$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  visitatio_rate, y = Seeds),shape=20)+ 
  labs(x = "visitation_rate",y="seed_number",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.2$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  visitatio_rate, y = Seeds),shape=20)+ 
  labs(x = "visitation_rate",y="seed_number",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.2$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  visitatio_rate, y = Seeds),shape=20)+ 
  labs(x = "visitation_rate",y="seed_number",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.2$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  visitatio_rate, y = Seeds),shape=20)+ 
  labs(x = "visitation_rate",y="seed_number",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.2$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  visitatio_rate, y = Seeds),shape=20)+ 
  labs(x = "visitation_rate",y="seed_number",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.2$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  visitatio_rate, y = Seeds),shape=20)+ 
  labs(x = "visitation_rate",y="seed_number",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  visitatio_rate, y = Seeds),shape=20)+ 
  labs(x = "visitation_rate",y="seed_number",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.1$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  visitatio_rate, y = Seeds),shape=20)+ 
  labs(x = "visitation_rate",y="seed_number",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.2$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  visitatio_rate, y = Seeds),shape=20)+ 
  labs(x = "visitation_rate",y="seed_number",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.2$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  visitatio_rate, y = Seeds),shape=20)+ 
  labs(x = "visitation_rate",y="seed_number",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))

# visitation rate without Apis mellifera

#plot
plots_Visitation_rate_no_apis <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=No_Apis_visit_rate, y=Seeds) + 
              geom_point() + ggtitle(.y[[1]]))

plots_Visitation_rate_no_apis[[1]]+plots_Visitation_rate_no_apis[[2]]+plots_Visitation_rate_no_apis[[3]]+plots_Visitation_rate_no_apis[[4]]
plots_Visitation_rate_no_apis[[5]]+plots_Visitation_rate_no_apis[[6]]+plots_Visitation_rate_no_apis[[7]]+plots_Visitation_rate_no_apis[[8]] 
plots_Visitation_rate_no_apis[[9]]+plots_Visitation_rate_no_apis[[10]]+plots_Visitation_rate_no_apis[[11]]+plots_Visitation_rate_no_apis[[12]]

#model
mod_seed_Vr_no_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~No_Apis_visit_rate+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_seed_Vr_no_apis_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~No_Apis_visit_rate+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_seed_Vr_no_apis_trial<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~No_Apis_visit_rate+(1|Site_ID),data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_seed_Vr_no_apis_trial$plots[[1]]) #Asphodelus fistulosus
plot(mod_seed_Vr_no_apis_trial$plots[[2]]) #Cistus crispus
plot(mod_seed_Vr_no_apis_trial$plots[[3]]) #Cistus ladanifer
plot(mod_seed_Vr_no_apis_trial$plots[[4]]) #Cistus libanotis
plot(mod_seed_Vr_no_apis_trial$plots[[5]]) #Cistus monspeliensis
plot(mod_seed_Vr_no_apis_trial$plots[[6]]) #Cistus salviifolius
plot(mod_seed_Vr_no_apis_trial$plots[[7]]) #Halimium calycinum
plot(mod_seed_Vr_no_apis_trial$plots[[8]]) #Halimium halimifolium
plot(mod_seed_Vr_no_apis_trial$plots[[9]]) #Lavandula pedunculata
plot(mod_seed_Vr_no_apis_trial$plots[[10]]) #Lavandula stoechas
plot(mod_seed_Vr_no_apis_trial$plots[[11]]) #Salvia rosmarinus
plot(mod_seed_Vr_no_apis_trial$plots[[12]]) #Teucrium fruticans

# model plot 
mod_seed_Vr_no_apis_trial.2 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~No_Apis_visit_rate+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "No_Apis_visit_rate"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed_Vr_no_apis_trial.2$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  No_Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="seed_number",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_no_apis_trial.2$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  No_Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="seed_number",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_no_apis_trial.2$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  No_Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="seed_number",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_no_apis_trial.2$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  No_Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="seed_number",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_no_apis_trial.2$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  No_Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="seed_number",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_no_apis_trial.2$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  No_Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="seed_number",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_no_apis_trial.2$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  No_Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="seed_number",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_no_apis_trial.2$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  No_Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="seed_number",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_no_apis_trial.2$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  No_Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="seed_number",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_no_apis_trial.2$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  No_Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="seed_number",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_no_apis_trial.2$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  No_Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="seed_number",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_no_apis_trial.2$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  No_Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="seed_number",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))


# visitation rate of Apis mellifera

#plot
plots_Visitation_rate_apis <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Apis_visit_rate, y=Seeds) + 
              geom_point() + ggtitle(.y[[1]]))

plots_Visitation_rate_apis[[1]]+plots_Visitation_rate_apis[[2]]+plots_Visitation_rate_apis[[3]]+plots_Visitation_rate_apis[[4]]
plots_Visitation_rate_apis[[5]]+plots_Visitation_rate_apis[[6]]+plots_Visitation_rate_apis[[7]]+plots_Visitation_rate_apis[[8]] 
plots_Visitation_rate_apis[[9]]+plots_Visitation_rate_apis[[10]]+plots_Visitation_rate_apis[[11]]+plots_Visitation_rate_apis[[12]]

#model
mod_seed_Vr_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Apis_visit_rate+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_seed_Vr_apis_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Apis_visit_rate+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()


# model plot 
mod_seed_Vr_apis_trial.2 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Apis_visit_rate+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "Apis_visit_rate"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed_Vr_apis_trial.2$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "Apis_visit_rate",y="seed_number",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_apis_trial.2$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "Apis_visit_rate",y="seed_number",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_apis_trial.2$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "Apis_visit_rate",y="seed_number",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_apis_trial.2$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "Apis_visit_rate",y="seed_number",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_apis_trial.2$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "Apis_visit_rate",y="seed_number",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_apis_trial.2$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "Apis_visit_rate",y="seed_number",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_apis_trial.2$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "Apis_visit_rate",y="seed_number",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_apis_trial.2$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "Apis_visit_rate",y="seed_number",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_apis_trial.2$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "Apis_visit_rate",y="seed_number",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_apis_trial.2$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "Apis_visit_rate",y="seed_number",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_apis_trial.2$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "Apis_visit_rate",y="seed_number",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_Vr_apis_trial.2$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  Apis_visit_rate, y = Seeds),shape=20)+ 
  labs(x = "Apis_visit_rate",y="seed_number",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))


# hymenoptera visitation

#plot
plots_seed_3 <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Hym_visit, y=Seeds) + 
              geom_point() + ggtitle(.y[[1]]))

plots_seed_3[[1]]+plots_seed_3[[2]]+plots_seed_3[[3]]+plots_seed_3[[4]]
plots_seed_3[[5]]+plots_seed_3[[6]]+plots_seed_3[[7]]+plots_seed_3[[8]] 
plots_seed_3[[9]]+plots_seed_3[[10]]+plots_seed_3[[11]]+plots_seed_3[[12]]

#model
mod_seed3<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Hym_visit+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_seed3_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Hym_visit+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_seed3_trial<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Hym_visit+(1|Site_ID),data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_seed3_trial$plots[[1]]) #Asphodelus fistulosus
plot(mod_seed3_trial$plots[[2]]) #Cistus crispus
plot(mod_seed3_trial$plots[[3]]) #Cistus ladanifer
plot(mod_seed3_trial$plots[[4]]) #Cistus libanotis
plot(mod_seed3_trial$plots[[5]]) #Cistus monspeliensis
plot(mod_seed3_trial$plots[[6]]) #Cistus salviifolius
plot(mod_seed3_trial$plots[[7]]) #Halimium calycinum
plot(mod_seed3_trial$plots[[8]]) #Halimium halimifolium
plot(mod_seed3_trial$plots[[9]]) #Lavandula pedunculata
plot(mod_seed3_trial$plots[[10]]) #Lavandula stoechas
plot(mod_seed3_trial$plots[[11]]) #Salvia rosmarinus
plot(mod_seed3_trial$plots[[12]]) #Teucrium fruticans

# model plot 
mod_seed.3 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Hym_visit+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "Hym_visit"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed.3$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  Hym_visit, y = Seeds),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="seed_number",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.3$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  Hym_visit, y = Seeds),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="seed_number",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.3$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  Hym_visit, y = Seeds),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="seed_number",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.3$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  Hym_visit, y = Seeds),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="seed_number",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.3$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  Hym_visit, y = Seeds),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="seed_number",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.3$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  Hym_visit, y = Seeds),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="seed_number",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.3$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  Hym_visit, y = Seeds),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="seed_number",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.3$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  Hym_visit, y = Seeds),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="seed_number",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.3$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  Hym_visit, y = Seeds),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="seed_number",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.3$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  Hym_visit, y = Seeds),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="seed_number",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.3$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  Hym_visit, y = Seeds),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="seed_number",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.3$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  Hym_visit, y = Seeds),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="seed_number",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))


# hymenoptera visitation without A.mellifera

#plot
plots_seed_Hym_visit_sin_Apis <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Hym_visit_sin_apis, y=Seeds) + 
              geom_point() + ggtitle(.y[[1]]))

plots_seed_Hym_visit_sin_Apis[[1]]+plots_seed_Hym_visit_sin_Apis[[2]]+plots_seed_Hym_visit_sin_Apis[[3]]+plots_seed_Hym_visit_sin_Apis[[4]]
plots_seed_Hym_visit_sin_Apis[[5]]+plots_seed_Hym_visit_sin_Apis[[6]]+plots_seed_Hym_visit_sin_Apis[[7]]+plots_seed_Hym_visit_sin_Apis[[8]] 
plots_seed_Hym_visit_sin_Apis[[9]]+plots_seed_Hym_visit_sin_Apis[[10]]+plots_seed_Hym_visit_sin_Apis[[11]]+plots_seed_Hym_visit_sin_Apis[[12]]

#model
mod_seed_hym_no_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Hym_visit_sin_apis+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_seed_hym_no_apis_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Hym_visit_sin_apis+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()


# model plot 
mod_seedhym_no_apis.2 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Hym_visit_sin_apis+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "Hym_visit_sin_apis"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seedhym_no_apis.2$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  Hym_visit_sin_apis, y = Seeds),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="seed_number",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seedhym_no_apis.2$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  Hym_visit_sin_apis, y = Seeds),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="seed_number",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seedhym_no_apis.2$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  Hym_visit_sin_apis, y = Seeds),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="seed_number",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seedhym_no_apis.2$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  Hym_visit_sin_apis, y = Seeds),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="seed_number",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seedhym_no_apis.2$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  Hym_visit_sin_apis, y = Seeds),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="seed_number",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seedhym_no_apis.2$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  Hym_visit_sin_apis, y = Seeds),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="seed_number",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seedhym_no_apis.2$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  Hym_visit_sin_apis, y = Seeds),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="seed_number",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seedhym_no_apis.2$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  Hym_visit_sin_apis, y = Seeds),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="seed_number",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seedhym_no_apis.2$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  Hym_visit_sin_apis, y = Seeds),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="seed_number",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seedhym_no_apis.2$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  Hym_visit_sin_apis, y = Seeds),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="seed_number",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seedhym_no_apis.2$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  Hym_visit_sin_apis, y = Seeds),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="seed_number",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seedhym_no_apis.2$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  Hym_visit_sin_apis, y = Seeds),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="seed_number",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))




# Frequency

#plot
plots_seed_4 <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Frequency, y=Seeds) + 
              geom_point() + ggtitle(.y[[1]]))

plots_seed_4[[1]]+plots_seed_4[[2]]+plots_seed_4[[3]]+plots_seed_4[[4]]
plots_seed_4[[5]]+plots_seed_4[[6]]+plots_seed_4[[7]]+plots_seed_4[[8]] 
plots_seed_4[[9]]+plots_seed_4[[10]]+plots_seed_4[[11]]+plots_seed_4[[12]]

#model
mod_seed4<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Frequency+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_seed4_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Frequency+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_seed4_trial<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Frequency+(1|Site_ID),data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_seed4_trial$plots[[1]]) #Asphodelus fistulosus
plot(mod_seed4_trial$plots[[2]]) #Cistus crispus
plot(mod_seed4_trial$plots[[3]]) #Cistus ladanifer
plot(mod_seed4_trial$plots[[4]]) #Cistus libanotis
plot(mod_seed4_trial$plots[[5]]) #Cistus monspeliensis
plot(mod_seed4_trial$plots[[6]]) #Cistus salviifolius
plot(mod_seed4_trial$plots[[7]]) #Halimium calycinum
plot(mod_seed4_trial$plots[[8]]) #Halimium halimifolium
plot(mod_seed4_trial$plots[[9]]) #Lavandula pedunculata
plot(mod_seed4_trial$plots[[10]]) #Lavandula stoechas
plot(mod_seed4_trial$plots[[11]]) #Salvia rosmarinus
plot(mod_seed4_trial$plots[[12]]) #Teucrium fruticans

# model plot 
mod_seed.4 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds~Frequency+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "Frequency"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed.4$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  Frequency, y = Seeds),shape=20)+ 
  labs(x = "Frequency",y="seed_number",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.4$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  Frequency, y = Seeds),shape=20)+ 
  labs(x = "Frequency",y="seed_number",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.4$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  Frequency, y = Seeds),shape=20)+ 
  labs(x = "Frequency",y="seed_number",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.4$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  Frequency, y = Seeds),shape=20)+ 
  labs(x = "Frequency",y="seed_number",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.4$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  Frequency, y = Seeds),shape=20)+ 
  labs(x = "Frequency",y="seed_number",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.4$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  Frequency, y = Seeds),shape=20)+ 
  labs(x = "Frequency",y="seed_number",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.4$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  Frequency, y = Seeds),shape=20)+ 
  labs(x = "Frequency",y="seed_number",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.4$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  Frequency, y = Seeds),shape=20)+ 
  labs(x = "Frequency",y="seed_number",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.4$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  Frequency, y = Seeds),shape=20)+ 
  labs(x = "Frequency",y="seed_number",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.4$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  Frequency, y = Seeds),shape=20)+ 
  labs(x = "Frequency",y="seed_number",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.4$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  Frequency, y = Seeds),shape=20)+ 
  labs(x = "Frequency",y="seed_number",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed.4$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  Frequency, y = Seeds),shape=20)+ 
  labs(x = "Frequency",y="seed_number",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))



# Models _ Seed weight---- 

require("lattice")
xyplot(Seed_weight ~ visitatio_rate|Plant_gen_sp, data = Pollinator_Plant, type = c("p", "r"))
xyplot(Seed_weight ~ richness|Plant_gen_sp, data = Pollinator_Plant, type = c("p", "r"))

# richnes

#plot
plots_seed_weight <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=richness, y=Seed_weight) + 
              geom_point() + ggtitle(.y[[1]]))

plots_seed_weight[[1]]+plots_seed_weight[[2]]+plots_seed_weight[[3]]+plots_seed_weight[[4]]
plots_seed_weight[[5]]+plots_seed_weight[[6]]+plots_seed_weight[[7]]+plots_seed_weight[[8]] 
plots_seed_weight[[9]]+plots_seed_weight[[10]]+plots_seed_weight[[11]]+plots_seed_weight[[12]]

#model
mod_seed_weight<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~richness+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_seed_weight_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~richness+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_seed_weight_trial<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~richness+(1|Site_ID),data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_seed_weight_trial$plots[[1]]) #Asphodelus fistulosus
plot(mod_seed_weight_trial$plots[[2]]) #Cistus crispus
plot(mod_seed_weight_trial$plots[[3]]) #Cistus ladanifer
plot(mod_seed_weight_trial$plots[[4]]) #Cistus libanotis
plot(mod_seed_weight_trial$plots[[5]]) #Cistus monspeliensis
plot(mod_seed_weight_trial$plots[[6]]) #Cistus salviifolius
plot(mod_seed_weight_trial$plots[[7]]) #Halimium calycinum
plot(mod_seed_weight_trial$plots[[8]]) #Halimium halimifolium
plot(mod_seed_weight_trial$plots[[9]]) #Lavandula pedunculata
plot(mod_seed_weight_trial$plots[[10]]) #Lavandula stoechas
plot(mod_seed_weight_trial$plots[[11]]) #Salvia rosmarinus
plot(mod_seed_weight_trial$plots[[12]]) #Teucrium fruticans


# model plot 
mod_seed_weight.1 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~richness+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "richness"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed_weight.1$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  richness, y = Seed_weight),shape=20)+ 
  labs(x = "richness",y="Seed_weight",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.1$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  richness, y = Seed_weight),shape=20)+ 
  labs(x = "richness",y="Seed_weight",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.1$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  richness, y = Seed_weight),shape=20)+ 
  labs(x = "richness",y="Seed_weight",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.1$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  richness, y = Seed_weight),shape=20)+ 
  labs(x = "richness",y="Seed_weight",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.1$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  richness, y = Seed_weight),shape=20)+ 
  labs(x = "richness",y="Seed_weight",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.1$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  richness, y = Seed_weight),shape=20)+ 
  labs(x = "richness",y="Seed_weight",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.1$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  richness, y = Seed_weight),shape=20)+ 
  labs(x = "richness",y="Seed_weight",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.1$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  richness, y = Seed_weight),shape=20)+ 
  labs(x = "richness",y="Seed_weight",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.1$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  richness, y = Seed_weight),shape=20)+ 
  labs(x = "richness",y="Seed_weight",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.1$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  richness, y = Seed_weight),shape=20)+ 
  labs(x = "richness",y="Seed_weight",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.1$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  richness, y = Seed_weight),shape=20)+ 
  labs(x = "richness",y="Seed_weight",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.1$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  richness, y = Seed_weight),shape=20)+ 
  labs(x = "richness",y="Seed_weight",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))



# visistation rate

#plot
plots_seed_weight2 <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=visitatio_rate, y=Seed_weight) + 
              geom_point() + ggtitle(.y[[1]]))

plots_seed_weight2[[1]]+plots_seed_weight2[[2]]+plots_seed_weight2[[3]]+plots_seed_weight2[[4]]
plots_seed_weight2[[5]]+plots_seed_weight2[[6]]+plots_seed_weight2[[7]]+plots_seed_weight2[[8]] 
plots_seed_weight2[[9]]+plots_seed_weight2[[10]]+plots_seed_weight2[[11]]+plots_seed_weight2[[12]]

#model
mod_seed_weight2<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~visitatio_rate+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_seed_weight2_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~visitatio_rate+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_seed_weight2_trial<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~visitatio_rate+(1|Site_ID),data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_seed_weight2_trial$plots[[1]]) #Asphodelus fistulosus
plot(mod_seed_weight2_trial$plots[[2]]) #Cistus crispus
plot(mod_seed_weight2_trial$plots[[3]]) #Cistus ladanifer
plot(mod_seed_weight2_trial$plots[[4]]) #Cistus libanotis
plot(mod_seed_weight2_trial$plots[[5]]) #Cistus monspeliensis
plot(mod_seed_weight2_trial$plots[[6]]) #Cistus salviifolius
plot(mod_seed_weight2_trial$plots[[7]]) #Halimium calycinum
plot(mod_seed_weight2_trial$plots[[8]]) #Halimium halimifolium
plot(mod_seed_weight2_trial$plots[[9]]) #Lavandula pedunculata
plot(mod_seed_weight2_trial$plots[[10]]) #Lavandula stoechas
plot(mod_seed_weight2_trial$plots[[11]]) #Salvia rosmarinus
plot(mod_seed_weight2_trial$plots[[12]]) #Teucrium fruticans

# model plot 
mod_seed_weight.2 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~visitatio_rate+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "visitatio_rate"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed_weight.2$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  visitatio_rate, y = Seed_weight),shape=20)+ 
  labs(x = "visitation_rate",y="Seed_weight",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.2$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  visitatio_rate, y = Seed_weight),shape=20)+ 
  labs(x = "visitation_rate",y="Seed_weight",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.2$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  visitatio_rate, y = Seed_weight),shape=20)+ 
  labs(x = "visitation_rate",y="Seed_weight",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.2$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  visitatio_rate, y = Seed_weight),shape=20)+ 
  labs(x = "visitation_rate",y="Seed_weight",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.2$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  visitatio_rate, y = Seed_weight),shape=20)+ 
  labs(x = "visitation_rate",y="Seed_weight",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.2$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  visitatio_rate, y = Seed_weight),shape=20)+ 
  labs(x = "visitation_rate",y="Seed_weight",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.2$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  visitatio_rate, y = Seed_weight),shape=20)+ 
  labs(x = "visitation_rate",y="Seed_weight",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.2$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  visitatio_rate, y = Seed_weight),shape=20)+ 
  labs(x = "visitation_rate",y="Seed_weight",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.2$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  visitatio_rate, y = Seed_weight),shape=20)+ 
  labs(x = "visitation_rate",y="Seed_weight",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.2$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  visitatio_rate, y = Seed_weight),shape=20)+ 
  labs(x = "visitation_rate",y="Seed_weight",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.2$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  visitatio_rate, y = Seed_weight),shape=20)+ 
  labs(x = "visitation_rate",y="Seed_weight",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.2$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  visitatio_rate, y = Seed_weight),shape=20)+ 
  labs(x = "visitation_rate",y="Seed_weight",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))

# visitation rate without Apis mellifera

#plot
plots_seed_weight_VR_no_apis <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=No_Apis_visit_rate, y=Seed_weight) + 
              geom_point() + ggtitle(.y[[1]]))

plots_seed_weight_VR_no_apis[[1]]+plots_seed_weight_VR_no_apis[[2]]+plots_seed_weight_VR_no_apis[[3]]+plots_seed_weight_VR_no_apis[[4]]
plots_seed_weight_VR_no_apis[[5]]+plots_seed_weight_VR_no_apis[[6]]+plots_seed_weight_VR_no_apis[[7]]+plots_seed_weight_VR_no_apis[[8]] 
plots_seed_weight_VR_no_apis[[9]]+plots_seed_weight_VR_no_apis[[10]]+plots_seed_weight_VR_no_apis[[11]]+plots_seed_weight_VR_no_apis[[12]]

#model
mod_seed_weight_VR_no_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~No_Apis_visit_rate+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


mod_seed_weight_VR_no_apis_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~No_Apis_visit_rate+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()

# model plot 
mod_seed_weight_VR_no_apis.2 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~No_Apis_visit_rate+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "No_Apis_visit_rate"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
 geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed_weight_VR_no_apis.2$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  No_Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="Seed_weight",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_VR_no_apis.2$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  No_Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="Seed_weight",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_VR_no_apis.2$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  No_Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="Seed_weight",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_VR_no_apis.2$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  No_Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="Seed_weight",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_VR_no_apis.2$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  No_Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="Seed_weight",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_VR_no_apis.2$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  No_Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="Seed_weight",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_VR_no_apis.2$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  No_Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="Seed_weight",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_VR_no_apis.2$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  No_Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="Seed_weight",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_VR_no_apis.2$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  No_Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="Seed_weight",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_VR_no_apis.2$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  No_Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="Seed_weight",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_VR_no_apis.2$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  No_Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="Seed_weight",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_VR_no_apis.2$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  No_Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "No_Apis_visit_rate",y="Seed_weight",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))


# Visitation rate of Apis mellifera

#plot
plots_seed_weight_apis <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Apis_visit_rate, y=Seed_weight) + 
 geom_point() + ggtitle(.y[[1]]))

plots_seed_weight_apis[[1]]+plots_seed_weight_apis[[2]]+plots_seed_weight_apis[[3]]+plots_seed_weight_apis[[4]]
plots_seed_weight_apis[[5]]+plots_seed_weight_apis[[6]]+plots_seed_weight_apis[[7]]+plots_seed_weight_apis[[8]] 
plots_seed_weight_apis[[9]]+plots_seed_weight_apis[[10]]+plots_seed_weight_apis[[11]]+plots_seed_weight_apis[[12]]

#model
mod_seed_weight_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Apis_visit_rate+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


mod_seed_weight_apis_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Apis_visit_rate+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()

# model plot 
mod_seed_weight_apis.2 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Apis_visit_rate+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "Apis_visit_rate"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed_weight_apis.2$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "Apis_visit_rate",y="Seed_weight",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_apis.2$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "Apis_visit_rate",y="Seed_weight",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_apis.2$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "Apis_visit_rate",y="Seed_weight",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_apis.2$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "Apis_visit_rate",y="Seed_weight",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_apis.2$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "Apis_visit_rate",y="Seed_weight",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_apis.2$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "Apis_visit_rate",y="Seed_weight",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_apis.2$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "Apis_visit_rate",y="Seed_weight",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_apis.2$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "Apis_visit_rate",y="Seed_weight",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_apis.2$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "Apis_visit_rate",y="Seed_weight",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_apis.2$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "Apis_visit_rate",y="Seed_weight",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_apis.2$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "Apis_visit_rate",y="Seed_weight",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_apis.2$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  Apis_visit_rate, y = Seed_weight),shape=20)+ 
  labs(x = "Apis_visit_rate",y="Seed_weight",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))



# Hymenoptera visitation

#plot
plots_seed_weight3 <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Hym_visit, y=Seed_weight) + 
              geom_point() + ggtitle(.y[[1]]))

plots_seed_weight3[[1]]+plots_seed_weight3[[2]]+plots_seed_weight3[[3]]+plots_seed_weight3[[4]]
plots_seed_weight3[[5]]+plots_seed_weight3[[6]]+plots_seed_weight3[[7]]+plots_seed_weight3[[8]] 
plots_seed_weight3[[9]]+plots_seed_weight3[[10]]+plots_seed_weight3[[11]]+plots_seed_weight3[[12]]

#model
mod_seed_weight3<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Hym_visit+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


mod_seed_weight3_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Hym_visit+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_seed_weight3_trial<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Hym_visit+(1|Site_ID),data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_seed_weight3_trial$plots[[1]]) #Asphodelus fistulosus
plot(mod_seed_weight3_trial$plots[[2]]) #Cistus crispus
plot(mod_seed_weight3_trial$plots[[3]]) #Cistus ladanifer
plot(mod_seed_weight3_trial$plots[[4]]) #Cistus libanotis
plot(mod_seed_weight3_trial$plots[[5]]) #Cistus monspeliensis
plot(mod_seed_weight3_trial$plots[[6]]) #Cistus salviifolius
plot(mod_seed_weight3_trial$plots[[7]]) #Halimium calycinum
plot(mod_seed_weight3_trial$plots[[8]]) #Halimium halimifolium
plot(mod_seed_weight3_trial$plots[[9]]) #Lavandula pedunculata
plot(mod_seed_weight3_trial$plots[[10]]) #Lavandula stoechas
plot(mod_seed_weight3_trial$plots[[11]]) #Salvia rosmarinus
plot(mod_seed_weight3_trial$plots[[12]]) #Teucrium fruticans

# model plot 
mod_seed_weight.3 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Hym_visit+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "Hym_visit"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed_weight.3$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  Hym_visit, y = Seed_weight),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="Seed_weight",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.3$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  Hym_visit, y = Seed_weight),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="Seed_weight",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.3$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  Hym_visit, y = Seed_weight),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="Seed_weight",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.3$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  Hym_visit, y = Seed_weight),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="Seed_weight",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.3$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  Hym_visit, y = Seed_weight),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="Seed_weight",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.3$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  Hym_visit, y = Seed_weight),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="Seed_weight",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.3$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  Hym_visit, y = Seed_weight),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="Seed_weight",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.3$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  Hym_visit, y = Seed_weight),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="Seed_weight",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.3$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  Hym_visit, y = Seed_weight),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="Seed_weight",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.3$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  Hym_visit, y = Seed_weight),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="Seed_weight",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.3$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  Hym_visit, y = Seed_weight),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="Seed_weight",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.3$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  Hym_visit, y = Seed_weight),shape=20)+ 
  labs(x = "Hymenoptera_visit",y="Seed_weight",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))



# hymenoptera visitation rate without A.mellifera
#plot
plots_seed_weight_hym_no_apis <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Hym_visit_sin_apis, y=Seed_weight) + 
              geom_point() + ggtitle(.y[[1]]))

plots_seed_weight_hym_no_apis[[1]]+plots_seed_weight_hym_no_apis[[2]]+plots_seed_weight_hym_no_apis[[3]]+plots_seed_weight_hym_no_apis[[4]]
plots_seed_weight_hym_no_apis[[5]]+plots_seed_weight_hym_no_apis[[6]]+plots_seed_weight_hym_no_apis[[7]]+plots_seed_weight_hym_no_apis[[8]] 
plots_seed_weight_hym_no_apis[[9]]+plots_seed_weight_hym_no_apis[[10]]+plots_seed_weight_hym_no_apis[[11]]+plots_seed_weight_hym_no_apis[[12]]

#model
mod_seed_weight_hym_no_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Hym_visit_sin_apis+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_seed_weight_hym_no_apis_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Hym_visit_sin_apis+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()


# model plot 
mod_seed_weight_hym_no_apis.2 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Hym_visit_sin_apis+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "Hym_visit_sin_apis"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
 geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed_weight_hym_no_apis.2$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  Hym_visit_sin_apis, y = Seed_weight),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="Seed_weight",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_hym_no_apis.2$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  Hym_visit_sin_apis, y = Seed_weight),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="Seed_weight",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_hym_no_apis.2$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  Hym_visit_sin_apis, y = Seed_weight),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="Seed_weight",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_hym_no_apis.2$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  Hym_visit_sin_apis, y = Seed_weight),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="Seed_weight",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_hym_no_apis.2$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  Hym_visit_sin_apis, y = Seed_weight),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="Seed_weight",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_hym_no_apis.2$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  Hym_visit_sin_apis, y = Seed_weight),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="Seed_weight",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_hym_no_apis.2$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  Hym_visit_sin_apis, y = Seed_weight),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="Seed_weight",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_hym_no_apis.2$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  Hym_visit_sin_apis, y = Seed_weight),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="Seed_weight",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_hym_no_apis.2$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  Hym_visit_sin_apis, y = Seed_weight),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="Seed_weight",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_hym_no_apis.2$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  Hym_visit_sin_apis, y = Seed_weight),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="Seed_weight",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_hym_no_apis.2$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  Hym_visit_sin_apis, y = Seed_weight),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="Seed_weight",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight_hym_no_apis.2$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  Hym_visit_sin_apis, y = Seed_weight),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="Seed_weight",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))




# frequency

#plot
plots_seed_weight4 <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Frequency, y=Seed_weight) + 
              geom_point() + ggtitle(.y[[1]]))

plots_seed_weight4[[1]]+plots_seed_weight4[[2]]+plots_seed_weight4[[3]]+plots_seed_weight4[[4]]
plots_seed_weight4[[5]]+plots_seed_weight4[[6]]+plots_seed_weight4[[7]]+plots_seed_weight4[[8]] 
plots_seed_weight4[[9]]+plots_seed_weight4[[10]]+plots_seed_weight4[[11]]+plots_seed_weight4[[12]]

#model
mod_seed_weight4<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Frequency+(1|Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_seed_weight4_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Frequency+(1|Site_ID),data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_seed_weight4_trial<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Frequency+(1|Site_ID),data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_seed_weight4_trial$plots[[1]]) #Asphodelus fistulosus
plot(mod_seed_weight4_trial$plots[[2]]) #Cistus crispus
plot(mod_seed_weight4_trial$plots[[3]]) #Cistus ladanifer
plot(mod_seed_weight4_trial$plots[[4]]) #Cistus libanotis
plot(mod_seed_weight4_trial$plots[[5]]) #Cistus monspeliensis
plot(mod_seed_weight4_trial$plots[[6]]) #Cistus salviifolius
plot(mod_seed_weight4_trial$plots[[7]]) #Halimium calycinum
plot(mod_seed_weight4_trial$plots[[8]]) #Halimium halimifolium
plot(mod_seed_weight4_trial$plots[[9]]) #Lavandula pedunculata
plot(mod_seed_weight4_trial$plots[[10]]) #Lavandula stoechas
plot(mod_seed_weight4_trial$plots[[11]]) #Salvia rosmarinus
plot(mod_seed_weight4_trial$plots[[12]]) #Teucrium fruticans


# model plot 
mod_seed_weight.4 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seed_weight~Frequency+(1|Site_ID),data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "Frequency"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_seed_weight.4$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  Frequency, y = Seed_weight),shape=20)+ 
  labs(x = "Frequency",y="Seed_weight",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.4$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  Frequency, y = Seed_weight),shape=20)+ 
  labs(x = "Frequency",y="Seed_weight",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.4$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  Frequency, y = Seed_weight),shape=20)+ 
  labs(x = "Frequency",y="Seed_weight",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.4$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  Frequency, y = Seed_weight),shape=20)+ 
  labs(x = "Frequency",y="Seed_weight",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.4$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  Frequency, y = Seed_weight),shape=20)+ 
  labs(x = "Frequency",y="Seed_weight",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.4$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  Frequency, y = Seed_weight),shape=20)+ 
  labs(x = "Frequency",y="Seed_weight",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.4$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  Frequency, y = Seed_weight),shape=20)+ 
  labs(x = "Frequency",y="Seed_weight",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.4$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  Frequency, y = Seed_weight),shape=20)+ 
  labs(x = "Frequency",y="Seed_weight",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.4$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  Frequency, y = Seed_weight),shape=20)+ 
  labs(x = "Frequency",y="Seed_weight",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.4$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  Frequency, y = Seed_weight),shape=20)+ 
  labs(x = "Frequency",y="Seed_weight",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.4$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  Frequency, y = Seed_weight),shape=20)+ 
  labs(x = "Frequency",y="Seed_weight",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_seed_weight.4$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  Frequency, y = Seed_weight),shape=20)+ 
  labs(x = "Frequency",y="Seed_weight",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))




# Models _ fruit proportion ----

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

plot(mod_fruitproportion_trial$plots[[1]]) #Asphodelus fistulosus
plot(mod_fruitproportion_trial$plots[[2]]) #Cistus crispus
plot(mod_fruitproportion_trial$plots[[3]]) #Cistus ladanifer
plot(mod_fruitproportion_trial$plots[[4]]) #Cistus libanotis
plot(mod_fruitproportion_trial$plots[[5]]) #Cistus monspeliensis
plot(mod_fruitproportion_trial$plots[[6]]) #Cistus salviifolius
plot(mod_fruitproportion_trial$plots[[7]]) #Halimium calycinum
plot(mod_fruitproportion_trial$plots[[8]]) #Halimium halimifolium
plot(mod_fruitproportion_trial$plots[[9]]) #Lavandula pedunculata
plot(mod_fruitproportion_trial$plots[[10]]) #Lavandula stoechas
plot(mod_fruitproportion_trial$plots[[11]]) #Salvia rosmarinus
plot(mod_fruitproportion_trial$plots[[12]]) #Teucrium fruticans


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



# visistation rate (all pollinators)

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

plot(mod_fruitproportion_trial2$plots[[1]]) #Asphodelus fistulosus
plot(mod_fruitproportion_trial2$plots[[2]]) #Cistus crispus
plot(mod_fruitproportion_trial2$plots[[3]]) #Cistus ladanifer
plot(mod_fruitproportion_trial2$plots[[4]]) #Cistus libanotis
plot(mod_fruitproportion_trial2$plots[[5]]) #Cistus monspeliensis
plot(mod_fruitproportion_trial2$plots[[6]]) #Cistus salviifolius
plot(mod_fruitproportion_trial2$plots[[7]]) #Halimium calycinum
plot(mod_fruitproportion_trial2$plots[[8]]) #Halimium halimifolium
plot(mod_fruitproportion_trial2$plots[[9]]) #Lavandula pedunculata
plot(mod_fruitproportion_trial2$plots[[10]]) #Lavandula stoechas
plot(mod_fruitproportion_trial2$plots[[11]]) #Salvia rosmarinus
plot(mod_fruitproportion_trial2$plots[[12]]) #Teucrium fruticans

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


# visitation rate of pollinators without A.mellifera
# se calcula la visitation rate de los polinizadores sin frecuencias de A.mellifera

#plot
plots_fruitproportion_polli_no_apis <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=No_Apis_visit_rate, y=fruit_proportion) + 
  geom_point() + ggtitle(.y[[1]]))

plots_fruitproportion_polli_no_apis[[1]]+plots_fruitproportion_polli_no_apis[[2]]+plots_fruitproportion_polli_no_apis[[3]]+plots_fruitproportion_polli_no_apis[[4]]
plots_fruitproportion_polli_no_apis[[5]]+plots_fruitproportion_polli_no_apis[[6]]+plots_fruitproportion_polli_no_apis[[7]]+plots_fruitproportion_polli_no_apis[[8]] 
plots_fruitproportion_polli_no_apis[[9]]+plots_fruitproportion_polli_no_apis[[10]]+plots_fruitproportion_polli_no_apis[[11]]+plots_fruitproportion_polli_no_apis[[12]]

#model
mod_fruitproportion_polli_no_api<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~No_Apis_visit_rate + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


mod_fruitproportion_polli_no_api_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~No_Apis_visit_rate + (1|Site_ID),family=binomial,data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_fruitproportion_polli_no_api_resid<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~No_Apis_visit_rate + (1|Site_ID),family=binomial,data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_fruitproportion_polli_no_api_resid$plots[[1]]) #Asphodelus fistulosus
plot(mod_fruitproportion_polli_no_api_resid$plots[[2]]) #Cistus crispus
plot(mod_fruitproportion_polli_no_api_resid$plots[[3]]) #Cistus ladanifer
plot(mod_fruitproportion_polli_no_api_resid$plots[[4]]) #Cistus libanotis
plot(mod_fruitproportion_polli_no_api_resid$plots[[5]]) #Cistus monspeliensis
plot(mod_fruitproportion_polli_no_api_resid$plots[[6]]) #Cistus salviifolius
plot(mod_fruitproportion_polli_no_api_resid$plots[[7]]) #Halimium calycinum
plot(mod_fruitproportion_polli_no_api_resid$plots[[8]]) #Halimium halimifolium
plot(mod_fruitproportion_polli_no_api_resid$plots[[9]]) #Lavandula pedunculata
plot(mod_fruitproportion_polli_no_api_resid$plots[[10]]) #Lavandula stoechas
plot(mod_fruitproportion_polli_no_api_resid$plots[[11]]) #Salvia rosmarinus
plot(mod_fruitproportion_polli_no_api_resid$plots[[12]]) #Teucrium fruticans

# model plot 
mod_fruitproportion_polli_no_api.2 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~No_Apis_visit_rate + (1|Site_ID),family=binomial,data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "No_Apis_visit_rate"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))


  
# salvia=Pollinator_Plant %>%
#   filter(Plant_gen_sp == "Salvia rosmarinus")
# 
# plot(salvia$No_Apis_visit_rate,salvia$fruit_proportion)
# 
# mod= glmer(cbind(fruit_formado, Fruit_No) ~ No_Apis_visit_rate + (1|Site_ID), family = binomial, data= salvia)
# summary(mod)
# 
# sjPlot::plot_model(mod, type = "eff", show.ci = TRUE)
# 
# library(effects)
# plot (allEffects(mod))
# library(ggeffects)
# ggpredict(mod, "No_Apis_visit_rate")
# me <- ggpredict(mod, "No_Apis_visit_rate")
# plot(me)


mod_fruitproportion_polli_no_api.2$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  No_Apis_visit_rate, y = fruit_formado/(fruit_formado + Fruit_No)),shape=20)+ 
  labs(x = "visitation_rate_no_Apis",y="fruit proportion",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_polli_no_api.2$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  No_Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate_no_Apis",y="fruit proportion",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_polli_no_api.2$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  No_Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate_no_Apis",y="fruit proportion",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_polli_no_api.2$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  No_Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate_no_Apis",y="fruit proportion",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_polli_no_api.2$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  No_Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate_no_Apis",y="fruit proportion",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_polli_no_api.2$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  No_Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate_no_Apis",y="fruit proportion",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_polli_no_api.2$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  No_Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate_no_Apis",y="fruit proportion",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_polli_no_api.2$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  No_Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate_no_Apis",y="fruit proportion",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_polli_no_api.2$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  No_Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate_no_Apis",y="fruit proportion",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_polli_no_api.2$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  No_Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate_no_Apis",y="fruit proportion",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_polli_no_api.2$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  No_Apis_visit_rate, y = fruit_formado/(fruit_formado + Fruit_No)),shape=20)+ 
  labs(x = "visitation_rate_no_Apis",y="fruit proportion",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_polli_no_api.2$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  No_Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "visitation_rate_no_Apis",y="fruit proportion",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))


# Visitation rate of Apis mellifera
#plot
plots_fruitproportion_apis <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Apis_visit_rate, y=fruit_proportion) + 
              geom_point() + ggtitle(.y[[1]]))

plots_fruitproportion_apis[[1]]+plots_fruitproportion_apis[[2]]+plots_fruitproportion_apis[[3]]+plots_fruitproportion_apis[[4]]
plots_fruitproportion_apis[[5]]+plots_fruitproportion_apis[[6]]+plots_fruitproportion_apis[[7]]+plots_fruitproportion_apis[[8]] 
plots_fruitproportion_apis[[9]]+plots_fruitproportion_apis[[10]]+plots_fruitproportion_apis[[11]]+plots_fruitproportion_apis[[12]]

#model
mod_fruitproportion__apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Apis_visit_rate + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_fruitproportion__apis_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Apis_visit_rate + (1|Site_ID),family=binomial,data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_fruitproportion__apis_resid<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Apis_visit_rate + (1|Site_ID),family=binomial,data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_fruitproportion__apis_resid$plots[[1]]) #Asphodelus fistulosus
plot(mod_fruitproportion__apis_resid$plots[[2]]) #Cistus crispus
plot(mod_fruitproportion__apis_resid$plots[[3]]) #Cistus ladanifer
plot(mod_fruitproportion__apis_resid$plots[[4]]) #Cistus libanotis
plot(mod_fruitproportion__apis_resid$plots[[5]]) #Cistus monspeliensis
plot(mod_fruitproportion__apis_resid$plots[[6]]) #Cistus salviifolius
plot(mod_fruitproportion__apis_resid$plots[[7]]) #Halimium calycinum
plot(mod_fruitproportion__apis_resid$plots[[8]]) #Halimium halimifolium
plot(mod_fruitproportion__apis_resid$plots[[9]]) #Lavandula pedunculata
plot(mod_fruitproportion__apis_resid$plots[[10]]) #Lavandula stoechas
plot(mod_fruitproportion__apis_resid$plots[[11]]) #Salvia rosmarinus
plot(mod_fruitproportion__apis_resid$plots[[12]]) #Teucrium fruticans

# model plot 
mod_fruitproportion__apis.2 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Apis_visit_rate + (1|Site_ID),family=binomial,data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "Apis_visit_rate"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_fruitproportion__apis.2$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "Apis_visit_rate",y="fruit proportion",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion__apis.2$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "Apis_visit_rate",y="fruit proportion",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion__apis.2$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "Apis_visit_rate",y="fruit proportion",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion__apis.2$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "Apis_visit_rate",y="fruit proportion",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion__apis.2$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "Apis_visit_rate",y="fruit proportion",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion__apis.2$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "Apis_visit_rate",y="fruit proportion",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion__apis.2$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "Apis_visit_rate",y="fruit proportion",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion__apis.2$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "Apis_visit_rate",y="fruit proportion",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion__apis.2$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "Apis_visit_rate",y="fruit proportion",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion__apis.2$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "Apis_visit_rate",y="fruit proportion",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion__apis.2$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "Apis_visit_rate",y="fruit proportion",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion__apis.2$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  Apis_visit_rate, y = fruit_proportion),shape=20)+ 
  labs(x = "Apis_visit_rate",y="fruit proportion",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))



# hymenoptera visitation

#plot
plots_fruitproportion_3 <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Hym_visit, y=fruit_proportion) + 
              geom_point() + ggtitle(.y[[1]]))

plots_fruitproportion_3[[1]]+plots_fruitproportion_3[[2]]+plots_fruitproportion_3[[3]]+plots_fruitproportion_3[[4]]
plots_fruitproportion_3[[5]]+plots_fruitproportion_3[[6]]+plots_fruitproportion_3[[7]]+plots_fruitproportion_3[[8]] 
plots_fruitproportion_3[[9]]+plots_fruitproportion_3[[10]]+plots_fruitproportion_3[[11]]+plots_fruitproportion_3[[12]]

#model
mod_plots_fruitproportion_3<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Hym_visit + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_fruitproportion_3_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Hym_visit + (1|Site_ID),family=binomial,data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_fruitproportion_trial3<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Hym_visit + (1|Site_ID),family=binomial,data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_fruitproportion_trial3$plots[[1]]) #Asphodelus fistulosus
plot(mod_fruitproportion_trial3$plots[[2]]) #Cistus crispus
plot(mod_fruitproportion_trial3$plots[[3]]) #Cistus ladanifer
plot(mod_fruitproportion_trial3$plots[[4]]) #Cistus libanotis
plot(mod_fruitproportion_trial3$plots[[5]]) #Cistus monspeliensis
plot(mod_fruitproportion_trial3$plots[[6]]) #Cistus salviifolius
plot(mod_fruitproportion_trial3$plots[[7]]) #Halimium calycinum
plot(mod_fruitproportion_trial3$plots[[8]]) #Halimium halimifolium
plot(mod_fruitproportion_trial3$plots[[9]]) #Lavandula pedunculata
plot(mod_fruitproportion_trial3$plots[[10]]) #Lavandula stoechas
plot(mod_fruitproportion_trial3$plots[[11]]) #Salvia rosmarinus
plot(mod_fruitproportion_trial3$plots[[12]]) #Teucrium fruticans

# model plot 
mod_fruitproportion.3 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Hym_visit + (1|Site_ID),family=binomial,data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "Hym_visit"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_fruitproportion.3$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  Hym_visit, y = fruit_proportion),shape=20)+ 
  labs(x = "Hymenotera_visit",y="fruit proportion",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.3$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  Hym_visit, y = fruit_proportion),shape=20)+ 
  labs(x = "Hymenotera_visit",y="fruit proportion",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.3$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  Hym_visit, y = fruit_proportion),shape=20)+ 
  labs(x = "Hymenotera_visit",y="fruit proportion",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.3$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  Hym_visit, y = fruit_proportion),shape=20)+ 
  labs(x = "Hymenotera_visit",y="fruit proportion",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.3$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  Hym_visit, y = fruit_proportion),shape=20)+ 
  labs(x = "Hymenotera_visit",y="fruit proportion",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.3$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  Hym_visit, y = fruit_proportion),shape=20)+ 
  labs(x = "Hymenotera_visit",y="fruit proportion",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.3$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  Hym_visit, y = fruit_proportion),shape=20)+ 
  labs(x = "Hymenotera_visit",y="fruit proportion",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.3$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  Hym_visit, y = fruit_proportion),shape=20)+ 
  labs(x = "Hymenotera_visit",y="fruit proportion",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.3$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  Hym_visit, y = fruit_proportion),shape=20)+ 
  labs(x = "Hymenotera_visit",y="fruit proportion",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.3$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  Hym_visit, y = fruit_proportion),shape=20)+ 
  labs(x = "Hymenotera_visit",y="fruit proportion",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.3$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  Hym_visit, y = fruit_proportion),shape=20)+ 
  labs(x = "Hymenotera_visit",y="fruit proportion",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.3$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  Hym_visit, y = fruit_proportion),shape=20)+ 
  labs(x = "Hymenotera_visit",y="fruit proportion",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))


# hymenoptera sin A.mellifera

#plot
plots_fruitproportion_hyme_no_apis <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Hym_visit_sin_apis, y=fruit_proportion) + 
              geom_point() + ggtitle(.y[[1]]))

plots_fruitproportion_hyme_no_apis[[1]]+plots_fruitproportion_hyme_no_apis[[2]]+plots_fruitproportion_hyme_no_apis[[3]]+plots_fruitproportion_hyme_no_apis[[4]]
plots_fruitproportion_hyme_no_apis[[5]]+plots_fruitproportion_hyme_no_apis[[6]]+plots_fruitproportion_hyme_no_apis[[7]]+plots_fruitproportion_hyme_no_apis[[8]] 
plots_fruitproportion_hyme_no_apis[[9]]+plots_fruitproportion_hyme_no_apis[[10]]+plots_fruitproportion_hyme_no_apis[[11]]+plots_fruitproportion_hyme_no_apis[[12]]


#model
mod_fruitproportion_hyme_no_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Hym_visit_sin_apis + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_fruitproportion_hyme_no_apis_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Hym_visit_sin_apis + (1|Site_ID),family=binomial,data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_fruitproportion_hyme_no_apis_resid<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Hym_visit_sin_apis + (1|Site_ID),family=binomial,data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_fruitproportion_hyme_no_apis_resid$plots[[1]]) #Asphodelus fistulosus
plot(mod_fruitproportion_hyme_no_apis_resid$plots[[2]]) #Cistus crispus
plot(mod_fruitproportion_hyme_no_apis_resid$plots[[3]]) #Cistus ladanifer
plot(mod_fruitproportion_hyme_no_apis_resid$plots[[4]]) #Cistus libanotis
plot(mod_fruitproportion_hyme_no_apis_resid$plots[[5]]) #Cistus monspeliensis
plot(mod_fruitproportion_hyme_no_apis_resid$plots[[6]]) #Cistus salviifolius
plot(mod_fruitproportion_hyme_no_apis_resid$plots[[7]]) #Halimium calycinum
plot(mod_fruitproportion_hyme_no_apis_resid$plots[[8]]) #Halimium halimifolium
plot(mod_fruitproportion_hyme_no_apis_resid$plots[[9]]) #Lavandula pedunculata
plot(mod_fruitproportion_hyme_no_apis_resid$plots[[10]]) #Lavandula stoechas
plot(mod_fruitproportion_hyme_no_apis_resid$plots[[11]]) #Salvia rosmarinus
plot(mod_fruitproportion_hyme_no_apis_resid$plots[[12]]) #Teucrium fruticans


# model plot 
mod_fruitproportion_hyme_no_apis.2 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Hym_visit_sin_apis + (1|Site_ID),family=binomial,data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "Hym_visit_sin_apis"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_fruitproportion_hyme_no_apis.2$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  Hym_visit_sin_apis, y = fruit_proportion),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="fruit proportion",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_hyme_no_apis.2$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  Hym_visit_sin_apis, y = fruit_proportion),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="fruit proportion",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_hyme_no_apis.2$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  Hym_visit_sin_apis, y = fruit_proportion),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="fruit proportion",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_hyme_no_apis.2$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  Hym_visit_sin_apis, y = fruit_proportion),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="fruit proportion",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_hyme_no_apis.2$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  Hym_visit_sin_apis, y = fruit_proportion),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="fruit proportion",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_hyme_no_apis.2$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  Hym_visit_sin_apis, y = fruit_proportion),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="fruit proportion",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_hyme_no_apis.2$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  Hym_visit_sin_apis, y = fruit_proportion),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="fruit proportion",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_hyme_no_apis.2$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  Hym_visit_sin_apis, y = fruit_proportion),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="fruit proportion",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_hyme_no_apis.2$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  Hym_visit_sin_apis, y = fruit_proportion),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="fruit proportion",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_hyme_no_apis.2$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  Hym_visit_sin_apis, y = fruit_proportion),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="fruit proportion",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_hyme_no_apis.2$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  Hym_visit_sin_apis, y = fruit_proportion),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="fruit proportion",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion_hyme_no_apis.2$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  Hym_visit_sin_apis, y = fruit_proportion),shape=20)+ 
  labs(x = "Hym_visit_sin_apis",y="fruit proportion",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))



# frequency

#plot
plots_fruitproportion_4 <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=Frequency, y=fruit_proportion) + 
              geom_point() + ggtitle(.y[[1]]))

plots_fruitproportion_4[[1]]+plots_fruitproportion_4[[2]]+plots_fruitproportion_4[[3]]+plots_fruitproportion_4[[4]]
plots_fruitproportion_4[[5]]+plots_fruitproportion_4[[6]]+plots_fruitproportion_4[[7]]+plots_fruitproportion_4[[8]] 
plots_fruitproportion_4[[9]]+plots_fruitproportion_4[[10]]+plots_fruitproportion_4[[11]]+plots_fruitproportion_4[[12]]

#model
mod_fruitproportion_4<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Frequency + (1|Site_ID),family=binomial,data)))%>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

mod_fruitproportion_4_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Frequency + (1|Site_ID),family=binomial,data))) %>%
  summarize(glance(model))%>%
  ungroup()

# Residuals with dharma 
mod_fruitproportion_trial4<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Frequency + (1|Site_ID),family=binomial,data)))%>%
  mutate(plots = list(DHARMa::simulateResiduals(fittedModel = model))) %>%
  ungroup()

plot(mod_fruitproportion_trial4$plots[[1]]) #Asphodelus fistulosus
plot(mod_fruitproportion_trial4$plots[[2]]) #Cistus crispus

plot(mod_fruitproportion_trial3$plots[[3]]) #Cistus ladanifer
plot(mod_fruitproportion_trial4$plots[[4]]) #Cistus libanotis
plot(mod_fruitproportion_trial4$plots[[5]]) #Cistus monspeliensis
plot(mod_fruitproportion_trial4$plots[[6]]) #Cistus salviifolius
plot(mod_fruitproportion_trial4$plots[[7]]) #Halimium calycinum
plot(mod_fruitproportion_trial4$plots[[8]]) #Halimium halimifolium
plot(mod_fruitproportion_trial4$plots[[9]]) #Lavandula pedunculata
plot(mod_fruitproportion_trial4$plots[[10]]) #Lavandula stoechas
plot(mod_fruitproportion_trial4$plots[[11]]) #Salvia rosmarinus
plot(mod_fruitproportion_trial4$plots[[12]]) #Teucrium fruticans

# model plot 
mod_fruitproportion.4 <- Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(glmer(cbind(fruit_formado, Fruit_No)~Frequency + (1|Site_ID),family=binomial,data))) %>%
  mutate(model1 = list(ggeffects::ggpredict(model, terms = "Frequency"))) %>%
  mutate(plots = list(ggplot(model1, aes(x, predicted)) + geom_line() +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)))



mod_fruitproportion.4$plots[[1]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Asphodelus fistulosus"), aes(x =  Frequency, y = fruit_proportion),shape=20)+ 
  labs(x = "Frequency",y="fruit proportion",subtitle = "A.fistulosus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.4$plots[[2]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus crispus"), aes(x =  Frequency, y = fruit_proportion),shape=20)+ 
  labs(x = "Frequency",y="fruit proportion",subtitle = "C.crispus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.4$plots[[3]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), aes(x =  Frequency, y = fruit_proportion),shape=20)+ 
  labs(x = "Frequency",y="fruit proportion",subtitle = "C.ladanifer")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.4$plots[[4]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus libanotis"), aes(x =  Frequency, y = fruit_proportion),shape=20)+ 
  labs(x = "Frequency",y="fruit proportion",subtitle = "C.libanotis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.4$plots[[5]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus monspeliensis"), aes(x =  Frequency, y = fruit_proportion),shape=20)+ 
  labs(x = "Frequency",y="fruit proportion",subtitle = "C.monspeliensis")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.4$plots[[6]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), aes(x =  Frequency, y = fruit_proportion),shape=20)+ 
  labs(x = "Frequency",y="fruit proportion",subtitle = "C.salviifolius")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.4$plots[[7]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium calycinum"), aes(x =  Frequency, y = fruit_proportion),shape=20)+ 
  labs(x = "Frequency",y="fruit proportion",subtitle = "H.calycinum")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.4$plots[[8]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), aes(x =  Frequency, y = fruit_proportion),shape=20)+ 
  labs(x = "Frequency",y="fruit proportion",subtitle = "H.halimifolium")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.4$plots[[9]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), aes(x =  Frequency, y = fruit_proportion),shape=20)+ 
  labs(x = "Frequency",y="fruit proportion",subtitle = "L.pedunculata")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.4$plots[[10]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Lavandula stoechas"), aes(x =  Frequency, y = fruit_proportion),shape=20)+ 
  labs(x = "Frequency",y="fruit proportion",subtitle = "L.stoechas")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.4$plots[[11]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Salvia rosmarinus"), aes(x =  Frequency, y = fruit_proportion),shape=20)+ 
  labs(x = "Frequency",y="fruit proportion",subtitle = "S.rosmarinus")+  theme_classic()+theme(text = element_text(size=rel(3)))

mod_fruitproportion.4$plots[[12]] + geom_point(data = Pollinator_Plant %>% filter(Plant_gen_sp == "Teucrium fruticans"), aes(x =  Frequency, y = fruit_proportion),shape=20)+ 
  labs(x = "Frequency",y="fruit proportion",subtitle = "T.fruticans")+  theme_classic()+theme(text = element_text(size=rel(3)))

##############################

# Incluyeno año como factor fijo ----
# seed number----
str(Pollinator_Plant)

Pollinator_Plant$Year <- factor(Pollinator_Plant$Year)


plots_seed <- Pollinator_Plant %>%
  group_by(Plant_gen_sp) %>%
  group_map(~ ggplot(.) + aes(x=visitatio_rate, y=Seeds, shape =Site_ID, col=Year)+
  scale_shape_manual(values = c(15,16,17,18,6,7,8,9,10,11,12,13,14)) + geom_point(size = 3) + ggtitle(.y[[1]]))

plots_seed[[1]]+plots_seed[[2]]+plots_seed[[3]]+plots_seed[[4]]
plots_seed[[5]]+plots_seed[[6]]+plots_seed[[7]]+plots_seed[[8]] 
plots_seed[[9]]+plots_seed[[10]]+plots_seed[[11]]+plots_seed[[12]]


# linear model: visitation * year (factores fijos) 

mod_seed_year<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(Seeds~visitatio_rate * Year,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

# monsp=Pollinator_Plant %>%
#   filter(Plant_gen_sp== "Cistus monspeliensis")
# mod_monsp= lm (Seeds~visitatio_rate * Year, data= monsp)
# summary(mod_monsp)
# plot(allEffects(mod_monsp))

mod_seed_year_glance<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(Seeds~visitatio_rate * Year,data))) %>%
  summarize(glance(mod))%>%
  ungroup()

library(sjPlot)

mod_seed_year.1<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(Seeds~visitatio_rate * Year,data)))

plot_model(mod_seed_year.1$mod[[1]], type = "int", title = " Asphodelus fistulosus",show.data = TRUE)
get_model_data(mod_seed_year.1$mod[[1]], type = "int")

plot_model(mod_seed_year.1$mod[[2]], type = "int", title = " Cistus crispus",show.data = TRUE)
get_model_data(mod_seed_year.1$mod[[2]], type = "int")

plot_model(mod_seed_year.1$mod[[3]], type = "int", title = " Cistus ladanifer",show.data = TRUE)
plot_model(mod_seed_year.1$mod[[4]], type = "int",title = "Cistus libanotis",show.data = TRUE)
plot_model(mod_seed_year.1$mod[[5]], type = "int", title = "Cistus monspeliensis",show.data = TRUE)
plot_model(mod_seed_year.1$mod[[6]], type = "int",title = " Cistus salviifolius",show.data = TRUE)
plot_model(mod_seed_year.1$mod[[7]], type = "int",title = " Halimium calycinum",show.data = TRUE)
plot_model(mod_seed_year.1$mod[[8]], type = "int",title = " Halimium halimifolium",show.data = TRUE)
plot_model(mod_seed_year.1$mod[[9]], type = "int",title = " Lavandula pedunculata",show.data = TRUE)
plot_model(mod_seed_year.1$mod[[10]], type = "int",title = " Lavandula stoechas",show.data = TRUE)
plot_model(mod_seed_year.1$mod[[11]], type = "int",title = " Salvia rosmarinus",show.data = TRUE)
plot_model(mod_seed_year.1$mod[[12]], type = "int",title = " Teucrium fruticans",show.data = TRUE)

# estimates of model 
plot_model(mod_seed_year.1$mod[[1]], title = " Asphodelus fistulosus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year.1$mod[[2]], title = "  Cistus crispus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year.1$mod[[3]], title = " Cistus ladanifer",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year.1$mod[[4]], title = "Cistus libanotis",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year.1$mod[[5]], title = "Cistus monspeliensis",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year.1$mod[[6]], title = " Cistus salviifolius",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year.1$mod[[7]], title = " Halimium calycinum",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year.1$mod[[8]], title = " Halimium halimifolium",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year.1$mod[[9]], title = " Lavandula pedunculata",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year.1$mod[[10]], title = " Lavandula stoechas",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year.1$mod[[11]], title = " Salvia rosmarinus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year.1$mod[[12]], title = " Teucrium fruticans",show.values = TRUE, value.offset = .3)


plot_model(mod_seed_year.1$mod[[1]], type = "diag", title = " Asphodelus fistulosus")
plot_model(mod_seed_year.1$mod[[2]], type = "diag", title = " Asphodelus fistulosus")



# sitio como random 
mod_mix_seed_year<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds ~ visitatio_rate * Year + (1 | Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

#model plot
mod_mix_seed_year.1<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lmer(Seeds ~ visitatio_rate * Year + (1 | Site_ID),data)))

plot_model(mod_mix_seed_year.1$mod[[1]], type = "int", title = " Asphodelus fistulosus",show.data = TRUE)
plot_model(mod_mix_seed_year.1$mod[[2]], type = "int", title = " Cistus crispus",show.data = TRUE)
#get_model_data(mod_mix_seed_year.1$mod[[2]], type = "int")
plot_model(mod_mix_seed_year.1$mod[[3]], type = "int", title = " Cistus ladanifer",show.data = TRUE)
plot_model(mod_mix_seed_year.1$mod[[4]], type = "int",title = "Cistus libanotis", show.data = TRUE)
plot_model(mod_mix_seed_year.1$mod[[5]], type = "int", title = " Cistus monspeliensis",show.data = TRUE)
#get_model_data(mod_mix_seed_year.1$mod[[5]], type = "int")
plot_model(mod_mix_seed_year.1$mod[[6]], type = "int",title = " Cistus salviifolius",show.data = TRUE)
plot_model(mod_mix_seed_year.1$mod[[7]], type = "int",title = " Halimium calycinum", show.data = TRUE)
plot_model(mod_mix_seed_year.1$mod[[8]], type = "int",title = " Halimium halimifolium",show.data = TRUE)
plot_model(mod_mix_seed_year.1$mod[[9]], type = "int",title = " Lavandula pedunculata",show.data = TRUE)
plot_model(mod_mix_seed_year.1$mod[[10]], type = "int",title = " Lavandula stoechas",show.data = TRUE)
plot_model(mod_mix_seed_year.1$mod[[11]], type = "int",title = " Salvia rosmarinus",show.data = TRUE)
plot_model(mod_mix_seed_year.1$mod[[12]], type = "int",title = " Teucrium fruticans",show.data = TRUE)


# estimates of model 
plot_model(mod_mix_seed_year.1$mod[[1]], title = " Asphodelus fistulosus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year.1$mod[[2]], title = "  Cistus crispus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year.1$mod[[3]], title = " Cistus ladanifer",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year.1$mod[[4]], title = "Cistus libanotis",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year.1$mod[[5]], title = "Cistus monspeliensis",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year.1$mod[[6]], title = " Cistus salviifolius",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year.1$mod[[7]], title = " Halimium calycinum",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year.1$mod[[8]], title = " Halimium halimifolium",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year.1$mod[[9]], title = " Lavandula pedunculata",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year.1$mod[[10]], title = " Lavandula stoechas",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year.1$mod[[11]], title = " Salvia rosmarinus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year.1$mod[[12]], title = " Teucrium fruticans",show.values = TRUE, value.offset = .3)


# plots the random effects
plot_model(mod_mix_seed_year.1$mod[[1]], type = "re", title = " Asphodelus fistulosus")
plot_model(mod_mix_seed_year.1$mod[[2]], type = "re", title = " Cistus crispus")
plot_model(mod_mix_seed_year.1$mod[[3]], type = "re", title = " Cistus ladanifer")
plot_model(mod_mix_seed_year.1$mod[[4]], type = "re",title = "Cistus libanotis")
plot_model(mod_mix_seed_year.1$mod[[5]], type = "re", title = " Cistus monspeliensis")
plot_model(mod_mix_seed_year.1$mod[[6]], type = "re",title = " Cistus salviifolius")
plot_model(mod_mix_seed_year.1$mod[[7]], type = "re",title = " Halimium calycinum")
plot_model(mod_mix_seed_year.1$mod[[8]], type = "re",title = " Halimium halimifolium")
plot_model(mod_mix_seed_year.1$mod[[9]], type = "re",title = " Lavandula pedunculata")
plot_model(mod_mix_seed_year.1$mod[[10]], type = "re",title = " Lavandula stoechas")
plot_model(mod_mix_seed_year.1$mod[[11]], type = "re",title = " Salvia rosmarinus")
plot_model(mod_mix_seed_year.1$mod[[12]], type = "re",title = " Teucrium fruticans")

#Check model assumptions
plot_model(mod_mix_seed_year.1$mod[[1]], type = "diag", title = " Asphodelus fistulosus")


#Visitaiton rate of apis mellifera
# linear model 
mod_seed_year_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(Seeds ~ Apis_visit_rate * Year,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()


#model plot
mod_seed_year_apis.1<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(Seeds ~ Apis_visit_rate * Year,data)))

plot_model(mod_seed_year_apis.1$mod[[1]], type = "int", title = " Asphodelus fistulosus",show.data = TRUE)
plot_model(mod_seed_year_apis.1$mod[[2]], type = "int", title = " Cistus crispus",show.data = TRUE)
get_model_data(mod_seed_year_apis.1$mod[[2]], type = "int")

plot_model(mod_seed_year_apis.1$mod[[3]], type = "int", title = " Cistus ladanifer",show.data = TRUE)
plot_model(mod_seed_year_apis.1$mod[[4]], type = "int",title = "Cistus libanotis",show.data = TRUE)

plot_model(mod_seed_year_apis.1$mod[[5]], type = "int", title = " Cistus monspeliensis",show.data = TRUE)
plot_model(mod_seed_year_apis.1$mod[[6]], type = "int",title = " Cistus salviifolius",show.data = TRUE)
plot_model(mod_seed_year_apis.1$mod[[7]], type = "int",title = " Halimium calycinum",show.data = TRUE)
plot_model(mod_seed_year_apis.1$mod[[8]], type = "int",title = " Halimium halimifolium",show.data = TRUE)
plot_model(mod_seed_year_apis.1$mod[[9]], type = "int",title = " Lavandula pedunculata",show.data = TRUE)
plot_model(mod_seed_year_apis.1$mod[[10]], type = "int",title = " Lavandula stoechas",show.data = TRUE)
plot_model(mod_seed_year_apis.1$mod[[11]], type = "int",title = " Salvia rosmarinus",show.data = TRUE)
plot_model(mod_seed_year_apis.1$mod[[12]], type = "int",title = " Teucrium fruticans",show.data = TRUE)

# estimates of model 
plot_model(mod_seed_year_apis.1$mod[[1]], title = " Asphodelus fistulosus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_apis.1$mod[[2]], title = "  Cistus crispus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_apis.1$mod[[3]], title = " Cistus ladanifer",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_apis.1$mod[[4]], title = "Cistus libanotis",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_apis.1$mod[[5]], title = "Cistus monspeliensis",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_apis.1$mod[[6]], title = " Cistus salviifolius",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_apis.1$mod[[7]], title = " Halimium calycinum",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_apis.1$mod[[8]], title = " Halimium halimifolium",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_apis.1$mod[[9]], title = " Lavandula pedunculata",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_apis.1$mod[[10]], title = " Lavandula stoechas",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_apis.1$mod[[11]], title = " Salvia rosmarinus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_apis.1$mod[[12]], title = " Teucrium fruticans",show.values = TRUE, value.offset = .3)



# sitio como random 
mod_mix_seed_year_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds ~ Apis_visit_rate * Year + (1 | Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

#model plot
mod_mix_seed_year_apis.1<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lmer(Seeds ~ Apis_visit_rate * Year + (1 | Site_ID),data)))

plot_model(mod_mix_seed_year_apis.1$mod[[1]], type = "int", title = " Asphodelus fistulosus",show.data = TRUE)
plot_model(mod_mix_seed_year_apis.1$mod[[2]], type = "int", title = " Cistus crispus",show.data = TRUE)
plot_model(mod_mix_seed_year_apis.1$mod[[3]], type = "int", title = " Cistus ladanifer",show.data = TRUE)
plot_model(mod_mix_seed_year_apis.1$mod[[4]], type = "int",title = "Cistus libanotis",show.data = TRUE)
plot_model(mod_mix_seed_year_apis.1$mod[[5]], type = "int", title = " Cistus monspeliensis",show.data = TRUE)
plot_model(mod_mix_seed_year_apis.1$mod[[6]], type = "int",title = " Cistus salviifolius",show.data = TRUE)
plot_model(mod_mix_seed_year_apis.1$mod[[7]], type = "int",title = " Halimium calycinum",show.data = TRUE)
plot_model(mod_mix_seed_year_apis.1$mod[[8]], type = "int",title = " Halimium halimifolium",show.data = TRUE)
plot_model(mod_mix_seed_year_apis.1$mod[[9]], type = "int",title = " Lavandula pedunculata",show.data = TRUE)
plot_model(mod_mix_seed_year_apis.1$mod[[10]], type = "int",title = " Lavandula stoechas",show.data = TRUE)
plot_model(mod_mix_seed_year_apis.1$mod[[11]], type = "int",title = " Salvia rosmarinus",show.data = TRUE)
plot_model(mod_mix_seed_year_apis.1$mod[[12]], type = "int",title = " Teucrium fruticans",show.data = TRUE)


# estimates of model 
plot_model(mod_mix_seed_year_apis.1$mod[[1]], title = " Asphodelus fistulosus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_apis.1$mod[[2]], title = "  Cistus crispus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_apis.1$mod[[3]], title = " Cistus ladanifer",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_apis.1$mod[[4]], title = "Cistus libanotis",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_apis.1$mod[[5]], title = "Cistus monspeliensis",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_apis.1$mod[[6]], title = " Cistus salviifolius",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_apis.1$mod[[7]], title = " Halimium calycinum",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_apis.1$mod[[8]], title = " Halimium halimifolium",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_apis.1$mod[[9]], title = " Lavandula pedunculata",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_apis.1$mod[[10]], title = " Lavandula stoechas",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_apis.1$mod[[11]], title = " Salvia rosmarinus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_apis.1$mod[[12]], title = " Teucrium fruticans",show.values = TRUE, value.offset = .3)


#Visitaiton rate of pollinator without apis mellifera
# linear model 
mod_seed_year_no_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(Seeds ~ No_Apis_visit_rate * Year,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

#model plot
mod_seed_year_no_apis.1<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(Seeds ~ No_Apis_visit_rate * Year,data)))

plot_model(mod_seed_year_no_apis.1$mod[[1]], type = "int", title = " Asphodelus fistulosus")
plot_model(mod_seed_year_no_apis.1$mod[[2]], type = "int", title = " Cistus crispus")
plot_model(mod_seed_year_no_apis.1$mod[[3]], type = "int", title = " Cistus ladanifer")
plot_model(mod_seed_year_no_apis.1$mod[[4]], type = "int",title = "Cistus libanotis")
plot_model(mod_seed_year_no_apis.1$mod[[5]], type = "int", title = " Cistus monspeliensis")
plot_model(mod_seed_year_no_apis.1$mod[[6]], type = "int",title = " Cistus salviifolius")
plot_model(mod_seed_year_no_apis.1$mod[[7]], type = "int",title = " Halimium calycinum")
plot_model(mod_seed_year_no_apis.1$mod[[8]], type = "int",title = " Halimium halimifolium")
plot_model(mod_seed_year_no_apis.1$mod[[9]], type = "int",title = " Lavandula pedunculata")
plot_model(mod_seed_year_no_apis.1$mod[[10]], type = "int",title = " Lavandula stoechas")
plot_model(mod_seed_year_no_apis.1$mod[[11]], type = "int",title = " Salvia rosmarinus")
plot_model(mod_seed_year_no_apis.1$mod[[12]], type = "int",title = " Teucrium fruticans")

# estimates of model 
plot_model(mod_seed_year_no_apis.1$mod[[1]], title = " Asphodelus fistulosus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_no_apis.1$mod[[2]], title = "  Cistus crispus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_no_apis.1$mod[[3]], title = " Cistus ladanifer",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_no_apis.1$mod[[4]], title = "Cistus libanotis",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_no_apis.1$mod[[5]], title = "Cistus monspeliensis",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_no_apis.1$mod[[6]], title = " Cistus salviifolius",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_no_apis.1$mod[[7]], title = " Halimium calycinum",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_no_apis.1$mod[[8]], title = " Halimium halimifolium",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_no_apis.1$mod[[9]], title = " Lavandula pedunculata",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_no_apis.1$mod[[10]], title = " Lavandula stoechas",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_no_apis.1$mod[[11]], title = " Salvia rosmarinus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_no_apis.1$mod[[12]], title = " Teucrium fruticans",show.values = TRUE, value.offset = .3)

# sitio como random 
mod_mix_seed_year_no_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds ~ No_Apis_visit_rate * Year + (1 | Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

levels(factor(Pollinator_Plant$Plant_gen_sp))

#model plot
mod_mix_seed_year_no_apis.1<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lmer(Seeds ~ No_Apis_visit_rate * Year + (1 | Site_ID),data)))

plot_model(mod_mix_seed_year_no_apis.1$mod[[1]], type = "int", title = " Asphodelus fistulosus")
plot_model(mod_mix_seed_year_no_apis.1$mod[[2]], type = "int", title = " Cistus crispus")
plot_model(mod_mix_seed_year_no_apis.1$mod[[3]], type = "int", title = " Cistus ladanifer")
plot_model(mod_mix_seed_year_no_apis.1$mod[[4]], type = "int",title = "Cistus libanotis")
plot_model(mod_mix_seed_year_no_apis.1$mod[[5]], type = "int", title = " Cistus monspeliensis")
plot_model(mod_mix_seed_year_no_apis.1$mod[[6]], type = "int",title = " Cistus salviifolius")
plot_model(mod_mix_seed_year_no_apis.1$mod[[7]], type = "int",title = " Halimium calycinum")
plot_model(mod_mix_seed_year_no_apis.1$mod[[8]], type = "int",title = " Halimium halimifolium")
plot_model(mod_mix_seed_year_no_apis.1$mod[[9]], type = "int",title = " Lavandula pedunculata")
plot_model(mod_mix_seed_year_no_apis.1$mod[[10]], type = "int",title = " Lavandula stoechas")
plot_model(mod_mix_seed_year_no_apis.1$mod[[11]], type = "int",title = " Salvia rosmarinus")
plot_model(mod_mix_seed_year_no_apis.1$mod[[12]], type = "int",title = " Teucrium fruticans")


# estimates of model 
plot_model(mod_mix_seed_year_no_apis.1$mod[[1]], title = " Asphodelus fistulosus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_no_apis.1$mod[[2]], title = "  Cistus crispus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_no_apis.1$mod[[3]], title = " Cistus ladanifer",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_no_apis.1$mod[[4]], title = "Cistus libanotis",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_no_apis.1$mod[[5]], title = "Cistus monspeliensis",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_no_apis.1$mod[[6]], title = " Cistus salviifolius",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_no_apis.1$mod[[7]], title = " Halimium calycinum",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_no_apis.1$mod[[8]], title = " Halimium halimifolium",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_no_apis.1$mod[[9]], title = " Lavandula pedunculata",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_no_apis.1$mod[[10]], title = " Lavandula stoechas",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_no_apis.1$mod[[11]], title = " Salvia rosmarinus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_no_apis.1$mod[[12]], title = " Teucrium fruticans",show.values = TRUE, value.offset = .3)


#Visitation rate of hymenoptera
# linear model 
mod_seed_year_hym<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(Seeds ~ Hym_visit * Year,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

#model plot
mod_seed_year_hym.1<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(Seeds ~ Hym_visit * Year,data)))

plot_model(mod_seed_year_hym.1$mod[[1]], type = "int", title = " Asphodelus fistulosus",show.data = TRUE)
plot_model(mod_seed_year_hym.1$mod[[2]], type = "int", title = " Cistus crispus",show.data = TRUE)
plot_model(mod_seed_year_hym.1$mod[[3]], type = "int", title = " Cistus ladanifer",show.data = TRUE)
plot_model(mod_seed_year_hym.1$mod[[4]], type = "int",title = "Cistus libanotis",show.data = TRUE)
plot_model(mod_seed_year_hym.1$mod[[5]], type = "int", title = " Cistus monspeliensis",show.data = TRUE)
plot_model(mod_seed_year_hym.1$mod[[6]], type = "int",title = " Cistus salviifolius",show.data = TRUE)
plot_model(mod_seed_year_hym.1$mod[[7]], type = "int",title = " Halimium calycinum",show.data = TRUE)
plot_model(mod_seed_year_hym.1$mod[[8]], type = "int",title = " Halimium halimifolium",show.data = TRUE)
plot_model(mod_seed_year_hym.1$mod[[9]], type = "int",title = " Lavandula pedunculata",show.data = TRUE)
plot_model(mod_seed_year_hym.1$mod[[10]], type = "int",title = " Lavandula stoechas",show.data = TRUE)
plot_model(mod_seed_year_hym.1$mod[[11]], type = "int",title = " Salvia rosmarinus",show.data = TRUE)
plot_model(mod_seed_year_hym.1$mod[[12]], type = "int",title = " Teucrium fruticans",show.data = TRUE)

# estimates of model 
plot_model(mod_seed_year_hym.1$mod[[1]], title = " Asphodelus fistulosus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_hym.1$mod[[2]], title = "  Cistus crispus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_hym.1$mod[[3]], title = " Cistus ladanifer",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_hym.1$mod[[4]], title = "Cistus libanotis",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_hym.1$mod[[5]], title = "Cistus monspeliensis",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_hym.1$mod[[6]], title = " Cistus salviifolius",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_hym.1$mod[[7]], title = " Halimium calycinum",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_hym.1$mod[[8]], title = " Halimium halimifolium",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_hym.1$mod[[9]], title = " Lavandula pedunculata",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_hym.1$mod[[10]], title = " Lavandula stoechas",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_hym.1$mod[[11]], title = " Salvia rosmarinus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_hym.1$mod[[12]], title = " Teucrium fruticans",show.values = TRUE, value.offset = .3)


# sitio como random 
mod_mix_seed_year_hym<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds ~ Hym_visit * Year + (1 | Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


#model plot
mod_mix_seed_year_hym.1<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lmer(Seeds ~ Hym_visit * Year + (1 | Site_ID),data)))

plot_model(mod_mix_seed_year_hym.1$mod[[1]], type = "int", title = " Asphodelus fistulosus",show.data = TRUE)
plot_model(mod_mix_seed_year_hym.1$mod[[2]], type = "int", title = " Cistus crispus",show.data = TRUE)
plot_model(mod_mix_seed_year_hym.1$mod[[3]], type = "int", title = " Cistus ladanifer",show.data = TRUE)
plot_model(mod_mix_seed_year_hym.1$mod[[4]], type = "int",title = "Cistus libanotis",show.data = TRUE)
plot_model(mod_mix_seed_year_hym.1$mod[[5]], type = "int", title = " Cistus monspeliensis",show.data = TRUE)
plot_model(mod_mix_seed_year_hym.1$mod[[6]], type = "int",title = " Cistus salviifolius",show.data = TRUE)
plot_model(mod_mix_seed_year_hym.1$mod[[7]], type = "int",title = " Halimium calycinum",show.data = TRUE)
plot_model(mod_mix_seed_year_hym.1$mod[[8]], type = "int",title = " Halimium halimifolium",show.data = TRUE)
plot_model(mod_mix_seed_year_hym.1$mod[[9]], type = "int",title = " Lavandula pedunculata",show.data = TRUE)
plot_model(mod_mix_seed_year_hym.1$mod[[10]], type = "int",title = " Lavandula stoechas",show.data = TRUE)
plot_model(mod_mix_seed_year_hym.1$mod[[11]], type = "int",title = " Salvia rosmarinus",show.data = TRUE)
plot_model(mod_mix_seed_year_hym.1$mod[[12]], type = "int",title = " Teucrium fruticans",show.data = TRUE)


# estimates of model 
plot_model(mod_mix_seed_year_hym.1$mod[[1]], title = " Asphodelus fistulosus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_hym.1$mod[[2]], title = "  Cistus crispus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_hym.1$mod[[3]], title = " Cistus ladanifer",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_hym.1$mod[[4]], title = "Cistus libanotis",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_hym.1$mod[[5]], title = "Cistus monspeliensis",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_hym.1$mod[[6]], title = " Cistus salviifolius",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_hym.1$mod[[7]], title = " Halimium calycinum",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_hym.1$mod[[8]], title = " Halimium halimifolium",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_hym.1$mod[[9]], title = " Lavandula pedunculata",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_hym.1$mod[[10]], title = " Lavandula stoechas",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_hym.1$mod[[11]], title = " Salvia rosmarinus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_hym.1$mod[[12]], title = " Teucrium fruticans",show.values = TRUE, value.offset = .3)


#Visitation rate of hymenoptera without A.mellifera
# linear model 
mod_seed_year_hym_no_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(Seeds ~ Hym_visit_sin_apis * Year,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

#model plot
mod_seed_year_hym_no_apis.1<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(Seeds ~ Hym_visit_sin_apis * Year,data)))

plot_model(mod_seed_year_hym_no_apis.1$mod[[1]], type = "int", title = " Asphodelus fistulosus",show.data = TRUE)
plot_model(mod_seed_year_hym_no_apis.1$mod[[2]], type = "int", title = " Cistus crispus",show.data = TRUE)
plot_model(mod_seed_year_hym_no_apis.1$mod[[3]], type = "int", title = " Cistus ladanifer",show.data = TRUE)
plot_model(mod_seed_year_hym_no_apis.1$mod[[4]], type = "int",title = "Cistus libanotis",show.data = TRUE)
plot_model(mod_seed_year_hym_no_apis.1$mod[[5]], type = "int", title = " Cistus monspeliensis",show.data = TRUE)
plot_model(mod_seed_year_hym_no_apis.1$mod[[6]], type = "int",title = " Cistus salviifolius",show.data = TRUE)
plot_model(mod_seed_year_hym_no_apis.1$mod[[7]], type = "int",title = " Halimium calycinum",show.data = TRUE)
plot_model(mod_seed_year_hym_no_apis.1$mod[[8]], type = "int",title = " Halimium halimifolium",show.data = TRUE)
plot_model(mod_seed_year_hym_no_apis.1$mod[[9]], type = "int",title = " Lavandula pedunculata",show.data = TRUE)
plot_model(mod_seed_year_hym_no_apis.1$mod[[10]], type = "int",title = " Lavandula stoechas",show.data = TRUE)
plot_model(mod_seed_year_hym_no_apis.1$mod[[11]], type = "int",title = " Salvia rosmarinus",show.data = TRUE)
plot_model(mod_seed_year_hym_no_apis.1$mod[[12]], type = "int",title = " Teucrium fruticans",show.data = TRUE)

# estimates of model 
plot_model(mod_seed_year_hym_no_apis.1$mod[[1]], title = " Asphodelus fistulosus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_hym_no_apis.1$mod[[2]], title = "  Cistus crispus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_hym_no_apis.1$mod[[3]], title = " Cistus ladanifer",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_hym_no_apis.1$mod[[4]], title = "Cistus libanotis",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_hym_no_apis.1$mod[[5]], title = "Cistus monspeliensis",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_hym_no_apis.1$mod[[6]], title = " Cistus salviifolius",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_hym_no_apis.1$mod[[7]], title = " Halimium calycinum",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_hym_no_apis.1$mod[[8]], title = " Halimium halimifolium",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_hym_no_apis.1$mod[[9]], title = " Lavandula pedunculata",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_hym_no_apis.1$mod[[10]], title = " Lavandula stoechas",show.values = TRUE,value.offset = .3)
plot_model(mod_seed_year_hym_no_apis.1$mod[[11]], title = " Salvia rosmarinus",show.values = TRUE, value.offset = .3)
plot_model(mod_seed_year_hym_no_apis.1$mod[[12]], title = " Teucrium fruticans",show.values = TRUE, value.offset = .3)


# sitio como random 
mod_mix_seed_year_hym_no_apis<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds ~ Hym_visit_sin_apis * Year + (1 | Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()


#model plot
mod_mix_seed_year_hym_no_apis.1<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lmer(Seeds ~ Hym_visit_sin_apis * Year + (1 | Site_ID),data)))

plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[1]], type = "int", title = " Asphodelus fistulosus",show.data = TRUE)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[2]], type = "int", title = " Cistus crispus",show.data = TRUE)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[3]], type = "int", title = " Cistus ladanifer",show.data = TRUE)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[4]], type = "int",title = "Cistus libanotis",show.data = TRUE)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[5]], type = "int", title = " Cistus monspeliensis",show.data = TRUE)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[6]], type = "int",title = " Cistus salviifolius",show.data = TRUE)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[7]], type = "int",title = " Halimium calycinum",show.data = TRUE)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[8]], type = "int",title = " Halimium halimifolium",show.data = TRUE)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[9]], type = "int",title = " Lavandula pedunculata",show.data = TRUE)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[10]], type = "int",title = " Lavandula stoechas",show.data = TRUE)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[11]], type = "int",title = " Salvia rosmarinus",show.data = TRUE)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[12]], type = "int",title = " Teucrium fruticans",show.data = TRUE)


# estimates of model 
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[1]], title = " Asphodelus fistulosus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[2]], title = "  Cistus crispus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[3]], title = " Cistus ladanifer",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[4]], title = "Cistus libanotis",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[5]], title = "Cistus monspeliensis",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[6]], title = " Cistus salviifolius",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[7]], title = " Halimium calycinum",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[8]], title = " Halimium halimifolium",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[9]], title = " Lavandula pedunculata",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[10]], title = " Lavandula stoechas",show.values = TRUE,value.offset = .3)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[11]], title = " Salvia rosmarinus",show.values = TRUE, value.offset = .3)
plot_model(mod_mix_seed_year_hym_no_apis.1$mod[[12]], title = " Teucrium fruticans",show.values = TRUE, value.offset = .3)



# year like random slope -----

mod_seed_slopes<-Pollinator_Plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds ~ visitatio_rate + (1 + Year | Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

# hay especies que dan problema porque hay menos observaciones que 
# random effect. El error dice:  the random-effects parameters and 
# the residual variance (or scale parameter) are probably unidentifiable
# las especies que dan problema y habria que eliminar son:
# Halimium calycinum, Lavandula pedunculata, Lavandula stoechas, Salvia rosmarinus
mod_seed_slopes<-Pollinator_Plant %>%
  filter(!Plant_gen_sp=="Halimium calycinum") %>%
  filter(!Plant_gen_sp=="Lavandula pedunculata") %>%
  filter(!Plant_gen_sp=="Lavandula stoechas") %>%
  filter(!Plant_gen_sp=="Salvia rosmarinus") %>%
  nest_by(Plant_gen_sp) %>%
  mutate(model = list(lmer(Seeds ~ visitatio_rate + (1 + Year | Site_ID),data))) %>%
  summarise(broom.mixed::tidy(model)) %>%
  ungroup()

# cuando se prueban los modelos con estas especies se observa el problema
caly=Pollinator_Plant%>%
  filter(Plant_gen_sp=="Halimium calycinum")
prueba= lmer(Seeds ~ visitatio_rate + (1 + Year | Site_ID),data=caly)

pedu=Pollinator_Plant%>%
  filter(Plant_gen_sp=="Lavandula pedunculata")
prueba_pedu= lmer(Seeds ~ visitatio_rate + (1 + Year | Site_ID),data=pedu)

stoechas=Pollinator_Plant%>%
  filter(Plant_gen_sp=="Lavandula stoechas")
prueba_sto= lmer(Seeds ~ visitatio_rate + (1 + Year | Site_ID),data=stoechas)

salvia=Pollinator_Plant%>%
  filter(Plant_gen_sp=="Salvia rosmarinus")
prueba_salvi= lmer(Seeds ~ visitatio_rate + (1 + Year | Site_ID),data=salvia)




## modelo general añadiento planta como random slope
trial= lmer(Seeds ~ visitatio_rate * Year + (1 + Plant_gen_sp | Site_ID),data=Pollinator_Plant)

summary(trial)
plot(allEffects(trial))
plot_model(trial, type="int", show.data = TRUE)
plot_model(trial, type = "re")

library(visreg)
visreg(trial, "visitatio_rate", by="Year", overlay=TRUE)

#########
# Lavandula species together -----
#(L.peduncalata (49 obs) and L.stoechas (24 obs))

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
