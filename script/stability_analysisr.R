
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
library(effects)
library(MuMIn)
library(sjPlot)
library(RColorBrewer)
library(Hmisc)


# stability data 
data_plant<- read.csv("Data/Stability.csv")
data_plant<-data_plant[,-1]


# eliminar plant species 
sp_out2=data_plant %>%
  group_by(Plant_gen_sp) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id <= 4) %>%
  dplyr::select(Plant_gen_sp) %>% pull(Plant_gen_sp)

#Data with species in at least 5 sites
data_plant = data_plant %>% filter(!Plant_gen_sp %in% sp_out2)

levels(factor(data_plant$Plant_gen_sp))


data_plant=data_plant %>%
  filter(!Plant_gen_sp=="Salvia rosmarinus") %>%
  filter(!Plant_gen_sp=="Halimium calycinum")%>%
  filter(!Plant_gen_sp=="Lavandula stoechas")


## Replace Inf and -Inf with NA
data_plant[is.na(data_plant) | data_plant == "Inf"] <- NA 
data_plant[is.na(data_plant) | data_plant == "-Inf"] <- NA 



# correlacion indices de sincronia
syn_cor<-data_plant %>% select(log_VR,syncLM,av_sync)
syn_cor <- mutate_all(syn_cor, ~replace(., is.na(.), 0))
cor(syn_cor)
cor_1 <- rcorr(as.matrix(syn_cor))
cor_1


# correlacion por plant species
syn_cor_pla<-data_plant %>% select(Plant_gen_sp,log_VR,syncLM,av_sync)
syn_cor_pla <- mutate_all(syn_cor_pla, ~replace(., is.na(.), 0))

b_cor <- syn_cor_pla %>%
  group_by(Plant_gen_sp) %>%
  do(cormat = cor(select(., -matches("Plant_gen_sp")))) 

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




# We can express asynchrony as 1 - φ, 
#where φ is Loreau and de Mazancourt’s (2008)
data_plant$asyn_LM= 1-data_plant$syncLM


# average and std.error
ave_visit <- data_plant%>% 
   summarize(mean_visit = mean(cv_1_visitation, na.rm=T),
            standarderror = sd(cv_1_visitation, na.rm = TRUE)/sqrt(n()))

ave_richness <- data_plant%>% 
  summarize(mean_richness = mean(S_total, na.rm=T),
            standarderror = sd(S_total, na.rm = TRUE)/sqrt(n()))

ave_asyn <- data_plant%>% 
  summarize(mean_asyn = mean(asyn_LM, na.rm=T),
            standarderror = sd(asyn_LM, na.rm = TRUE)/sqrt(n()))


ave_fruit <- data_plant%>% 
  summarize(mean_fruit = mean(cv_1_fruit, na.rm=T),
            standarderror = sd(cv_1_fruit, na.rm = TRUE)/sqrt(n()))

ave_seed <- data_plant%>% 
  summarize(mean_seed = mean(cv_1_seed, na.rm=T),
            standarderror = sd(cv_1_seed, na.rm = TRUE)/sqrt(n()))





# we analyze whether the stability of visitation rate is affect 
#by total richness and asynchrony (L & M index) 
#incorporating site and plant species as random effects 

#pollinator abundance is corrected by the sampling effort
#to calculate the synchrony index

mod1_sta= lmer (cv_1_visitation ~ S_total + (1|Site_ID) + (1|Plant_gen_sp), data = data_plant)
mod1_sta_2= lmer (cv_1_visitation ~ asyn_LM + (1|Site_ID) + (1|Plant_gen_sp), data = data_plant)


summary(mod1_sta)
summary(mod1_sta_2)

r.squaredGLMM(mod1_sta)

car::Anova(mod1_sta,Type="III")

 
plot_model(mod1_sta_2, type="pred", show.data = T)
plot_model(mod1_sta, type = "re")

#getting effects for asyncLM 
effe_mod_sta <-data.frame( effect("asyn_LM", mod1_sta, se = TRUE))
#plot model 
p1=ggplot(effe_mod_sta, aes(asyn_LM, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3,alpha=0.1, colour = "black")+
  labs(color='Plant species')+
  geom_point(data = data_plant, aes(x =  asyn_LM, y = cv_1_visitation, color=Plant_gen_sp), size=3)+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+
  labs(x = "Asynchrony of pollinator",y="Stability of visitation rate")


#getting effects for richness
effe2_mod_sta <-data.frame( effect("S_total", mod1_sta, se = TRUE))
#plor model 
p2=ggplot(effe2_mod_sta, aes(S_total, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3, alpha=0.1, colour = "black")+
  labs(color='Plant species')+
  geom_point(data = data_plant, aes(x =  S_total, y = cv_1_visitation, color=Plant_gen_sp), size=3)+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+
  labs(x = "Richness of pollinator",y="")
 
p_cv_visit=p1+p2+ 
  plot_annotation(tag_levels = "A")& 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = -3.5, vjust = 1.5, size=15))& theme(legend.position = "bottom",legend.background = element_blank(),legend.box.background = element_rect(colour = "black"))
p_cv_visit+ plot_layout(guides = "collect")



# relationship between richness and asynchrony
mod_rich_asyn= lmer (asyn_LM ~ S_total +  (1|Site_ID) + (1|Plant_gen_sp), weight=asyn_LM,data = data_plant)
summary(mod_rich_asyn)
plot_model(mod_rich_asyn,type="pred", show.data = T)

#getting effects for richness
effe_rich_mod_sta <-data.frame( effect("S_total", mod_rich_asyn, se = TRUE))
#plor model 
p1.2=ggplot(effe_rich_mod_sta, aes(S_total, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3, alpha=0.1, colour = "black")+
  labs(color='Plant species')+
  geom_point(data = data_plant, aes(x =  S_total, y = asyn_LM, color=Plant_gen_sp), size=3)+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+
  labs(x = "Richness of pollinator",y="Asynchrony")


#########################################3
# out c.crispus el pozo = valor de estabilidad en tasa de visita muy alto
# ver si cambia el modelo

data_sin_out=data_plant %>%
  filter(!(Site_ID=="El_pozo" & Plant_gen_sp=="Cistus crispus")) 


#model richness
mod_sta_out= lmer (cv_1_visitation ~ S_total + (1|Site_ID) + (1|Plant_gen_sp), data = data_sin_out)
summary(mod_sta_out)

#getting effects for richness
effe2_mod_sta_out <-data.frame( effect("S_total", mod_sta_out, se = TRUE))
#plor model 
ggplot(effe2_mod_sta_out, aes(S_total, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3, alpha=0.1, colour = "black")+
  labs(color='Plant species')+
  geom_point(data = data_sin_out, aes(x =  S_total, y = cv_1_visitation, color=Plant_gen_sp), size=3)+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+
  labs(x = "Richness of pollinator",y="Satability of visitaion rate")




# model asynchrony
mod1_sta_2out= lmer (cv_1_visitation ~ asyn_LM + (1|Site_ID) + (1|Plant_gen_sp), data = data_sin_out)
summary(mod1_sta_2out)

#getting effects for asyncLM 
effe_mod_sta2_out <-data.frame( effect("asyn_LM", mod1_sta_2out, se = TRUE))
#plot model 
ggplot(effe_mod_sta2_out, aes(asyn_LM, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3,alpha=0.1, colour = "black")+
  labs(color='Plant species')+
  geom_point(data = data_sin_out, aes(x =  asyn_LM, y = cv_1_visitation, color=Plant_gen_sp), size=3)+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+
  labs(x = "Asynchrony of pollinator",y="Stability of visitation rate")



##################################################




# we analyze whether the stability of fruit set is affect 
#by stability of visitation rate
#incorporating site and plant species as random effects 

mod2_sta= lmer(cv_1_fruit ~ cv_1_visitation + (1|Plant_gen_sp)+(1|Site_ID), data = data_plant)
summary(mod2_sta)

plot_model(mod2_sta, type="pred",  show.data = T)
plot_model(mod2_sta, type = "re")
 
#getting effects 
effe3_mod_sta <-data.frame( effect("cv_1_visitation", mod2_sta, se = TRUE))
#plor model 
p3=ggplot(effe3_mod_sta, aes(cv_1_visitation, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = data_plant, aes(x =  cv_1_visitation, y = cv_1_fruit, color=Plant_gen_sp), size=3)+
  labs(y = "fruit set stability",x="visitation rate stability")
 
#########################################3
# out c.salviifolius la cunya = valor de estabilidad en fruit proportion muy alto
# ver si cambia el modelo

data_slavi_out=data_plant %>%
  filter(!(Site_ID=="La_cunya" & Plant_gen_sp=="Cistus salviifolius")) 

mod2_sta_out= lmer(cv_1_fruit ~ cv_1_visitation + (1|Plant_gen_sp)+(1|Site_ID), data = data_slavi_out)
summary(mod2_sta_out)

#getting effects 
effe_mod_sta_out <-data.frame( effect("cv_1_visitation", mod2_sta_out, se = TRUE))
#plot model 
p4_out=ggplot(effe_mod_sta_out, aes(cv_1_visitation, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = data_slavi_out, aes(x =  cv_1_visitation, y = cv_1_fruit, color=Plant_gen_sp), size=3)+
  labs(y = "fruit set stability",x="visitation rate stability")



#########################################3

# we analyze whether the stability of seed set is affect 
#by stability of visitation rate
#incorporating site and plant species as random effects 

mod3_sta= lmer(cv_1_seed ~ cv_1_visitation + (1|Plant_gen_sp)+(1|Site_ID), data = data_plant)
summary(mod3_sta)

plot_model(mod3_sta, type="pred",  show.data = T)
plot_model(mod3_sta, type = "re")

#getting effects 
effe4_mod_sta <-data.frame( effect("cv_1_visitation", mod3_sta, se = TRUE))
#plot model 
p4=ggplot(effe4_mod_sta, aes(cv_1_visitation, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = data_plant, aes(x =  cv_1_visitation, y = cv_1_seed, color=Plant_gen_sp), size=3)+
  labs(y = "seed set stability",x="visitation rate stability")


#########################################3
# out H.halimifolium el pozo = valor de estabilidad en numero de semilla muy alto
# ver si cambia el modelo

data_hali_out=data_plant %>%
  filter(!(Site_ID=="El_pozo" & Plant_gen_sp=="Halimium halimifolium")) 

mod3_sta_out= lmer(cv_1_seed ~ cv_1_visitation + (1|Plant_gen_sp)+(1|Site_ID), data = data_hali_out)
summary(mod3_sta_out)

#getting effects 
effe_mod_sta_out <-data.frame( effect("cv_1_visitation", mod3_sta_out, se = TRUE))
#plot model 
p4_out=ggplot(effe_mod_sta_out, aes(cv_1_visitation, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = data_hali_out, aes(x =  cv_1_visitation, y = cv_1_seed, color=Plant_gen_sp), size=3)+
  labs(y = "seed set stability",x="visitation rate stability")



#########################################3


# we analysed the effect of richness and synchrony on stability of visitation rate
# per plant species separately
# stability ~ richness + synchrony per plant species 
sta_polli_data_plant<-data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

sta_polli_glance<-data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+asyn_LM,data))) %>%
  summarize(glance(mod))%>%
  ungroup()

# model2 plot (cv_1_visitation~total richness)
sta_polli_data_plant.2 <- data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "S_total"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line(size=1) +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = 3, alpha=0.1, colour = "black")))



p1=sta_polli_data_plant.2$plots[[1]] + geom_point(data =data_plant %>% filter(Plant_gen_sp == "Cistus crispus"), 
  aes(x =  S_total, y = cv_1_visitation),color="#e41a1c", size=2)+ 
  labs(x = "",y="Stability of visitation rate",subtitle = "Cistus crispus")+scale_x_continuous(limits = c(9, 16))+
 theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))
  

p2=sta_polli_data_plant.2$plots[[2]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), 
 aes(x =  S_total, y = cv_1_visitation),color="#377eb8", size=2)+ 
  labs(x = "",y=NULL,subtitle = "Cistus ladanifer")+scale_x_continuous(limits = c(3, 16))+ 
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))
 

p3=sta_polli_data_plant.2$plots[[3]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), 
  aes(x =  S_total, y = cv_1_visitation),color= "#4daf4a", size=2)+ 
  labs(x = "Richness",y=NULL,subtitle = "Cistus salviifolius")+scale_x_continuous(limits = c(3, 20))+  
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))
              

p4=sta_polli_data_plant.2$plots[[4]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), 
   aes(x =  S_total, y = cv_1_visitation), color= "#984ea3", size=2)+ 
  labs(x = "",y=NULL,subtitle = "Halimium halimifolium")+ scale_x_continuous(limits = c(4, 14))+ 
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))
  

p5=sta_polli_data_plant.2$plots[[5]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), 
  aes(x =  S_total, y = cv_1_visitation),color="#ff7f00", size=2)+ 
  labs(x = "",y=NULL,subtitle = "Lavandula pedunculata")+  
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))
  




# model2 plot (cv_1_visitation~syncLM)
sta_polli_data_plant.3 <- data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~asyn_LM,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "asyn_LM"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line(size=1) +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high),linetype = 3, alpha=0.1, colour = "black")))




p1.1=sta_polli_data_plant.3$plots[[1]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus crispus"), 
   aes(x =  asyn_LM, y = cv_1_visitation),color="#e41a1c", size=2)+ 
  labs(x = "",y="Stability of visitaion rate")+
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


p2.1=sta_polli_data_plant.3$plots[[2]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), 
 aes(x =  asyn_LM, y = cv_1_visitation),color="#377eb8", size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))
 


p3.1=sta_polli_data_plant.3$plots[[3]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), 
 aes(x =  asyn_LM, y = cv_1_visitation),color= "#4daf4a", size=2)+ 
  labs(x = "Asynchrony of pollinators",y= NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))
  

p4.1=sta_polli_data_plant.3$plots[[4]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), 
  aes(x =  asyn_LM, y = cv_1_visitation), color= "#984ea3", size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))

   

p5.1=sta_polli_data_plant.3$plots[[5]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), 
 aes(x =  asyn_LM, y = cv_1_visitation), color="#ff7f00", size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))
  


p1 +p2+p3+p4+p5+p1.1+p2.1+p3.1+p4.1+p5.1+plot_layout(ncol = 5)& theme(axis.title = element_text(face="bold"))



# we analysed the stability of visitation rate on stability of fruit proportion
# per plant species separately
sta_polli_fruit<-data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_fruit~cv_1_visitation,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

# model plot (cv_1_fruit~cv_1_visitation)
sta_polli_fruit.2 <- data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_fruit~cv_1_visitation,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "cv_1_visitation"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line(size=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),linetype = 3, alpha=0.1, colour = "black")))


p1.fruit=sta_polli_fruit.2$plots[[1]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus crispus"), 
  aes(x =  cv_1_visitation, y = cv_1_fruit),color="#e41a1c",fill="coral2", pch=21, size=2)+ 
  labs(x = "",y="Stability of fruit proportion")+
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))



p2.fruit=sta_polli_fruit.2$plots[[2]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), 
aes(x =  cv_1_visitation, y = cv_1_fruit),color="yellow4",fill="yellow4",pch=21, size=2)+ 
labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


p3.fruit=sta_polli_fruit.2$plots[[3]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), 
  aes(x =  cv_1_visitation, y = cv_1_fruit),color= "springgreen3",fill="springgreen3",pch=21, size=2)+ 
  labs(x = "Stability of visitation rate",y= NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


p4.fruit=sta_polli_fruit.2$plots[[4]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), 
aes(x =  cv_1_visitation, y = cv_1_fruit), color= "cyan3",fill="cyan3", pch=21, size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


p5.fruit=sta_polli_fruit.2$plots[[5]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), 
  aes(x =  cv_1_visitation, y = cv_1_fruit), color="hotpink",fill="hotpink",pch=21, size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))




# we analysed the stability of visitation rate on stability of fruit proportion
# per plant species separately
sta_polli_seed<-data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_seed~cv_1_visitation,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()


# model plot (cv_1_fruit~cv_1_visitation)
sta_polli_seed.2 <- data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_seed~cv_1_visitation,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "cv_1_visitation"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line(size=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),linetype = 3, alpha=0.1, colour = "black")))


p1.seed=sta_polli_seed.2$plots[[1]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus crispus"), 
aes(x =  cv_1_visitation, y = cv_1_seed),color="coral3",fill="coral2", pch=21, size=2)+ 
  labs(x = "",y="Stability of seed number")+
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


p2.seed=sta_polli_seed.2$plots[[2]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), 
 aes(x =  cv_1_visitation, y = cv_1_seed),color="yellow4",fill="yellow4",pch=21, size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


p3.seed=sta_polli_seed.2$plots[[3]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), 
  aes(x =  cv_1_visitation, y = cv_1_seed),color= "springgreen3",fill="springgreen3",pch=21, size=2)+ 
  labs(x = "Stability of visitation rate",y= NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


p4.seed=sta_polli_seed.2$plots[[4]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), 
 aes(x =  cv_1_visitation, y = cv_1_seed), color= "cyan3",fill="cyan3", pch=21, size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


p5.seed=sta_polli_seed.2$plots[[5]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), 
aes(x =  cv_1_visitation, y = cv_1_seed), color="hotpink",fill="hotpink",pch=21, size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))




p1.fruit +p2.fruit+p3.fruit+p4.fruit+p5.fruit+
  p1.seed+p2.seed+p3.seed+p4.seed+ p5.seed + 
  plot_layout(ncol = 5)& theme(axis.title = element_text(face="bold"))


