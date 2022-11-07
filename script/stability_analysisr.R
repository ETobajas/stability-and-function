
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


# stability data 
data_plant<- read.csv("Data/Stability.csv")
data_plant<-data_plant[,-1]


# eliminar plant species 
sp_out2=data_plant %>%
  group_by(Plant_gen_sp) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id <= 3) %>%
  select(Plant_gen_sp) %>% pull(Plant_gen_sp)

#Data with species in more than 3 sites
data_plant2 = data_plant %>% filter(!Plant_gen_sp %in% sp_out2)

levels(factor(data_plant2$Plant_gen_sp))


## Replace Inf and -Inf with NA
data_plant2[is.na(data_plant2) | data_plant2 == "Inf"] <- NA 
data_plant2[is.na(data_plant2) | data_plant2 == "-Inf"] <- NA 



# We can express asynchrony as 1 - φ, 
#where φ is Loreau and de Mazancourt’s (2008)
data_plant2$asyn_LM= 1-data_plant2$syncLM



# average and std.error
ave_visit <- data_plant2%>% 
   summarize(mean_visit = mean(cv_1_visitation, na.rm=T),
            standarderror = sd(cv_1_visitation, na.rm = TRUE)/sqrt(n()))

ave_richness <- data_plant2%>% 
  summarize(mean_visit = mean(S_total, na.rm=T),
            standarderror = sd(S_total, na.rm = TRUE)/sqrt(n()))

ave_asyn <- data_plant2%>% 
  summarize(mean_visit = mean(asyn_LM, na.rm=T),
            standarderror = sd(asyn_LM, na.rm = TRUE)/sqrt(n()))


ave_fruit <- data_plant2%>% 
  summarize(mean_visit = mean(cv_1_fruit, na.rm=T),
            standarderror = sd(cv_1_fruit, na.rm = TRUE)/sqrt(n()))

ave_seed <- data_plant2%>% 
  summarize(mean_visit = mean(cv_1_seed, na.rm=T),
            standarderror = sd(cv_1_seed, na.rm = TRUE)/sqrt(n()))



# we analyze whether the stability of visitation rate is affect 
#by total richness and asynchrony (L & M index) 
#incorporating site and plant species as random effects 

#pollinator abundance is corrected by the sampling effort
#to calculate the synchrony index

mod1_sta= lmer (cv_1_visitation ~ S_total + asyn_LM + (1|Site_ID) + (1|Plant_gen_sp), data = data_plant2)


summary(mod1_sta)
r.squaredGLMM(mod1_sta)
vif(mod1_sta)

car::Anova(mod1_sta,Type="III")

 
plot_model(mod1_sta, type="pred", show.data = T)
plot_model(mod1_sta, type = "re")

#getting effects for asyncLM 
effe_mod_sta <-data.frame( effect("asyn_LM", mod1_sta, se = TRUE))
#plot model 
p1=ggplot(effe_mod_sta, aes(asyn_LM, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3,alpha=0.1, colour = "black")+
  labs(color='Plant species')+
  geom_point(data = data_plant2, aes(x =  asyn_LM, y = cv_1_visitation, color=Plant_gen_sp), size=3)+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+
  labs(x = "Asynchrony of pollinator",y="Stability of visitation rate")


#getting effects for richness
effe2_mod_sta <-data.frame( effect("S_total", mod1_sta, se = TRUE))
#plor model 
p2=ggplot(effe2_mod_sta, aes(S_total, fit)) + geom_line(size=1)+
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 3, alpha=0.1, colour = "black")+
  labs(color='Plant species')+
  geom_point(data = data_plant2, aes(x =  S_total, y = cv_1_visitation, color=Plant_gen_sp), size=3)+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+
  labs(x = "Richness of pollinator",y="")
 
p_cv_visit=p1+p2+ 
  plot_annotation(tag_levels = "A")& 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = -3.5, vjust = 1.5, size=15))& theme(legend.position = "bottom",legend.background = element_blank(),legend.box.background = element_rect(colour = "black"))
p_cv_visit+ plot_layout(guides = "collect")






# we analyze whether the stability of fruit set is affect 
#by stability of visitation rate
#incorporating site and plant species as random effects 

mod2_sta= lmer(cv_1_fruit ~ cv_1_visitation + (1|Plant_gen_sp)+(1|Site_ID), data = data_plant2)
summary(mod2_sta)

plot_model(mod2_sta, type="pred",  show.data = T)
plot_model(mod2_sta, type = "re")
 
#getting effects 
effe3_mod_sta <-data.frame( effect("cv_1_visitation", mod2_sta, se = TRUE))
#plor model 
p3=ggplot(effe3_mod_sta, aes(cv_1_visitation, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = data_plant2, aes(x =  cv_1_visitation, y = cv_1_fruit, color=Plant_gen_sp), size=3)+
  labs(y = "fruit set stability",x="visitation rate stability")
 


# we analyze whether the stability of seed set is affect 
#by stability of visitation rate
#incorporating site and plant species as random effects 

mod3_sta= lmer(cv_1_seed ~ cv_1_visitation + (1|Plant_gen_sp)+(1|Site_ID), data = data_plant2)
summary(mod3_sta)

plot_model(mod3_sta, type="pred",  show.data = T)
plot_model(mod3_sta, type = "re")

#getting effects 
effe4_mod_sta <-data.frame( effect("cv_1_visitation", mod3_sta, se = TRUE))
#plot model 
p4=ggplot(effe4_mod_sta, aes(cv_1_visitation, fit)) + geom_line()+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2)+
  geom_point(data = data_plant2, aes(x =  cv_1_visitation, y = cv_1_seed, color=Plant_gen_sp), size=3)+
  labs(y = "seed set stability",x="visitation rate stability")



# we analysed the effect of richness and synchrony on stability of visitation rate
# per plant species separately
# stability ~ richness + synchrony per plant species 
sta_polli_data_plant<-data_plant2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+asyn_LM,data))) %>%
  summarize(tidy(mod))%>%
  ungroup()

sta_polli_glance<-data_plant2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+asyn_LM,data))) %>%
  summarize(glance(mod))%>%
  ungroup()

# model2 plot (cv_1_visitation~total richness)
sta_polli_data_plant.2 <- data_plant2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+asyn_LM,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "S_total"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line(size=1) +
                        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = 3, alpha=0.1, colour = "black")))



p1=sta_polli_data_plant.2$plots[[1]] + geom_point(data =data_plant2 %>% filter(Plant_gen_sp == "Cistus crispus"), 
  aes(x =  S_total, y = cv_1_visitation),color="coral3",fill="coral2", pch=21, size=2)+ 
  labs(x = "",y="Stability of visitation rate",subtitle = "Cistus crispus")+scale_x_continuous(limits = c(9, 16))+
 theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))
  

p2=sta_polli_data_plant.2$plots[[2]] + geom_point(data = data_plant2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), 
 aes(x =  S_total, y = cv_1_visitation),color="yellow4",fill="yellow4",pch=21, size=2)+ 
  labs(x = "",y=NULL,subtitle = "Cistus ladanifer")+scale_x_continuous(limits = c(3, 16))+ 
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))
 

p3=sta_polli_data_plant.2$plots[[3]] + geom_point(data = data_plant2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), 
  aes(x =  S_total, y = cv_1_visitation),color= "springgreen3",fill="springgreen3",pch=21, size=2)+ 
  labs(x = "Richness",y=NULL,subtitle = "Cistus salviifolius")+scale_x_continuous(limits = c(3, 20))+  
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))
              

p4=sta_polli_data_plant.2$plots[[4]] + geom_point(data = data_plant2 %>% filter(Plant_gen_sp == "Halimium halimifolium"), 
   aes(x =  S_total, y = cv_1_visitation), color= "cyan3",fill="cyan3", pch=21, size=2)+ 
  labs(x = "",y=NULL,subtitle = "Halimium halimifolium")+ scale_x_continuous(limits = c(4, 14))+ 
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))
  

p5=sta_polli_data_plant.2$plots[[5]] + geom_point(data = data_plant2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), 
  aes(x =  S_total, y = cv_1_visitation),color="hotpink",fill="hotpink",pch=21, size=2)+ 
  labs(x = "",y=NULL,subtitle = "Lavandula pedunculata")+  
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))
  

p6=sta_polli_data_plant.2$plots[[6]] + geom_point(data = data_plant2 %>% filter(Plant_gen_sp == "Lavandula stoechas"), 
 aes(x =  S_total, y = cv_1_visitation),color="hotpink",fill="hotpink",pch=21, size=2)+ 
  labs(x = "",y=NULL,subtitle = "Lavandula stoechas")+  
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))



# model2 plot (cv_1_visitation~syncLM)
sta_polli_data_plant.3 <- data_plant2 %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+asyn_LM,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "asyn_LM"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line(size=1) +
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high),linetype = 3, alpha=0.1, colour = "black")))




p1.1=sta_polli_data_plant.3$plots[[1]] + geom_point(data = data_plant2 %>% filter(Plant_gen_sp == "Cistus crispus"), 
   aes(x =  asyn_LM, y = cv_1_visitation),color="coral3",fill="coral2", pch=21, size=2)+ 
  labs(x = "",y="Stability of visitaion rate")+
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


p2.1=sta_polli_data_plant.3$plots[[2]] + geom_point(data = data_plant2 %>% filter(Plant_gen_sp == "Cistus ladanifer"), 
 aes(x =  asyn_LM, y = cv_1_visitation),color="yellow4",fill="yellow4",pch=21, size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))
 


p3.1=sta_polli_data_plant.3$plots[[3]] + geom_point(data = data_plant2 %>% filter(Plant_gen_sp == "Cistus salviifolius"), 
 aes(x =  asyn_LM, y = cv_1_visitation),color= "springgreen3",fill="springgreen3",pch=21, size=2)+ 
  labs(x = "Asynchrony of pollinators",y= NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))
  

p4.1=sta_polli_data_plant.3$plots[[4]] + geom_point(data = data_plant2 %>% filter(Plant_gen_sp == "Halimium halimifolium"), 
  aes(x =  asyn_LM, y = cv_1_visitation), color= "cyan3",fill="cyan3", pch=21, size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))

   

p5.1=sta_polli_data_plant.3$plots[[5]] + geom_point(data = data_plant2 %>% filter(Plant_gen_sp == "Lavandula pedunculata"), 
 aes(x =  asyn_LM, y = cv_1_visitation), color="hotpink",fill="hotpink",pch=21, size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))
  

p6.1=sta_polli_data_plant.3$plots[[6]] + geom_point(data = data_plant2 %>% filter(Plant_gen_sp == "Lavandula stoechas"), 
 aes(x =  asyn_LM, y = cv_1_visitation), color="hotpink",fill="hotpink",pch=21, size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))



jpeg('stability.jpg', width = 15, height = 7, units = 'in', res = 1200)

p1 +p2+p3+p4+p5+p1.1+p2.1+p3.1+p4.1+p5.1+plot_layout(ncol = 5)& theme(axis.title = element_text(face="bold"))
dev.off()


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
  aes(x =  cv_1_visitation, y = cv_1_fruit),color="coral3",fill="coral2", pch=21, size=2)+ 
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
