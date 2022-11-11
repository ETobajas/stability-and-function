
library(tidyverse)
library(effects)
library(patchwork)

# stability data for five plant species present at least in five sites
# C.crispu, C. ladanifer, C. salviifolius, H.halimifolium and L. pedunculata

# stability data 
data_plant<- read.csv("Data/Stability.csv")
data_plant<-data_plant[,-1]


# eliminar plant species 
sp_out2=data_plant %>%
  group_by(Plant_gen_sp) %>%
  summarise(Unique_Id = n_distinct(Site_ID)) %>%
  filter(Unique_Id <= 4) %>%
  select(Plant_gen_sp) %>% pull(Plant_gen_sp)

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


# We can express asynchrony as 1 - φ, 
#where φ is Loreau and de Mazancourt’s (2008)
data_plant$asyn_LM= 1-data_plant$syncLM



# model plot (cv_1_visitation~S_total+asynchrony)
# plot for richness
plot_stabi_plant <- data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+asyn_LM,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "S_total"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line(size=1) +
 geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = 3, alpha=0.2, colour = "black")))

#Cistus crispus
p1=plot_stabi_plant$plots[[1]] + geom_point(data =data_plant %>% filter(Plant_gen_sp == "Cistus crispus"), 
  aes(x =  S_total, y = cv_1_visitation),color="#e41a1c", size=2)+ 
  labs(x = "",y="Stability of visitation rate")+scale_x_continuous(limits = c(9, 16))+
  scale_y_continuous(limits = c(-5,9.5))+
  theme_bw ()


#Cistus ladanifer
p2=plot_stabi_plant$plots[[2]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), 
  aes(x =  S_total, y = cv_1_visitation),color="#377eb8", size=2)+ 
  labs(x = "",y=NULL)+scale_x_continuous(limits = c(3, 16))+ 
  scale_y_continuous(limits = c(-1.55,4))+
  theme_bw()

#Cistus salviifolius
p3=plot_stabi_plant$plots[[3]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), 
  aes(x =  S_total, y = cv_1_visitation),color= "#4daf4a", size=2)+ 
  labs(x = "Richness",y=NULL)+scale_x_continuous(limits = c(3, 20))+  
  scale_y_continuous(limits = c(-3,6))+
  theme_bw()


#Halimium halimifolium
p4=plot_stabi_plant$plots[[4]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), 
  aes(x =  S_total, y = cv_1_visitation), color= "#984ea3", size=2)+ 
  labs(x = "",y=NULL)+ scale_x_continuous(limits = c(4, 14))+ 
  scale_y_continuous(limits = c(-0.7,3))+
  theme_bw()


#Lavandula pedunculata
p5=plot_stabi_plant$plots[[5]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), 
  aes(x =  S_total, y = cv_1_visitation),color="#ff7f00", size=2)+ 
  labs(x = "",y=NULL)+  
  scale_y_continuous(limits = c(-0.7,4.7))+
  theme_bw()




# model plot for asynchrony
plot_stabi_plant_2 <- data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+asyn_LM,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "asyn_LM"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line(size=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),linetype = 3, alpha=0.2, colour = "black")))


#Cistus crispus
p1.1=plot_stabi_plant_2$plots[[1]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus crispus"), 
  aes(x =  asyn_LM, y = cv_1_visitation),color="#e41a1c", size=2)+ 
  labs(x = "",y="Stability of visitaion rate",subtitle = "Cistus crispus")+
  scale_y_continuous(limits = c(-5,9.5))+
   theme_bw()+theme( plot.subtitle = element_text(face = "italic"))


#Cistus ladanifer
p2.1=plot_stabi_plant_2$plots[[2]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), 
  aes(x =  asyn_LM, y = cv_1_visitation),color="#377eb8", size=2)+ 
  labs(x = "",y=NULL,subtitle = "Cistus ladanifer")+ 
  scale_y_continuous(limits = c(-1.55,4))+
  theme_bw()+theme( plot.subtitle = element_text(face = "italic"))


#Cistus salviifolius
p3.1=plot_stabi_plant_2$plots[[3]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), 
  aes(x =  asyn_LM, y = cv_1_visitation),color= "#4daf4a", size=2)+ 
  labs(x = "Asynchrony of pollinators",y= NULL,subtitle = "Cistus salviifolius")+ 
  scale_y_continuous(limits = c(-3,6))+
  theme_bw()+theme( plot.subtitle = element_text(face = "italic"))


#Halimium halimifolium
p4.1=plot_stabi_plant_2$plots[[4]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), 
  aes(x =  asyn_LM, y = cv_1_visitation), color= "#984ea3", size=2)+ 
  labs(x = "",y=NULL,subtitle = "Halimium halimifolium")+ 
  scale_y_continuous(limits = c(-0.7,3))+
  theme_bw() +theme( plot.subtitle = element_text(face = "italic"))


#Lavandula pedunculata
p5.1=plot_stabi_plant_2$plots[[5]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), 
  aes(x =  asyn_LM, y = cv_1_visitation), color="#ff7f00", size=2)+ 
  labs(x = "",y=NULL,subtitle = "Lavandula pedunculata")+  
  scale_y_continuous(limits = c(-0.7,4.7))+
  theme_bw() +theme( plot.subtitle = element_text(face = "italic"))


#join plot
p1.1+p2.1+p3.1+p4.1+p5.1+p1+p2+p3+p4+p5+plot_layout(ncol = 5)& 
  theme(axis.title = element_text(face="bold"))&
  theme(text = element_text(size=30))
