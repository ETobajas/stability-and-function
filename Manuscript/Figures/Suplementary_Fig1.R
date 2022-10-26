
library(tidyverse)
library(effects)
library(patchwork)

# stability data for five plant species present at least in five sites
# C.crispu, C. ladanifer, C. salviifolius, H.halimifolium and L. pedunculata

data_plant<- read.csv("Data/Stability_fivespecies.csv")
data_plant<-data_plant[,-1]


# Replace inf with NA
data_plant[sapply(data_plant, is.infinite)] <- NA

# We can express asynchrony as 1 - φ, 
#where φ is Loreau and de Mazancourt’s (2008)
data_plant$asynchrony= 1-data_plant$syncLM



# model plot (cv_1_visitation~S_total+asynchrony)
# plot for richness
plot_stabi_plant <- data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+asynchrony,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "S_total"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line(size=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = 3, alpha=0.1, colour = "black")))

#Cistus crispus
p1=plot_stabi_plant$plots[[1]] + geom_point(data =data_plant %>% filter(Plant_gen_sp == "Cistus crispus"), 
  aes(x =  S_total, y = cv_1_visitation),color="coral3",fill="coral2", pch=21, size=2)+ 
  labs(x = "",y="Stability of visitation rate",subtitle = "Cistus crispus")+scale_x_continuous(limits = c(9, 16))+
  theme_classic ()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))

#Cistus ladanifer
p2=plot_stabi_plant$plots[[2]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), 
  aes(x =  S_total, y = cv_1_visitation),color="yellow4",fill="yellow4",pch=21, size=2)+ 
  labs(x = "",y=NULL,subtitle = "Cistus ladanifer")+scale_x_continuous(limits = c(3, 16))+ 
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))

#Cistus salviifolius
p3=plot_stabi_plant$plots[[3]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), 
 aes(x =  S_total, y = cv_1_visitation),color= "springgreen3",fill="springgreen3",pch=21, size=2)+ 
  labs(x = "Richness",y=NULL,subtitle = "Cistus salviifolius")+scale_x_continuous(limits = c(3, 20))+  
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))

#Halimium halimifolium
p4=plot_stabi_plant$plots[[4]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), 
  aes(x =  S_total, y = cv_1_visitation), color= "cyan3",fill="cyan3", pch=21, size=2)+ 
  labs(x = "",y=NULL,subtitle = "Halimium halimifolium")+ scale_x_continuous(limits = c(4, 14))+ 
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))

#Lavandula pedunculata
p5=plot_stabi_plant$plots[[5]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), 
  aes(x =  S_total, y = cv_1_visitation),color="hotpink",fill="hotpink",pch=21, size=2)+ 
  labs(x = "",y=NULL,subtitle = "Lavandula pedunculata")+  
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))+theme( plot.subtitle = element_text(face = "italic"))




# model plot for asynchrony
plot_stabi_plant_2 <- data_plant %>%
  nest_by(Plant_gen_sp) %>%
  mutate(mod = list(lm(cv_1_visitation~S_total+asynchrony,data))) %>%
  mutate(mod1 = list(ggeffects::ggpredict(mod, terms = "asynchrony"))) %>%
  mutate(plots = list(ggplot(mod1, aes(x, predicted)) + geom_line(size=1) +
 geom_ribbon(aes(ymin = conf.low, ymax = conf.high),linetype = 3, alpha=0.1, colour = "black")))


#Cistus crispus
p1.1=plot_stabi_plant_2$plots[[1]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus crispus"), 
 aes(x =  asynchrony, y = cv_1_visitation),color="coral3",fill="coral2", pch=21, size=2)+ 
  labs(x = "",y="Stability of visitaion rate")+
  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


#Cistus ladanifer
p2.1=plot_stabi_plant_2$plots[[2]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus ladanifer"), 
  aes(x =  asynchrony, y = cv_1_visitation),color="yellow4",fill="yellow4",pch=21, size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


#Cistus salviifolius
p3.1=plot_stabi_plant_2$plots[[3]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Cistus salviifolius"), 
   aes(x =  asynchrony, y = cv_1_visitation),color= "springgreen3",fill="springgreen3",pch=21, size=2)+ 
  labs(x = "Synchrony of pollinators",y= NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


#Halimium halimifolium
p4.1=plot_stabi_plant_2$plots[[4]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Halimium halimifolium"), 
  aes(x =  asynchrony, y = cv_1_visitation), color= "cyan3",fill="cyan3", pch=21, size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


#Lavandula pedunculata
p5.1=plot_stabi_plant_2$plots[[5]] + geom_point(data = data_plant %>% filter(Plant_gen_sp == "Lavandula pedunculata"), 
 aes(x =  asynchrony, y = cv_1_visitation), color="hotpink",fill="hotpink",pch=21, size=2)+ 
  labs(x = "",y=NULL)+  theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA))


#join plot
p1+p2+p3+p4+p5+p1.1+p2.1+p3.1+p4.1+p5.1+plot_layout(ncol = 5)& theme(axis.title = element_text(face="bold"))
